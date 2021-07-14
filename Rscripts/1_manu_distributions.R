library(here)
library(rgbif)
library(scrubr)
library(spocc)
library(tidyverse)

# ------------------------------------------------------------------
# get plant occurrences from Cusco, Madre de Dios, Ucayali, and Puno


# plant list compiled from Tropicos
names  <- 
  read.csv(here('Data/raw_data/species_list_Manu.csv'), header=TRUE) 

# fetch occurrence records from GBIF
plants <- 
  spocc::occ(query = names$species[1:19], from = 'gbif', limit = 1000, has_coords=T,
             geometry = 'POLYGON((-74.35 -7.33, -76.97 -9.60, -69.53 -16.07, -69.02 -11.93, -74.35 -7.33))')


# save raw data
saveRDS(plants, file = here('Data/derived_data/plant_occurrences.rds'))



# ----------------------------------------

plants <- readRDS(file = here('Data/derived_data/plant_occurrences.rds'))

# simplfy data structure
plants_simple <- plants$gbif$data

# count number of records per species
sapply(plants_simple, nrow)

useful_variables <- c("name", "longitude", "latitude", "basisOfRecord", "acceptedScientificName")

plants_combined <-
  reduce(plants_simple, full_join, by = useful_variables) %>% 
  select(useful_variables)

# clean records
plants_fil <-
  plants_combined %>%
  filter(acceptedScientificName %in% names$gbif_name) %>%
  scrubr::dedup() %>% 
  scrubr::coord_impossible() %>%
  coord_incomplete() %>%
  coord_unlikely() 

# fetch elevation data from GeoNames.org
plants_fil$elevation <-
  rgbif::elevation(latitude = plants_fil$latitude,
                   longitude = plants_fil$longitude,
                   elevation_model = 'srtm1',
                   username = 'masahiroasada')$elevation_geonames

# fix elevation entry estimated from latlong
plants_fil$elevation <- as.numeric(gsub(-1, 0, plants_fil$elevation))
 

# save RDS
saveRDS(plants_fil, file = here('Data/derived_data/plant_occurrences_welevation.rds'))



# -------------------------------------------------------------------
# get Eutoxeres records from Huánuco, Cusco, Madre de Dios, Ucayali, and Puno

hermits <- 
  spocc::occ(query = c("Eutoxeres condamini", "Eutoxeres aquila", "Phaethornis guy"), from = 'gbif', limit = 1000, has_coords = TRUE,
             geometry = 'POLYGON((-74.35 -7.33, -76.97 -9.60, -69.53 -16.07, -69.02 -11.93, -74.35 -7.33))')

# check for White-tipped Sicklebill
hermits$gbif$data$Eutoxeres_aquila #none

# save raw data
saveRDS(hermits, file = here('Data/derived_data/hermit_occurrences.rds'))

# simplify data structure
hermits_simple <- hermits$gbif$data
 
# drop empty E. aquila dataframe 
hermits_combined <-
  reduce(hermits_simple[c(1,3)], full_join, by = useful_variables) %>% 
  select(useful_variables)

# simplify and clean data structure
birdnames <- c("Eutoxeres condamini (Bourcier, 1851)", "Phaethornis guy (Lesson, 1833)")

hermits_fil <- 
  hermits_combined %>% 
  filter(acceptedScientificName %in% birdnames) %>% 
  scrubr::dedup() %>% 
  scrubr::coord_impossible() %>%
  coord_incomplete() %>%
  coord_unlikely() 
  

hermits_fil$elevation <-
  rgbif::elevation(latitude = hermits_fil$latitude,
                   longitude = hermits_fil$longitude,
                   elevation_model = 'srtm1',
                   username = 'masahiroasada')$elevation_geonames

# fix elevation entry estimated from latlong
hermits_fil$elevation <- as.numeric(gsub(-1, 0, hermits_fil$elevation))

# saveRDS
saveRDS(hermits_fil, file = here('Data/derived_data/hermit_occurrences_welevation.rds'))


# ----------------------------------------
# combine and clean data

# import data
plants2 <- 
  readRDS(file = here('Data/derived_data/plant_occurrences_welevation.rds')) %>% 
  rename(species = acceptedScientificName)

hermits2 <- 
  readRDS(file = here('Data/derived_data/hermit_occurrences_welevation.rds')) %>% 
  rename(species = acceptedScientificName)

# original list of names and groups
names  <- 
  read.csv(here('Data/raw_data/species_list_Manu.csv'), header=TRUE) 

# combine plant and hummingbird data
manudata <- rbind(plants2, hermits2) 

# assign genera for downstream grouping
manudata <-
  manudata %>% 
  mutate(genus = case_when(species %in% names$gbif_name[1:6] ~ "Centropogon",
                           species %in% names$gbif_name[7:11] ~ "Heliconia",
                           species %in% names$gbif_name[12:15] ~ "Siphocampylus",
                           species %in% names$gbif_name[16:19] ~ "Heliconia subgen. Heliconia",
                           species %in% names$gbif_name[20] ~ "Eutoxeres",
                           species %in% names$gbif_name[21] ~ "Phaethornis"))
                             

# ----------------------------------------
# visualise
ggplot(data = manudata, aes(x = reorder(genus, elevation, FUN=median), y = elevation, fill = genus, colour = genus), group = genus) +
  geom_boxplot(alpha=0, lwd=2, fatten=1,outlier.size=0, outlier.alpha=0) +
  geom_jitter(shape=19, position=position_jitter(0.2), size = 4.5, alpha=0.9) +
  scale_fill_manual(values=c("#56B4E9", "#009E73", "#F0E442", "#E69F00", "#CC79A7", "#999999")) +
  scale_colour_manual(values=c("#56B4E9", "#009E73","#F0E442", "#E69F00", "#CC79A7", "#999999")) +
  theme_bw() +
  theme(
    panel.border = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), 
    axis.line = element_line(colour = "black"),
    axis.text.x = element_text(angle=90)) 
