# motivation: fit splines to ventral bird bills and flowers
# then, compute curvature

# https://www.inaturalist.org/observations/29598196
# https://www.inaturalist.org/observations/20837525
# https://www.inaturalist.org/observations/51833952
# https://www.inaturalist.org/observations/59910938
# https://www.inaturalist.org/observations/68673336

library(curvr)
library(geomorph)
library(here)
library(Momocs)
library(tidyverse)

# -----------------------------------
# import landmark data


# import ventral landmarks
lm_data <- readland.tps(here("Data/raw_data/centropogon_pollination/graphical_abstract.TPS"), readcurves=T, specID='imageID')

# Momocs::Ldk() converts to Coo object
ventral <-
  lm_data[10:18,,] %>%
  Ldk()

# simplify nested coo objects
ventral_simple <- unlist(ventral, recursive=F)


# ------------------------------------------
# estimate total curvature

# extract the lower and upper bounds from b[5] and b[1], respectively
ventral_baselines <-
  ventral_simple %>%
  lapply(., function(b) c(unlist(b[9])[1], unlist(b[1])[1]))


# fit interpolating splines and compute total curvature
# units are in degrees per unit length
# taxa are ordered alphabetically (unlike shape data)
ventral_curvature <-
  mapply(curvature_spline, ventral_simple, ventral_baselines, 'smooth') %>%
  enframe() %>%
  mutate(total_K = abs(value)*(180/pi))

ventral_tangle <-
  mapply(curvature_tangle, ventral_simple, 5) %>% 
  enframe() %>%
  mutate(total_K = abs(value)*(180/pi))

ventral_tangle$name <- dimnames(lm_data)[[3]]
# reassign taxonomic names
ventral_curvature$name <- dimnames(lm_data)[[3]]



# ----------------------------------
# plotting splines
coords <- matrix()
s0 <- vector()

for (i in 1:length(ventral_simple)){
  # test coords
  coords <- ventral_simple[[i]]
  
  s0 <- smooth.spline(coords) # or smooth.spline() for "smoothing spline"
  
  # plot
  plot(coords)
  lines(s0, col='red', lw=2)
  title(main = paste('specimen #', i, sep=""))
}

