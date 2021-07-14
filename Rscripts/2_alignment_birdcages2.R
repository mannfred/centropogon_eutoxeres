library(DECIPHER)
library(here)
library(msa)
library(stringi)
library(tidyverse)

# import pollinator-excluded data
development_data_bc <- 
  read.csv(here('Data/Bird_cage_compiled.csv')) %>% 
  mutate(stage = as.character(stage))  # for replace() below
  

# create AAStringSet 
bc_stringset <- 
  development_data_bc %>% 
  group_by(indiv_flower) %>% 
  mutate(days = row_number()) %>% 
  pivot_wider(indiv_flower,
              names_from = days,
              values_from = stage) %>% 
  replace(., is.na(.), "") %>% 
  unite(seq, 2:34, sep="", remove=FALSE) %>% # unites stages from columns 2:34 into a single seq
  mutate(indiv = stri_extract_first_regex(indiv_flower, "[0-9]+")) %>% # allows alignment by indiv
  ungroup() %>% 
  dplyr::slice(-(c(34, 43, 62))) # remove rows with non-sequential develop data (discovered by visual inspection of $seq)


# --------------------------------------
# length of stages for boxplot ("3_alignment_plotting.R")

bc_length <- list()

for (i in 1:8){
 bc_length[[i]] <-
   bc_stringset %>% 
   dplyr::select(seq) %>% 
   str_extract_all(., paste0("[", i, "]", "+")) %>% # extract only A stage
   unlist() %>% 
   data.frame(stage = ., length = nchar(.))
   
}

# convert to a data.frame
bc_stagelength <- 
  do.call(rbind, lapply(bc_length, as.data.frame)) %>% 
  mutate(treatment = 'pollinator-excluded') %>% 
  mutate(stage2 =  substr(.$stage, start=1, stop=1)) 
  

saveRDS(bc_stagelength, file=here('Data/derived_data/bc_stagelength.rds'))
