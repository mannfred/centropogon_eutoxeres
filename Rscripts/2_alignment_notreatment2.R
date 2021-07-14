library(DECIPHER)
library(here)
library(msa)
library(stringi)
library(tidyverse)

# import pollinator-excluded data
development_data_ct <- 
  read.csv(here('Data/No_treat_compiled.csv')) %>% 
  mutate(stage = as.character(stage))  # for replace() below


# create AAStringSet 
ct_stringset <- 
  development_data_ct %>% 
  group_by(indiv_flower) %>% 
  mutate(days = row_number()) %>% 
  pivot_wider(indiv_flower,
              names_from = days,
              values_from = stage) %>% 
  replace(., is.na(.), "") %>% 
  unite(seq, 2:34, sep="", remove=FALSE) %>% # unites stages from columns 2:34 into a single seq
  mutate(indiv = stri_extract_first_regex(indiv_flower, "[0-9]+")) %>% # allows alignment by indiv
  ungroup() %>% 
  dplyr::slice(-(c(18, 58, 76, 77, 83, 101, 107, 113))) # remove rows with non-sequential develop data (discovered by visual inspection of $seq)


# --------------------------------------
# length of stages for boxplot ("3_alignment_plotting.R")

ct_length <- list()

for (i in 1:8){
  ct_length[[i]] <-
    ct_stringset %>% 
    dplyr::select(seq) %>% 
    str_extract_all(., paste0("[", i, "]", "+")) %>% # extract only A stage
    unlist() %>% 
    data.frame(stage = ., length = nchar(.))
  
}

# convert to a data.frame
ct_stagelength <- 
  do.call(rbind, lapply(ct_length, as.data.frame)) %>% 
  mutate(treatment = 'control') %>% 
  mutate(stage2 =  substr(.$stage, start=1, stop=1)) 


saveRDS(ct_stagelength, file=here('Data/derived_data/ct_stagelength.rds'))
