library(DECIPHER)
library(here)
library(msa)
library(stringi)
library(tidyverse)

# ------------ msa alignment

# rename stages to A:E
development_data_notreat <- 
  read.csv(here('Data/No_treat_compiled.csv')) %>% 
  mutate(new_stage = 
           case_when(stage == 1 ~ "A",
                     stage == 2 ~ "B",
                     stage == 3 ~ "B",
                     stage == 4 ~ "B",
                     stage == 5 ~ "C",
                     stage == 6 ~ "C",
                     stage == 7 ~ "D",
                     stage == 8 ~ "D"))

# create AAStringSet for msa()
notreat_stringset <- 
  development_data_notreat %>% 
  group_by(indiv_flower) %>% 
  mutate(days = row_number()) %>% 
  pivot_wider(indiv_flower,
              names_from = days,
              values_from = new_stage) %>% 
  replace(., is.na(.), "") %>% 
  unite(seq, 2:34, sep="", remove=FALSE) %>% # unites stages from columns 2:34 into a single seq
  mutate(indiv = stri_extract_first_regex(indiv_flower, "[0-9]+")) %>% # allows alignment by indiv
  ungroup() %>% 
  dplyr::slice(-(c(18, 58, 76, 77, 83, 101, 107, 113))) # remove rows with non-sequential develop data (discovered by visual inspection of $seq)
  


#identity substiution matrix from NCBI  ftp://ftp.ncbi.nih.gov/blast/matrices/
matchmatrix<-
  read.table(here("Data/match_matrix.txt")) %>% 
  as.matrix

colnames(matchmatrix)[24]<-"*" #gets turned into ".X" during read.table for some reason..


# ----------- split early dev from late dev

# early development
early <- 
  notreat_stringset %>% 
  filter(grepl("A", seq)) %>% # include earliest stage
  filter(!grepl("C", seq)) %>% # exculde mid-stage and beyond
  pull(seq) %>% 
  AAStringSet()

# align early stages
e1 <- 
  msa::msaClustalW(
    early,
    cluster="nj", #neighbour joining 
    maxiters = 1000,
    gapOpening = 1, #terminal gaps are not penalized
    gapExtension = 0.2, 
    substitutionMatrix = matchmatrix,
    type="protein" ) %>% 
  AAStringSet()

detail(e1)

# early stages consensus seq 
ConsensusSequence(
  e1, 
  threshold = 0.15, 
  ignoreNonBases = TRUE, 
  includeTerminalGaps = TRUE)


# late development
late <- 
  notreat_stringset %>% 
  filter(grepl("E", seq)) %>% 
  filter(!grepl("B", seq)) %>% 
  pull(seq) %>% 
  AAStringSet()

# align
l1 <- 
  msa::msaClustalW(
    late,
    cluster="nj", #neighbour joining 
    maxiters = 1000,
    gapOpening = 1, #terminal gaps are not penalized
    gapExtension = 0.2, 
    substitutionMatrix = matchmatrix,
    type="protein" ) %>% 
  AAStringSet()

detail(l1)

# late stages consensus seq 
ConsensusSequence(
  l1, 
  threshold = 0.15, 
  ignoreNonBases = TRUE, 
  includeTerminalGaps = TRUE)


# when stages 3,4 = C, 5,6 = D, and 7,8 = E
# AAAAAAAAAAAAAAAAAAAAAAAAAAAABBBBCCCCCCCCCCCCCCCC
#                                 CCCCCCCCCCCCCCCDDDDDEEEEEEEEEEEEEEEEEEEEEEEEEEE
#
# AAAAAAAAAAAAAAAAAAAAAAAAAAAA 28
# BBBB 4
# CCCCCCCCCCCCCCC 15
# DDDDD 5 
# EEEEEEEEEEEEEEEEEEEEEEEEEEE 27
#
# 79 days total

# run2


# AAAAAAAAAAAAAAAAAAAAAAAAAAAABBBBBBBBBBBBBBBBBBBBB
# CCCXXXXXDDDDDDDDDDDDDDXXXXXXDDDDDDD #84 days
# AAAAAAAAAAAAAAAAAAAAAAAAAAAA 28
# BBBBBBBBBBBBBBBBBBBBB 21
# CCCXXXXX 8
# DDDDDDDDDDDDDDXXXXXXDDDDDDD 27
