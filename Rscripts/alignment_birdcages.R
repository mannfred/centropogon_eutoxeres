library(DECIPHER)
library(here)
library(msa)
library(stringi)
library(tidyverse)

# ------------ msa alignment

# rename stages to A:E
development_data_bc <- 
  read.csv(here('Data/Bird_cage_compiled.csv')) %>% 
  mutate(new_stage = 
           case_when(stage == 1 ~ "A",
                     stage == 2 ~ "B",
                     stage == 3 ~ "C",
                     stage == 4 ~ "C",
                     stage == 5 ~ "D",
                     stage == 6 ~ "D",
                     stage == 7 ~ "E",
                     stage == 8 ~ "E"))

# create AAStringSet for msa()
bc_stringset <- 
  development_data_bc %>% 
  group_by(indiv_flower) %>% 
  mutate(days = row_number()) %>% 
  pivot_wider(indiv_flower,
              names_from = days,
              values_from = new_stage) %>% 
  replace(., is.na(.), "") %>% 
  unite(seq, 2:34, sep="", remove=FALSE) %>% # unites stages from columns 2:34 into a single seq
  mutate(indiv = stri_extract_first_regex(indiv_flower, "[0-9]+")) %>% # allows alignment by indiv
  ungroup() %>% 
  slice(-(c(34, 43, 62))) # remove rows with non-sequential develop data (discovered by visual inspection of $seq)



#identity substiution matrix from NCBI  ftp://ftp.ncbi.nih.gov/blast/matrices/
matchmatrix<-
  read.table(here("Data/match_matrix.txt")) %>% 
  as.matrix

colnames(matchmatrix)[24]<-"*" #gets turned into ".X" during read.table for some reason..


# ----------- split early dev from late dev

# early development
early2 <- 
  bc_stringset %>% 
  filter(grepl("A", seq)) %>% # include earliest stage
  filter(!grepl("D", seq)) %>% # exculde mid-stage and beyond
  pull(seq) %>% 
  AAStringSet()

# align early stages
e2 <- 
  msa::msaClustalW(
    early2,
    cluster="nj", #neighbour joining 
    maxiters = 1000,
    gapOpening = 1, #terminal gaps are not penalized
    gapExtension = 0.2, 
    substitutionMatrix = matchmatrix,
    type="protein" ) %>% 
  AAStringSet()

detail(e2)

# early stages consensus seq 
ConsensusSequence(
  e2, 
  threshold = 0.6, 
  minInformation = 0.3, 
  ignoreNonBases = TRUE, 
  includeTerminalGaps = TRUE)


# late development
late2 <- 
  bc_stringset %>% 
  filter(grepl("E", seq)) %>% 
  filter(!grepl("B", seq)) %>% 
  pull(seq) %>% 
  AAStringSet()

# align
l2 <- 
  msa::msaClustalW(
    late2,
    cluster="upgma", #neighbour joining 
    maxiters = 1000,
    gapOpening = 1, #terminal gaps are not penalized
    gapExtension = 0.2, 
    substitutionMatrix = matchmatrix,
    type="protein" ) %>% 
  AAStringSet()

detail(l2)

# late stages consensus seq 
ConsensusSequence(
  l2, 
  threshold = 0.6, 
  minInformation = 0.3, 
  ignoreNonBases = TRUE, 
  includeTerminalGaps = TRUE)

# AAAAAAAAAAAAAAAAAAAAAAABBBBCCCCCCCCCCCCCC (early)
#                     (late) CCCCCCCCCCCCCCCDDDDDEEEEEEEEE

# AAAAAAAAAAAAAAAAAAAAAAA 23
# BBBB 4
# CCCCCCCCCCCCCC 14
# DDDDD 5
# EEEEEEEEE 9
#
# 41 days total

