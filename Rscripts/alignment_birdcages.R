library(here)
library(msa)
library(tidyverse)

# ------------ msa alignment

# rename stages to A:H
development_data_birdcage <- 
  read.csv(here('Data/Bird_cage_compiled.csv')) %>% 
  mutate(new_stage = 
           case_when(stage == 1 ~ "A",
                     stage == 2 ~ "A",
                     stage == 3 ~ "B",
                     stage == 4 ~ "B",
                     stage == 5 ~ "C",
                     stage == 6 ~ "D",
                     stage == 7 ~ "E",
                     stage == 8 ~ "F"))

# create AAStringSet for msa()
birdcage_stringset <- 
  development_data_birdcage %>% 
  group_by(indiv_flower) %>% 
  mutate(days = row_number()) %>% 
  pivot_wider(indiv_flower,
              names_from = days,
              values_from = new_stage) %>% 
  replace(., is.na(.), "") %>% 
  unite(seq, 2:34, sep="", remove=FALSE) %>% #unites stages from columns 2:34 into a single seq
  pull(seq) %>% #isolate the seqs column
  AAStringSet() #creats AAStringSet for msa (from character vectors)

# preview
as.character(birdcage_stringset)


#identity substiution matrix from NCBI  ftp://ftp.ncbi.nih.gov/blast/matrices/
matchmatrix<-
  read.table(here("Data/match_matrix.txt")) %>% 
  as.matrix

colnames(matchmatrix)[24]<-"*" #gets turned into ".X" during read.table for some reason..

# multiple sequence alignment
birdcage_align <- 
  msa::msaClustalW(
    birdcage_stringset, 
    cluster="nj", #neighbour joining 
    maxiters = 1000,
    gapOpening = 100, #terminal gaps are not penalized
    gapExtension = 20, 
    substitutionMatrix = matchmatrix,
    type="protein")

# visualize alignment
detail(birdcage_align)

# compute consensus sequence
msaConsensusSequence(
  birdcage_align, 
  type="upperlower", 
  thresh=c(10, 0.01), 
  ignoreGaps=TRUE)

consensusMatrix(birdcage_align)