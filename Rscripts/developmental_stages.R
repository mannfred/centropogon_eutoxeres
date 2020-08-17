library(here)
library(msa)
library(tidyverse)

# -----------------------
# import data

# Bird_exclusion_exp_2017.xslx ->
# broken into many .csvs (Bird_cage_x.csv, No_treat_x.csv) ->
# compiled (averaged) into means_bird_cage.csv and means_no_treat.csv -> (but did not consider that data was censored! I should maybe get these values AFTER the survival analysis) 
# combined into means_melted.csv 

# PROBLEM: how to estimate time per stage when both ends
# i.e. stage 1 and stage 8 are left and right censored, respectively?

# see: 'Clustered survival data' and
# see: https://stats.stackexchange.com/questions/199744/using-survival-analysis-with-multiple-events

mydata <- read.csv(here('Data/means_melted.csv'), header = TRUE)


as.factor(mydata$group)
mydata$stage<-as.factor(mydata$stage)
as.numeric(mydata$days)


ggplot(data = mydata, aes(x=stage, y=days)) + 
  theme_minimal() +
  geom_boxplot(aes(fill=group), alpha=0.7) +
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  scale_fill_brewer(palette="Dark2")


# how many days per stage?

# ------------ msa alignment

# rename stages to A:H
development_data_birdcage <- 
  read.csv(here('Data/Bird_cage_compiled.csv')) %>% 
  mutate(new_stage = 
           case_when(stage == 1 ~ "A",
                     stage == 2 ~ "B",
                     stage == 3 ~ "C",
                     stage == 4 ~ "D",
                     stage == 5 ~ "E",
                     stage == 6 ~ "F",
                     stage == 7 ~ "G",
                     stage == 8 ~ "H"))

# create AAStringSet for msa()
birdcage_stringset <- 
  development_data_birdcage %>% 
  group_by(indiv_flower) %>% 
  pivot_wider(indiv_flower,
              names_from = date,
              values_from = new_stage) %>% 
  replace(., is.na(.), "") %>% 
  unite(seq, 2:32, sep="", remove=FALSE) %>% #unites stages from columns 2:32 into a single seq
  pull(seq) %>% #isolate the seqs column
  AAStringSet() #creats AAStringSet for msa (from character vectors)

# give seqs names
#add names to stringset
names(birdcage_stringset) = 
  paste(birdcage_stringset %>% 
          pull(indiv_flower) %>%
          unique(.),
       sep="")

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
  ignoreGaps=FALSE)

consensusMatrix(birdcage_align)
