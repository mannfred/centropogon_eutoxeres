library(DECIPHER)
library(here)
library(msa)
library(odseq)
library(stringi)
library(tidyverse)

# ------------ msa alignment

# 1. create unique ids for each flower (122)
# 2. remove problem flowers 18, 58, 76, 77, 83, 101, 107, 113
# 3. use split() to separate data by inflorescence (plant indiv)
# 4. run AAStringSet() on each of ten inflorescences
# 5. create 10 alignments
# 6. align alignments??

# rename stages to A:H
development_data_notreat <- 
  read.csv(here('Data/No_treat_compiled.csv')) %>% 
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
  slice(-(c(18, 58, 76, 77, 83, 101, 107, 113))) # remove rows with non-sequential develop data 
  

# --------------- split analysis

# split into individual dataframes for alignment
notreat_split <-
  split(notreat_stringset, notreat_stringset$indiv)

# create function that isolates 'seq' column and converts to AAStringSet
f <- function(x) x %>% pull(seq) %>% Biostrings::AAStringSet()

split_stringset <- lapply(notreat_split, f)


#identity substiution matrix from NCBI  ftp://ftp.ncbi.nih.gov/blast/matrices/
matchmatrix<-
  read.table(here("Data/match_matrix.txt")) %>% 
  as.matrix

colnames(matchmatrix)[24]<-"*" #gets turned into ".X" during read.table for some reason..

notreat_align <- 
  lapply(
    split_stringset, 
    msa::msaClustalW, 
      cluster="nj", #neighbour joining 
      maxiters = 1000,
      gapOpening = 1, #terminal gaps are not penalized
      gapExtension = 0.2, 
      substitutionMatrix = matchmatrix,
      type="protein" )

# visualize alignment
detail(notreat_align$`1`)

# compute consensus sequence
notreat_con <- 
  lapply(notreat_align,
msaConsensusSequence,
  type="upperlower", 
  thresh=c(4, 1), 
  ignoreGaps=TRUE)

# DECIPHER
notreat_con2 <-
  lapply(split_stringset,
         ConsensusSequence, threshold = 0.99, minInformation = 0.1, ignoreNonBases = FALSE     )



# -------------- not split

ss <- notreat_stringset %>% pull(seq) %>% AAStringSet()

# align
a1 <- msa::msaClustalW(
ss,
cluster="nj", #neighbour joining 
maxiters = 1000,
gapOpening = 100, #terminal gaps are not penalized
gapExtension = 1, 
substitutionMatrix = matchmatrix,
type="protein" )

# visualize alignment
detail(a1)

# detect outliers
m <- odseq(a1, distance_metric = "linear", B = 100, threshold = 0.0001)

# remove outliers
ss2 <-
  notreat_stringset %>% 
  mutate(outlier = m) %>% 
  filter(outlier == FALSE) %>% 
  pull(seq) %>% 
  AAStringSet ()

# re-align
a2 <- msa::msaClustalW(
  ss2,
  cluster="nj", #neighbour joining 
  maxiters = 1000,
  gapOpening = 100, #terminal gaps are not penalized
  gapExtension = 20, 
  substitutionMatrix = matchmatrix,
  type="protein" )
  

# consenesus seq
ConsensusSequence(ss2, threshold = 0.999, minInformation = 0.05, ignoreNonBases = TRUE, includeTerminalGaps
= TRUE )


# ----------- split early dev from late dev

# early development
early <- 
  notreat_stringset %>% 
  filter(grepl("A", seq)) %>% 
  filter(!grepl("D", seq)) %>% 
  pull(seq) %>% 
  AAStringSet()

# align
e1 <- msa::msaClustalW(
early,
cluster="nj", #neighbour joining 
maxiters = 1000,
gapOpening = 1, #terminal gaps are not penalized
gapExtension = 0.2, 
substitutionMatrix = matchmatrix,
type="protein" ) %>% 
  AAStringSet()

detail(e1)

# consensus seq 
ConsensusSequence(
  e1, 
  threshold = 0.999, 
  minInformation = 0.001, 
  ignoreNonBases = TRUE, 
  includeTerminalGaps = TRUE )


# late development
late <- 
  notreat_stringset %>% 
  filter(grepl("E", seq)) %>% 
  filter(!grepl("B", seq)) %>% 
  pull(seq) %>% 
  AAStringSet()

# align
l1 <- msa::msaClustalW(
late,
cluster="upgma", #neighbour joining 
maxiters = 1000,
gapOpening = 1, #terminal gaps are not penalized
gapExtension = 0.2, 
substitutionMatrix = matchmatrix,
type="protein" ) %>% 
  AAStringSet()

detail(l1)

# consensus seq 
ConsensusSequence(
  l1, 
  threshold = 0.999, 
  minInformation = 0.001, 
  ignoreNonBases = TRUE, 
  includeTerminalGaps = TRUE )

# AAAAAAAAAAAAAAAAAAAAAAAAAAAABBBBBBBBCCCCCCCDDDDDDDDD (early)
#                   (late)         BBCCCCCCCDDDDDDDDDDDEEFEXFFGGGHXHHHHHHHHHHHHHHHHHHHHHH
# stages E,F are ambiguous, as are G and H

# when E, F and G,H are combined: 
# AAAAAAAAAAAAAAAAAAAAAAAAAAAABBBBBBBBCCCCCCCDDDDDDDDD
#                                      CCDDDCXXDDDDDDDEEEEEFFFFFFFFFFFFFFFFFFFFFFFFFFF

# when stages 3,4 = C, 5,6 = D, and 7,8 = E
# AAAAAAAAAAAAAAAAAAAAAAAAAAAABBBBCCCCCCCCCCCCCCCC
#                                 CCCCCCCCCCCCCCCDDDDDEEEEEEEEEEEEEEEEEEEEEEEEEEE
# ~ 33 days of development
