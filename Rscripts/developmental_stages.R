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


