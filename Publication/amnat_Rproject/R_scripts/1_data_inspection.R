library(effectsize)
library(emmeans)
library(here)
library(tidyverse)

# -----------------------
# import data

# Bird_exclusion_exp_2017.xslx ->
# broken into many .csvs (Bird_cage_x.csv, No_treat_x.csv) ->
# compiled (averaged) into means_bird_cage.csv and means_no_treat.csv -> (but did not consider that data was censored! I should maybe get these values AFTER the survival analysis) 
# combined into means_melted.csv 

# PROBLEM: how to estimate time per stage when both ends
# of each flowers' data is left and right censored?

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




# --------------------------------------------------
# does pollinator exclusion affect flower production?

control <- read.csv(file = here('Data/No_treat_compiled.csv'))
treatment <- read.csv(file = here('Data/Bird_cage_compiled.csv'))
unique(control$indiv_flower)
unique(treatment$indiv_flower)

control_flowers <- c(17, 4, 15, 4, 5, 7, 17, 24, 24, 5)
treatmt_flowers <- c(12, 7, 11, 6, 19, 12)


# is data normal?
plot(density(control_flowers))
plot(density(treatmt_flowers))

shapiro.test(control_flowers)
shapiro.test(treatmt_flowers)

# are means comparable between treatments?
wilcox.test(control_flowers, treatmt_flowers, paired = FALSE, alternative = "two.sided")
mean(control_flowers) #12.2
mean(treatmt_flowers) #11.2
sqrt(var(control_flowers)) #8.1
sqrt(var(treatmt_flowers)) #4.6

numberof_flowers <- data.frame(treatment = c(rep("control", length(control_flowers)),
                         rep("treatmt", length(treatmt_flowers))),
           flowers = c(control_flowers, treatmt_flowers))

nof_model <- lm(flowers ~ treatment, data = numberof_flowers)
pairs(emmeans(nof_model, "treatment"))

cohens_f(nof_model)


# ---------------------------------------
# estimate average duration for each stage

# bird cage data
# group stages as A:D
devbc <- 
  read.csv(here('Data/Bird_cage_compiled.csv')) %>% 
  mutate(new_stage = 
           case_when(stage == 1 ~ "A",
                     stage == 2 ~ "B",
                     stage == 3 ~ "B",
                     stage == 4 ~ "B",
                     stage == 5 ~ "C",
                     stage == 6 ~ "C",
                     stage == 7 ~ "D",
                     stage == 8 ~ "D"))

# estimate average duration (days)
# for each stage
devbc_avg <-
  devbc %>% 
  group_by(indiv_flower) %>% 
  count(new_stage) %>% 
  ungroup() %>% 
  group_by(new_stage) %>% 
  summarise(mean = mean(n)) 

# estimate variance
devbc_std <-
  devbc %>% 
  group_by(indiv_flower) %>% 
  count(new_stage) %>% 
  ungroup() %>% 
  group_by(new_stage) %>% 
  summarise(std = var(n))


