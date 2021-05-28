library(here)
library(tidyverse)

# filtering out censored data, i.e.
# we only want data from indivuals who went through *all* of stages 5 (C1) and 6 (C2)
# this problem is posed in "1_data_inspection.R" 

# group by flower, then keep groups that have 4 AND 5 AND 6 AND 7
bcdata <- 
  read.csv(here('Data/Bird_cage_compiled.csv')) %>%
  as_tibble() %>% 
  group_by(indiv_flower) %>% 
  filter(any(stage == 4) & any(stage == 5) & any(stage == 6) & any(stage ==7)) %>% 
  filter(stage == 5 | stage == 6) %>% 
  count(indiv_flower, stage) %>%  # count the instances of stage 5 and 6 per individual
  mutate(treatment = 'pollinator_excluded') 

ctdata <- 
  read.csv(here('Data/No_treat_compiled.csv')) %>%
  as_tibble() %>% 
  group_by(indiv_flower) %>% 
  filter(any(stage == 4) & any(stage == 5) & any(stage == 6) & any(stage ==7)) %>% 
  filter(stage == 5 | stage == 6) %>% 
  count(indiv_flower, stage)  %>% # count the instances of stage 5 and 6 per individual
  mutate(treatment = 'control')

mydata <- full_join(ctdata, bcdata)
boxcolours <- c("#009E73", "#E69F00")
ptcolours <-c(rep("#009E73", 80), rep("#E69F00", 40))


ggplot(data = mydata) + 
  theme_minimal() +
  geom_boxplot(aes(x=factor(stage), y=n, color=ptcolours), position=position_dodge(0.8), size=1, alpha=0.7, outlier.shape = NA) +
  geom_jitter(aes(x=factor(stage), y=n, colour=ptcolours), position=position_jitterdodge(jitter.width = 0.25, jitter.height = 0.07), size = 4, alpha=0.9) +

  theme_bw() +
  theme(
    panel.border = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), 
    axis.line = element_line(colour = "black"))

# ---------------------------------
# fit models

library(lmerTest)
library(emmeans)

model1 <- lm(n ~ treatment*stage, data=mydata)
summary(model1) #no effect of treatment*stage on elapsed days


# no effect of treatment on elapsed days even when stages considered separately
tukey_results<-
  emmeans(model1, list(pairwise ~ treatment*stage), adjust = "tukey")




