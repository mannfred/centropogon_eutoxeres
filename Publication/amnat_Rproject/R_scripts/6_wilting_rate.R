library(broom)
library(here)
library(lubridate)
library(tidyverse)

# 1. group by indiv_flower
# 2. find earliest date that stage = 7
# 3. find next date that stage = 7
# 4. count number of days between 2) and 3). 

# bird cage data
# for each inflorescence, 
# calculate the number of days between wilting events 
bc_data <- 
  read.csv(here('Data/Bird_cage_compiled.csv')) %>%
  as_tibble() %>% 
  filter(stage == 7) %>% #stage 7 = E = wilting (remove other stages)
  group_by(indiv_flower) %>% 
  arrange(date) %>% 
  summarise(min_date = head(date, 1)) %>% 
  mutate(min_date = ymd(min_date),  # for lubridate
         indiv_ID = as.numeric(str_split(indiv_flower, '_', simplify=T)[,1]), #create ID for each individual
         flower_ID = as.numeric(str_split(indiv_flower, '_', simplify=T)[,2])) %>%  #create ID for each flower
  group_by(indiv_ID)  %>% # group by individual inflorescences
  arrange(min_date, .by_group = TRUE) %>% 
  mutate(elapsed_days = min_date - min(min_date), # days since first wilting event
         wilt_number = row_number()) %>% # number of wilt events per inflorescence
  filter(indiv_ID != 4 & indiv_ID != 1) # remove individuals 1 and 4 because there's <3 data points

  
ggplot(data = bc_data, aes(x = as.numeric(elapsed_days), y = wilt_number, color = factor(indiv_ID), group = indiv_ID)) +
  geom_point(size = 4) +
  stat_smooth(method = 'lm', fullrange = TRUE, se = FALSE) +
  mytheme


# control data
# for each inflorescence, 
# calculate the number of days between wilt events 
ct_data <- 
  read.csv(here('Data/No_treat_compiled.csv')) %>%
  as_tibble() %>% 
  filter(stage == 7) %>% #stage 7 = E = wilting (remove other stages)
  group_by(indiv_flower) %>% 
  arrange(date) %>% 
  summarise(min_date = head(date, 1)) %>% 
  mutate(min_date = ymd(min_date),  # for lubridate
         indiv_ID = as.numeric(str_split(indiv_flower, '_', simplify=T)[,1]), #create ID for each individual
         flower_ID = as.numeric(str_split(indiv_flower, '_', simplify=T)[,2])) %>%  #create ID for each flower
  group_by(indiv_ID)  %>% # group by individual inflorescences
  arrange(min_date, .by_group = TRUE) %>% 
  mutate(elapsed_days = min_date - min(min_date), # days since first wilt event
         wilt_number = row_number()) %>% # number of wilt events per inflorescence
  filter(indiv_ID != 2 & indiv_ID != 4 & indiv_ID != 6 & indiv_ID != 7 & indiv_ID != 10) # remove individuals 2,4,6,7 because they have <5 data points


ggplot(data = ct_data, aes(x = as.numeric(elapsed_days), y = wilt_number, color = factor(indiv_ID), group = indiv_ID)) +
  geom_point(size = 4) +
  stat_smooth(method = 'lm', fullrange = TRUE, se = FALSE) +
  theme_bw()

# combine data and plot together
alldata <-
  full_join(bc_data, ct_data) %>% 
  ungroup() %>% 
  mutate(treatment = c(rep("treatment", 24), rep("control", 45))) %>% 
  mutate(indiv_treatment = paste(treatment, indiv_ID)) 

mycolours <- c(rep("#009E73", 24), rep("#E69F00", 45))

ggplot(data = alldata, aes(x = as.numeric(elapsed_days), y = wilt_number, group = indiv_treatment)) +
  geom_point(size = 4, color = mycolours) +
  stat_smooth(method = 'lm', fullrange =TRUE, se = FALSE, aes(color = mycolours)) +
  mytheme 




# ---------------------------------------------------------
# fitting linear models using `broom`

# pollinator-excluded models
bc_models <-
  do(bc_data, glance(lm(wilt_number ~ as.numeric(elapsed_days), data = .)))

bc_models2 <- 
  do(bc_data, tidy(lm(wilt_number ~ as.numeric(elapsed_days), data = .))) %>% 
  filter(term != "(Intercept)") # just interested in slopes


# control models
ct_models <-
  do(ct_data, glance(lm(wilt_number ~ as.numeric(elapsed_days), data = .))) 

ct_models2 <- 
  do(ct_data, tidy(lm(wilt_number ~ as.numeric(elapsed_days), data = .))) %>% 
  filter(term != "(Intercept)") # just interested in slopes


# combine model parameters into one dataframe
allmodels <-
  full_join(bc_models, ct_models) %>% 
  ungroup() %>% 
  mutate(treatment = c(rep("treatment", 4), rep("control", 6))) %>% 
  mutate(slope = c(bc_models2$estimate, ct_models2$estimate)) %>% 
  mutate(unique_ID = row_number())

# mean and std dev of R^2
mean(allmodels$adj.r.squared) #0.899
sqrt(var(allmodels$adj.r.squared))#0.069

# does adjusted R^2 vary between individuals
summary(aov(adj.r.squared ~ unique_ID, data = allmodels))

# wilting rate vary between individuals?
summary(aov(slope ~ unique_ID, data = allmodels))

# does wilting rate vary between treatments?
summary(aov(slope ~ treatment, data = allmodels))

lmerTest::lmer(slope ~ treatment + (1|unique_ID), data = allmodels)


