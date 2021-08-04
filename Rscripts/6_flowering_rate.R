library(broom)
library(here)
library(lubridate)
library(tidyverse)

# 1. group by indiv_flower
# 2. find earliest date that stage = 5
# 3. find next date that stage = 5
# 4. count number of days between 2) and 3). 

# bird cage data
# for each inflorescence, 
# calculate the number of days between anthesis events 
bc_data <- 
  read.csv(here('Data/Bird_cage_compiled.csv')) %>%
  as_tibble() %>% 
  filter(stage == 5) %>% #stage 5 = C1 = anthesis (remove other stages)
  group_by(indiv_flower) %>% 
  arrange(date) %>% 
  summarise(min_date = head(date, 1)) %>% # earliest day of occurrence 
  mutate(min_date = ymd(min_date),  # for lubridate
         indiv_ID = as.numeric(str_split(indiv_flower, '_', simplify=T)[,1]), #create ID for each individual
         flower_ID = as.numeric(str_split(indiv_flower, '_', simplify=T)[,2])) %>%  #create ID for each flower
  group_by(indiv_ID)  %>% # group by individual inflorescences
  arrange(min_date, .by_group = TRUE) %>%
  mutate(elapsed_days = min_date - min(min_date), # days since first anthesis event
         flower_number = row_number()) %>% # number of flowers per inflorescence
  filter(indiv_ID != 4) # remove individual 4 because there's only one data point


ggplot(data = bc_data, aes(x = as.numeric(elapsed_days), y = flower_number, color = factor(indiv_ID), group = indiv_ID)) +
  geom_point(size = 4) +
  stat_smooth(method = 'lm', fullrange = TRUE, se = FALSE) +
  mytheme

# control data
# for each inflorescence, 
# calculate the number of days between anthesis events 
ct_data <- 
  read.csv(here('Data/No_treat_compiled.csv')) %>%
  as_tibble() %>% 
  filter(stage == 5) %>% #stage 5 = C1 = anthesis (remove other stages)
  group_by(indiv_flower) %>% 
  arrange(date) %>% 
  summarise(min_date = head(date, 1)) %>% 
  mutate(min_date = ymd(min_date),  # for lubridate
         indiv_ID = as.numeric(str_split(indiv_flower, '_', simplify=T)[,1]), #create ID for each individual
         flower_ID = as.numeric(str_split(indiv_flower, '_', simplify=T)[,2])) %>%  #create ID for each flower
  group_by(indiv_ID)  %>% # group by individual inflorescences
  arrange(min_date, .by_group = TRUE) %>% 
  mutate(elapsed_days = min_date - min(min_date), # days since first anthesis event
         flower_number = row_number()) %>% # number of flowers per inflorescence
  filter(indiv_ID != 6 & indiv_ID != 4 & indiv_ID != 7 & indiv_ID != 10) # remove individuals 4,6,7,10 because they have <4 data points


ggplot(data = ct_data, aes(x = as.numeric(elapsed_days), y = flower_number, color = factor(indiv_ID), group = indiv_ID)) +
  geom_point(size = 4) +
  stat_smooth(method = 'lm', fullrange = TRUE, se = FALSE) +
  mytheme

# combine data and plot together
alldata <-
  full_join(bc_data, ct_data) %>% 
  ungroup() %>% 
  mutate(treatment = c(rep("treatment", 33), rep("control", 43))) %>% 
  mutate(indiv_treatment = paste(treatment, indiv_ID)) 

mycolours <- c(rep("#009E73", 33), rep("#E69F00", 43))
ggplot(data = alldata, aes(x = as.numeric(elapsed_days), y = flower_number, group = indiv_treatment)) +
  geom_point(size = 4, color = mycolours) +
  stat_smooth(method = 'lm', fullrange =TRUE, se = FALSE, aes(color = treatment)) +
  scale_colour_manual(values=c("#E69F00", "#009E73")) +
  mytheme

# ---------------------------------------------------------
# fitting linear models using `broom`

# pollinator-excluded models
bc_models <-
  do(bc_data, glance(lm(flower_number ~ as.numeric(elapsed_days), data = .)))

bc_models2 <- 
  do(bc_data, tidy(lm(flower_number ~ as.numeric(elapsed_days), data = .))) %>% 
  filter(term != "(Intercept)") # just interested in slopes


# control models
ct_models <-
  do(ct_data, glance(lm(flower_number ~ as.numeric(elapsed_days), data = .))) 

ct_models2 <- 
  do(ct_data, tidy(lm(flower_number ~ as.numeric(elapsed_days), data = .))) %>% 
  filter(term != "(Intercept)") # just interested in slopes


# combine model parameters into one dataframe
allmodels <-
  full_join(bc_models, ct_models) %>% 
  ungroup() %>% 
  mutate(treatment = c(rep("treatment", 5), rep("control", 6))) %>% 
  mutate(slope = c(bc_models2$estimate, ct_models2$estimate)) %>% 
  mutate(unique_ID = row_number())

# mean and std err of R^2
mean(allmodels$adj.r.squared) #0.949
sqrt(var(allmodels$adj.r.squared))#0.036

# does adjusted R^2 vary between individuals
summary(lm(adj.r.squared ~ unique_ID, data = allmodels))


# mean and std err of flowering rate (slope) 
1/mean(ct_models2$estimate) # how many days bw anthesis events?
sqrt(var(ct_models2$estimate))

1/mean(ct_models2$estimate[-6]) # how many days bw anthesis events?
sqrt(var(ct_models2$estimate[-6]))

1/mean(bc_models2$estimate)
sqrt(var(bc_models2$estimate))

# does flowering rate vary between individuals?
summary(lm(slope ~ unique_ID, data = allmodels))

# does flowering rate vary between treatments?
summary(lm(slope ~ treatment, data = allmodels))
