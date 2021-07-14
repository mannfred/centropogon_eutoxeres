library(effectsize)
library(here)
library(lubridate)
library(tidyverse)

# bird cage data
# for each inflorescence, 
# calculate the number of days between anthesis events 
# then count the number of open flowers, the number of senesced flowers
# and subtract to calculate the number of flowers open on a given day

# first, count open flowers
bc_open <- 
  read.csv(here('Data/Bird_cage_compiled.csv')) %>%
  as_tibble() %>% 
  filter(stage == 5) %>% #stage 5 = C1 = anthesis (remove other stages)
  group_by(indiv_flower) %>% 
  arrange(date) %>% 
  summarise(min_date = head(date, 1)) %>% # find earliest occurrence of event (onset of stage 5)
  mutate(min_date = ymd(min_date),  # for lubridate
         indiv_ID = as.numeric(str_split(indiv_flower, '_', simplify=T)[,1]), #create ID for each individual
         flower_ID = as.numeric(str_split(indiv_flower, '_', simplify=T)[,2])) %>%  #create ID for each flower
  group_by(indiv_ID)  %>% # group by individual inflorescences
  arrange(min_date, .by_group = TRUE) %>% 
  mutate(elapsed_days = min_date - min(min_date), # days since first anthesis event
         flower_number = row_number()) %>% # number of flowers per inflorescence
  filter(indiv_ID != 4) %>% # remove individual 4 because they have <4 data points
  mutate(rate = flower_number - dplyr::lag(flower_number, default = 0)) %>% 
  select(2,3,7) 
         

# second, count closed flowers
bc_close <-
  read.csv(here('Data/Bird_cage_compiled.csv')) %>%
  as_tibble() %>% 
  filter(stage == 7) %>% #stage 7 = D1 = senescence (remove other stages)
  group_by(indiv_flower) %>% 
  arrange(date) %>% 
  summarise(min_date = head(date, 1)) %>% # find earliest occurrence of event (onset of stage 5)
  mutate(min_date = ymd(min_date),  # for lubridate
         indiv_ID = as.numeric(str_split(indiv_flower, '_', simplify=T)[,1]), #create ID for each individual
         flower_ID = as.numeric(str_split(indiv_flower, '_', simplify=T)[,2])) %>%  #create ID for each flower
  group_by(indiv_ID)  %>% # group by individual inflorescences
  arrange(min_date, .by_group = TRUE) %>% 
  mutate(elapsed_days = min_date - min(min_date), # days since first anthesis event
         flower_number = row_number()) %>% # number of flowers per inflorescence
  filter(indiv_ID != 4) %>% # remove individual 4 because they have <4 data points
  mutate(rate = -1*(flower_number - dplyr::lag(flower_number, default = 0))) %>% 
  select(2,3,7) 

# merge
bc_cycle <- 
  full_join(bc_open, bc_close) %>% 
  arrange(min_date, .by_group = TRUE) %>% 
  mutate(sum = cumsum(rate),
         date = lubridate::ymd(min_date),
         elapsed_days = as.numeric(date - min(date)))

# plot
mytheme <- 
  theme_bw()  +
  theme(
    panel.border = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), 
    axis.line = element_line(colour = "black"))

ggplot(data = bc_cycle, aes(x = elapsed_days, y = sum, color = factor(indiv_ID), group = indiv_ID)) +
  geom_jitter(size=2, width=0.1, height=0.1) +
  geom_path(size=2,alpha=0.5) +
  facet_grid(indiv_ID~.) +
  mytheme
  

  

# ------------------------------
# control data
  

# first, count open flowers
ct_open <- 
  read.csv(here('Data/No_treat_compiled.csv')) %>%
  as_tibble() %>% 
  filter(stage == 5) %>% #stage 5 = C1 = anthesis (remove other stages)
  group_by(indiv_flower) %>% 
  arrange(date) %>% 
  summarise(min_date = head(date, 1)) %>% # find earliest occurrence of event (onset of stage 5)
  mutate(min_date = ymd(min_date),  # for lubridate
         indiv_ID = as.numeric(str_split(indiv_flower, '_', simplify=T)[,1]), #create ID for each individual
         flower_ID = as.numeric(str_split(indiv_flower, '_', simplify=T)[,2])) %>%  #create ID for each flower
  group_by(indiv_ID)  %>% # group by individual inflorescences
  arrange(min_date, .by_group = TRUE) %>% 
  mutate(elapsed_days = min_date - min(min_date), # days since first anthesis event
         flower_number = row_number()) %>% # number of flowers per inflorescence
  filter(indiv_ID != 6 & indiv_ID != 4 & indiv_ID != 7 & indiv_ID != 10) %>% # remove individuals 4,6,7,10 because they have <4 data points
  mutate(rate = flower_number - dplyr::lag(flower_number, default = 0)) %>% 
  select(2,3,7)  
  

  
# second, count closed flowers
ct_close <- 
  read.csv(here('Data/No_treat_compiled.csv')) %>%
  as_tibble() %>% 
  filter(stage == 7) %>% #stage 7 = D1 = senescence (remove other stages)
  group_by(indiv_flower) %>% 
  arrange(date) %>% 
  summarise(min_date = head(date, 1)) %>% # find earliest occurrence of event (onset of stage 7)
  mutate(min_date = ymd(min_date),  # for lubridate
         indiv_ID = as.numeric(str_split(indiv_flower, '_', simplify=T)[,1]), #create ID for each individual
         flower_ID = as.numeric(str_split(indiv_flower, '_', simplify=T)[,2])) %>%  #create ID for each flower
  group_by(indiv_ID)  %>% # group by individual inflorescences
  arrange(min_date, .by_group = TRUE) %>% 
  mutate(elapsed_days = min_date - min(min_date), # days since first anthesis event
         flower_number = row_number()) %>% # number of flowers per inflorescence
  filter(indiv_ID != 6 & indiv_ID != 4 & indiv_ID != 7 & indiv_ID != 10) %>% # remove individuals 4,6,7,10 because they have <4 data points
  mutate(rate = -1*(flower_number - dplyr::lag(flower_number, default = 0))) %>% 
  select(2,3,7)  
  
  
# merge
ct_cycle <- 
  full_join(ct_open, ct_close) %>% 
  arrange(min_date, .by_group = TRUE) %>% 
  mutate(sum = cumsum(rate),
         date = lubridate::ymd(min_date),
         elapsed_days = as.numeric(date - min(date)))
  
ggplot(data = ct_cycle, aes(x = elapsed_days, y = sum, color = factor(indiv_ID), group = indiv_ID)) +
  geom_jitter(size=2, width=0.1, height=0.1) +
  geom_path(size=2,alpha=0.5) +
  facet_grid(indiv_ID~.) +
  mytheme
  
# fit splines
# knots: https://stats.stackexchange.com/questions/7316/setting-knots-in-natural-cubic-splines-in-r
# stat_smooth(method = 'lm', formula = y ~ splines::bs(x, knots=seq(1,29,2), Boundary.knots = c(0,30), degree=2), se= FALSE) + 
  
# ---------------------------
# plot cycles on same graph

alldata <- 
  full_join(bc_cycle, ct_cycle) %>% 
  ungroup() %>% 
  mutate(treatment = c(rep('treatment', 60), rep('control', 87))) %>% 
  mutate(grouptreatment = paste(treatment, indiv_ID, sep='_'))

ggplot(data = alldata, aes(x = elapsed_days, y = sum, color = factor(treatment), group = grouptreatment)) +
  geom_jitter(size=2, width=0.1, height=0.1) +
  geom_path(size=2,alpha=0.5) +
  facet_grid(grouptreatment ~.) +
  mytheme

# ------------------------------------------
# calculate number of flowers open on any given day


# shift sums so that min = 0
alldata$sum[c(61:77, 124:147)] <- alldata$sum[c(61:77, 124:147)] + 2
alldata$sum[86:99] <- alldata$sum[86:99] + 1

# treatment has no effect on #flowers open 
lmerTest::lmer(sum ~ treatment + (1|grouptreatment), data=alldata) %>% summary

