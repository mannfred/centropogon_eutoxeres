library(effectsize)
library(emmeans)
library(fitdistrplus)
library(here)
library(tidyverse)

# filtering out censored data, i.e.
# we only want data from individuals who went through *all* of stages 5 (C1) and 6 (C2)
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

mydata <- 
  full_join(ctdata, bcdata) %>% 
  mutate(indiv_id = paste(str_split_fixed(indiv_flower, "_", n=3)[,1], treatment, sep=""))



ggplot(data = mydata, aes(x=factor(stage), y=n, colour=treatment)) + 
  geom_violin(position=position_dodge(0.8), size=1, alpha=0.6) +
  geom_jitter(position=position_jitterdodge(jitter.width = 0.2, jitter.height = 0.07), size = 4, alpha=0.9) +
  scale_colour_manual(values=c("#E69F00", "#009E73")) +
  mytheme

# ------------------------------------------------
# does treatment affect male-female stage duration?

# using a GLMM instead of Mann Whitney U test 
# based on: https://stats.stackexchange.com/questions/362272/how-to-account-for-multiple-measurements-of-same-person-in-either-two-group-comp


# filter for male and females stages
s5data <- mydata %>% filter(stage == 5) 
s6data <- mydata %>% filter(stage == 6)


# inspect distribution of data
fitdistrplus::descdist(s5data$n, discrete=T, boot=100) #poisson dist.
fitdistrplus::descdist(s6data$n, discrete=T, boot=100) #poisson dist.


# fit and visualize distributions
fitdist(s5data$n, "pois") %>% plot()
fitdist(s6data$n, "pois") %>% plot()


# fit model to male phase
m5 <- lme4::glmer(n ~ treatment + (1|indiv_id), data=s5data, family=poisson(link='log'))  
summary(m5) #p=0.217, t=1.234, df=57
z_to_d(1.234, 57) #0.33


# fit models to female phase
m6 <- lme4::glmer(n ~ treatment + (1|indiv_id), data=s6data, family=poisson(link='log'))  
summary(m6) #p=0.784, t=0.274, df=57
z_to_d(0.274, 57) #0.07



# estimate means for male phase
emmeans(m5, 'treatment', type='response')


# estimate means for female phase
emmeans(m6, 'treatment', type='response')








