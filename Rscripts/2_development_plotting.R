library(here)
library(tidyverse)

# import median survival times for each treatment
bc_meds <- readRDS(file=here('Data/derived_data/medians_pollexcluded.rds'))
bc_meds$treatment <- rep('pollinator_excluded', nrow(bc_meds))


ct_meds <- readRDS(file=here('Data/derived_data/medians_controls.rds'))
ct_meds$treatment <- rep('controls', nrow(ct_meds))

med_data <- full_join(bc_meds, ct_meds) #used in Table S2

ggplot(data=med_data, aes(x=stage, y=cummed, colour=treatment)) +
  geom_point(position=position_dodge(width = 0.5), size=4) + 
  geom_errorbar(aes(ymin=lclprop, ymax=uclprop), size=1, position=position_dodge(width = 0.5)) +
  scale_colour_manual(values=c("#E69F00", "#009E73")) +
  mytheme
  
xmin_bc <- med_data$cummed[1:8] - med_data$cummed[1]
xmin_ct <- med_data$cummed[9:16] - med_data$cummed[9]

med_data$xmin <- c(0, med_data$cummed[1:7], 0, med_data$cummed[9:15])


ggplot(data=med_data, aes(x=cummed, y=stage, colour=treatment)) +
  geom_crossbar(aes(xmin=xmin, xmax=cummed, fill=treatment), fatten=0, width=0.6, position=position_dodge(width = 0.63)) +
  geom_errorbar(aes(xmin=lclprop, xmax=uclprop), size=1.5, width=0.35, position=position_dodge(width = 0.63)) +
  scale_fill_manual(values=c("#E69F00", "#009E73")) +
  scale_colour_manual(values=c("#f0c566", "#66c4ab")) + #tints from https://www.color-hex.com/
  scale_x_continuous(limits=c(0, 90)) +
  xlab("elapsed days") +
  mytheme

# difference is median durations of Stage H (berry development)
# numbers from 'med_data'
24.231619 - 7.195999 # 17.03562 days
sqrt((24.231619 - 19.761254)^2 + (7.195999 - 4.419347)^2) # 5.262505 = 95% CI


# --------------------------------------------------
# putting time on x axis and setting stage A as day 0

# set median estimates to start at 0
adj_cummed_bc <-
  med_data %>% 
  filter(treatment == 'pollinator_excluded') %>% 
  mutate(adj_cummed = cummed - est[1]) 
  
adj_cummed_ct <-
  med_data %>% 
  filter(treatment == 'controls') %>% 
  mutate(adj_cummed = cummed - est[1]) 

med_data$adj_cummed <- c(adj_cummed_bc[,11], adj_cummed_ct[,11])  

# set 95CIs to start at 0
med_data$adj_lcl  <- c(med_data$lclprop[1:8] - med_data$cummed[1], med_data$lclprop[9:16] - med_data$cummed[9])
med_data$adj_ucl  <- c(med_data$uclprop[1:8] - med_data$cummed[1], med_data$uclprop[9:16] - med_data$cummed[9])




ggplot(data=med_data, aes(x=adj_cummed, y=stage, colour=treatment)) +
  geom_point(position=position_dodge(width = 0.5), size=4) + 
  geom_errorbar(aes(xmin=adj_lcl, xmax=adj_ucl), size=1, position=position_dodge(width = 0.5)) +
  scale_colour_manual(values=c("#E69F00", "#009E73")) +
  mytheme
