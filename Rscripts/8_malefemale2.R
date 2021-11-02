library(flexsurv)
library(here)
library(survival)
library(tidyverse)

# import data
ct_df <- 
  readRDS(file=here('Data/derived_data/survdata_controls.rds')) %>% 
  mutate(treatment = rep('control', nrow(.)))


bc_df <- 
  readRDS(file=here('Data/derived_data/survdata_pollexcluded.rds')) %>% 
  mutate(treatment = rep('pollinator_excluded', nrow(.)))



# ---------------------------------------
# differences in male survival

surv_df <- full_join(ct_df, bc_df) %>% filter(stage == 5)

# no difference in duration of male stage (lrt=0.07, df=1, z=0.256, p=0.798)
survdiff(Surv(duration, status) ~ treatment, data=surv_df)
coxph(Surv(duration, status) ~ treatment, data=surv_df) #z = 0.235



# ---------------------------------------
# differences in female survival
surv_df <- full_join(ct_df, bc_df) %>% filter(stage == 6)

# no difference in duration of female stage (lrt=0.05, df=1, z=0.235, p=0.814)
survdiff(Surv(duration, status) ~ treatment, data=surv_df)
coxph(Surv(duration, status) ~ treatment, data=surv_df) 
