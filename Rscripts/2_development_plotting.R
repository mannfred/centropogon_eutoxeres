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
  

# difference is median durations of Stage H (berry development)
# numbers from 'med_data'
24.231619 - 7.195999 # 17.03562 days
sqrt((24.231619 - 19.761254)^2 + (7.195999 - 4.419347)^2) # 5.262505 = 95% CI
