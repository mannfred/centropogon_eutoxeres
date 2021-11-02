library(here)
library(tidyverse)

# 12 observations to 6 indivs
# taken from table s3 (study started on aug17)
indiv <- as.factor(c(1, 1, 1, 2, 1, 1, 3, 4, 5, 5, 5, 6))
date <- (c('aug18', 'aug22', 'aug22', 'aug22', 'aug23', 'aug24', 'aug25', 'aug26', 'sep15', 'sep16', 'sep19', 'sep19'))
studyday <- c(2, 6, 6, 6, 7, 8, 9, 10, 30, 31, 34, 34)
visit_count <- as.factor(c(1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1))
time <- c('pm', 'am', 'pm', 'am', 'am', 'am', 'pm', 'am', 'am', 'am', 'am', 'am')


visit_data <- data.frame(studyday, visit_count, time)

ggplot(data=visit_data, aes(x=studyday, y=visit_count, group=time)) +
  geom_point(aes(shape=time, colour=time), size=6, position=position_jitter(height=0, width=1), alpha=0.8) + 
  scale_colour_manual(values=c('#56B4E9', "#CC79A7")) +
  scale_shape_manual(values=c(17, 19)) +
  mytheme
