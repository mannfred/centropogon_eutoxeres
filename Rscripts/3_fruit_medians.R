library(here)
library(tidyverse)

# import duration/status data from "2_development_*.R"
ct_df <- readRDS(here('Data/derived_data/survdata_controls.rds'))
bc_df <- readRDS(here('Data/derived_data/survdata_pollexcluded.rds'))

# filter for observations where fruit development completed (status = 0)
# note: though status=1 indicates that the event (abscission) was observed,
# these fruits tended to have been dropped prematurely, in this case
# using the non-censored data gives a better estimate of the time to fruit maturity
ct_fruit <- ct_df %>% filter(stage == 8 & status == 0)
bc_fruit <- bc_df %>% filter(stage == 8 & status == 0)

# the data to be used are from individuals marked 
# with "c" (for 'collected') in the raw data
ct_fruit <- ct_fruit %>% slice(11:13, 15:17, 21:22)
median(ct_fruit$duration) #24 days
sort(ct_fruit$duration)[qbinom(c(.025,.975), length(ct_fruit$duration), 0.5)] #4-30 days

# note: 
# no appropriate data exists for pollinator excluded flowers,
# the only mature fruits observed were in development *before* the experiment began

