library(flexsurv)
library(here)
library(stringi)
library(tidyverse)


# import control data
development_data_bc <- 
  read.csv(here('Data/derived_data/Bird_cage_compiled.csv')) %>% 
  mutate(stage = as.character(stage))  # for replace() below

# create AAStringSet 
# note: 17 non-censored indiv wilted or produced berries,
# 2 of them (11.7%) developed berries for ~18days before aborting
bc_stringset <- 
  development_data_bc %>% 
  group_by(indiv_flower) %>% 
  mutate(days = row_number()) %>% 
  pivot_wider(indiv_flower,
              names_from = days,
              values_from = stage) %>% 
  replace(., is.na(.), "") %>% 
  unite(seq, 2:34, sep="", remove=FALSE) %>% # unites stages from columns 2:34 into a single seq
  mutate(indiv = stri_extract_first_regex(indiv_flower, "[0-9]+")) %>% # allows alignment by indiv
  ungroup() 



dflist <- list()
for (i in 1:nrow(bc_stringset)){
  
  split2 <- str_split_fixed(bc_stringset$seq[i], pattern="", n=nchar(bc_stringset$seq[i])) %>% as.numeric()
  split1 <- str_split_fixed(bc_stringset$seq[i], pattern="(?<=(.))(?!\\1)", n=length(unique(split2))) %>% as.character() # solution from https://stackoverflow.com/questions/23523597/split-string-into-repeated-characters
  
  
  stage <- unique(split2)
  
  status <- numeric()
  for (j in 1:length(stage)){
    if (stage[j] == 9) {status[j-1] <- 1} #not censored
    else if (j == 1 | j == length(stage)) {status[j] <- 0} #censored
    else status[j] <- 1 #not censored
  }
  
  duration <- nchar(split1)
  
  indiv <- rep(bc_stringset$indiv_flower[i], length(split1))
  
  dflist[[i]] <- 
    data.frame(stage = stage, duration = duration, indiv = indiv) %>% 
    filter(stage != 9) %>% 
    mutate(status = status)
}


# flatten lists into one df
bc_df <- do.call(rbind, lapply(dflist, as.data.frame)) 

# flowers 1_9-10, 2_6-7, 3_8-10, 4_6, have non-censored stage 1 data
bc_df$status[c(33, 36, 67, 74, 105, 109, 113, 131)] <- 1

# saveRDS(bc_df, file=here('Data/derived_data/survdata_pollexcluded.rds'))

# Survival function derived estimates of median duration per stage
# "The Weibull and Gompertz distributions are appropriate when the hazard is always increasing or decreasing" (Bradburn etal 2003, Brit. J. Cancer)
bfit2 <- flexsurvreg(Surv(duration, status) ~ factor(stage), data=bc_df, dist='gompertz') #gom=604.206, weib=587.148, gam=572.674, llogis=565.037 



# median and 95% CIs
dflist2 <- summary(bfit2, type='median')

meds_df <- 
  do.call(rbind, lapply(dflist2, as.data.frame)) %>% 
  mutate(stage = str_extract(rownames(.), "[[:digit:]]+")) %>% 
  data.table::setorder(., stage) 


# now add medians and use error propagation to account for 95CIs
meds_df$cummed <- cumsum(meds_df$est)
meds_df$CI <- meds_df$ucl - meds_df$lcl


# estimate propagated CIs
meds_df$CIprop <- sqrt(cumsum(meds_df$CI^2))
meds_df$lclprop <- meds_df$cummed - (meds_df$CIprop/2)
meds_df$uclprop <- meds_df$cummed + (meds_df$CIprop/2)

# no appropriate data exists for pollinator excluded flowers,
# the only mature fruits observed were in development 
# *before* the experiment began
meds_df[8,] <- c(rep(NA,3), 8, rep(NA,5))

ggplot(data = meds_df, aes(x=stage, y=cummed)) +
  geom_point() + geom_errorbar(aes(ymin=lclprop, ymax=uclprop))

saveRDS(meds_df, file=here('Data/derived_data/medians_pollexcluded.rds'))
