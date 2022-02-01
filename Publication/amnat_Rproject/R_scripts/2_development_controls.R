library(flexsurv)
library(here)
library(stringi)
library(tidyverse)


# import control data
development_data_ct <- 
  read.csv(here('Data/No_treat_compiled.csv')) %>% 
  mutate(stage = as.character(stage))  # for replace() below

# create AAStringSet 
ct_stringset <- 
  development_data_ct %>% 
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
for (i in 1:nrow(ct_stringset)){
  
  split2 <- str_split_fixed(ct_stringset$seq[i], pattern="", n=nchar(ct_stringset$seq[i])) %>% as.numeric()
  split1 <- str_split_fixed(ct_stringset$seq[i], pattern="(?<=(.))(?!\\1)", n=length(unique(split2))) %>% as.character() # solution from https://stackoverflow.com/questions/23523597/split-string-into-repeated-characters
  
 
  stage <- unique(split2)
  
  status <- numeric()
  for (j in 1:length(stage)){
    if (stage[j] == 9) {status[j-1] <- 1} #not censored
    else if (j == 1 | j == length(stage)) {status[j] <- 0} #censored
         else status[j] <- 1 #not censored
    }
  
  duration <- nchar(split1)
  
  indiv <- rep(ct_stringset$indiv_flower[i], length(split1))
  
  dflist[[i]] <- 
    data.frame(stage = stage, duration = duration, indiv = indiv) %>% 
    filter(stage != 9) %>% 
    mutate(status = status)
}

# flatten lists into one df
ct_df <- do.call(rbind, lapply(dflist, as.data.frame)) 

# flowers 1_17, 3_12-14, 7_11-12, 8_17-19, 9_18-20, 9_23 have non-censored stage 1 data
ct_df$status[c(56, 132, 135, 137, 237, 239, 306, 309, 313, 383, 387, 390, 398)] <- 1

# for use in '8_malefemale.rds'
# saveRDS(ct_df, file=here('Data/derived_data/survdata_controls.rds'))


# Survival function derived estimates of median duration per stage
# "The Weibull and Gompertz distributions are appropriate when the hazard is always increasing or decreasing" (Bradburn etal 2003, Brit. J. Cancer)
cfit2 <- flexsurvreg(Surv(duration, status) ~ factor(stage), data=ct_df, dist='gompertz') #gom=1293.126, weib=1226.852, gam=1223.702, llogis=1217.859  



# median and 95% CIs
dflist2 <- summary(cfit2, type='median')

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


ggplot(data = meds_df, aes(x=stage, y=cummed)) +
  geom_point() + geom_errorbar(aes(ymin=lclprop, ymax=uclprop))

saveRDS(meds_df, file=here('Data/derived_data/medians_controls.rds'))
