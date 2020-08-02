library(here)
library(survival)
library(tidyverse)


# -----------------------
# import data

mydata <- read.csv(here('Data/means_melted.csv'), header = TRUE)
 


as.factor(mydata$group)
mydata$stage<-as.factor(mydata$stage)
as.numeric(mydata$days)



ggplot(data = mydata, aes(x=stage, y=days)) + 
  theme_minimal() +
  geom_boxplot(aes(fill=group), alpha=0.7) +
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  scale_fill_brewer(palette="Dark2")


is.factor(mydata$group)
is.factor(mydata$stage)
is.numeric(mydata$days)

# ---------------------
# survival analysis

# for survival::Surv() function, 
# when event time > censoring time (censored) = 1
# when event time < censoring time (fruit formation observed) = 2


cage <- read.csv(here('Data/survival_analysis_bird_cage.csv'), header=T)
control <- read.csv(here('Data/survival_analysis_no_treatment.csv'), header=T)

tail(control) # 0 = censored

time1 <- cage$time
status1 <- cage$status
time2 <- control$time
status2 <- control$status

fit1 <- survfit(Surv(time1, status1) ~ 1)
fit2 <- survfit(Surv(time2, status2) ~ 1)

plot(fit1, conf.int= 'none', col = 'blue', lwd=5, xlab = 'Time (days)', ylab = 'Survival Probability', xlim=c(0,30)) #bird cage
lines(fit2, conf.int= 'none', col = 'red', lwd=5) #no treatment
legend(14, 1,c('birds excluded', 'no treatment'), col = c('blue','red'), lty = 1)
title(main='KM-Curves for post-anthesis flower survival')


#log rank test

logrank <- read.csv(here('Data/survival_analysis_log_rank_test.csv'), header=T)

time <- logrank$time
status <- logrank$censored
treatment <- logrank$group

fit <- survdiff(Surv(time, status) ~ treatment)
fit #Chisq= 4.5, 
#p = 0.0336, p-value is the probability of obtaining a test statistic 
#at least as extreme as the one that was actually observed. 
