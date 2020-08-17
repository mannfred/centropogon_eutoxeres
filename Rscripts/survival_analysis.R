library(here)
library(survival)
library(survminer)
library(tidyverse)



# ---------------------
# survival analysis

# data structure: count how many days from stage 5 (anthesis) until either fruit set or censor date. 
# for survival::Surv() function, 
# when event time > censoring time (censored) = 1
# when event time < censoring time (fruit formation observed) = 2
# treatment = 1 (bird cage), treatment = 2 (control)
# "status" = "censored"
# time = number of days that the individual existed in the experiment 

# import data
data <- read.csv(here('Data/survival_analysis.csv'), header = T)


# fit survival models
treatment_fit <- survfit(Surv(data$treatment[1:29], treatment$status[1:29]) ~ 1)
control_fit <- survfit(Surv(control$time[30:90], control$status[30:90]) ~ 1)

t_fit <- survfit(Surv(data$treatment_time, data$treatment_status) ~ 1)
c_fit <- survfit(Surv(data$control_time, data$control_status) ~ 1)

# combine models
combined_fit <- list(t_fit, c_fit)

# plot using survminer
ggsurvplot(c_fit, data = data[, 4:6])
ggsurvplot(t_fit, data = data[, 1:3])

# colours
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

ggsurvplot_combine(
  combined_fit, 
  data = data,
  conf.int = TRUE,
  palette = cbbPalette[c(4,2)],
  size = 2,
  xlab = "Days since anthesis",
  legend.labs = c("Hummingbirds excluded", "Control"),
  )


title(main='KM-Curves for post-anthesis flower survival')


# -----------------------
#log rank test

logrank <- read.csv(here('Data/survival_analysis_log_rank_test.csv'), header=T)

time <- logrank$time
status <- logrank$censored
treatment <- logrank$group

fit <- survdiff(Surv(time, status) ~ treatment)
fit #Chisq= 4.5, 
#p = 0.0336, p-value is the probability of obtaining a test statistic 
#at least as extreme as the one that was actually observed. 
