library(here)
library(tidyverse)


# ----------------------------------
# plot IQR of raw values instead

# create required dfs in "2_alignment_*.R"
bc_stagelength <- readRDS(file=here('Data/derived_data/bc_stagelength.rds')) %>% mutate(stage2 =  substr(.$stage, start=1, stop=1)) 
ct_stagelength <- readRDS(file=here('Data/derived_data/ct_stagelength.rds')) %>% mutate(stage2 =  substr(.$stage, start=1, stop=1))

# estimate 2nd quantiles for Stages A and F
q2_s1_bc <- quantile(bc_stagelength %>% filter(stage2 == '1') %>% select(length) %>% deframe)[2] #second quantile
q2_s8_bc <- quantile(bc_stagelength %>% filter(stage2 == '8') %>% select(length) %>% deframe)[2]
q2_s1_ct <- quantile(ct_stagelength %>% filter(stage2 == '1') %>% select(length) %>% deframe)[2] #second quantile
q2_s8_ct <- quantile(ct_stagelength %>% filter(stage2 == '8') %>% select(length) %>% deframe)[2]

# tag entries with duration lower than 2nd quantile with "NA"
for (i in 1:nrow(ct_stagelength)){
  
  if(bc_stagelength$stage2[i] == '1' &
     bc_stagelength$length[i] < q2_s1_bc) {
    bc_stagelength$length[i] <- NA }
  
  if(bc_stagelength$stage2[i] == '8' &
     bc_stagelength$length[i] < q2_s8_bc) {
    bc_stagelength$length[i] <- NA }
  
  if(ct_stagelength$stage2[i] == '1' &
     ct_stagelength$length[i] < q2_s1_ct) {
    ct_stagelength$length[i] <- NA }
  
  if(ct_stagelength$stage2[i] == '8' &
     ct_stagelength$length[i] < q2_s8_ct) {
    ct_stagelength$length[i] <- NA }
}

# drop tagged entries
bc_stagelength <- na.omit(bc_stagelength)
ct_stagelength <- na.omit(ct_stagelength)

# merge pollinator-excluded and control data
lengthdata <-  
  full_join(ct_stagelength, bc_stagelength) %>% 
  group_by(stage2, treatment) %>% 
  mutate(stage2 = as.numeric(stage2))

# calculate medians per treatment and stage
meds <- lengthdata %>% summarise(median = median(as.numeric(length)))


# calculate medians for each stage and add to subsequent stage median
# start by adding stage 1 median to stage 2 data

for (i in 2:8){

# controls
index_ct <- which(lengthdata$stage2 == i & lengthdata$treatment == "control") #present stage
index_prev_ct <- which(lengthdata$stage2 == i-1 & lengthdata$treatment == "control") #previous stage

lengthdata$length[index_ct] <- lengthdata$length[index_ct] + median(lengthdata$length[index_prev_ct])


# pollinator excluded
index_bc <- which(lengthdata$stage2 == i & lengthdata$treatment == "pollinator-excluded")
index_prev_bc <- which(lengthdata$stage2 == i-1 & lengthdata$treatment == "pollinator-excluded")

lengthdata$length[index_bc] <- lengthdata$length[index_bc] + median(lengthdata$length[index_prev_bc])
}




ggplot(data = lengthdata, aes(x=factor(stage2), y=length, colour=treatment)) + 
  geom_jitter(position=position_jitterdodge(jitter.width = 0.2, jitter.height = 0.07), size = 4, alpha=0.4) +
  stat_summary(fun = mean,  geom = 'line', aes(group=treatment), size=2, position=position_jitterdodge(jitter.width = 0.2, jitter.height = 0.07)) +  #this has to be added
  stat_summary(fun.data=mean_cl_boot, geom='errorbar', size=1.5, position=position_jitterdodge(jitter.width = 0.2, jitter.height = 0.07)) +
  # stat_summary(fun.data = mean_se, geom = "errorbar", fun.args = list(mult = 1), size=1.5, position=position_jitterdodge(jitter.width = 0.2, jitter.height = 0.07)) +
  # geom_path(aes(colour = factor(treatment)),  size = 2) +
  scale_colour_manual(values=c("#E69F00", "#009E73")) +
  mytheme
