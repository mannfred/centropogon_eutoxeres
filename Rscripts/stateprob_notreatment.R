# "The AJ estimate is very flexible; subjects can visit 
# multiple states during the course of a study, 
# subjects can start after time 0 (delayed entry), and
# they can start in any of the states." -pg.3 Therneau, Crowson, Atkinson (2021)

# left censoring
# https://stackoverflow.com/questions/41968606/left-censoring-for-survival-data-in-r
# https://www.rdocumentation.org/packages/survival/versions/2.11-4/topics/Surv

# delayed entry
# https://stats.stackexchange.com/questions/503234/how-to-deal-with-this-survival-data
# " Although the participants might enter the study on different calendar dates, 
# simply set time = 0 for each individual to the date of the 1st treatment protocol and 
# express all subsequent times as differences from that date. That's not "delayed entry"; 
# different calendar dates of entry happen in randomized clinical trials, too."

# Time-dependent covariates, recurrent events, and different event types mean you have to 
# break up the data for each patient into separate (Start, Stop, Event) pieces for each 
# treatment and event, with a categorical Event value instead of a simple 0/1.

# --------------------------------------

library(here)
library(msSurv)
library(stringi)
library(tidyverse)


# import pollinator-excluded data
development_data_ct <- 
  read.csv(here('Data/No_treat_compiled.csv')) %>% 
  mutate(stage = as.character(stage))  # for replace() below
# mutate(new_stage = 
#          case_when(stage == 1 ~ "1",
#                    stage == 2 ~ "2",
#                    stage == 3 ~ "2",
#                    stage == 4 ~ "2",
#                    stage == 5 ~ "3",
#                    stage == 6 ~ "4",
#                    stage == 7 ~ "5",
#                    stage == 8 ~ "6"))


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
  ungroup() %>% 
  dplyr::slice(-(c(18, 58, 76, 77, 83, 101, 107, 113))) %>% # remove rows with non-sequential develop data (discovered by visual inspection of $seq)
  filter(`1` == 1)

# assign censoring state per individual
# determined by comparing raw data 'Bird_exclusion_exp_2017.xlsx"
# to ct_stringset 
censored_ct <- c(rep('Y', 3), rep('N', 3), 'Y', 'N', rep('Y', 9), #indiv1
                 'N', 'Y', 'N', #indiv2
                 rep('Y', 3), rep('N', 2), rep('Y', 10), #indiv3
                 rep('N', 3), 'Y', #indiv4
                 'Y', 'N', rep('Y', 3), #indiv5
                 rep('Y', 7), #indiv6
                 rep('N', 5), rep('Y', 11), #indiv7
                 rep('N', 5), rep('Y', 16), #indiv8
                 'N', 'Y', rep('N', 2), rep('Y', 17), #indiv9
                 rep('N', 2), 'Y', rep('N', 2)) #indiv10
                 
censored_ct <- c(rep('Y', 52))
ct_stringset$censored <- censored_ct

# this algorithm creates a list of dataframes from bc_stringset: 
# each df is a matrix describing the duration of each stage for an individual flower

dflist <- list()
for (i in 1:nrow(ct_stringset)){
  
  split2 <- str_split_fixed(ct_stringset$seq[i], pattern="", n=nchar(ct_stringset$seq[i])) %>% as.numeric()
  split1 <- str_split_fixed(ct_stringset$seq[i], pattern="(?<=(.))(?!\\1)", n=length(unique(split2))) %>% as.character() # solution from https://stackoverflow.com/questions/23523597/split-string-into-repeated-characters
  
  start <- numeric()
  for (j in 1:length(split1)) {
    
    if (j==1){start[j] <- 0}
    else {split3 <- as.numeric(str_split_fixed(split1[j], "", n=nchar(split1[j])))
    start[j] <- min(which(split2 == split3))}
  }
  
  stop <- numeric()
  for (k in 1:length(split1)) {
    
    if (k < length(split1)) {
      split4 <- as.numeric(str_split_fixed(split1[k], "", n=nchar(split1[k])))
      stop[k] <- min(which(split2 == split4 + 1)) }
    else stop[k] <- length(split2) + 1
  }
  
  start.stage <- unique(split2)
  
  # end.stage <- numeric()
  # if (ct_stringset$censored[i] == "N" & max(start.stage) < 8) {end.stage <- start.stage + 1}
  # 
  # else {if (ct_stringset$censored[i] == "N" & max(start.stage) == 8) 
  # {end.stage <- start.stage + 1
  #  end.stage[length(start.stage)] <- 8}
  #    else {end.stage <- start.stage + 1
  #          end.stage[length(start.stage)] <- 0 } #right censored
  # }
  
  end.stage <- numeric()
  if (ct_stringset$censored[i] == "N") {end.stage <- start.stage + 1}
  else {end.stage <- start.stage + 1
  end.stage[length(start.stage)] <- 0 } #right censored
  
  id <- rep(ct_stringset$indiv_flower[i], length(split1))
  
  dflist[[i]] <- 
    data.frame(id = id, 
               start = start, 
               stop = stop, 
               start.stage = start.stage, 
               end.stage = end.stage)
}

# flatten lists into one df
ct_df <- do.call(rbind, lapply(dflist, as.data.frame)) 

# specify stages
Nodes <- c("1", "2", "3", "4", "5", "6", "7")
Edges <- list("1" = list(edges = "2"),
              "2" = list(edges = "3"),
              "3" = list(edges = "4"),
              "4" = list(edges = "5"),
              "5" = list(edges = "6"),
              "6" = list(edges = "7"),
              "7" = list(edges = NULL))

#NEL = node edge list
treeobj <- new("graphNEL", nodes = Nodes, edgeL = Edges, edgemode = "directed")
ex2 <- msSurv(ct_df, treeobj, bs = T, LT = T)
plot(ex2)
summary(ex2)


# -----------------------------

#aligned
id <- c(1,1,1,2,2)
start <- c(0,3,6,4,6)
stop <- c(3,6,8,6,8)
start.stage <- c(1, 2, 3, 2, 3)
end.stage <- c(2, 3, 0, 3, 0)
df1 <- data.frame(id=id, start=start, stop=stop, start.stage=start.stage, end.stage=end.stage)


Nodes <- c("1", "2", "3")
Edges <- list("1" = list(edges = "2"),
              "2" = list(edges = "3"),
              "3" = list(edges = NULL))
treeobj <- new("graphNEL", nodes = Nodes, edgeL = Edges, edgemode = "directed")

ex3 <- msSurv(df1, treeobj, bs = F, LT = T)
plot(ex3)


# now everyone starts at t=0
id <- c(1,1,1,2,2)
start <- c(0,3,6,0,3)
stop <- c(3,6,8,3,5)
start.stage <- c(1, 2, 3, 2, 3)
end.stage <- c(2, 3, 0, 3, 0)

df2 <- data.frame(id=id, start=start, stop=stop, start.stage=start.stage, end.stage=end.stage)


Nodes <- c("1", "2", "3")
Edges <- list("1" = list(edges = "2"),
              "2" = list(edges = "3"),
              "3" = list(edges = NULL))
treeobj <- new("graphNEL", nodes = Nodes, edgeL = Edges, edgemode = "directed")

ex4 <- msSurv(df2, treeobj, bs = F, LT = T)
plot(ex4)
