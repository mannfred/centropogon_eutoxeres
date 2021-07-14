# "The AJ estimate is very flexible; subjects can visit 
# multiple states during the course of a study, 
# subjects can start after time 0 (delayed entry), and
# they can start in any of the states." -pg.3 Therneau, Crowson, Atkinson (2021)

# left censoring
# https://stackoverflow.com/questions/41968606/left-censoring-for-survival-data-in-r
# https://www.rdocumentation.org/packages/survival/versions/2.11-4/topics/Surv

# delayed entry (not the same as left-censoring)
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
development_data_bc <- 
  read.csv(here('Data/Bird_cage_compiled.csv')) %>% 
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
  ungroup() %>% 
  dplyr::slice(-(c(34, 43, 62))) # remove rows with non-sequential develop data (discovered by visual inspection of $seq)

# assign censoring state per individual
# determined by comparing raw data 'Bird_exclusion_exp_2017.xlsx"
# to bc_stringset 
censored_bc <- c(rep('N', 2), rep('Y', 10),  #indiv1
                 'N', 'Y', 'N', rep('Y', 4), #indiv2
                 rep('N', 2), rep('Y', 2), rep('N', 2), rep('Y', 5), #indiv3
                 rep('N', 3), rep('Y', 2), #indiv4
                 rep('N', 2), rep('Y', 2), rep('N', 2), rep('Y', 12), #indiv5
                 'N', rep('Y', 4), 'N', rep('Y', 5)) #indiv6

bc_stringset$censored <- censored_bc

# this algorithm creates a list of dataframes from bc_stringset: 
# each df is a matrix describing the duration of each stage for an individual flower

dflist <- list()
for (i in 1:nrow(bc_stringset)){
  
  split2 <- str_split_fixed(bc_stringset$seq[i], pattern="", n=nchar(bc_stringset$seq[i])) %>% as.numeric()
  split1 <- str_split_fixed(bc_stringset$seq[i], pattern="(?<=(.))(?!\\1)", n=length(unique(split2))) %>% as.character() # solution from https://stackoverflow.com/questions/23523597/split-string-into-repeated-characters

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
  
  end.stage <- numeric()
  if (bc_stringset$censored[i] == "N") {end.stage <- start.stage + 1}
  else {end.stage <- start.stage + 1
        end.stage[length(start.stage)] <- 0 } #right censored
  
  id <- rep(bc_stringset$indiv_flower[i], length(split1))
  
  dflist[[i]] <- 
    data.frame(id = id, 
               start = start, 
               stop = stop, 
               start.stage = start.stage, 
               end.stage = end.stage)
}

# flatten lists into one df
bc_df <- do.call(rbind, lapply(dflist, as.data.frame)) 

# specify stages
Nodes <- c("1", "2", "3", "4", "5", "6", "7", "8", "9")
Edges <- list("1" = list(edges = "2"),
              "2" = list(edges = "3"),
              "3" = list(edges = "4"),
              "4" = list(edges = "5"),
              "5" = list(edges = "6"),
              "6" = list(edges = "7"),
              "7" = list(edges = "8"),
              "8" = list(edges = "9"),
              "9" = list(edges = NULL))

treeobj <- new("graphNEL", nodes = Nodes, edgeL = Edges, edgemode = "directed")
ex1 <- msSurv(bc_df, treeobj, bs = T, LT = T)
plot(ex1)
summary(ex1)
