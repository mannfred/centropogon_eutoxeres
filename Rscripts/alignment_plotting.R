library(here)
library(tidyverse)


# days to each stage
# cage <- c(23, 4, 14, 5, 9)
# control <- c(28, 4, 15, 5, 27)

# cumilative days to each stage
cage <- c(23, 27, 41, 46, 55)
control <- c(28, 32, 47, 52, 79)

# build tbl_df
df <- 
  tibble(
  'stage' = c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5),
  'elapsed_days' = c(rbind(cage, control)),
  'treatment' = rep(c('hummingbird-excluded', 'control'), 5))


# plot
ggplot(
  data = df, 
  mapping = aes(x = stage, y = elapsed_days)) +
geom_point(
  aes(colour = factor(treatment)), 
  size = 5) +
geom_path(
  aes(colour = factor(treatment)),
  size = 2) +
scale_colour_manual(
  name= "Treatment", 
  values = c("#E69F00", "#009E73")) +
theme_bw() +
theme(
  panel.border = element_blank(), 
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(), 
  axis.line = element_line(colour = "black"))

