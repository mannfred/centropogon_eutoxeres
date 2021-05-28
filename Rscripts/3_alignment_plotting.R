library(here)
library(tidyverse)


# cumilative days to each stage
cage <- c(23, 42, 48, 57)
control <- c(28, 49, 57, 84)

# build tbl_df
df <- 
  tibble(
  'stage' = c(1, 1, 2, 2, 3, 3, 4, 4),
  'elapsed_days' = c(rbind(cage, control)),
  'treatment' = rep(c('hummingbird-excluded', 'control'), 4))


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

