library(mapr)
library(tidyverse)

x <- data.frame(latitude = -13, longitude = -71.5, name = "site")

map_ggplot(x, color = "#56B4E9") + 
  coord_fixed(xlim = c(-85, -67), ylim = c(-20, 0))
