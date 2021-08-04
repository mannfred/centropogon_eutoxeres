mytheme <-
  theme_minimal() +
  theme_bw() +
  theme(
    panel.border = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), 
    axis.line = element_line(colour = "black", size=1.2),
    axis.ticks = element_line(colour = "black", size = 1.2))
