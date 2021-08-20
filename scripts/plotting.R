# Creating plots using the spatial.ops data

#### Libraries ####

library(tidyverse)

#### Occurance of TS within demographies ####

ggplot(elect.spec.cover.status)
elect.spec.cover.status


# elect.spec.cover.status <- 
ggplot(elect.spec.cover.status) +
  aes(x = elect_range_covers, y = THREATENED_STATUS, fill = elect_range_covers) +
  geom_point(aes(group = THREATENED_STATUS))
               
 
  #               , scale = "width", draw_quantiles = c(0.25, 0.5, 0.75)) +
  # scale_fill_manual(values = c("#4CB04A83", "#E61C1C83", "#377FBA80")) +
  # theme_set(theme_bw()) +
  # theme(panel.grid = element_blank()) +
  # coord_cartesian(xlim = c(-5e+06, 3e+07)) +
  # labs(title = "Senate", 
  #      x = "\nTotal amount party received ($)", y = "Agreement score\n")
