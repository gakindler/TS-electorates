# Creating plots using the spatial.ops data

#### Libraries ####

library(tidyverse)
library(viridis)

#### Data ####

elect.spec.cover.status <- read.csv("analysed_data/HPC_spatial_ops_output/elect.spec.cover.status.csv")

elect.spec.cover.status$THREATENED_STATUS <- elect.spec.cover.status$THREATENED_STATUS %>% 
  replace_na("Unknown")

elect.spec.cover.status$elect_range_covers <- elect.spec.cover.status$elect_range_covers %>% 
  replace_na(0)


#### Occurance of TS within demographies ####

elect.spec.cover.status$THREATENED_STATUS <- factor(
  elect.spec.cover.status$THREATENED_STATUS, 
  levels = c("Extinct in the wild", "Conservation Dependent", 
             "Critically Endangered", "Endangered", "Vulnerable",
             "Unknown"))

elect.spec.cover.status.histogram <- ggplot(elect.spec.cover.status) +
  aes(x = elect_range_covers, 
      fill = THREATENED_STATUS) +
  geom_histogram(binwidth = 1) + 
  geom_vline(aes(xintercept = median(elect_range_covers)), 
             col = 'red', 
             size = 1,
             linetype = "dashed") +
  scale_fill_viridis_d(direction = -1,
                       name = "Threatened status") +
  labs(x = "Electorates within species's range", 
       y = "Number of threatened species") +
  theme_classic()

ggsave("plots/elect_spec_cover_status_histogram.png", elect.spec.cover.status.histogram)

# 
# ggplot(elect.spec.cover.status, aes(elect_range_covers, fill = THREATENED_STATUS)) +
#   geom_histogram(binwidth = 1) +
#   scale_y_continuous(trans='log10')
# 
# geom_point(show.legend = FALSE) +
#   scale_colour_viridis_d() 
# labels = scales::comma
# 
# # elect.spec.cover.status <- 
# ggplot(elect.spec.cover.status) +
#   aes(x = elect_range_covers, y = THREATENED_STATUS, fill = elect_range_covers) +
#   geom_point(aes(group = THREATENED_STATUS))
#                
#  
#   #               , scale = "width", draw_quantiles = c(0.25, 0.5, 0.75)) +
#   # scale_fill_manual(values = c("#4CB04A83", "#E61C1C83", "#377FBA80")) +
#   # theme_set(theme_bw()) +
#   # theme(panel.grid = element_blank()) +
#   # coord_cartesian(xlim = c(-5e+06, 3e+07)) +
#   # labs(title = "Senate", 
#   #      x = "\nTotal amount party received ($)", y = "Agreement score\n")
