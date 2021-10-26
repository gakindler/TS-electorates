# Mapping for demo.spec

#### Libraries ####

library(tidyverse)
library(sf)
library(viridis)
library(grid)
library(cartogram)
library(httpgd) 

#### Import ####

# spec.per.elect <- st_read(dsn = "analysed_data/HPC_spatial_ops_output/spec.per.elect.gpkg")
print(object.size(spec.per.elect), units = "Kb")

#### Correlation plot ####

# spec.per.elect.point <- 
ggplot(spec.per.elect) +
  aes(x = elect_area_sqkm,
     y = total_unique_spec,
     fill = Elect_div,
     size = Electors) +
  geom_point(aes(colour = Demographic_class),
     alpha = 0.8,
     show.legend = c(size = FALSE,
          fill = FALSE)) +
  scale_size(range = c(.5, 15)) +
  scale_colour_viridis(discrete = TRUE,
     option = "H") +
  labs(x = bquote("Electorate area"~(km^2)),
     y = "Number of threatened species") +
  guides(colour = guide_legend(title = "Demographic class")) +
  theme_classic()

ggsave("figures/spec.per.elect.point.png", spec.per.elect.point)
