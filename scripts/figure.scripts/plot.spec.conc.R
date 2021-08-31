# Species concentration

#### Libraries ####

library(tidyverse)
library(sf)
library(tmap)
library(leaflet)
library(viridis)
library(grid)
library(cartogram)
library(rmapshaper)

#### Import and simplify data ####

spec.per.elect <- st_read(dsn = "analysed_data/HPC_spatial_ops_output/spec.per.elect.gpkg")
print(object.size(spec.per.elect), units = "Kb")

spec.per.elect <- ms_simplify(spec.per.elect,
                              keep = 0.1,
                              keep_shape = TRUE) 
spec.per.elect <- spec.per.elect %>% 
  st_make_valid() %>% 
  st_crop(xmin = 113, ymin = -43.740482, # drop those pesky islands
          xmax = 154, ymax = -9.219937)

print(object.size(spec.per.elect), units = "Kb")

st_geometry(spec.per.elect) <- NULL

#### Correlation plot ####

spec.conc.correl <- ggplot(spec.per.elect) +
  aes(x = elects_area_sqm, y = total_unique_spec, fill = Elect_div) +
  geom_point(show.legend = FALSE) +
  scale_colour_viridis_d() +
  scale_x_continuous(trans='log10') + 
  labs(x = "Electorate area (m^2)", 
       y = "Number of vulnerable species") +
  theme_classic()

  theme(axis.text.x = element_text(vjust = -2))   

ggsave("plots/spec_conc_correl.png", spec.conc.correl)

ggplot(spec.per.elect) +
  aes(x = elects_area_sqm, y = total_unique_spec, fill = Elect_div) +
  geom_point(aes(colour = Elect_div), show.legend = FALSE) +
  scale_colour_viridis_d() +
  scale_x_continuous(trans='log10') + 
  labs(x = "\nElectorate area (m^2)", 
       y = "Number of vulnerable species\n") +
  theme_classic()












