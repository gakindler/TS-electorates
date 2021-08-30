# Endemic and eighty percent range species

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

spec.endemic.elect <- st_read(dsn = "analysed_data/HPC_spatial_ops_output/spec.endemic.elect.gpkg")
print(object.size(spec.endemic.elect), units = "Kb")
spec.range.elect.eighty <- st_read(dsn = "analysed_data/HPC_spatial_ops_output/spec.range.elect.eighty.gpkg")
print(object.size(spec.range.elect.eighty), units = "Kb")

spec.endemic.elect <- ms_simplify(spec.endemic.elect,
                              keep = 0.1,
                              keep_shape = TRUE) 
spec.range.elect.eighty <- ms_simplify(spec.range.elect.eighty,
                                       keep = 0.1,
                                       keep_shape = TRUE) 

spec.endemic.elect <- spec.endemic.elect %>% 
  st_make_valid() %>% 
  st_crop(xmin = 113, ymin = -43.740482, # drop those pesky islands
          xmax = 154, ymax = -9.219937)
spec.range.elect.eighty <- spec.range.elect.eighty %>% 
  st_make_valid() %>% 
  st_crop(xmin = 113, ymin = -43.740482, # drop those pesky islands
          xmax = 154, ymax = -9.219937)

print(object.size(spec.endemic.elect), units = "Kb")
print(object.size(spec.range.elect.eighty), units = "Kb")

#### Export ####




