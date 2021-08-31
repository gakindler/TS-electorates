# spec.outside.elect

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

spec.outside.elect <- st_read(dsn = "analysed_data/HPC_spatial_ops_output/spec.outside.elect.gpkg")
spec.outside.elect.logical <- st_read(dsn = "analysed_data/HPC_spatial_ops_output/spec.outside.elect.gpkg")


print(object.size(spec.outside.elect), units = "Kb")

spec.outside.elect <- ms_simplify(spec.outside.elect,
                              keep = 0.1,
                              keep_shape = TRUE) 

spec.outside.elect <- spec.outside.elect %>% 
  st_make_valid() %>% 
  st_crop(xmin = 113, ymin = -43.740482, # drop those pesky islands
          xmax = 154, ymax = -9.219937)

print(object.size(spec.outside.elect), units = "Kb")

st_geometry(spec.outside.elect) <- NULL


spec.outside.elect






