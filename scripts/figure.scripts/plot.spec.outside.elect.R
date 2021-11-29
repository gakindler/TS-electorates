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
spec.outside.elect.logical <- st_read(dsn = "analysed_data/HPC_spatial_ops_output/spec.outside.elect.logical.gpkg")

print(object.size(spec.outside.elect), units = "Kb")
print(object.size(spec.outside.elect.logical), units = "Kb")


spec.outside.elect <- ms_simplify(spec.outside.elect,
                              keep = 0.1,
                              keep_shape = TRUE) 
spec.outside.elect.logical <- ms_simplify(spec.outside.elect.logical,
                                  keep = 0.1,
                                  keep_shape = TRUE) 

print(object.size(spec.outside.elect), units = "Kb")
print(object.size(spec.outside.elect.logical), units = "Kb")

st_geometry(spec.outside.elect) <- NULL
st_geometry(join.spec.outside.elect) <- NULL


join.spec.outside.elect <- inner_join(spec.outside.elect.logical, specs.ss,
                                      by = c("SCIENTIFIC_NAME" = "SCIENTIFIC_NAME")) %>% 
  select(-"Shape")






