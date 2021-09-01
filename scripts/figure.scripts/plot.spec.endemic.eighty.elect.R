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
spec.eighty.elect <- st_read(dsn = "analysed_data/HPC_spatial_ops_output/spec.range.elect.eighty.gpkg")
print(object.size(spec.eighty.elect), units = "Kb")
electorates <- st_read("raw_data/AEC_electoral_boundaries_2019/COM_ELB_region.shp")
print(object.size(electorates), units = "Kb")

spec.endemic.elect <- ms_simplify(spec.endemic.elect,
                              keep = 0.01,
                              keep_shape = TRUE) 
spec.eighty.elect <- ms_simplify(spec.eighty.elect,
                                       keep = 0.01,
                                       keep_shape = TRUE) 
electorates <- ms_simplify(electorates,
                                 keep = 0.01,
                                 keep_shape = TRUE) 

spec.endemic.elect <- spec.endemic.elect %>% 
  st_make_valid() %>% 
  st_crop(xmin = 113, ymin = -43.740482, # drop those pesky islands
          xmax = 154, ymax = -9.219937)
spec.eighty.elect <- spec.eighty.elect %>% 
  st_make_valid() %>% 
  st_crop(xmin = 113, ymin = -43.740482, # drop those pesky islands
          xmax = 154, ymax = -9.219937)
electorates <- electorates %>% 
  st_make_valid() %>% 
  st_crop(xmin = 113, ymin = -43.740482, # drop those pesky islands
          xmax = 154, ymax = -9.219937)

print(object.size(spec.endemic.elect), units = "Kb")
print(object.size(spec.eighty.elect), units = "Kb")
print(object.size(electorates), units = "Kb")

st_geometry(spec.endemic.elect) <- NULL
st_geometry(spec.eighty.elect) <- NULL
st_geometry(electorates) <- NULL


join.spec.eighty.elect <- inner_join(spec.eighty.elect, demography, 
                        by = c("Elect_div" = "Electoral division"))

#### Mapping ####

spec.endemic.elect.chloro <- tm_shape(electorates) +
  tm_polygons(border.alpha = 0.3) +
  tm_shape(spec.endemic.elect, 
                                  bbox = st_bbox(c(xmin = 113, 
                                                   ymin = -43.740482,
                                                   xmax = 154, 
                                                   ymax = -9.219937), 
                                                 crs = st_crs(spec.endemic.elect))) +
  tm_fill("total_unique_spec", 
          style = "jenks", 
          title = "Number of endemic \nthreatened species",
          palette = "-magma") + 
  tm_text("Elect_div", size = "AREA") + 
  tm_borders(alpha = 0.3) +
  # tm_compass(position = c("left", "bottom")) +
  # tm_scale_bar(position = c("left", "bottom"), 
  #              width = 0.2) +
  tm_layout(frame = FALSE)

spec.eighty.elect.chloro <- tm_shape(electorates) +
  tm_polygons(border.alpha = 0.3) +
  tm_shape(spec.eighty.elect, 
           bbox = st_bbox(c(xmin = 113, 
                            ymin = -43.740482,
                            xmax = 154, 
                            ymax = -9.219937), 
                          crs = st_crs(spec.eighty.elect))) +
  tm_fill("total_unique_spec", 
          style = "jenks", 
          title = "Number of threatened species \nwith 80% of range within",
          palette = "-magma") + 
  tm_text("Elect_div", size = "AREA") + 
  tm_borders(alpha = 0.3) +
  tm_compass(position = c("left", "bottom")) +
  tm_scale_bar(position = c("left", "bottom"), 
               width = 0.2) +
  tm_layout(frame = FALSE)

spec.endemic.eighty.elect.combined.chloro <- tmap_arrange(spec.endemic.elect.chloro, spec.eighty.elect.chloro)

tmap_save(spec.endemic.eighty.elect.combined.chloro, 
          file = "plots/spec_endemic_eighty_elect_combined_chloro.png")

#### Export ####




