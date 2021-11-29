# Creating maps of Australia using spatial.ops data

# https://jforbes14.github.io/eechidna/articles/plotting-electorates.html

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
          xmax = 154, ymax = -9.219937) %>% 
  mutate(Elect_div_abbrev = substr(Elect_div, start = 1, stop = 2))

print(object.size(spec.per.elect), units = "Kb")

st_geometry(spec.per.elect) <- NULL

#### Chloropleth ####

# Check bounding box 
st_bbox(spec.per.elect.aus)
# xmin       ymin       xmax       ymax 
# 96.816948 -43.740482 167.996851  -9.219937 

# Continental Australia
# Remove Australian continent borders while maintaining internal borders?
# Saving as a pdf by changing the border.alpha from 0.001 to 0.01 made them massive
chloro.spec.per.elect <- tm_shape(spec.per.elect, 
                bbox = st_bbox(c(xmin = 113, 
                                 ymin = -43.740482,
                                 xmax = 154, 
                                 ymax = -9.219937), 
                               crs = st_crs(spec.per.elect))) +
  tm_fill("total_unique_spec", 
          style = "jenks", 
          title = "Number of threatened species",
          palette = "-inferno") + 
  tm_text("Elect_div", size = "AREA") + 
  tm_borders(alpha = 0.3) +
  tm_compass(position = c("left", "bottom")) +
  tm_scale_bar(position = c("left", "bottom"), 
               width = 0.2) +
  tm_layout(frame = FALSE)

# Inset maps?
syd.region <- tm_shape(spec.per.elect.aus, 
                       bbox = st_bbox(c(xmin = 150.3871677534, 
                                        ymin = -34.2640872964,
                                        xmax = 151.4690855434, 
                                        ymax = -33.455856658), 
                                      crs = st_crs(spec.per.elect.aus))) +
  tm_fill("total_unique_spec", 
          style = "jenks", 
          title = "Number of vulnerable specs",
          palette = "-viridis") + 
  tm_text("Elect_div", size = "AREA") + 
  tm_borders(alpha = 0.3) +
  tm_layout(frame = FALSE)

tm1
print(syd.region, vp = viewport(0.8, 0.27, width = 0.5, height = 0.5))


tmap_save(chloro.spec.per.elect, file = "plots/spec_per_elect_chloro.png")

#### concentration chloropleth ####

# plot.spec.conc.per.elect <- 
tm_shape(spec.per.elect, 
                                bbox = st_bbox(c(xmin = 113, 
                                                 ymin = -43.740482,
                                                 xmax = 154, 
                                                 ymax = -9.219937), 
                                               crs = st_crs(spec.per.elect))) +
  tm_fill("species_concentration",
          style = "jenks",
          title = "Number of vulnerable species",
          palette = "-inferno") +
  tm_text("Elect_div", size = "AREA") + 
  tm_borders(alpha = 0.3) +
  tm_compass(position = c("left", "bottom")) +
  tm_scale_bar(position = c("left", "bottom"), 
               width = 0.2) +
  tm_layout(frame = FALSE)

#### other ####

# Non-continental Australia
# Norfolk
tm_shape(spec.per.elect.aus, 
         bbox = st_bbox(c(xmin = 167.863479, 
                          ymin = -29.164764,
                          xmax = 168.042694, 
                          ymax = -28.964304), 
                        crs = st_crs(4283))) +
  tm_fill("total_unique_spec", 
          style = "jenks", 
          title = "Number of vulnerable specs",
          palette = "-viridis") + 
  tm_text("Elect_div", size = "AREA") + 
  tm_borders(alpha = 0.3) +
  tm_compass(position = c("left", "bottom")) +
  tm_scale_bar(position = c("left", "bottom")) +
  tm_layout(frame = FALSE)

# Lord Howe
tm_shape(spec.per.elect.aus, 
         bbox = st_bbox(c(xmin = 159.010215, 
                          ymin = -31.618085,
                          xmax = 159.133983, 
                          ymax = -31.469301), 
                        crs = st_crs(4283))) +
  tm_fill("total_unique_spec", 
          style = "jenks", 
          title = "Number of vulnerable specs",
          palette = "-viridis") + 
  tm_text("Elect_div", size = "AREA") + 
  tm_borders(alpha = 0.3) +
  tm_compass(position = c("left", "bottom")) +
  tm_scale_bar(position = c("left", "bottom")) +
  tm_layout(frame = FALSE)

# Christmas Island
tm_shape(spec.per.elect.aus, 
         bbox = st_bbox(c(xmin = 105.470347, 
                          ymin = -10.618522,
                          xmax = 105.773844, 
                          ymax = -10.35723), 
                        crs = st_crs(4283))) +
  tm_fill("total_unique_spec", 
          style = "jenks", 
          title = "Number of vulnerable specs",
          palette = "-viridis") + 
  tm_text("Elect_div", size = "AREA") + 
  tm_borders(alpha = 0.3) +
  tm_compass(position = c("left", "bottom")) +
  tm_scale_bar(position = c("left", "bottom")) +
  tm_layout(frame = FALSE)

# Cocos islands
tm_shape(spec.per.elect.aus, 
         bbox = st_bbox(c(xmin = 96.736216, 
                          ymin = -12.265961, 
                          xmax = 96.986155, 
                          ymax = -11.781081), 
                        crs = st_crs(4283))) +
  tm_fill("total_unique_spec", 
          style = "jenks", 
          title = "Number of vulnerable specs",
          palette = "-viridis") + 
  tm_text("Elect_div", size = "AREA") + 
  tm_borders(alpha = 0.3) +
  tm_compass(position = c("left", "bottom")) +
  tm_scale_bar(position = c("left", "bottom")) +
  tm_layout(frame = FALSE, legend.position = center)



