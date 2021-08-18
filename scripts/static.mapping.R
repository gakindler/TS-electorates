# Creating maps of Australia using spatial.ops data

# As outlined in eechidna (https://jforbes14.github.io/eechidna/articles/plotting-electorates.html),
# mapping electorates is not conducive to a chloropleth due to the high density
# of electorates in ubran regions
# A non-contiguous, dorling cartogram may be more appropriate here?
# Or make the dots proportionally sized?
# But probs not, maybe some bbboxes for the city regions?
# Or fuck maps and bar chart it? Maybe in supps?

#### Libraries ####

library(sf)
library(tmap)
library(leaflet)
library(ggplot2)
library(viridis)

#### spec.per.elect ####

plot(spec.per.elect.aus)

# Check bounding box 
st_bbox(spec.per.elect.aus)
# xmin       ymin       xmax       ymax 
# 96.816948 -43.740482 167.996851  -9.219937 

# Continental Australia
# Remove Australian continent borders while maintaining internal borders?
# Saving as a pdf by changing the border.alpha from 0.001 to 0.01 made them massive
tm1 <- tm_shape(spec.per.elect.aus, 
                bbox = st_bbox(c(xmin = 113, 
                                 ymin = -43.740482,
                                 xmax = 154, 
                                 ymax = -9.219937), 
                               crs = st_crs(4283))) +
  tm_fill("total_unique_spec", 
          style = "jenks", 
          title = "Number of vulnerable specs",
          palette = "-viridis") + 
  tm_text("Elect_div", size = "AREA") + 
  tm_borders(alpha = 0.3) +
  tm_compass(position = c("left", "bottom")) +
  tm_scale_bar(position = c("left", "bottom"), 
               width = 0.2) +
  tm_layout(frame = FALSE)

tmap_save(tm1, file = "plots/draft_spec.per.elect.png")

#### spec.range.elect ####

tm_shape(spec.range.elect.eighty.aus, 
                bbox = st_bbox(c(xmin = 113, 
                                 ymin = -43.740482,
                                 xmax = 154, 
                                 ymax = -9.219937), 
                               crs = st_crs(4283))) +
  tm_fill("total_unique_spec", 
          style = "jenks", 
          title = "Number of vulnerable specs",
          palette = "-viridis") + 
  tm_text("Elect_div", size = "AREA") + 
  tm_borders(alpha = 0.3) +
  tm_compass(position = c("left", "bottom")) +
  tm_scale_bar(position = c("left", "bottom"), 
               width = 0.2) +
  tm_layout(frame = FALSE)

tmap_save(tm1, file = "plots/draft_spec.range.elect.eighty.png")

#### 


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



