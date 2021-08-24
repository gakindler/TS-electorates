# Creating maps of Australia using spatial.ops data

# As outlined in eechidna (https://jforbes14.github.io/eechidna/articles/plotting-electorates.html),
# mapping electorates is not conducive to a chloropleth due to the high density
# of electorates in ubran regions
# A non-contiguous, dorling cartogram may be more appropriate here?
# Or make the dots proportionally sized?
# But probs not, maybe some bbboxes for the city regions?
# Or fuck maps and bar chart it? Maybe in supps?

#### Libraries ####

library(tidyverse)
library(sf)
library(tmap)
library(leaflet)
library(viridis)
library(grid)
library(cartogram)

#### spec.per.elect: chloropleth ####

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
                               crs = st_crs(spec.per.elect.aus))) +
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


tmap_save(tm1, file = "plots/draft_spec.per.elect.png")

#### spec.per.elect: tmap dorling cartogram ####
# https://geocompr.robinlovelace.net/adv-map.html
tm_shape(spec.per.elect.aus, 
                bbox = st_bbox(c(xmin = 113, 
                                 ymin = -43.740482,
                                 xmax = 154, 
                                 ymax = -9.219937), 
                               crs = st_crs(spec.per.elect.aus))) +
  tm_polygons() +
  tm_symbols(col = "grey", size = "total_unique_spec")

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
  
#### spec.per.elect: ggplot dorling cartogram ####
# https://r-charts.com/spatial/cartogram-ggplot2/#dorling

# Pass to dorling cartogram function
spec.per.elect.aus.dorl <- st_transform(spec.per.elect.aus, 3112) %>% 
    cartogram_dorling(weight = "total_unique_spec")

ggplot(spec.per.elect.aus.dorl) +
  geom_sf(aes(fill = total_unique_spec), color = "grey50") +
  scale_fill_viridis(direction = -1) + 
  theme_void() +
  theme(legend.position = "top",
        legend.margin = margin(t = 10, b = 2),
        legend.title = element_text(size = 7))

#### spec.per.elect: ggplot non-contiguous cartogram ####



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

#### demo.spec ####

tm_shape(demo.spec.aus, bbox = st_bbox(c(xmin = 113, 
                                         ymin = -43.740482,
                                         xmax = 154, 
                                         ymax = -9.219937), 
                                       crs = st_crs(4283))) +
  tm_fill("Demographic.classification")


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



