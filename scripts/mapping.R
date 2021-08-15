# Mapping EA tables

#### Libraries ####

library(sf)
library(tmap)
library(leaflet)
library(ggplot2)
library(viridis)

#### Static: spec.per.elect ####

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
          title = "Number of vulnerable species",
          palette = "-viridis") + 
  tm_text("Elect_div", size = "AREA") + 
  tm_borders(alpha = 0.3) +
  tm_compass(position = c("left", "bottom")) +
  tm_scale_bar(position = c("left", "bottom"), 
               width = 0.2) +
  tm_layout(frame = FALSE)

tmap_save(tm1, file = "plots/draft_spec.per.elect.png")

#### Static: spec.range.elect ####

tm_shape(spec.range.elect.eighty.aus, 
                bbox = st_bbox(c(xmin = 113, 
                                 ymin = -43.740482,
                                 xmax = 154, 
                                 ymax = -9.219937), 
                               crs = st_crs(4283))) +
  tm_fill("total_unique_spec", 
          style = "jenks", 
          title = "Number of vulnerable species",
          palette = "-viridis") + 
  tm_text("Elect_div", size = "AREA") + 
  tm_borders(alpha = 0.3) +
  tm_compass(position = c("left", "bottom")) +
  tm_scale_bar(position = c("left", "bottom"), 
               width = 0.2) +
  tm_layout(frame = FALSE)

tmap_save(tm1, file = "plots/draft_spec.range.elect.eighty.png")

#### Static: other ####

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
          title = "Number of vulnerable species",
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
          title = "Number of vulnerable species",
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
          title = "Number of vulnerable species",
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
          title = "Number of vulnerable species",
          palette = "-viridis") + 
  tm_text("Elect_div", size = "AREA") + 
  tm_borders(alpha = 0.3) +
  tm_compass(position = c("left", "bottom")) +
  tm_scale_bar(position = c("left", "bottom")) +
  tm_layout(frame = FALSE, legend.position = center)


#### Interactive mapping ####

# Check size, for interactive maps?
print(object.size(spec.per.elect.aus), units = "Kb")

# Change CRS to what Leaflet likes, look up codes?
spec.per.elect.aus.WGS84 <- st_transform(spec.per.elect.aus, crs = )
st_crs(spec.per.elect.aus)

# Mapping
bins <- c(0, 25, 50, 75, 100, 125, 150, 175, 200, 225, 250, Inf)
pal <- colorBin("YlOrRd", domain = spec.per.elect.aus$total_unique_spec, bins = bins)
labels <- sprintf(
  "<strong>Electorate</strong><br/>%s<br/>
  <strong>Number of species</strong><br/>%d",
  spec.per.elect.aus$Elect_div, spec.per.elect.aus$total_unique_spec
  ) %>% lapply(htmltools::HTML)


leaflet(spec.per.elect.aus) %>% 
  addPolygons(fillColor = ~pal(total_unique_spec), 
              weight = 2,
              opacity = 1,
              color = "white",
              dashArray = "3",
              fillOpacity = 0.7,
              highlight = highlightOptions(
                weight = 2,
                color = "#666",
                dashArray = "",
                fillOpacity = 0.7,
                bringToFront = TRUE),
              label = labels,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto"))
  


