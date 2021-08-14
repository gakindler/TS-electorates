# Mapping

library(sf)
library(tmap)
library(leaflet)
library(ggplot2)
library(viridis)

# Check size, for interactive maps?
print(object.size(spec.per.elect.aus), units = "Kb")

# Check bounding box 
st_bbox(spec.per.elect.aus)

# Remove Australian continent borders while maintaining internal borders?
tm1 <- tm_shape(spec.per.elect.aus, 
                bbox = st_bbox(c(xmin = 113, 
                                 xmax = 154, 
                                 ymin = -43.740482, 
                                 ymax = -9.219937), 
                               crs = st_crs(4283))) +
  tm_fill("total_unique_spec", 
          style = "jenks", 
          title = "Number of vulnerable species",
          palette = "-magma") + 
  tm_text("Elect_div", size = "AREA") + 
  tm_borders(alpha = 0.5) +
  tm_compass(position = c("left", "bottom")) +
  tm_scale_bar(position = c("left", "bottom"), 
               width = 0.2) +
  tm_layout(frame = FALSE)

tmap_save(tm1, file = "plots/draft_spec.per.elect.png")

#### Leaflet mapping ####

# Change CRS to what Leaflet likes
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
  


