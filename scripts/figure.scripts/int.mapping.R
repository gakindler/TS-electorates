# Creating interactive maps using the spatial.ops data

#### Libraries ####

library(sf)
library(tmap)
library(leaflet)
library(ggplot2)
library(viridis)

#### spec.per.elect ####

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
  <strong>Number of specs</strong><br/>%d",
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


