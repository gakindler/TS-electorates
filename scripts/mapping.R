# Mapping

library(sf)
library(tmap)
library(leaflet)
library(ggplot2)

print(object.size(spec.per.elect.aus), units = "Kb")

tm_shape(spec.per.elect.aus) +
  tm_polygons("total_unique_spec") +
  tm_text("Elect_div", size = "AREA")

leaflet(spec.per.elect.aus) %>% 
  addPolygons(fillColor = "total_unique_spec")
  
  

