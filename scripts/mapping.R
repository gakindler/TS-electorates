# Mapping

library(tmap)
library(leaflet)

print(object.size(elect.spec.uniq.spec), units = "Kb")

tm_shape(join.intersect) +
  tm_fill() +
  tm_borders()
