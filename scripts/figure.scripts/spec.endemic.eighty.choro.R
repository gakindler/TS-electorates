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
library(httpgd)

#### Import and simplify data ####

spec.endemic.elect <- st_read(
  dsn = "analysed_data/21-12-06_local_analysis_output/spec.eighty.elect.gpkg"
)
spec.eighty.elect <- st_read(
  dsn = "analysed_data/21-12-06_local_analysis_output/spec.eighty.elect.gpkg"
)
elect <- st_read("clean_data/elect.clean.gpkg")

print(object.size(spec.endemic.elect), units = "Kb")
print(object.size(spec.eighty.elect), units = "Kb")
print(object.size(elect), units = "Kb")

spec.endemic.elect <- ms_simplify(spec.endemic.elect,
  keep = 0.01,
  keep_shape = TRUE
) %>%
  st_make_valid()
spec.eighty.elect <- ms_simplify(spec.eighty.elect,
  keep = 0.01,
  keep_shape = TRUE
) %>%
  st_make_valid()
elect <- ms_simplify(elect,
  keep = 0.01,
  keep_shape = TRUE
) %>%
  st_make_valid()

print(object.size(spec.endemic.elect), units = "Kb")
print(object.size(spec.eighty.elect), units = "Kb")
print(object.size(elect), units = "Kb")

#### Mapping ####

spec.endemic.elect.chloro <- tm_shape(elect) +
  tm_polygons(border.alpha = 0) +
  tm_shape(spec.endemic.elect) +
  tm_fill("total_unique_spec",
    style = "jenks",
    title = "Number of endemic \nthreatened species",
    palette = "-magma"
  ) +
  tm_text("Elect_div", size = "AREA") +
  tm_borders(
    col = "black", alpha = 0.4) +
  # tm_compass(position = c("left", "bottom")) +
  # tm_scale_bar(position = c("left", "bottom"),
  #              width = 0.2) +
  tm_layout(frame = FALSE)

tmap_save(spec.endemic.elect.chloro,
  file = "figures/post_processing/spec_endemic_elect_chloro.svg",
  width = 13, height = 13, units = "cm"
)

# spec.eighty.elect.chloro <-
tm_shape(elect) +
  tm_polygons(border.alpha = 0.3) +
  tm_shape(spec.eighty.elect) +
  tm_fill("total_unique_spec",
    style = "jenks",
    title = "Number of threatened species \nwith 80% of range within",
    palette = "-magma"
  ) +
  tm_text("Elect_div", size = "AREA") +
  tm_borders(alpha = 0.3) +
  tm_compass(position = c("left", "bottom")) +
  tm_scale_bar(
    position = c("left", "bottom"),
    width = 0.2
  ) +
  tm_layout(frame = FALSE)

spec.endemic.eighty.elect.combined.chloro <- tmap_arrange(spec.endemic.elect.chloro, spec.eighty.elect.chloro)

tmap_save(spec.endemic.eighty.elect.combined.chloro,
  file = "plots/spec_endemic_eighty_elect_combined_chloro.png"
)
