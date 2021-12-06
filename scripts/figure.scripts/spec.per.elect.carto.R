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

spec.per.elect <- st_read(
  dsn = "analysed_data/21-12-06_local_analysis_output/spec.per.elect.gpkg"
)
aus <- st_read(
  "clean_data/aus.clean.gpkg"
)
elect <- st_read(
  "clean_data/elect.clean.gpkg"
)

print(object.size(spec.per.elect), units = "Kb")
print(object.size(aus), units = "Kb")
print(object.size(elect), units = "Kb")

spec.per.elect <- ms_simplify(
  spec.per.elect,
  keep = 0.001,
  keep_shape = TRUE
)
aus <- ms_simplify(
  aus,
  keep = 0.001,
  keep_shape = TRUE
)
elect <- ms_simplify(
  elect,
  keep = 0.001,
  keep_shape = TRUE
) %>%
  st_make_valid()

print(object.size(spec.per.elect), units = "Kb")
print(object.size(aus), units = "Kb")
print(object.size(elect), units = "Kb")


#### Non-overlapping circles cartogram (Dorling) ####

# Pass to dorling cartogram function
spec.per.elect.dorl.weight <- st_transform(spec.per.elect, 3112) %>%
  cartogram_dorling(weight = "total_unique_spec")

## tmap ##
# https://geocompr.robinlovelace.net/adv-map.html
# https://github.com/sjewo/cartogram 

spec.per.elect.dorl <-
# tm_shape(elect) +
#   tm_borders() +
tm_shape(spec.per.elect.dorl.weight) +
  tm_fill(
    "total_unique_spec",
    style = "jenks",
    # style = "cont",
    title = "Number of threatened species",
    palette = "-inferno"
  ) +
  tm_text(
    "Elect_div",
    size = "AREA"
  ) +
  tm_borders(
    alpha = 0.7
  ) +
  # tm_compass(position = c("left", "bottom")) +
  # tm_scale_bar(
  #   position = c("left", "bottom"),
  #   width = 0.2
  # ) +
  tm_layout(
    frame = FALSE,
    # legend.outside = TRUE
  )

tmap_save(spec.per.elect.dorl,
  file = "figures/spec_per_elect_dorl.pdf"
  # height = 8, width = 8, units = "cm"
)

## ggplot ##
# https://r-charts.com/spatial/cartogram-ggplot2/#dorling
# https://rpubs.com/frankhecker/434695
# https://rud.is/rpubs/hello-dorling.html

# spec.per.elect.dorl <-
ggplot(spec.per.elect.dorl) +
  geom_sf(
    aes(fill = total_unique_spec),
    color = "grey50"
  ) +
  scale_fill_viridis(
    option = "C",
    direction = -1,
    n.breaks = 10,
    guide_colorbar(
      barwidth = 50,
      barheight = 50,
      title = "Number of threatened species",
      title.position = "right",
      title.vjust = 0.1,
      ticks = FALSE
    )
  ) +
  geom_sf_text(
    aes(label = Elect_div, size = total_unique_spec),
    show.legend = FALSE
  ) +
  theme_void()

ggplot(spec.per.elect.dorl) +
  geom_sf(
    aes(fill = total_unique_spec),
    color = "grey50"
  ) +
  # geom_sf_text(aes(label = Elect_div),
  #              check_overlap = TRUE) +
  scale_fill_viridis(
    direction = -1,
    n.breaks = 10,
    guide_colorbar(
      barwidth = 50,
      barheight = 50,
      title = "Number of vulnerable species",
      title.position = "right",
      title.vjust = 0.1,
      ticks = FALSE
    )
  ) +
  theme_void() +
  theme(
    legend.position = "top",
    legend.margin = margin(t = 5, b = 0),
    legend.title = element_text(size = 7),
    legend.text = element_text(
      angle = 45,
      margin = margin(t = 5)
    )
  )

ggsave("figures/spec.per.elect.plot.pdf",
  width = 8, height = 8, units = "cm"
)

#### Proportional symbol map ####

elect_centroid <- st_centroid(spec.per.elect, of_largest_polygon = TRUE)

ggplot() +
  geom_sf(data = spec.per.elect, fill = "grey95") +
  geom_sf(
    data = elect_centroid,
    aes(fill = total_unique_spec)
  ) +
  geom_sf(
    data = elect_centroid,
    aes(size = total_unique_spec),
    show.legend = FALSE
  ) +
  scale_fill_viridis(direction = -1) +
  # scale_size(range = c(1, 9),
  #            guide = guide_legend()) +
  theme_void() +
  theme(legend.position = "top")

#### Non-contiguous cartogram ####

spec.per.elect.ncont.weight <- st_transform(spec.per.elect, 3112) %>%
  cartogram_ncont(weight = "total_unique_spec")

# spec.per.elect.ncont <-
tm_shape(elect) +
  tm_borders() +
  tm_shape(spec.per.elect.ncont.weight) +
  tm_fill("total_unique_spec",
    style = "jenks",
    title = "Number of threatened species",
    palette = "-inferno"
  ) +
  tm_text("Elect_div_abbrev", size = "AREA") +
  tm_borders(alpha = 0.3) +
  tm_compass(position = c("left", "bottom")) +
  tm_scale_bar(
    position = c("left", "bottom"),
    width = 0.2
  ) +
  tm_layout(
    frame = FALSE,
    legend.outside = TRUE
  )

tmap_save(spec.per.elect.ncont,
  file = "plots/spec_per_elect_ncont.pdf",
  height = 8, width = 8, units = "cm"
)