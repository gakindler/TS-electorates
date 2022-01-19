# Creating maps of Australia using spatial.ops data

#### Libraries ####

library(tidyverse)
library(sf)
library(tmap)
library(leaflet)
library(viridis)
library(grid)
library(cartogram)
library(rmapshaper)
library(tmaptools)
library(magrittr)

#### Import and simplify data ####

spec.per.elect.counts.summary <- st_read(
  "analysed_data/local_analysis_output/spec.per.elect.counts.summary.gpkg"
)
aus <- st_read(
  "clean_data/aus.clean.gpkg"
)
elect <- st_read(
  "clean_data/elect.clean.gpkg"
)

print(object.size(spec.per.elect.counts.summary), units = "Kb")
print(object.size(aus), units = "Kb")
print(object.size(elect), units = "Kb")

spec.per.elect.counts.summary <- ms_simplify(
  spec.per.elect.counts.summary,
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

print(object.size(spec.per.elect.counts.summary), units = "Kb")
print(object.size(aus), units = "Kb")
print(object.size(elect), units = "Kb")

# https://jforbes14.github.io/eechidna/articles/plotting-electorates.html
# one could do it like Forbes but the dorling cartogram option seems easier and  better

#### Calc: dorling cartogram ####

spec.per.elect.unique.dorl.weight <- spec.per.elect.counts.summary %>%
  st_transform(
    3112
  ) %>%
  cartogram_dorling(
    weight = "total_unique_spec",
    k = 0.3,
    m_weight = 1
  )

st_bbox(spec.per.elect.unique.dorl.weight)
    xmin     ymin     xmax     ymax
-1861273 -4972750  2105058 -1426405
st_bbox(elect)
      xmin       ymin       xmax       ymax
112.929704 -43.627943 153.629864  -9.115517


#### tmap ####

# spec.per.elect.unique.spec.dorl <-
tm_shape(
  elect,
  bbox = st_bbox(c(
    xmin = 112.929704,
    ymin = -43.627943,
    xmax = 154.629864,
    ymax = -9.115517
    )
  )) +
  tm_borders(
    "white",
    lwd = 1
  ) +
  tm_fill(
    "#d8d8d8"
  ) +
  tm_shape(
    spec.per.elect.unique.dorl.weight
  ) +
  # tm_shape(spec.per.elect.elect.size.dorl.weight) +
  tm_fill(
    "total_unique_spec",
    style = "jenks",
    # style = "cont",
    title = "Threatened species",
    palette = "-inferno"
  ) +
  tm_text(
    "electorate_abbrev",
    size = "AREA"
  ) +
  tm_borders(
    alpha = 0.6
  ) +
  tm_legend(
    # legend.position = c("left", "top")
    # title.position = c("left", "bottom")
  ) +
  # tm_compass(position = c("left", "bottom")) +
  # tm_scale_bar(
  #   position = c("left", "bottom"),
  #   width = 0.2
  # ) +
  tm_layout(
    outer.margins = c(1, 1 ,1 , 1),
    frame = FALSE,
    legend.width = 0.3,
    legend.height = 0.3,
    # legend.title.size = 1.5
    # legend.outside = TRUE
  )

tmap_save(spec.per.elect.unique.spec.dorl,
  file = "figures/spec.per.elect.unique.spec.dorl.pdf",
  # height = 10, width = 12, units = "cm"
)

#### ggplot ####
# https://r-charts.com/spatial/cartogram-ggplot2/#dorling
# https://rpubs.com/frankhecker/434695
# https://rud.is/rpubs/hello-dorling.html

# spec.per.elect.counts.summary.dorl <-
ggplot(spec.per.elect.counts.summary.dorl) +
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
    aes(label = electorate, size = total_unique_spec),
    show.legend = FALSE
  ) +
  theme_void()

ggplot(spec.per.elect.counts.summary.dorl) +
  geom_sf(
    aes(fill = total_unique_spec),
    color = "grey50"
  ) +
  # geom_sf_text(aes(label = electorate),
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

ggsave("figures/spec.per.elect.counts.summary.plot.pdf",
  width = 8, height = 8, units = "cm"
)

#### Proportional symbol map ####

elect_centroid <- st_centroid(spec.per.elect.counts.summary, of_largest_polygon = TRUE)

ggplot() +
  geom_sf(data = spec.per.elect.counts.summary, fill = "grey95") +
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

spec.per.elect.counts.summary.ncont.weight <- st_transform(spec.per.elect.counts.summary, 3112) %>%
  cartogram_ncont(weight = "total_unique_spec")

# spec.per.elect.counts.summary.ncont <-
tm_shape(elect) +
  tm_borders() +
  tm_shape(spec.per.elect.counts.summary.ncont.weight) +
  tm_fill("total_unique_spec",
    style = "jenks",
    title = "Number of threatened species",
    palette = "-inferno"
  ) +
  tm_text("electorate_abbrev", size = "AREA") +
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

tmap_save(spec.per.elect.counts.summary.ncont,
  file = "plots/spec_per_elect_ncont.pdf",
  height = 8, width = 8, units = "cm"
)