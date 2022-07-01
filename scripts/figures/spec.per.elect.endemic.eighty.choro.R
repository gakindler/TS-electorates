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
library(tmaptools)

#### Import, filter and simplify data ####

spec.per.elect.counts.summary <- st_read(
  "analysed_data/local_analysis_output/spec.per.elect.counts.summary.gpkg"
)
spec.range.elect.endemic.expanded <- read.csv(
  "analysed_data/local_analysis_output/spec.range.elect.endemic.expanded.csv"
)

spec.per.elect.endemic <- spec.per.elect.counts.summary %>%
  filter(total_endemic_unique_species >= 1)

spec.per.elect.eighty <- spec.per.elect.counts.summary %>%
  filter(total_eighty_unique_species >= 1)

elect <- st_read("clean_data/elect.clean.gpkg")

print(object.size(spec.per.elect.counts.summary), units = "Kb")
print(object.size(spec.per.elect.endemic), units = "Kb")
print(object.size(spec.per.elect.eighty), units = "Kb")
print(object.size(elect), units = "Kb")

spec.per.elect.counts.summary <- ms_simplify(
  spec.per.elect.counts.summary,
  keep = 0.01,
  keep_shape = TRUE
) %>%
  st_make_valid()

spec.per.elect.endemic <- ms_simplify(
  spec.per.elect.endemic,
  keep = 0.01,
  keep_shape = TRUE
) %>%
  st_make_valid()

spec.per.elect.eighty <- ms_simplify(
  spec.per.elect.eighty,
  keep = 0.01,
  keep_shape = TRUE
) %>%
  st_make_valid()

elect <- ms_simplify(
  elect,
  keep = 0.01,
  keep_shape = TRUE
) %>%
  st_make_valid()

print(object.size(spec.per.elect.counts.summary), units = "Kb")
print(object.size(spec.per.elect.endemic), units = "Kb")
print(object.size(spec.per.elect.eighty), units = "Kb")
print(object.size(elect), units = "Kb")

#### Mapping ####

spec.per.elect.endemic.chloro <-
tm_shape(elect) +
  tm_polygons(border.alpha = 0) +
  tm_shape(spec.per.elect.endemic) +
  tm_fill("total_endemic_unique_species",
    style = "jenks",
    title = "Number of endemic \nthreatened species",
    palette = "-magma"
  ) +
  tm_text("electorate", size = "AREA") +
  tm_borders(
    col = "black", alpha = 0.4
  ) +
  tm_compass(
    position = c("left", "bottom"),
    size = 1.25
  ) +
  tm_scale_bar(
    position = c("left", "bottom"),
    width = 0.2
  ) +
  tm_layout(
    frame = FALSE,
    legend.width = 0.3,
    legend.height = 0.3
  )

tmap_save(spec.per.elect.endemic.chloro,
  file = "figures/post_processing/22-02-11_spec.per.elect.endemic.chloro.svg",
  width = 10, height = 10, units = "cm"
)

# spec.per.elect.eighty.chloro <-
tm_shape(elect) +
  tm_polygons(border.alpha = 0.3) +
  tm_shape(spec.per.elect.counts.summary) +
  tm_fill("total_unique_spec",
    style = "jenks",
    title = "Number of threatened species \nwith 80% of range within",
    palette = "-magma"
  ) +
  tm_text("electorate", size = "AREA") +
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

#### ggplot gird experiements ####

ggplot(spec.per.elect.endemic) +
  geom_sf(
    aes(fill = total_endemic_unique_spec),
    color = "grey50"
  ) +
  scale_fill_viridis(
    option = "C",
    direction = -1,
    n.breaks = 10,
    guide_colorbar(
      barwidth = 50,
      barheight = 50,
      title = "Threatened species",
      title.position = "right",
      title.vjust = 0.1,
      ticks = FALSE
    )
  ) +
  geom_sf_text(
    aes(
      label = electorate_abbrev,
      size = total_unique_spec
    ),
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

#### Calculations ####

table(spec.range.elect.endemic.expanded$demographic_class)

spec.range.elect.endemic.expanded %>%
  group_by(demographic_class) %>%
  summarise(total_unique_species = n_distinct(scientific_name)) %>%
  ungroup() %>%
  mutate(percentage_of_total_EPBC_species = total_unique_species / 801)

spec.range.elect.endemic.expanded %>%
  group_by(demographic_class) %>%
  summarise(elect_demo = n_distinct(electorate))

round(prop.table(table(spec.per.elect.endemic$demographic_class)),2)
