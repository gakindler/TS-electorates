# Creating maps of Australia using spatial.ops data

#### Libraries ####

pacman::p_load(tidyverse, sf, tmap, leaflet, viridis, grid, cartogram, rmapshaper, tmaptools, magrittr, httpgd, here)

#### Import and simplify data ####

spec.per.elect.counts.summary <- st_read(here("analysed_data/local_analysis_output/spec.per.elect.counts.summary.gpkg")
)
aus <- st_read(
  here(
  "clean_data/aus.clean.gpkg")
)
elect <- st_read(
  here(
  "clean_data/elect.clean.gpkg")
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
  keep = 0.1,
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
    weight = "total_unique_species",
    k = 0.8
    # m_weight = 1
  )

#### tmap ####

spec.per.elect.unique.spec.dorl <-
tm_shape(
  elect,
  bbox = st_bbox(c(
    xmin = 112.929704,
    ymin = -43.627943,
    xmax = 155.629864,
    ymax = -9.115517
    )
  )
  ) +
  tm_borders(
    "white",
    lwd = 1
  ) +
  tm_fill(
    "#d8d8d8"
  ) +
  tm_shape(
    spec.per.elect.unique.dorl.weight,
    bbox = st_bbox(c(
      xmin = 112.929704,
      ymin = -43.627943,
      xmax = 155.629864,
      ymax = -9.115517
    )
  )
  ) +
  tm_fill(
    "total_unique_species",
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
  # tm_legend(
    # legend.position = c("left", "top")
    # title.position = c("left", "bottom")
  # ) +
  tm_compass(position = c("left", "bottom")) +
  tm_scale_bar(
    position = c("left", "bottom"),
    width = 0.2
  ) +
  tm_layout(
    frame = FALSE,
    # legend.width = 1,
    # legend.height = 0.8,
    # legend.title.size = 1.5
    # legend.outside = TRUE
  )

spec.per.elect.unique.spec.dorl

tmap_save(spec.per.elect.unique.spec.dorl,
  file = "figures/spec.per.elect.unique.spec.dorl.png",
  # height = 15, width = 15, units = "cm"
)
