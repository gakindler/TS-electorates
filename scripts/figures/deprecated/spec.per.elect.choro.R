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

spec.per.elect.counts.summary<- st_read(
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

#### choropleth ####

# Saving as a pdf by changing the border.alpha from 0.001 to 0.01 made them massive
spec.per.elect.counts.summary.choro <- tm_shape(spec.per.elect.counts.summary) +
        tm_fill("total_unique_spec",
                style = "jenks",
                title = "Number of threatened species",
                palette = "-inferno"
        ) +
        tm_text("Elect_div", size = "AREA") +
        tm_borders(alpha = 0.3) +
        tm_compass(position = c("left", "bottom")) +
        tm_scale_bar(
                position = c("left", "bottom"),
                width = 0.2
        ) +
        tm_layout(frame = FALSE)

# Inset maps?
syd.region <- tm_shape(spec.per.elect.counts.summary.aus) +
        tm_fill("total_unique_spec",
                style = "jenks",
                title = "Number of vulnerable specs",
                palette = "-viridis"
        ) +
        tm_text("Elect_div", size = "AREA") +
        tm_borders(alpha = 0.3) +
        tm_layout(frame = FALSE)

print(syd.region, vp = viewport(0.8, 0.27, width = 0.5, height = 0.5))

tmap_save(spec.per.elect.counts.summary.choro,
        file = "plots/spec_per_elect_choro.pdf",
        height = 8, width = 8, units = "cm"
)

#### Concentration choropleth ####

# plot.spec.conc.per.elect <-
tm_shape(spec.per.elect.counts.summary) +
        tm_fill("species_concentration",
                style = "jenks",
                title = "Number of vulnerable species",
                palette = "-inferno"
        ) +
        tm_text("Elect_div", size = "AREA") +
        tm_borders(alpha = 0.3) +
        tm_compass(position = c("left", "bottom")) +
        tm_scale_bar(
                position = c("left", "bottom"),
                width = 0.2
        ) +
        tm_layout(frame = FALSE)