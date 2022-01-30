# Mapping for demo.spec

#### Libraries ####

library(tidyverse)
library(sf)
library(viridis)
library(grid)
library(cartogram)
library(httpgd)
library(jsonlite)

#### Import ####

spec.per.elect.counts.summary <- st_read(
  "analysed_data/local_analysis_output/spec.per.elect.counts.summary.gpkg"
) %>%
  st_set_geometry(NULL)

spec.range.elect.expanded.summary <- read.csv(
   "analysed_data/local_analysis_output/spec.range.elect.expanded.summary.csv"
)

spec.per.elect.expanded.summary <- read.csv(
  "analysed_data/local_analysis_output/spec.per.elect.expanded.summary.csv"
)

print(object.size(spec.per.elect.counts.summary), units = "Kb")

# #### Regression model ####

# model <- lm(total_unique_spec ~ elect_area_sqkm, data = spec.per.elect.counts.summary)

# summary(model)

# #### Fitting existing model to a plot ####

# xmin <- min(spec.per.elect.counts.summary$elect_area_sqkm)
# xmax <- max(spec.per.elect.counts.summary$elect_area_sqkm)

# predicted <- data.frame(elect_area_sqkm = seq(xmin, xmax, length.out = 100))

#### Scatter plot with regression line ####

# spec.per.elect.point.smooth <-
ggplot(spec.per.elect.counts.summary) +
  aes(
    x = electorate_area_sqkm,
    y = total_unique_spec
    # fill = Elect_div
  ) +
  geom_point(aes(colour = demographic_class),
    alpha = 0.6,
    show.legend = c(
      size = FALSE,
      fill = FALSE
    ),
    size = 4
  ) +
  # stat_smooth(
  #   method = lm
  # )
  geom_smooth(
    # method = "loess",
    method = "lm",
    show.legend = FALSE,
    colour = "black"
  ) +
  # geom_line(
  #   data = predicted, size = 1
  # ) +
  scale_size(range = c(.5, 10)) +
  scale_colour_viridis(
    # begin = 0,
    # end = 0.9,
    # direction = -1,
    discrete = TRUE,
    option = "H"
  ) +
  scale_x_continuous(
    trans = "log10",
    labels = scales::comma
  ) +
  scale_shape_manual(values = c(1, 2)) +
  # annotation_logticks(sides = "b") +
  labs(
    x = bquote("Electorate area" ~ (km^2)),
    y = "Threatened species"
  ) +
  guides(
    colour = guide_legend(
      title = "Demographic class",
      override.aes = list(size = 3)
    )
  ) +
  theme_classic()

ggsave("figures/spec.per.elect.point.smooth.pdf",
  width = 15, height = 10, units = "cm"
)

ggplot(spec.per.elect.counts.summary) +
  aes(
    x = elect_area_sqkm,
    y = total_unique_spec
  ) +
  geom_point(aes(colour = Demographic_class)) +
  geom_smooth(
    method = "lm",
    show.legend = FALSE,
    colour = "black"
  )

#### Calculations ####
#
# spec.per.elect <- read.csv(
#   "analysed_data/local_analysis_output/spec.per.elect.csv"
# )

summary(spec.per.elect.counts.summary)

proportions(table(spec.per.elect.counts.summary$demographic_class))
0.1655629+0.2516556

proportions(table(spec.per.elect.counts.summary$state_territory))

spec.per.elect.demo.counts <- spec.per.elect.expanded.summary %>%
  group_by(demographic_class) %>%
  summarise(total_unique_species = n_distinct(scientific_name)) %>%
  ungroup() %>%
  mutate(percentage_of_total_EPBC_species = total_unique_species / 1651)

spec.per.elect.state.counts <- spec.per.elect.expanded.summary %>%
  group_by(state_territory) %>%
  summarise(total_unique_species = n_distinct(scientific_name)) %>%
  ungroup() %>%
  mutate(percentage_of_total_EPBC_species = total_unique_species / 1651)

spec.per.elect.top.ten.counts <- spec.range.elect.expanded.summary %>%
  filter(electorate %in% c("O'Connor", "Durack",
                          "Maranoa", "Kennedy",
                          "Eden-Monaro", "New England",
                          "Page", "Leichhardt",
                          "Lyons", "Grey")) %>%
  summarise(total_unique_species = n_distinct(scientific_name))
1134/1651

spec.per.elect.demo.area <- spec.per.elect.counts.summary %>%
  group_by(demographic_class) %>%
  summarise(sum_elect_area_sqkm = sum(electorate_area_sqkm))
proportions(spec.per.elect.demo.area$sum_elect_area_sqkm)
0.0308560479+0.9654603253
0.0007473473 + 0.0029362795
