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

spec.per.elect <- st_read(
  dsn = "analysed_data/21-12-06_local_analysis_output/spec.per.elect.gpkg"
)

print(object.size(spec.per.elect), units = "Kb")

#### Regression model ####

model <- lm(total_unique_spec ~ elect_area_sqkm, data = spec.per.elect)

summary(model)

#### Fitting existing model to a plot ####

xmin <- min(spec.per.elect$elect_area_sqkm)
xmax <- max(spec.per.elect$elect_area_sqkm)

predicted <- data.frame(elect_area_sqkm = seq(xmin, xmax, length.out = 100))

#### Scatter plot with regression line ####

# spec.per.elect.point.smooth <-
ggplot(spec.per.elect) +
  aes(
    x = elect_area_sqkm,
    y = total_unique_spec,
    fill = Elect_div
  ) +
  geom_point(aes(colour = Demographic_class),
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
  # geom_smooth(
  #   # method = "loess",
  #   method = "lm",
  #   show.legend = FALSE,
  #   colour = "black"
  # ) +
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
    y = "Number of threatened species"
  ) +
  guides(
    colour = guide_legend(
      title = "Demographic class",
      override.aes = list(size = 3)
    )
  ) +
  theme_classic()

ggsave("figures/spec_per_elect_point_smooth.pdf",
  # width = 8, height = 8, units = "cm"
)

ggplot(spec.per.elect) +
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

#### Calcualtions ####

summary(spec.per.elect)

prop.table(table(spec.per.elect$Demographic_class))

spec.per.elect %>%
  group_by(Demographic_class) %>%
  summarise(sum(total_unique_spec))

spec.per.elect %>%
  summarise(sum(total_unique_spec))