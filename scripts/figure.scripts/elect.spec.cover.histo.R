# Creating plots using the spatial.ops data

#### Libraries ####

library(tidyverse)
library(viridis)
library(httpgd)

#### elect.spec.cover.status ####

# elect.spec.cover$THREATENED_STATUS <- factor(
#   elect.spec.cover$THREATENED_STATUS,
#   levels = c("Extinct in the wild",
# "Critically Endangered",
# "Endangered",
# "Vulnerable",
#              "Unknown"))

#### Histogram ####

ggplot(elect.spec.cover) +
  aes(
    x = elect_range_covers
  ) +
  geom_histogram(binwidth = 1) +
  geom_vline(aes(xintercept = median(elect_range_covers)),
    col = "red",
    size = 0.6,
    linetype = "dashed"
  ) +
  labs(
    x = "Electorates within species's range",
    y = "Number of threatened species"
  ) +
  theme_classic()

ggsave("figures/elect_spec_cover_status_histo.pdf",
  width = 8, height = 8, units = "cm"
)

#### Histogram, density curve ####

ggplot(elect.spec.cover) +
  aes(
    x = elect_range_covers
  ) +
  # geom_histogram(
  #   binwidth = 1
  # ) +
  geom_density(
    fill = "blue",
    alpha = .3,
    outline.type = "full"
  ) +
  # geom_vline(
  #   aes(
  #     xintercept = median(elect_range_covers)
  #     ),
  #   col = "red",
  #   size = 0.6,
  #   linetype = "dashed"
  # ) +
  labs(
    x = "Electorates within species's range",
    y = "Threatened species density"
  ) +
  theme_classic()

ggsave("figures/elect_spec_cover_density.pdf",
  width = 16, height = 12, units = "cm"
)

#### Calculations ####

summary(elect.spec.cover.status)

prop.table(table(elect.spec.cover.status$elect_range_covers))
0.4499478624 + 0.1892596455 + 0.0740354536 + 0.0464025026

elect.spec.cover.status.4.greater.cover <- elect.spec.cover.status %>%
  filter(elect_range_covers > 4)

elect.spec.cover.status.4.less.cover <- elect.spec.cover.status %>%
  filter(elect_range_covers < 5)

table(elect.spec.cover.status$MIGRATORY_STATUS)
table(elect.spec.cover.status.4.greater.cover$MIGRATORY_STATUS)
table(elect.spec.cover.status.4.less.cover$MIGRATORY_STATUS)
table(elect.spec.cover.status.4.cover$MIGRATORY_STATUS)