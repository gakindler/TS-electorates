# Creating plots using the spatial.ops data

#### Libraries ####

library(tidyverse)
library(viridis)
library(httpgd)

#### elect.spec.cover.status ####

elect.spec.cover <- read.csv(
  "analysed_data/21-12-18_local_analysis_output/elect.spec.cover.csv"
)

# elect.spec.cover$THREATENED_STATUS <- factor(
#   elect.spec.cover$THREATENED_STATUS,
#   levels = c("Extinct in the wild",
# "Critically Endangered",
# "Endangered",
# "Vulnerable",
#              "Unknown"))

table(elect.spec.cover$elect_range_covers, useNA = "ifany")
table(elect.spec.cover$MIGRATORY_STATUS, useNA = "ifany")

#### Histogram ####

ggplot(elect.spec.cover) +
  aes(x = elect_range_covers) +
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
  width = 20, height = 15, units = "cm"
)

#### Calculations ####

summary(elect.spec.cover)

table(elect.spec.cover$elect_range_covers)

prop.table(table(elect.spec.cover$elect_range_covers))
0.4499478624 + 0.1892596455 + 0.0740354536 + 0.0464025026

elect.spec.cover.4.greater.cover <- elect.spec.cover %>%
  filter(elect_range_covers > 3)

table(elect.spec.cover.4.greater.cover$elect_range_covers)

elect.spec.cover.4.less.cover <- elect.spec.cover %>%
  filter(elect_range_covers < 4)

elect.spec.cover.migratory <- elect.spec.cover %>%
  filter(MIGRATORY_STATUS %in% "Migratory")

elect.spec.cover.fishes <- elect.spec.cover %>%
  filter(TAXON_GROUP == "fishes")

table(elect.spec.cover$MIGRATORY_STATUS)
table(elect.spec.cover.4.greater.cover$MIGRATORY_STATUS)
table(elect.spec.cover.4.less.cover$MIGRATORY_STATUS)
table(elect.spec.cover.4.cover$MIGRATORY_STATUS)

0.0811298077 + 0.2133413462 +0.4663461538
