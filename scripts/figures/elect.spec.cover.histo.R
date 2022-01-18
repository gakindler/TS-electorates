# Creating plots using the spatial.ops data

#### Libraries ####

library(tidyverse)
library(viridis)
library(httpgd)

#### Import ####

spec.range.elect.counts <- read.csv(
  "analysed_data/local_analysis_output/spec.range.elect.counts.csv"
)

table(spec.range.elect.counts$species_range_covers_n_electorates, useNA = "ifany")

table(spec.range.elect.counts$migratory_status, useNA = "ifany")

#### Histogram ####

ggplot(spec.range.elect.counts) +
  aes(x = species_range_covers_n_electorates) +
  geom_histogram(binwidth = 1) +
  geom_vline(aes(xintercept = median(species_range_covers_n_electorates)),
    col = "red",
    size = 0.6,
    linetype = "dashed"
  ) +
  labs(
    x = "Electorate coverage",
    y = "Number of threatened species"
  ) +
  theme_classic()

ggsave("figures/spec.range.elect.status.histo.pdf",
  width = 8, height = 8, units = "cm"
)

#### Histogram, density curve ####

ggplot(spec.range.elect.counts) +
  aes(
    x = species_range_covers_n_electorates
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
  #     xintercept = median(species_range_covers_n_electorates)
  #     ),
  #   col = "red",
  #   size = 0.6,
  #   linetype = "dashed"
  # ) +
  labs(
    x = "Electorate coverage",
    y = "Threatened species density"
  ) +
  theme_classic()


ggsave("figures/spec.range.elect.density.pdf",
  width = 15, height = 10, units = "cm"
)

#### ECDF function ####

ggplot(
  spec.range.elect.counts
) +
  aes(
    x = species_range_covers_n_electorates
  ) +
  stat_ecdf(
    geom = "step",
    pad = FALSE
  ) +
  # geom_vline(
  #   aes(
  #     xintercept = quantile(
  #       species_range_covers_n_electorates
  #     )[3],
  #     col = "red",
  #     size = 0.0001,
  #     linetype = "dashed"
  #   )
  # ) +
  # geom_vline(
  #   aes(
  #     xintercept = quantile(
  #       species_range_covers_n_electorates
  #     )[4],
  #     col = "red",
  #     size = 0.1,
  #     linetype = "dashed"
  #   )
  # ) +
  labs(
    x = "Electorate coverage",
    y = "Threatened species cumulative proportion"
  ) +
  scale_x_continuous(
    breaks = seq(0, 150, by = 5)
  ) +
  scale_y_continuous(
    breaks = seq(0.5, 1.0, by = 0.05)
  ) +
  theme_classic()

ggsave("figures/spec.range.elect.ecdf.pdf",
  width = 15, height = 10, units = "cm"
)

#### Calculations ####

summary(spec.range.elect.counts)

table(spec.range.elect.counts$species_range_covers_n_electorates)

prop.table(table(spec.range.elect.counts$species_range_covers_n_electorates))
0.4499478624 + 0.1892596455 + 0.0740354536 + 0.0464025026

spec.range.elect.counts.4.greater.cover <- spec.range.elect.counts %>%
  filter(species_range_covers_n_electorates > 3)

table(spec.range.elect.counts.4.greater.cover$species_range_covers_n_electorates)

spec.range.elect.counts.4.less.cover <- spec.range.elect.counts %>%
  filter(species_range_covers_n_electorates < 4)

spec.range.elect.counts.migratory <- spec.range.elect.counts %>%
  filter(migratory_status %in% "Migratory")

spec.range.elect.counts.fishes <- spec.range.elect.counts %>%
  filter(taxon_group == "fishes")

table(spec.range.elect.counts$migratory_status)
table(spec.range.elect.counts.4.greater.cover$migratory_status)
table(spec.range.elect.counts.4.less.cover$migratory_status)
table(spec.range.elect.counts.4.cover$migratory_status)