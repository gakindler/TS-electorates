# Creating plots using the spatial.ops data

#### Libraries ####

library(tidyverse)
library(viridis)
library(httpgd)
library(data.table)

#### Import ####

spec.elect.coverage.expanded <- read.csv(
  "analysed_data/local_analysis_output/spec.elect.coverage.expanded.csv"
)
spec.range.elect.expanded <- read.csv(
  "analysed_data/local_analysis_output/spec.range.elect.expanded.summary.csv"
)

table(spec.elect.coverage.expanded$species_range_covers_n_electorates, useNA = "ifany")

table(spec.elect.coverage.expanded$migratory_status, useNA = "ifany")

#### Histogram ####

ggplot(spec.elect.coverage.expanded) +
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

ggplot(spec.elect.coverage.expanded) +
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

spec.elect.coverage.expanded.ecdf.clean <- spec.elect.coverage.expanded %>%
  select(species_range_covers_n_electorates)

spec.elect.coverage.expanded.ecdf <- ecdf(data.table(spec.elect.coverage.expanded)[[6]])

ggplot(
  spec.elect.coverage.expanded
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
    x = "Number of electorates threatened species' range covers",
    y = "Cumulative proportion"
  ) +
  scale_x_continuous(
    breaks = seq(0, 150, 10)
  ) +
  scale_y_continuous(
    breaks = seq(0.5, 1.0, 0.05)
  ) +
  theme_classic()

ggsave("figures/spec.range.elect.ecdf.png",
  width = 20, height = 15, units = "cm"
)

#### Calculations ####

summary(spec.elect.coverage.expanded)

table(spec.elect.coverage.expanded$species_range_covers_n_electorates)

prop.table(table(spec.elect.coverage.expanded$species_range_covers_n_electorates))
0.4499478624 + 0.1892596455 + 0.0740354536 + 0.0464025026

0.2101756511 + 0.0793458510 + 0.0502725621

spec.range.elect.expanded.1 <- spec.range.elect.expanded %>%
  filter(species_range_covers_n_electorates == 1)

prop.table(table(spec.range.elect.expanded.1$demographic_class))

spec.elect.coverage.expanded.4.greater.cover <- spec.elect.coverage.expanded %>%
  filter(species_range_covers_n_electorates > 3)

table(spec.elect.coverage.expanded.4.greater.cover$species_range_covers_n_electorates)

spec.elect.coverage.expanded.4.less.cover <- spec.elect.coverage.expanded %>%
  filter(species_range_covers_n_electorates < 4)

spec.elect.coverage.expanded.migratory <- spec.elect.coverage.expanded %>%
  filter(migratory_status %in% "Migratory")

spec.elect.coverage.expanded.fishes <- spec.elect.coverage.expanded %>%
  filter(taxon_group == "fishes")

table(spec.elect.coverage.expanded$migratory_status)
table(spec.elect.coverage.expanded.4.greater.cover$migratory_status)
table(spec.elect.coverage.expanded.4.less.cover$migratory_status)
table(spec.elect.coverage.expanded.4.cover$migratory_status)