# Creating plots using the spatial.ops data

#### Libraries ####

library(tidyverse)
library(viridis)
library(httpgd)
library(data.table)
library(dplyr)

#### Import ####

spec.elect.coverage.expanded <- read.csv(
    "analysed_data/local_analysis_output/spec.elect.coverage.expanded.csv"
)

spec.range.elect.expanded <- read.csv(
    "analysed_data/local_analysis_output/spec.range.elect.expanded.summary.csv"
) %>%
as_tibble()

spec.range.elect.endemic.expanded <- read.csv(
  "analysed_data/local_analysis_output/spec.range.elect.endemic.expanded.csv"
)

#### Attempts at optimising ####

spec.range.elect.expanded.rob <- spec.range.elect.expanded %>%
    mutate(
        robust_species_range_covers_n_electorates = case_when(
            percent_range_within == 1 ~ 1,
            # species_range_covers_n_electorates == 1 ~ 1,
            TRUE ~ as.numeric(species_range_covers_n_electorates)
        )
    ) %>%
    group_by(scientific_name) %>%
    summarise(
        robust_species_range_covers_n_electorates = min(
          robust_species_range_covers_n_electorates
        )
    ) %>%
    ungroup() %>%
    mutate(
    robust_species_range_covers_n_electorates = as.integer(
      robust_species_range_covers_n_electorates
    )
  )

spec.range.elect.expanded.rob %>%
    group_by(scientific_name) %>%
    filter(n()>1)

#### ECDF ####

# zoomed
ggplot(
  spec.range.elect.expanded.rob
) +
    aes(
        x = robust_species_range_covers_n_electorates
    ) +
    stat_ecdf(
        geom = "step",
        pad = FALSE
    ) +
    labs(
        x = "Threatened species' Commonwealth Electoral Division coverage",
        y = "Cumulative proportion"
    ) +
  scale_x_continuous(
    breaks = seq(1, 10, 1)
  ) +
  coord_cartesian(
    xlim = c(1, 10)
  ) +
    scale_y_continuous(
        breaks = seq(0, 1.0, 0.05)
    ) +
    theme_classic()

ggsave("figures/spec.range.elect.zoom.ecdf.png",
    width = 20, height = 15, units = "cm"
)

ggplot(
  spec.range.elect.expanded.rob
) +
    aes(
        x = robust_species_range_covers_n_electorates
    ) +
    stat_ecdf(
        geom = "step",
        pad = FALSE
    ) +
    labs(
        x = "Threatened species' Commonwealth Electoral Division coverage",
        y = "Cumulative proportion"
    ) +
  scale_x_continuous(
    breaks = seq(0, 150, 5)
  ) +
#   coord_cartesian(
#     xlim = c(1, 10)
#   ) +
    scale_y_continuous(
        breaks = seq(0, 1.0, 0.05)
    ) +
    theme_classic()

ggsave("figures/spec.range.elect.full.ecdf.png",
    width = 20, height = 15, units = "cm"
)

# Getting "Error in f(...) : Graphics API version mismatch" error

### Calculations ####

summary(spec.elect.coverage.expanded)

table(spec.range.elect.expanded.rob$robust_species_range_covers_n_electorates)

prop.table(table(spec.elect.coverage.expanded$species_range_covers_n_electorates))
0.4499478624 + 0.1892596455 + 0.0740354536 + 0.0464025026

0.2101756511 + 0.0793458510 + 0.0502725621

spec.range.elect.expanded.1 <- spec.range.elect.expanded %>%
    filter(species_range_covers_n_electorates == 1)

prop.table(table(spec.range.elect.expanded.1$demographic_class))

spec.range.elect.expanded.rob %>%
    filter(robust_species_range_covers_n_electorates <= 4) %>%
    nrow() / 1651

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

######################

<- spec.range.elect.expanded.rob


spec.elect.coverage.expanded.1.greater.cover <- spec.range.elect.expanded.rob %>%
    filter(robust_species_range_covers_n_electorates == 1)
dim(spec.elect.coverage.expanded.1.greater.cover)[1]
dim(spec.elect.coverage.expanded.1.greater.cover)[1]/1651

spec.elect.coverage.expanded.24.cover <- spec.range.elect.expanded.rob %>%
    filter(between(robust_species_range_covers_n_electorates, 2, 4))
dim(spec.elect.coverage.expanded.24.cover)[1]
dim(spec.elect.coverage.expanded.24.cover)[1]/1651

spec.elect.coverage.expanded.5.greater.cover <- spec.range.elect.expanded.rob %>%
    filter(robust_species_range_covers_n_electorates > 4)
dim(spec.elect.coverage.expanded.5.greater.cover)[1]
dim(spec.elect.coverage.expanded.5.greater.cover)[1]/1651


table(spec.elect.coverage.expanded.5.greater.cover$robust_species_range_covers_n_electorates)
