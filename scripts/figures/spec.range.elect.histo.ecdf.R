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

# table(spec.elect.coverage.expanded$species_range_covers_n_electorates, useNA = "ifany")

# table(spec.elect.coverage.expanded$migratory_status, useNA = "ifany")

#### Attempts at optimising ####

# table(spec.range.elect.expanded$species_range_covers_n_electorates, useNA = "ifany")

table(spec.range.elect.expanded.ecdf$robust_species_range_covers_n_electorates, useNA = "ifany")


spec.range.elect.expanded.ecdf <- spec.range.elect.expanded %>%
    mutate(
        robust_species_range_covers_n_electorates = case_when(
            percent_range_within == 1 ~ 1,
            # species_range_covers_n_electorates == 1 ~ 1,
            TRUE ~ as.numeric(species_range_covers_n_electorates)
        )
    ) %>%
    # select(!species_range_covers_n_electorates) %>%
    group_by(scientific_name) %>%
    summarise(robust_species_range_covers_n_electorates = min(robust_species_range_covers_n_electorates)) %>%
    ungroup()

spec.range.elect.expanded.ecdf %>%
    group_by(scientific_name) %>%
    filter(n()>1)

#### faceting the ECDF ####

# spec.elect.coverage.expanded.ecdf <- spec.elect.coverage.expanded %>%
#     mutate(
#         ecdf_cut_off = case_when(
#             species_range_covers_n_electorates <= 10 ~ "Less than or equal 10",
#             species_range_covers_n_electorates > 10 ~ "Greater than 10"
#         )
#     )

# factor(ecdf_cut_off, c("Less than or equal 10", "Greater than 10"))

# levels(spec.elect.coverage.expanded.ecdf$ecdf_cut_off)

# spec.elect.coverage.expanded.ecdf$ecdf_cut_off <- factor(spec.elect.coverage.expanded.ecdf$ecdf_cut_off, c("Less than or equal 10", "Greater than 10"))

#### ECDF ####

ggplot(
    spec.range.elect.expanded.ecdf
) +
    aes(
        x = robust_species_range_covers_n_electorates
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
        x = "Threatened species' Commonwealth Electoral Division coverage",
        y = "Cumulative proportion"
    ) +
    scale_x_continuous(
        breaks = seq(0, 150, 10)
    ) +
    scale_y_continuous(
        breaks = seq(0, 1.0, 0.05)
    ) +
    theme_classic()
# facet_wrap(
#     ~ ecdf_cut_off,
#     scales = "free",
#     ncol = 2
# )

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