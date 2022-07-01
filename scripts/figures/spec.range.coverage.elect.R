# Creating plot for range size and CED coverage

#### Libraries ####

library(tidyverse)
library(viridis)
library(httpgd)
library(data.table)
library(dplyr)
library(ggrepel)

#### Import ####

spec.elect.coverage.expanded <- read.csv(
    "analysed_data/local_analysis_output/spec.elect.coverage.expanded.csv"
)

spec.range.elect.endemic.expanded <- read.csv(
  "analysed_data/local_analysis_output/spec.range.elect.endemic.expanded.csv"
)

spec.range.elect.expanded.summary <- read_csv(
    "analysed_data/local_analysis_output/spec.range.elect.expanded.summary.csv"
)

species_clean <- st_read(
    "clean_data/species.clean.gpkg"
)

#### Further robust/clean up ####

spec.robust.coverage <- spec.range.elect.expanded.summary %>%
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

spec.robust.coverage %>%
    group_by(scientific_name) %>%
    filter(n()>1)

table(spec.robust.coverage$robust_species_range_covers_n_electorates)

spec.range <- spec.range.elect.expanded.summary %>%
    group_by(scientific_name, vernacular_name, species_range_area_sqkm) %>%
    summarise()

spec.coverage.range <- spec.robust.coverage %>%
    inner_join(spec.range) %>%
    mutate(
        vernacular_name_other = word(
            vernacular_name, 2, -1,
            sep = fixed(", ")
        )
    ) %>%
    mutate(
        vernacular_name_first = word(
            vernacular_name, 1, 1,
            sep = fixed(", ")
        )
    ) %>%
    mutate(
        vernacular_name_first_clean = word(
            vernacular_name_first, 1, 1,
            sep = fixed("(")
        )
    )

#### boxplot ####

ggplot(spec.coverage.range) +
    aes(
        x = species_range_area_sqkm,
        y = robust_species_range_covers_n_electorates,
        group = robust_species_range_covers_n_electorates
    ) +
    geom_boxplot()

ggsave("figures/spec.coverage.range.box.png",
    width = 20, height = 15, units = "cm"
)

#### non-parametric regression ####

# Figure 5. The relationship between range size and CED coverage (scatter plot, x -axis range size, y-axis CED coverage). Do some stats on it – be logistic regression from memory.

hist(log2(spec.coverage.range$species_range_area_sqkm), breaks=100)

ggplot(spec.coverage.range) +
    aes(
        x = species_range_area_sqkm,
        y = robust_species_range_covers_n_electorates
    ) +
    geom_point(
        alpha = 0.4
    ) +
    geom_text_repel(
        data = spec.coverage.range %>% filter(
            species_range_area_sqkm>1000000 | robust_species_range_covers_n_electorates>50
        ),
        aes(label = vernacular_name_first_clean)
    ) +
    # geom_smooth(
    #     # method = "lm",
    #     show.legend = FALSE,
    #     colour = "black",
    #     se = FALSE
    # ) +
    labs(
        x = bquote(~ "Species range area" ~ (km^2)),
        y = "Threatened species' Commonwealth Electoral Division coverage"
    ) +
    theme_bw() +
    theme(
        strip.background = element_blank()
    )

ggsave("figures/spec.coverage.range.scatter.png",
    width = 20, height = 15, units = "cm"
)

summary <- spec.coverage.range %>%
    group_by(robust_species_range_covers_n_electorates) %>%
    summarise(
        min = min(species_range_area_sqkm),
        q1 = quantile(species_range_area_sqkm, prob = 0.25),
        median = median(species_range_area_sqkm),
        q3 = quantile(species_range_area_sqkm, prob = 0.75),
        max = max(species_range_area_sqkm)
    )

ggplot(summary) +
    aes(
        x = robust_species_range_covers_n_electorates,
        y = max
    ) +
    geom_line() + coord_flip()

ggsave(
    "figures/spec.coverage.range.line.max.png",
    width = 20, height = 15, units = "cm"
)

one <- spec.coverage.range %>%
    filter(robust_species_range_covers_n_electorates == 1)

two_four <- spec.coverage.range %>%
    filter(robust_species_range_covers_n_electorates>=2 & robust_species_range_covers_n_electorates<=4)

species_clean_two_four <- species_clean %>%
    right_join(two_four) %T>%
    st_write(
        "analysed_data/species.clean.two_four.gpkg"
    )
