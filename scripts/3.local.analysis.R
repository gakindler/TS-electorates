# Back end spatial operations to answer research questions
# TODO: rerun 22-01-09,

#### Libraries ####

library(tidyverse)
library(sf)
library(rmapshaper) # For installing use 'library(remotes)'
library(jsonlite)
library(magrittr)
library(units)

#### Import: HPC outputs ####

spec.range.elect <- fromJSON(
    "analysed_data/HPC_spatial_ops_output/spec.range.elect.json"
) |>
    as_tibble()
spec.outside.elect <- st_read(
    "analysed_data/HPC_spatial_ops_output/spec.outside.elect.gpkg"
)

#### Import: Australia, electorates, species, demography ####

aus <- st_read("clean_data/aus.clean.gpkg")
aus.union <- st_read("clean_data/aus.union.clean.gpkg")
demo <- read.csv("clean_data/demo.clean.csv")
elect <- st_read("clean_data/elect.clean.gpkg")
elect.union <- st_read("clean_data/elect.union.clean.gpkg")
species <- st_read("clean_data/species.clean.gpkg")
species.unclipped <- st_read("clean_data/species.clean.unclipped.gpkg")

#### elect.demo - summary ####

elect.demo <- elect %>%
    st_set_geometry(NULL) %>%
    inner_join(demo) %>%
    mutate(
      across(
        .cols = electorate_area_sqkm,
        .fns = ~ signif(.x, digits = 3)
        )
      ) |>
    as_tibble() %T>%
    write.csv(
        "analysed_data/local_analysis_output/elect.demo.summary.csv",
        row.names = FALSE
    )

#### applying threshold

spec.range.elect.threshold <- spec.range.elect |>
  filter(percent_range_within >= 0.10)

#### Creating the tables from the 2.HPC spatial ops

spec.per.elect.counts <- spec.range.elect.threshold %>%
  group_by(electorate) %>%
  summarise(total_unique_species = n_distinct(scientific_name)) %>%
  ungroup()

write.csv(spec.per.elect.counts,
  "analysed_data/local_analysis_output/spec.per.elect.counts.csv",
  row.names = FALSE
)

spec.per.elect.indiv <- spec.range.elect.threshold %>%
  group_by(electorate) %>%
  mutate(total_unique_species = n_distinct(scientific_name)) %>%
  ungroup()

write.csv(spec.per.elect.indiv,
  "analysed_data/local_analysis_output/spec.per.elect.indiv.csv",
  row.names = FALSE
)

#### spec.elect.coverage - species electorate coverage  ####

spec.elect.coverage.counts <- spec.range.elect.threshold %>%
  group_by(
      scientific_name, vernacular_name, threatened_status,
      migratory_status, taxon_group
  ) %>%
  summarise(species_range_covers_n_electorates = n_distinct(electorate))

write.csv(spec.elect.coverage.counts,
  "analysed_data/local_analysis_output/spec.elect.coverage.counts.csv",
  row.names = FALSE
)

spec.elect.coverage.indiv <- spec.range.elect.threshold %>%
  group_by(
      scientific_name, vernacular_name, threatened_status,
      migratory_status, taxon_group
  ) %>%
  mutate(species_range_covers_n_electorates = n_distinct(electorate))

write.csv(spec.elect.coverage.indiv,
  "analysed_data/local_analysis_output/spec.elect.coverage.indiv.csv",
  row.names = FALSE
)

#### expanded indiv summary - spec.per.elect.indiv ####

spec.per.elect.expanded.summary <- spec.per.elect.indiv %>%
    select(!electorate_area_sqkm) %>%
    inner_join(elect.demo) %>%
    relocate(
        scientific_name, vernacular_name, threatened_status,
        taxon_group, migratory_status, species_range_area_sqkm,
        electorate, electorate_abbrev, state_territory,
        state_territory_abbrev, demographic_class,
        electorate_area_sqkm, total_unique_species
    )

    write.csv(spec.per.elect.expanded.summary,
        "analysed_data/local_analysis_output/spec.per.elect.expanded.summary.csv",
        row.names = FALSE
    )

#### expanded range summary - spec.range.elect.threshold/spec.elect.coverage.indiv ####

spec.range.elect.threshold.temp <- spec.range.elect.threshold %>%
    select(!c(electorate_area_sqkm, species_range_area_sqkm)) %>%
    inner_join(spec.elect.coverage.indiv) %>%
    select(!electorate_area_sqkm) %>%
    inner_join(elect.demo) %>%
    relocate(
        scientific_name, vernacular_name, threatened_status,
        taxon_group, migratory_status, species_range_area_sqkm,
        species_range_covers_n_electorates,
        electorate, electorate_abbrev, state_territory,
        state_territory_abbrev, demographic_class,
        electorate_area_sqkm, intersection_area_sqkm,
        percent_range_within
    )

spec.range.elect.threshold.robust <- spec.range.elect.threshold.temp %>%
    mutate(
        robust_species_range_covers_n_electorates = case_when(
            percent_range_within == 1 ~ 1,
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

spec.range.elect.threshold.expanded.summary <- spec.range.elect.threshold.temp |>
  full_join(spec.range.elect.threshold.robust) |>
  as_tibble() |>
  select(
    scientific_name, vernacular_name, threatened_status,
    taxon_group, migratory_status, species_range_area_sqkm,
    robust_species_range_covers_n_electorates,
    electorate, electorate_abbrev, state_territory,
    state_territory_abbrev, demographic_class,
    electorate_area_sqkm, intersection_area_sqkm,
    percent_range_within
  ) |>
  rename(
    species_range_covers_n_electorates = robust_species_range_covers_n_electorates
    ) |>
    write.csv(
        "analysed_data/local_analysis_output/spec.range.elect.expanded.summary.csv",
        row.names = FALSE
    )

#### summary counts - spec.per.elect ####

spec.eighty.elect.counts <- spec.range.elect.threshold %>%
    select(!c(electorate_area_sqkm, species_range_area_sqkm)) %>%
    inner_join(spec.elect.coverage.indiv) %>%
    # there were 17 species distributions that overhang along borders such as marine
    # and thus their percent range
    # making it possible to capture
    filter(
        percent_range_within >= 0.8 | species_range_covers_n_electorates == 1
    ) %>%
    group_by(electorate) %>%
    summarise(total_eighty_unique_species = n_distinct(scientific_name)) %>%
    ungroup()

spec.endemic.elect.counts <- spec.range.elect.threshold %>%
    select(!c(electorate_area_sqkm, species_range_area_sqkm)) %>%
    inner_join(spec.elect.coverage.indiv) %>%
    filter(
        percent_range_within == 1 | species_range_covers_n_electorates == 1
    ) %>%
    group_by(electorate) %>%
    summarise(total_endemic_unique_species = n_distinct(scientific_name)) %>%
    ungroup()

spec.per.elect.counts.summary <- spec.per.elect.counts %>%
    full_join(spec.eighty.elect.counts) %>%
    full_join(spec.endemic.elect.counts) %>%
    inner_join(elect.demo) %>%
    mutate(
        species_per_sqkm = total_unique_species / electorate_area_sqkm
    ) %>%
    mutate(across(species_per_sqkm, signif, digits = 3)) %>%
    select(!electorate_area_sqkm) %>%
    inner_join(elect) |>
    st_as_sf() %>%
    mutate(across(electorate_area_sqkm, signif, digits = 3)) %>%
    relocate(
        electorate, electorate_abbrev, state_territory,
        state_territory_abbrev, demographic_class,
        electorate_area_sqkm, total_unique_species,
        species_per_sqkm, total_eighty_unique_species,
        total_endemic_unique_species, geom
    )

st_write(spec.per.elect.counts.summary,
    "analysed_data/local_analysis_output/spec.per.elect.counts.summary.gpkg",
    layer = "spec.per.elect.counts.summary", append = FALSE
)

spec.per.elect.counts.summary |>
    st_set_geometry(NULL) |>
    write.csv(
        "analysed_data/local_analysis_output/spec.per.elect.counts.summary.csv",
        row.names = FALSE
    )

spec.per.elect.counts.summary.all <- spec.per.elect.counts.summary |>
    st_set_geometry(NULL) |>
    full_join(elect.demo)

spec.per.elect.counts.summary.all |>
    write.csv(
        "analysed_data/local_analysis_output/spec.per.elect.counts.summary.all.csv",
        row.names = FALSE
    )

#### expanded endemic/eighty - spec.eighty.elect.indiv ####

spec.range.elect.threshold.eighty.expanded <- spec.range.elect.threshold %>%
    select(!c(electorate_area_sqkm, species_range_area_sqkm)) %>%
    inner_join(spec.elect.coverage.indiv) %>%
    select(!electorate_area_sqkm) %>%
    inner_join(elect.demo) %>%
    filter(
        percent_range_within >= 0.8 | species_range_covers_n_electorates == 1
    ) %>%
    group_by(electorate) %>%
    mutate(total_eighty_unique_spec = n_distinct(scientific_name)) %>%
    ungroup() %>%
    relocate(
        scientific_name, vernacular_name, threatened_status,
        taxon_group, migratory_status, species_range_area_sqkm,
        electorate, electorate_abbrev, state_territory,
        state_territory_abbrev, demographic_class,
        electorate_area_sqkm, intersection_area_sqkm,
        percent_range_within
    ) %T>%
    write.csv(
        "analysed_data/local_analysis_output/spec.range.elect.eighty.expanded.csv",
        row.names = FALSE
    )

spec.range.elect.threshold.endemic.expanded <- spec.range.elect.threshold %>%
    select(!c(electorate_area_sqkm, species_range_area_sqkm)) %>%
    inner_join(spec.elect.coverage.indiv) %>%
    select(!electorate_area_sqkm) %>%
    inner_join(elect.demo) %>%
    filter(
        percent_range_within == 1 | species_range_covers_n_electorates == 1
    ) %>%
    group_by(electorate) %>%
    mutate(total_endemic_unique_spec = n_distinct(scientific_name)) %>%
    ungroup() %>%
    relocate(
        scientific_name, vernacular_name, threatened_status,
        taxon_group, migratory_status, species_range_area_sqkm,
        electorate, electorate_abbrev, state_territory,
        state_territory_abbrev, demographic_class,
        electorate_area_sqkm, intersection_area_sqkm,
        percent_range_within
    ) %T>%
    write.csv(
        "analysed_data/local_analysis_output/spec.range.elect.endemic.expanded.csv",
        row.names = FALSE
    )

#### expanded eighty outside - spec.eighty.outside.elect ####

spec.outside.elect.expanded <- spec.outside.elect %>%
    st_set_geometry(NULL) %T>%
    write.csv(
        "analysed_data/local_analysis_output/spec.outside.elect.expanded.csv",
        row.names = FALSE
    )

#### ???? - spec.elect.coverage ####

spec.elect.coverage.expanded <- spec.elect.coverage.counts %>%
    relocate(
        scientific_name, vernacular_name,
        threatened_status, taxon_group,
        migratory_status, species_range_covers_n_electorates
    ) %T>%
    write.csv(
        "analysed_data/local_analysis_output/spec.elect.coverage.expanded.csv",
        row.names = FALSE
    )

#### elect.aus.union.difference ####

aus.union.simp <- aus.union %>%
    ms_simplify(
        keep = 0.01,
        keep_shape = TRUE
    ) %>%
    st_make_valid()

elect.union.simp <- elect.union %>%
    ms_simplify(
        keep = 0.01,
        keep_shape = TRUE
    ) %>%
    st_make_valid()

elect.aus.union.difference <- elect.union.simp %>%
    mutate(electorate_union_sqkm = units::set_units(st_area(.), km^2) %>%
        as.numeric()) %>%
    st_sym_difference(aus.union.simp) %>%
    mutate(aus_union_difference_sqkm = units::set_units(st_area(.), km^2) %>%
        as.numeric()) %>%
    st_set_geometry(NULL) %T>%
    write.csv(
        "analysed_data/local_analysis_output/elect.aus.union.difference.csv",
        row.names = FALSE
    )
