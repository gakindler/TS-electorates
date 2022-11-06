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

spec.elect.coverage.counts <- fromJSON(
    "analysed_data/HPC_spatial_ops_output/spec.elect.coverage.counts.json"
)
spec.elect.coverage.indiv <- fromJSON(
    "analysed_data/HPC_spatial_ops_output/spec.elect.coverage.indiv.json"
)
spec.outside.elect <- st_read(
    "analysed_data/HPC_spatial_ops_output/spec.outside.elect.gpkg"
)
spec.per.elect.counts <- st_read(
    "analysed_data/HPC_spatial_ops_output/spec.per.elect.counts.gpkg"
)
spec.per.elect.indiv <- fromJSON(
    "analysed_data/HPC_spatial_ops_output/spec.per.elect.indiv.json"
)
spec.range.elect <- fromJSON(
    "analysed_data/HPC_spatial_ops_output/spec.range.elect.json"
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
    mutate(across(electorate_area_sqkm, signif, digits = 3)) %T>%
    write.csv(
        "analysed_data/local_analysis_output/elect.demo.summary.csv",
        row.names = FALSE
    )

#### Quick clean that need to be fixed TODO ####

spec.range.elect <- spec.range.elect %>%
    mutate(across(percent_range_within, signif, digits = 3))

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
    ) %T>%
    write.csv(
        "analysed_data/local_analysis_output/spec.per.elect.expanded.summary.csv",
        row.names = FALSE
    )

#### expanded range summary - spec.range.elect/spec.elect.coverage.indiv ####

spec.range.elect.temp <- spec.range.elect %>%
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

spec.range.elect.robust <- spec.range.elect.temp %>%
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

spec.range.elect.expanded.summary <- spec.range.elect.temp |>
  full_join(spec.range.elect.robust) |>
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

spec.eighty.elect.counts <- spec.range.elect %>%
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

spec.endemic.elect.counts <- spec.range.elect %>%
    select(!c(electorate_area_sqkm, species_range_area_sqkm)) %>%
    inner_join(spec.elect.coverage.indiv) %>%
    filter(
        percent_range_within == 1 | species_range_covers_n_electorates == 1
    ) %>%
    group_by(electorate) %>%
    summarise(total_endemic_unique_species = n_distinct(scientific_name)) %>%
    ungroup()

spec.per.elect.counts.summary <- spec.per.elect.counts %>%
    st_set_geometry(NULL) %>%
    full_join(spec.eighty.elect.counts) %>%
    full_join(spec.endemic.elect.counts) %>%
    inner_join(elect.demo) %>%
    mutate(
        species_per_sqkm = total_unique_species / electorate_area_sqkm
    ) %>%
    mutate(across(species_per_sqkm, signif, digits = 3)) %>%
    select(!electorate_area_sqkm) %>%
    inner_join(elect) %>%
    st_as_sf() %>%
    mutate(across(electorate_area_sqkm, signif, digits = 3)) %>%
    relocate(
        electorate, electorate_abbrev, state_territory,
        state_territory_abbrev, demographic_class,
        electorate_area_sqkm, total_unique_species,
        species_per_sqkm, total_eighty_unique_species,
        total_endemic_unique_species, geom
    ) %T>%
    st_write(
        "analysed_data/local_analysis_output/spec.per.elect.counts.summary.gpkg",
        layer = "spec.per.elect.counts.summary", append = FALSE
    ) %>%
    st_set_geometry(NULL) %T>%
    write.csv(
        "analysed_data/local_analysis_output/spec.per.elect.counts.summary.csv",
        row.names = FALSE
    )

#### expanded endemic/eighty - spec.eighty.elect.indiv ####

spec.range.elect.eighty.expanded <- spec.range.elect %>%
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

spec.range.elect.endemic.expanded <- spec.range.elect %>%
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
