# Back end spatial operations to answer research questions
# Run on the UQ HPC with associated data stored of UQ RDM
# Project: Q4107
# TODO: rerun 22-02-09 but as interactive,

#### Libraries ####

library(tidyverse)
library(sf)
library(jsonlite)
library(magrittr)
library(units)

#### Import data - aus, elect, species ####

aus <- st_read(
    "/QRISdata/Q4107/TS_electorates/clean_data/aus.clean.gpkg"
)
aus.union <- st_read(
    "/QRISdata/Q4107/TS_electorates/clean_data/aus.union.clean.gpkg"
)
elect <- st_read(
    "/QRISdata/Q4107/TS_electorates/clean_data/elect.clean.gpkg"
)
elect.union <- st_read(
    "/QRISdata/Q4107/TS_electorates/clean_data/elect.union.clean.gpkg"
)
species <- st_read(
    "/QRISdata/Q4107/TS_electorates/clean_data/species.clean.gpkg"
)
species.unclipped <- st_read(
    "/QRISdata/Q4107/TS_electorates/clean_data/species.clean.unclipped.gpkg"
)

#### spec.per.elect ####

spec.per.elect <- elect %>%
    st_join(species)

spec.per.elect.counts <- spec.per.elect %>%
    group_by(electorate) %>%
    summarise(total_unique_species = n_distinct(scientific_name)) %>%
    ungroup() %T>%
    st_write(
        "/QRISdata/Q4107/TS_electorates/analysed_data/HPC_spatial_ops_output/spec.per.elect.counts.gpkg",
        layer = "spec.per.elect.counts", append = FALSE, delete_dsn = TRUE
    )

spec.per.elect.indiv <- spec.per.elect %>%
    st_set_geometry(NULL) %>%
    group_by(electorate) %>%
    mutate(total_unique_species = n_distinct(scientific_name)) %>%
    ungroup() %T>%
    write_json(
        "/QRISdata/Q4107/TS_electorates/analysed_data/HPC_spatial_ops_output/spec.per.elect.indiv.json"
    )

#### spec.elect.coverage - species electorate coverage  ####

spec.elect.coverage.counts <- spec.per.elect %>%
    st_set_geometry(NULL) %>%
    group_by(
        scientific_name, vernacular_name, threatened_status,
        migratory_status, taxon_group
    ) %>%
    summarise(species_range_covers_n_electorates = n_distinct(electorate)) %T>%
    write_json(
        "/QRISdata/Q4107/TS_electorates/analysed_data/HPC_spatial_ops_output/spec.elect.coverage.counts.json"
    )

spec.elect.coverage.indiv <- spec.per.elect %>%
    st_set_geometry(NULL) %>%
    group_by(
        scientific_name, vernacular_name, threatened_status,
        migratory_status, taxon_group
    ) %>%
    mutate(species_range_covers_n_electorates = n_distinct(electorate)) %T>%
    write_json(
        "/QRISdata/Q4107/TS_electorates/analysed_data/HPC_spatial_ops_output/spec.elect.coverage.indiv.json"
    )

#### spec.range.elect - species range within each electorate ####

spec.range.elect <- species %>%
    st_intersection(elect) %>%
    st_make_valid() %>%
    mutate(
        across(species_range_area_sqkm, signif, digits = 3)
    ) %>%
    mutate(
        intersection_area_sqkm = units::set_units(st_area(.), km^2) %>% as.numeric()
    ) %>%
    mutate(
        across(
            intersection_area_sqkm, signif,
            digits = 3
        )
    ) %>%
    mutate(
        percent_range_within = intersection_area_sqkm / species_range_area_sqkm
    ) %>%
    mutate(across(
        percent_range_within, signif,
        digits = 3
    )) %>%
    st_set_geometry(NULL) %T>%
    write_json(
        "/QRISdata/Q4107/TS_electorates/analysed_data/HPC_spatial_ops_output/spec.range.elect.json"
    )

#### spec.outside.elect ####

spec.outside.elect <- species.unclipped %>%
    st_difference(elect.union) %>%
    st_make_valid() %>%
    mutate(species_difference_area_sqkm = units::set_units(st_area(.), km^2) %>% as.numeric()) %>%
    mutate(percent_range_difference = species_difference_area_sqkm / species_range_area_sqkm) %>%
    mutate(across(percent_range_difference, signif, digits = 3)) %T>%
    st_write(
        "/QRISdata/Q4107/TS_electorates/analysed_data/HPC_spatial_ops_output/spec.outside.elect.gpkg",
        layer = "spec.outside.elect", append = FALSE, delete_dsn = TRUE
    )