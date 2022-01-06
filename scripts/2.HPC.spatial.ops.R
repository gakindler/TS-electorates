# Back end spatial operations to answer research questions
# Run on the UQ HPC with associated data stored of UQ RDM
# Project: Q4107

#### Libraries ####

library(tidyverse)
library(sf)
library(jsonlite)
library(magrittr)
library(units)

#### Import data - aus, elect, species ####

aus <- st_read("/QRISdata/Q4107/TS_electorates/clean_data/aus.clean.gpkg")
elect <- st_read("/QRISdata/Q4107/TS_electorates/clean_data/elect.clean.gpkg")
elect.union <- st_read("/QRISdata/Q4107/TS_electorates/clean_data/elect.union.clean.gpkg")
species <- st_read("/QRISdata/Q4107/TS_electorates/clean_data/species.clean.gpkg")

#### elect.aus.union.difference ####
# Quantifying the difference between electorates and terrestrial Aus

elect.aus.union.difference <- elect.union %>%
  mutate(elect_union_sqkm = units::set_units(st_area(.), km^2) %>%
    as.numeric()) %>%
  st_sym_difference(aus.union) %>%
  mutate(aus_union_difference_sqkm = units::set_units(st_area(.), km^2) %>%
    as.numeric()) %T>%
  st_write(
    "/QRISdata/Q4107/TS_electorates/analysed_data/HPC_spatial_ops_output/elect.aus.union.difference.gpkg",
    layer = "elect.aus.union.difference", append = FALSE, delete_dsn = TRUE
  )

#### spec.per.elect ####

spec.per.elect <- elect %>%
  st_join(species)

spec.per.elect.counts <- spec.per.elect %>%
  group_by(Elect_div) %>%
  summarise(total_unique_spec = n_distinct(SCIENTIFIC_NAME)) %>%
  ungroup() %T>%
  st_write(
    "/QRISdata/Q4107/TS_electorates/analysed_data/HPC_spatial_ops_output/spec.per.elect.counts.gpkg",
    layer = "spec.per.elect.counts", append = FALSE, delete_dsn = TRUE
  )

spec.per.elect.indiv <- spec.per.elect %>%
  st_set_geometry(NULL) %>%
  group_by(Elect_div) %>%
  mutate(total_unique_spec = n_distinct(SCIENTIFIC_NAME)) %>%
  ungroup() %T>%
  write_json(
    "/QRISdata/Q4107/TS_electorates/analysed_data/HPC_spatial_ops_output/spec.per.elect.indiv.json"
  )

#### elect.spec.cover ####

elect.spec.cover.counts <- spec.per.elect  %>%
  st_set_geometry(NULL) %>%
  group_by(
    SCIENTIFIC_NAME, VERNACULAR_NAME, THREATENED_STATUS,
    MIGRATORY_STATUS, TAXON_GROUP
  ) %>%
  summarise(elect_range_covers = n_distinct(Elect_div)) %T>%
  write_json(
    "/QRISdata/Q4107/TS_electorates/analysed_data/HPC_spatial_ops_output/elect.spec.cover.counts.json"
  )

elect.spec.cover.indiv <- spec.per.elect  %>%
  st_set_geometry(NULL) %>%
  group_by(
    SCIENTIFIC_NAME, VERNACULAR_NAME, THREATENED_STATUS,
    MIGRATORY_STATUS, TAXON_GROUP
  ) %>%
  mutate(elect_range_covers = n_distinct(Elect_div)) %T>%
  write_json(
    "/QRISdata/Q4107/TS_electorates/analysed_data/HPC_spatial_ops_output/elect.spec.cover.indiv.json"
  )

#### spec.range.elect - species range within each electorate ####
# Calculate the percentage of species area within each electorate
spec.range.elect <- species %>%
  st_intersection(elect) %>%
  st_make_valid() %>%
  mutate(
    intersection_area_sqkm = units::set_units(st_area(.), km^2) %>% as.numeric()
  ) %>%
  mutate(percent_range_within = intersection_area_sqkm / species_area_sqkm) %>%
  # Negates floating point problems (hopefully)
  mutate(across(percent_range_within, round, digits = 2)) %>%
  st_set_geometry(NULL) %>%
  inner_join(elect) %>%
  st_as_sf() %T>%
  st_write(
    "/QRISdata/Q4107/TS_electorates/analysed_data/HPC_spatial_ops_output/spec.range.elect.gpkg",
    layer = "spec.range.elect", append = FALSE, delete_dsn = TRUE
  )

#### spec.outside.elect ####

spec.outside.elect <- species %>%
  st_difference(elect.union) %>%
  st_make_valid() %>%
  mutate(species_difference_area_sqkm = units::set_units(st_area(.), km^2) %>% as.numeric()) %>%
  mutate(percent_range_difference = species_difference_area_sqkm / species_area_sqkm) %>%
  mutate(across(percent_range_outside, round, digits = 2)) %T>%
  st_write(
    "/QRISdata/Q4107/TS_electorates/analysed_data/HPC_spatial_ops_output/spec.outside.elect.gpkg",
    layer = "spec.outside.elect", append = FALSE, delete_dsn = TRUE
  )
