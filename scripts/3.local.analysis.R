# Back end spatial operations to answer research questions

#### Libraries ####

library(tidyverse)
library(sf)
library(rmapshaper) # For installing use 'library(remotes)'
library(jsonlite)
library(magrittr)
library(units)

#### Import HPC outputs ####

# elect.aus.union.difference <- st_read(
#   "analysed_data/HPC_spatial_ops_output/elect.aus.union.difference.dpkg"
# )
spec.per.elect.counts <- st_read(
  "analysed_data/HPC_spatial_ops_output/spec.per.elect.counts.gpkg"
)
spec.per.elect.indiv <- fromJSON(
  "analysed_data/HPC_spatial_ops_output/spec.per.elect.indiv.json"
)
elect.spec.cover.counts <- fromJSON(
  "analysed_data/HPC_spatial_ops_output/elect.spec.cover.counts.json"
)
elect.spec.cover.indiv <- fromJSON(
  "analysed_data/HPC_spatial_ops_output/elect.spec.cover.indiv.json"
)
spec.range.elect <- fromJSON(
  "analysed_data/HPC_spatial_ops_output/spec.range.elect.json"
)
spec.outside.elect <- st_read(
  "analysed_data/HPC_spatial_ops_output/spec.outside.elect.gpkg"
)

#### Aus, elect, species: Import clean data ####

aus <- st_read("clean_data/aus.clean.gpkg")
aus.union <- st_read("clean_data/aus.union.clean.gpkg")
elect <- st_read("clean_data/elect.clean.gpkg")
elect.union <- st_read("clean_data/elect.union.clean.gpkg")
species <- st_read("clean_data/species.clean.gpkg")

# #### Demography: Import and clean ####

# demo <- readxl::read_xlsx("raw_data/AEC_demographic-classification-1-january-2019/01-demographic-classification-as-at-1-january-2019.xlsx")

demo <- readxl::read_xlsx(
  "raw_data/demographic-classification-as-at-2-august-2021.xlsx"
)

demo <- demo %>%
  rename(
    State_territory = "State or territory",
    Demographic_class = "Demographic classification",
    Elect_div = "Electoral division"
  )

#### Elect.demo ####

elect.demo <- elect %>%
  st_set_geometry(NULL) %>%
  inner_join(demo) %>%
  mutate(Elect_div_abbrev = abbreviate(Elect_div, minlength = 4L))

#### spec.per.elect ####

spec.per.elect.counts <- spec.per.elect.counts %>%


spec.per.elect.indiv.wide <- spec.per.elect.indiv %>%
  unite(
    "VERNACULAR_NAME-SCIENTIFIC_NAME",
    VERNACULAR_NAME:SCIENTIFIC_NAME,
    sep = "-"
  ) %>%
  select(c(
    "Elect_div",
    "VERNACULAR_NAME-SCIENTIFIC_NAME",
    "total_unique_spec"
    )
  ) %>%
  pivot_wider(
    names_from = Elect_div
    values_from =
      elect_area_sqkm, 
      VERNACULAR_NAME-SCIENTIFIC_NAME,
      total_unique_spec
  )

[1] "Elect_div"                       "elect_area_sqkm"                
[3] "VERNACULAR_NAME-SCIENTIFIC_NAME" "THREATENED_STATUS"              
[5] "TAXON_GROUP"                     "species_area_sqkm"              
[7] "total_unique_spec"               "MIGRATORY_STATUS"

#### elect.spec.cover ####

elect.spec.cover.counts <- elect.spec.cover.counts %>%
  write.csv(
    "analysed_data/local_analysis_outpu/elect.spec.cover.counts.csv",
    row.names = FALSE
  )

elect.spec.cover.indiv <- elect.spec.cover.indiv %>%
  unite(
    "VERNACULAR_NAME-SCIENTIFIC_NAME",
    VERNACULAR_NAME:SCIENTIFIC_NAME,
    sep = "-"
  ) %>%
  select(c(
    "Elect_div",
    "VERNACULAR_NAME-SCIENTIFIC_NAME",
    "total_unique_spec"
    )
  ) %>%
  pivot_wider(
    names_from = Elect_div
    values_from =
      elect_area_sqkm, 
      VERNACULAR_NAME-SCIENTIFIC_NAME,
      total_unique_spec
  )
  write.csv(
  "analysed_data/local_analysis_output/elect.spec.cover.csv",
  row.names = FALSE
)

#### spec.range.elect ####

spec.range.elect <- spec.range.elect %T>%
  # mutate(across(percent_range_within, round, digits = 2)) %>%
  # relocate(
  #   SCIENTIFIC_NAME, VERNACULAR_NAME, THREATENED_STATUS,
  #   TAXON_GROUP, MIGRATORY_STATUS, species_area_sqkm,
  #   Elect_div, elect_area_sqkm, intersection_area_sqkm,
  #   percent_range_within
  # ) %T>%
  write.csv(
  "analysed_data/local_analysis_output/spec.range.elect.csv",
  row.names = FALSE
)

#### spec.per.elect.counts.summary ####

spec.eighty.elect.counts <- spec.range.elect %>%
  mutate(across(percent_range_within, round, digits = 2)) %>%
  filter(percent_range_within >= 0.8) %>%
  group_by(Elect_div) %>%
  summarise(total_eighty_unique_spec = n_distinct(SCIENTIFIC_NAME)) %>%
  ungroup()

spec.endemic.elect.counts <- spec.range.elect %>%
  mutate(across(percent_range_within, round, digits = 2)) %>%
  filter(percent_range_within == 1) %>%
  group_by(Elect_div) %>%
  summarise(total_endemic_unique_spec = n_distinct(SCIENTIFIC_NAME)) %>%
  ungroup()

spec.per.elect.counts.summary <- spec.per.elect.counts %>%
  st_set_geometry(NULL) %>%
  full_join(spec.eighty.elect.counts) %>%
  full_join(spec.endemic.elect.counts) %>%
  inner_join(elect.demo) %>%
  mutate(
    species_per_sqkm = total_unique_spec / elect_area_sqkm
  ) %>%
  inner_join(elect) %>%
  st_as_sf() %>%
  relocate(
    Elect_div, Elect_div_abbrev, State_territory,
    Demographic_class, elect_area_sqkm, total_unique_spec,
    species_per_sqkm, total_eighty_unique_spec,
    total_endemic_unique_spec, geom
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

#### spec.eighty.elect.indiv ####

spec.eighty.elect.indiv <- spec.range.elect %>%
  mutate(across(percent_range_within, round, digits = 2)) %>%
  filter(percent_range_within >= 0.8) %>%
  group_by(Elect_div) %>%
  mutate(total_eighty_unique_spec = n_distinct(SCIENTIFIC_NAME)) %>%
  ungroup() %>%
  st_set_geometry(NULL) %T>%
  write.csv(
    "analysed_data/local_analysis_output/spec.eighty.elect.indiv.csv",
    row.names = FALSE
  )

spec.endemic.elect.indiv <- spec.range.elect %>%
  filter(percent_range_within == 1) %>%
  group_by(Elect_div) %>%
  mutate(total_endemic_unique_spec = n_distinct(SCIENTIFIC_NAME)) %>%
  ungroup() %>%
  st_set_geometry(NULL) %T>%
  write.csv(
    "analysed_data/local_analysis_output/spec.endemic.elect.indiv.csv",
    row.names = FALSE
  )

#### spec.eighty.outside.elect ####

spec.outside.elect.eighty <- spec.outside.elect %T>%
  write.csv(
    "analysed_data/local_analysis_output/spec.outside.elect.csv",
    row.names = FALSE
  ) %>%
  filter(percent_range_difference >= .8) %>%
  write.csv(
    "analysed_data/local_analysis_output/spec.outside.elect.eighty.csv",
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
  mutate(elect_union_sqkm = units::set_units(st_area(.), km^2) %>%
    as.numeric()) %>%
  st_sym_difference(aus.union.simp) %>%
  mutate(aus_union_difference_sqkm = units::set_units(st_area(.), km^2) %>%
    as.numeric()) %T>%
  st_write(
    "analysed_data/local_analysis_output/elect.aus.union.difference.gpkg",
    layer = "elect.aus.union.difference", append = FALSE, delete_dsn = TRUE
  )
