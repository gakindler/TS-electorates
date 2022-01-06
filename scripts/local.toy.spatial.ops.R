# Simplified data to experiment on before mounting on the HPC with the real data

#### Libraries ####

library(tidyverse)
library(sf)
library(rmapshaper) # For installing use 'library(remotes)'
library(jsonlite)
library(magrittr)
library(units)
library(httpgd)

#### Aus boundary: Import/clean ####

aus <- st_read("raw_data/ASGS_Edition_3_Aust_2021_shapefile/AUS_2021_AUST_GDA94.shp")
aus <- aus %>%
  select(geometry) %>%
  slice(1) %>%
  st_crop(
    xmin = 112.921114, ymin = -43.740510, # drop those pesky islands
    xmax = 153.638727, ymax = -9.115517
  )

aus.union <- aus %>%
  st_union(by_feature = FALSE) %>%
  st_sf()

#### Electorates: Import/clean ####

elect <- st_read("raw_data/AEC_electoral_boundaries_2019/COM_ELB_region.shp")
# The 'elect' file has a couple of contractions that do not match 'demo' file
elect$Elect_div <- gsub("Eden-monaro", "Eden-Monaro", elect$Elect_div)
elect$Elect_div <- gsub("Mcewen", "McEwen", elect$Elect_div)
elect$Elect_div <- gsub("Mcmahon", "McMahon", elect$Elect_div)
elect$Elect_div <- gsub("Mcpherson", "McPherson", elect$Elect_div)
elect$Elect_div <- gsub("O'connor", "O'Connor", elect$Elect_div)
elect <- elect %>%
  select(-c(
    "Numccds", "Actual", "Projected", "Area_SqKm",
    "Total_Popu", "Australian", "Sortname"
  )) %>%
  st_make_valid() %>%
  st_crop(
    xmin = 111.921114, ymin = -44.740510,
    xmax = 154.638727, ymax = -8.115517
    # remove the islands
  ) %>%
  mutate(
    elect_area_sqkm = units::set_units(st_area(.), km^2) %>%
      as.numeric()
  )

elect.union <- elect %>%
  st_union(by_feature = FALSE) %>%
  st_sf()

#### Species: Import/clean ####

species <- st_read("raw_data/SNES_public_1july2021.gdb")
species <- species %>%
  filter(PRESENCE_RANK == 2) %>%
  filter(!is.na(THREATENED_STATUS)) %>%
  filter(THREATENED_STATUS %in% c(
    "Vulnerable",
    "Endangered",
    "Critically Endangered",
    "Extinct in the wild"
  )) %>% # as in wardNationalscaleDatasetThreats2021
  filter(!MARINE %in% "Listed") %>%
  filter(!SCIENTIFIC_NAME %in% c(
    "Brachionichthys hirsutus", # Spotted Handfish
    "Brachiopsilus ziebelli", # Ziebell's Handfish, Waterfall Bay Handfish
    "Carcharias taurus (east coast population)", # Grey Nurse Shark (east coast population)
    "Carcharias taurus (west coast population)", # Grey Nurse Shark (west coast population)
    "Carcharodon carcharias", # White Shark, Great White Shark
    "Epinephelus daemelii", # Black Rockcod, Black Cod, Saddled Rockcod
    "Glyphis garricki", # Northern River Shark, New Guinea River Shark
    "Glyphis glyphis", # Speartooth Shark
    "Pristis clavata", # Dwarf Sawfish, Queensland Sawfish
    "Rhincodon typus", # Whale Shark
    "Thymichthys politus", # Red Handfish
    "Zearaja maugeana" # Maugean Skate, Port Davey Skate
  )) %>%
  # some marine species were not being filtered out, remove all marine species, keep inland freshwater species
  filter(!SCIENTIFIC_NAME %in% c(
    "Calidris canutus", # Red Knot, Knot
    "Calidris ferruginea", # Curlew Sandpiper
    "Calidris tenuirostris", # Great Knot
    "Hirundapus caudacutus" # White-throated Needletail
  )) %>%
  # MARINE == "Listed - overfly marine area"
  filter(!CETACEAN %in% "Cetacean") %>%
  select(c(
    "SCIENTIFIC_NAME", "VERNACULAR_NAME", "THREATENED_STATUS",
    "MIGRATORY_STATUS", "TAXON_GROUP", "Shape"
  )) %>%
  st_make_valid() %>%
  # Merge species at the broad taxa as there are duplicate polygons
  # Probably attributable to subspecies populations, but still ¯\_(ツ)_/¯
  group_by(
    SCIENTIFIC_NAME, VERNACULAR_NAME, THREATENED_STATUS,
    MIGRATORY_STATUS, TAXON_GROUP
  ) %>%
  summarise() %>%
  ungroup() %>%
  st_make_valid() %>%
  mutate(
    species_area_sqkm = units::set_units(st_area(.), km^2) %>%
      as.numeric()
  )

#### CRS check ####

if (st_crs(species) == st_crs(elect) && st_crs(elect) == st_crs(aus)) {
  print("Species, elect, and Aus boundary CRS's are the same")
} else {
  print("We got a CRS mismatch")
}

#### Validation check ####

if (all(st_is_valid(species)) &&
  all(st_is_valid(elect)) &&
  all(st_is_valid(aus))) {
  print("All species, elect, and Aus geometry are TRUE")
} else {
  print("All species, elect, and Aus geometry are NOT TRUE")
}

#### Simplify geometry ####

species <- species %>%
  st_simplify(
    dTolerance = 30000
  ) %>% # units of metres, this NULLs geoms < the dTolerance
  st_make_valid() %>%
  filter(!is.na(st_dimension(Shape))) %>%
  slice_sample(n = 100)
elect <- elect %>%
  ms_simplify(
    keep = 0.001,
    keep_shape = TRUE
  ) %>%
  st_make_valid()
elect.union <- elect.union %>%
  ms_simplify(
    keep = 0.001,
    keep_shape = TRUE
  ) %>%
  select(geometry) %>%
  st_make_valid()
aus <- aus %>%
  ms_simplify(
    keep = 0.01,
    keep_shape = TRUE
  ) %>%
  select(geometry) %>%
  st_make_valid()
aus.union <- aus.union %>%
  ms_simplify(
    keep = 0.01,
    keep_shape = TRUE
  ) %>%
  select(geometry) %>%
  st_make_valid()

#### elect.aus.union.difference - quantifying the overlap ####

elect.aus.union.difference <- elect.union %>%
  mutate(elect_union_sqkm = units::set_units(st_area(.), km^2) %>%
    as.numeric()) %>%
  st_sym_difference(aus.union) %>%
  mutate(aus_union_difference_sqkm = units::set_units(st_area(.), km^2) %>%
    as.numeric())

#### spec.per.elect - no. of species per electorates, demography, and concentration ####

spec.per.elect <- elect %>%
  st_join(species)

spec.per.elect.counts <- spec.per.elect %>%
  group_by(Elect_div) %>%
  summarise(total_unique_spec = n_distinct(SCIENTIFIC_NAME)) %>%
  ungroup()

spec.per.elect.indiv <- spec.per.elect %>%
  st_set_geometry(NULL) %>%
  group_by(Elect_div) %>%
  mutate(total_unique_spec = n_distinct(SCIENTIFIC_NAME)) %>%
  ungroup()

elect.spec.cover.counts <- spec.per.elect  %>%
  st_set_geometry(NULL) %>%
  group_by(
    SCIENTIFIC_NAME, VERNACULAR_NAME, THREATENED_STATUS,
    MIGRATORY_STATUS, TAXON_GROUP
  ) %>%
  summarise(elect_range_covers = n_distinct(Elect_div))

elect.spec.cover.indiv <- spec.per.elect  %>%
  st_set_geometry(NULL) %>%
  group_by(
    SCIENTIFIC_NAME, VERNACULAR_NAME, THREATENED_STATUS,
    MIGRATORY_STATUS, TAXON_GROUP
  ) %>%
  mutate(elect_range_covers = n_distinct(Elect_div))

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
  st_as_sf()

#### spec.outside.elect ####

spec.outside.elect <- species %>%
  st_difference(elect.union) %>%
  st_make_valid() %>%
  mutate(species_difference_area_sqkm = units::set_units(st_area(.), km^2) %>% as.numeric()) %>%
  mutate(percent_range_difference = species_difference_area_sqkm / species_area_sqkm) %>%
  mutate(across(percent_range_outside, round, digits = 2))
