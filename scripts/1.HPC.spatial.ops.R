# Back end spatial operations to answer research questions
# Run on the UQ HPC with associated data stored of UQ RDM
# Project: Q4107

#### Libraries ####

library(tidyverse)
library(sf)
library(jsonlite)
library(magrittr)
library(units)

#### Aus boundary: Import/clean ####

aus <- st_read("/QRISdata/Q4107/raw_data/ASGS_Edition_3_Aust_2021_shapefile/AUS_2021_AUST_GDA94.shp")
aus <- aus %>%
  select(geometry) %>%
  slice(1) %>%
  st_crop(xmin = 113, ymin = -43.740482, # drop those pesky islands
          xmax = 154, ymax = -9.219937)

#### Electorates: Import/clean ####

elect <- st_read("/QRISdata/Q4107/raw_data/AEC_2019_superseded/COM_ELB_region.shp")
# The 'elect' file has a couple of contractions that do not match 'demo' file
elect$Elect_div <- gsub("Eden-monaro", "Eden-Monaro", elect$Elect_div)
elect$Elect_div <- gsub("Mcewen", "McEwen", elect$Elect_div)
elect$Elect_div <- gsub("Mcmahon", "McMahon", elect$Elect_div)
elect$Elect_div <- gsub("Mcpherson", "McPherson", elect$Elect_div)
elect$Elect_div <- gsub("O'connor", "O'Connor", elect$Elect_div)
elect.no.crop <- elect %>%
  select(-c("Numccds", "Actual", "Projected", "Area_SqKm",
            "Total_Popu", "Australian", "Sortname")) %>%
  st_make_valid()

elect.crop <- elect %>%
  select(-c("Numccds", "Actual", "Projected", "Area_SqKm",
            "Total_Popu", "Australian", "Sortname")) %>%
  st_make_valid() %>%
  st_crop(xmin = 113, ymin = -43.740482, # drop those pesky islands
          xmax = 154, ymax = -9.219937)

elect.everything <- elect %>%
  select(-c("Numccds", "Actual", "Projected", "Area_SqKm",
            "Total_Popu", "Australian", "Sortname")) %>%
  st_make_valid() %>%
  st_crop(xmin = 113, ymin = -43.740482, # drop those pesky islands
          xmax = 154, ymax = -9.219937) %>%
  st_intersection(aus) %>% # because it split geoms it creates geom collections which is bad for analysis
  st_make_valid() %>%


table(st_geometry_type(elect.no.crop))
table(st_geometry_type(elect.crop))
table(st_geometry_type(elect.everything))

elect.union <- st_union(elect, by_feature = FALSE) %>%
  st_sf()

#### Species: Import/clean/crop ####

species <- st_read("/QRISdata/Q4107/raw_data/SNES_public_1july2021.gdb")
species <- species %>%
  filter(PRESENCE_RANK == 2) %>%
  filter(!is.na(THREATENED_STATUS)) %>%
  filter(THREATENED_STATUS %in% c(
        "Vulnerable",
        "Endangered",
        "Critically Endangered",
        "Extinct in the wild")) %>% # as Ward_database_2021(?)
  filter(!MARINE %in% "Listed") %>%
  filter(!SCIENTIFIC_NAME %in% c(
    "Calidris canutus", "Calidris ferruginea",
    "Calidris tenuirostris", "Hirundapus caudacutus")) %>%
  # 1 - Red Knot, Knot; 2 - Curlew Sandpiper;
  # 4 - Great Knot; 6 - White-throated Needletail
  # of the MARINE == "Listed - overfly marine area"
  filter(!CETACEAN %in% "Cetacean") %>%
  select(c("SCIENTIFIC_NAME", "VERNACULAR_NAME", "THREATENED_STATUS",
           "MIGRATORY_STATUS", "TAXON_GROUP", "Shape_Area", "Shape")) %>%
  st_make_valid() %>%
  # Merge species at the broad taxa as there are duplicate polygons
  # Probably attributable to subspecies populations, but still ¯\_(ツ)_/¯
  group_by(SCIENTIFIC_NAME, VERNACULAR_NAME, THREATENED_STATUS,
           MIGRATORY_STATUS, TAXON_GROUP) %>%
  summarise() %>%
  ungroup() %>%
  st_make_valid()

species.aus <- species %>%
  st_intersection(aus) %>%
  st_make_valid()

#### aus/species overlap check ####

elect.aus.overlap.check <- elect.union %>%
  mutate(elect_union_sqkm = units::set_units(st_area(.), km^2) %>%
          as.numeric()) %>%
  st_sym_difference(aus) %>%
  mutate(difference_sqkm = units::set_units(st_area(.), km^2) %>%
          as.numeric()) %T>%
  st_write(dsn = "/QRISdata/Q4107/analysed_data/HPC_spatial_ops_output/elect.aus.overlap.check.gpkg",
    layer = 'elect.aus.overlap.check', append = FALSE)

#### spec.per.elect - no. of species per electorates, demography, and concentration ####

spec.per.elect <- elect %>%
  st_join(species) %>%
  group_by(Elect_div) %>%
  summarise(total_unique_spec = n_distinct(SCIENTIFIC_NAME)) %>%
  ungroup() %>%
  mutate(elect_area_sqkm = units::set_units(st_area(.), km^2) %>% as.numeric()) %>%
  mutate(species_per_sqkm = total_unique_spec / elect_area_sqkm) %T>%
  st_write(dsn = "/QRISdata/Q4107/analysed_data/HPC_spatial_ops_output/spec.per.elect.gpkg",
    layer = 'spec.per.elect', append = FALSE)

#### spec.range.elect - species range within each electorate ####
# Calculate total area of each species's range
species.area <- species %>%
  mutate(spec_area_sqkm = units::set_units(st_area(.), km^2) %>% as.numeric())

# Calculate the percentage of species area within each electorate
spec.range.elect <- st_intersection(species.area, elect) %>%
  st_make_valid() %>%
  mutate(intersection_area_sqkm = units::set_units(st_area(.), km^2) %>% as.numeric()) %>%
  mutate(percent_range_within = intersection_area_sqkm / spec_area_sqkm) %>%
  # Negates floating point problems (hopefully)
  mutate(across(percent_range_within, round, digits = 2))

#### spec.eighty.elect - species range >80% in each electorate ####
# Filter for species which have >80% of their range within an electorate
spec.eighty.elect <- spec.range.elect %>%
  filter(percent_range_within >= 0.8) %>%
  st_set_geometry(NULL) %>%
  inner_join(elect, by = c("Elect_div" = "Elect_div")) %>%
  st_as_sf() %>%
  group_by(Elect_div) %>%
  summarise(total_unique_spec = n_distinct(SCIENTIFIC_NAME)) %T>%
  st_write(dsn = "/QRISdata/Q4107/analysed_data/HPC_spatial_ops_output/spec.eighty.elect.gpkg",
         layer = 'spec.eighty.elect', append = FALSE)

spec.eighty.elect.aus <- st_intersection(aus, spec.eighty.elect) %>%
  st_make_valid() %T>%
  st_write(dsn = "/QRISdata/Q4107/analysed_data/HPC_spatial_ops_output/spec.eighty.elect.aus.gpkg",
         layer = 'spec.eighty.elect.aus', append = FALSE)

#### spec.endemic.elect - species endemic to each electorate ####

# Filter for species which are endemic to each electorate
spec.endemic.elect <- spec.range.elect %>%
  filter(percent_range_within == 1) %>%
  st_set_geometry(NULL) %>%
  inner_join(elect, by = c("Elect_div" = "Elect_div")) %>%
  st_as_sf() %>%
  group_by(Elect_div) %>%
  summarise(total_unique_spec = n_distinct(SCIENTIFIC_NAME)) %T>%
  st_write(dsn = "/QRISdata/Q4107/analysed_data/HPC_spatial_ops_output/spec.endemic.elect.gpkg",
         layer = 'spec.endemic.elect', append = FALSE)

spec.endemic.elect.aus <- st_intersection(aus, spec.endemic.elect) %>%
  st_make_valid() %T>%
  st_write(dsn = "/QRISdata/Q4107/analysed_data/HPC_spatial_ops_output/spec.endemic.elect.aus.gpkg",
         layer = 'spec.endemic.elect.aus', append = FALSE)

# TO DO: Count no. of endemic species per electorate in wide format?

#### elect.spec.cover - How many electorates does each species's range cover? ####
elect.spec.cover <- elect %>%
  st_join(species, left = FALSE) %>%
  group_by(SCIENTIFIC_NAME) %>%
  summarise(elect_range_covers = n_distinct(Elect_div))

elect.spec.cover.status <- species %>%
  as.data.frame() %>%
  select(c("SCIENTIFIC_NAME", "THREATENED_STATUS")) %>%
  left_join(elect.spec.cover, by = "SCIENTIFIC_NAME") %>%
  as.data.frame() %>%
  select(-"geometry") %T>%
  write_json(path = "/QRISdata/Q4107/analysed_data/HPC_spatial_ops_output/elect.spec.cover.status.json")

#### spec.outside.elect ####
# Two methods here:
## 1. Same procedure as species.range but with st_difference, allows the distinguishing
## of how much of the species's range is outside electorates

spec.outside.elect <- st_difference(species.area, elect.union) %>%
  st_make_valid() %>%
  mutate(outside_area_sqkm = units::set_units(st_area(.), km^2) %>% as.numeric()) %>%
  mutate(percent_range_outside = outside_area_sqkm / spec_area_sqkm) %>%
  mutate(across(percent_range_outside, round, digits = 2)) %>%
  filter(percent_range_outside >= .8) %T>% # 80%, who chose this? Did you? Did I?
  st_write(dsn = "/QRISdata/Q4107/analysed_data/HPC_spatial_ops_output/spec.outside.elect.gpkg",
         layer = 'spec.outside.elect', append = FALSE)

## 2. Logical vector method
# More from Ryan Peek - https://ryanpeek.org/mapping-in-R-workshop/03_spatial_joins.html
# Make all electorates into the same feature
outside <- sapply(st_intersects(species, elect.union), function(x){
  length(x) == 0
  })
spec.outside.elect.logical <- species[outside, ] %T>%
  st_write(dsn = "/QRISdata/Q4107/analysed_data/HPC_spatial_ops_output/spec.outside.elect.logical.gpkg",
         layer = 'spec.outside.elect.logical', append = FALSE)
