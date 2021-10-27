# Back end spatial operations to answer research questions

#### Libraries ####

library(tidyverse)
library(sf)
library(rmapshaper) # For installing use 'library(remotes)'
library(jsonlite)
library(magrittr)
library(units)

#### Species: Import/clean ####

species <- st_read("raw_data/SNES_public_1july2021.gdb")
species <- species %>%
  filter(PRESENCE_RANK == 2) %>%
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

#### Demography, population, and electorates: Import/clean ####

demo <- readxl::read_xlsx("raw_data/AEC_demographic-classification-1-january-2019/01-demographic-classification-as-at-1-january-2019.xlsx")
demo <- demo %>%
  select(!"State or territory") %>%
  rename(Demographic_class = "Demographic classification",
         Elect_div = "Electoral division")

pop <- read.csv("raw_data/AEC_elector_count_2019.csv", skip = 2)
pop <- pop %>%
  rename(Elect_div = Division, Electors = Electors.on.2019.Certified.list) %>%
  select(Elect_div, Electors)
pop$Electors <- as.numeric(gsub(",", "", pop$Electors))
pop$Elect_div <- str_to_title(pop$Elect_div)
pop$Elect_div <- str_squish(pop$Elect_div)
pop$Elect_div <- gsub("Eden-monaro", "Eden-Monaro", pop$Elect_div)
pop$Elect_div <- gsub("Mcewen", "McEwen", pop$Elect_div)
pop$Elect_div <- gsub("Mcmahon", "McMahon", pop$Elect_div)
pop$Elect_div <- gsub("Mcpherson", "McPherson", pop$Elect_div)
pop$Elect_div <- gsub("O'connor", "O'Connor", pop$Elect_div)

elect <- st_read("raw_data/AEC_electoral_boundaries_2019/COM_ELB_region.shp")
# The 'elect' file has a couple of contractions that do not match 'demo' file
elect$Elect_div <- gsub("Eden-monaro", "Eden-Monaro", elect$Elect_div)
elect$Elect_div <- gsub("Mcewen", "McEwen", elect$Elect_div)
elect$Elect_div <- gsub("Mcmahon", "McMahon", elect$Elect_div)
elect$Elect_div <- gsub("Mcpherson", "McPherson", elect$Elect_div)
elect$Elect_div <- gsub("O'connor", "O'Connor", elect$Elect_div)
elect <- elect %>%
  inner_join(demo) %>%
  inner_join(pop) %>%
  select(-c("Numccds", "Actual", "Projected", "Area_SqKm",
            "Total_Popu", "Australian", "Sortname")) %>% 
  st_make_valid()

#### Aus boundary: Import/clean ####

aus <- st_read("raw_data/ASGS_Edition_3_Aust_2021_shapefile/AUS_2021_AUST_GDA94.shp")
aus <- aus %>%
  select(geometry) %>%
  slice(1)

#### CRS check ####

if(st_crs(species) == st_crs(elect) && st_crs(elect) == st_crs(aus)) {
  print("Species, elect, and Aus boundary CRS's are the same")
} else {
  print("We got a CRS mismatch")
}

#### Validation check ####

if(all(st_is_valid(species)) && 
   all(st_is_valid(elect)) && 
   all(st_is_valid(aus))) {
  print("All species, elect, and Aus geometry are TRUE")
} else {
  print("All species, elect, and Aus geometry are NOT TRUE")
}

#### Simplify geometry ####

species <- st_simplify(species,
                        dTolerance = 30000) %>% # units of metres, this deletes geoms < the dTolerance
  st_make_valid() %>%
  filter(!is.na(st_dimension(Shape))) %>%
  slice_sample(n = 100)
elect <- ms_simplify(elect,
                         keep = 0.01,
                         keep_shape = TRUE) %>%  # __% of original?
  st_make_valid()
aus <- ms_simplify(aus,
                      keep = 0.01,
                      keep_shape = TRUE) %>%
  select(geometry) %>%
  st_make_valid()

#### spec.per.elect - no. of species per electorates, demography, and concentration ####

spec.per.elect <- elect %>%
  st_join(species) %>%
  group_by(Elect_div) %>%
  summarise(total_unique_spec = n_distinct(SCIENTIFIC_NAME)) %>%
  ungroup() %>%
  mutate(elect_area_sqkm = units::set_units(st_area(.), km^2) %>% as.numeric()) %>%
  mutate(species_per_sqkm = total_unique_spec / elect_area_sqkm) %>%
  inner_join(demo) %>%
  inner_join(pop) %>%
  relocate(Elect_div, Demographic_class, Electors, total_unique_spec,
           elect_area_sqkm, species_per_sqkm, geometry) %T>%
  st_write(dsn = "analysed_data/spatial_ops_output/spec.per.elect.gpkg",
         layer = 'spec.per.elect', append = FALSE) %>%
  st_set_geometry(NULL) %T>%
  write_json(path = "analysed_data/spatial_ops_output/spec.per.elect.json")

#### spec.per.elect - nested no. of species per electorate ####

spec.per.elect.nest <- elect %>%
  st_join(species) %>%
  group_by(Elect_div) %>%
  mutate(total_unique_spec = n_distinct(SCIENTIFIC_NAME)) %>%
  ungroup() %>%
  st_set_geometry(NULL) %>%
  nest_by(Elect_div, State, Demographic_class, total_unique_spec) %T>%
  write_json(path = "analysed_data/spatial_ops_output/spec.per.elect.nest.json")

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
  st_write(dsn = "analysed_data/spatial_ops_output/spec.eighty.elect.gpkg",
         layer = 'spec.eighty.elect', append = FALSE)

spec.eighty.elect.aus <- st_intersection(aus, spec.eighty.elect) %>%
  st_make_valid() %T>%
  st_write(dsn = "analysed_data/spatial_ops_output/spec.eighty.elect.aus.gpkg",
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
  st_write(dsn = "analysed_data/spatial_ops_output/spec.endemic.elect.gpkg",
         layer = 'spec.endemic.elect', append = FALSE)

spec.endemic.elect.aus <- st_intersection(aus, spec.endemic.elect) %>%
  st_make_valid() %T>%
  st_write(dsn = "analysed_data/spatial_ops_output/spec.endemic.elect.aus.gpkg",
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
  write_json(path = "analysed_data/spatial_ops_output/elect.spec.cover.status.json")

#### spec.outside.elect ####
# Two methods here:
## 1. Same procedure as species.range but with st_difference, allows the distinguishing
## of how much of the species's range is outside electorates
elect.union <- st_union(elect, by_feature = FALSE) %>%
  st_sf()

spec.outside.elect <- st_difference(species.area, elect.union) %>%
  st_make_valid() %>%
  mutate(outside_area_sqkm = units::set_units(st_area(.), km^2) %>% as.numeric()) %>%
  mutate(percent_range_outside = outside_area_sqkm / spec_area_sqkm) %>%
  mutate(across(percent_range_outside, round, digits = 2)) %>%
  filter(percent_range_outside >= .8) %T>% # 80%, who chose this? Did you? Did I?
  st_write(dsn = "analysed_data/spatial_ops_output/spec.outside.elect.gpkg",
         layer = 'spec.outside.elect', append = FALSE)

## 2. Logical vector method
# More from Ryan Peek - https://ryanpeek.org/mapping-in-R-workshop/03_spatial_joins.html
# Make all electorates into the same feature
outside <- sapply(st_intersects(species, elect.union), function(x){
  length(x) == 0
  })
spec.outside.elect.logical <- species[outside, ] %T>%
  st_write(dsn = "analysed_data/spatial_ops_output/spec.outside.elect.logical.gpkg",
         layer = 'spec.outside.elect.logical', append = FALSE)
