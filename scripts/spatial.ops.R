# Back end spatial operations to answer research questions

#### Libraries ####

library(tidyverse)
library(sf)
library(rmapshaper) # For installing use 'library(remotes)'
# library(rjson)
library(jsonlite)

#### Species: Import/clean ####

# species.public <- st_read("raw_data/snes_public_grids_08Aug2019.gdb", layer = "species_combined")
# st_layers("raw_data/snes_public_grids_08Aug2019.gdb")
species <- st_read("raw_data/SNES_public_1july2021.gdb")
# species$TAXON_GROUP <- str_to_sentence(species$TAXON_GROUP)
# species <- species[!(is.na(species$THREATENED_STATUS)), ] # Could just filter(!is.na(target.col))
# species$THREATENED_STATUS <- species$THREATENED_STATUS %>% 
#   replace_na("Unknown")
species <- species %>% 
  filter(PRESENCE_RANK == 2) %>% 
  # mutate(THREATENED_STATUS = replace(THREATENED_STATUS, THREATENED_STATUS == "Critically Endangered", "CE")) %>% 
  # mutate(THREATENED_STATUS = replace(THREATENED_STATUS, THREATENED_STATUS == "Conservation Dependent", "CD")) %>% 
  # mutate(TAXON_GROUP = replace(TAXON_GROUP, TAXON_GROUP == "Other-animals", "Other animals")) %>% 
  select(c("SCIENTIFIC_NAME", "VERNACULAR_NAME", "THREATENED_STATUS", 
           "MIGRATORY_STATUS", "TAXON_GROUP", "Shape_Area", "Shape")) %>% 
  st_make_valid() %>% 
  group_by(SCIENTIFIC_NAME, VERNACULAR_NAME, THREATENED_STATUS, # Merge species at the broad taxa
           MIGRATORY_STATUS, TAXON_GROUP) %>%
  summarise() %>% 
  ungroup()

#### Electorates and demography: Import/clean ####

demography <- readxl::read_xlsx("raw_data/AEC_demographic-classification-1-january-2019/01-demographic-classification-as-at-1-january-2019.xlsx")
elects <- st_read("raw_data/AEC_electoral_boundaries_2019/COM_ELB_region.shp")
elects$Elect_div <- gsub("Eden-monaro", "Eden-Monaro", elects$Elect_div) # The 'elects' file has a couple of contractions that do not match 'demography' file
elects$Elect_div <- gsub("Mcewen", "McEwen", elects$Elect_div) 
elects$Elect_div <- gsub("Mcmahon", "McMahon", elects$Elect_div) 
elects$Elect_div <- gsub("Mcpherson", "McPherson", elects$Elect_div) 
elects$Elect_div <- gsub("O'connor", "O'Connor", elects$Elect_div) 
elects <- elects %>% 
  left_join(demography, by = c("Elect_div" = "Electoral division")) %>% 
  select(-c("Numccds", "Actual", "Projected", 
            "Total_Popu", "Australian", "Sortname", "State or territory")) %>% 
  rename(Demographic_class = "Demographic classification")

#### Aus boundary: Import/clean ####

aus <- st_read("raw_data/ASGS_Edition_3_Aust_2021_shapefile/AUS_2021_AUST_GDA94.shp")
aus <- aus %>%
  select(geometry) %>%
  slice(1)

#### CRS check ####

if(st_crs(species) == st_crs(elects)) {
  print(st_crs(elects) == st_crs(aus))
  print("Species, elects, and Aus boundary CRS's are the same if above value is TRUE")
} else {
  print("We got a CRS mismatch")
}

#### Simplify geometry ####

species <- st_simplify(species,
                        dTolerance = 30000) %>% # units of metres, this deletes geoms < the dTolerance
  st_make_valid() %>% 
  filter(!is.na(st_dimension(Shape))) %>% 
  slice_sample(n = 200)
elects <- ms_simplify(electorates,
                         keep = 0.01,
                         keep_shape = TRUE) %>%  # __% of original?
  st_make_valid()
aus <- ms_simplify(aus,
                      keep = 0.01,
                      keep_shape = TRUE) %>%
  select(geometry) %>%
  st_make_valid()

#### spec.per.elect - no. of species per electorate and concentration ####

spec.per.elect <- elects %>% 
  st_join(species) %>% 
  group_by(Elect_div) %>% 
  summarise(total_unique_spec = n_distinct(SCIENTIFIC_NAME)) %>% 
  mutate(elects_area_sqm = st_area(.) %>% as.numeric()) %>% 
  mutate(species_per_sqm = total_unique_spec / elects_area_sqm) %T>% 
  st_write(dsn = "analysed_data/spatial_ops_output/spec.per.elect.gpkg", 
         layer = 'spec.per.elect') %>% 
  st_set_geometry(NULL) %T>% 
  write_json(path = "analysed_data/spatial_ops_output/spec.per.elect.json")

# spec.per.elect.aus <- st_intersection(aus, spec.per.elect) %>% 
#   st_make_valid()
# st_write(spec.per.elect.aus, 
#          dsn = "analysed_data/spatial_ops_output/spec.per.elect.aus.gpkg", 
#          layer = 'spec.per.elect.aus')

#### spec.per.elect - no. of species per electorate and concentration with species list ####

# spec.per.elect.mutate <- elects %>% 
#   st_join(species) %>% 
#   group_by(Elect_div) %>% 
#   mutate(total_unique_spec = n_distinct(SCIENTIFIC_NAME))
# st_write(spec.per.elect.mutate, 
#          dsn = "/QRISdata/Q4107/analysed_data/HPC_spatial_ops_output/spec.per.elect.mutate.gpkg", 
#          layer = 'spec.per.elect.mutate')
# 
# elect.spec.uniq.spec.exp <- join.intersect %>% 
#   as_tibble() %>% 
#   group_by(Elect_div) %>% 
#   mutate()
#   summarise(total_unique_spec = n_distinct(SCIENTIFIC_NAME))
#   
# exp <- split(species.sl)

#### demo.spec - demography and species ####

demo.spec <- elects %>% 
  st_join(species) %>% 
  group_by(Demographic_class) %>% 
  summarise(total_unique_spec = n_distinct(SCIENTIFIC_NAME))
# st_write(demo.spec, 
#          dsn = "analysed_data/spatial_ops_output/demo.spec.gpkg", 
#          layer = 'demo.spec')

demo.spec.aus <- st_intersection(aus, demo.spec) %>% 
  st_make_valid()
st_write(demo.spec.aus, 
         dsn = "analysed_data/spatial_ops_output/demo.spec.aus.gpkg", 
         layer = 'demo.spec.aus')

#### spec.range.elect - species range within each electorate ####
# Calculate total area of each species's range
species.area <- species %>% 
  mutate(spec_area_sqm = st_area(.) %>% as.numeric())

# Calculate the percentage of species area within each electorate 
spec.range.elect <- st_intersection(species.area, elects) %>%
  mutate(intersection_area_sqm = st_area(.) %>% as.numeric()) %>% 
  mutate(percent_range_within = intersection_area_sqm / spec_area_sqm) %>% 
  mutate(across(percent_range_within, round, digits = 2)) # Negates floating point problems (hopefully)

# Filter for species which have >80% of their range within an electorate
spec.range.elect.eighty <- spec.range.elect %>% 
  filter(percent_range_within >= 0.8) %>% 
  select(-c("State", "Shape_Area", "spec_area_sqm", "Area_SqKm",
             "intersection_area_sqm", "Demographic_class")) %>% 
  st_set_geometry(NULL) %>% 
  inner_join(elects, by = c("Elect_div" = "Elect_div")) %>% 
  st_as_sf() %>% 
  group_by(Elect_div) %>% 
  summarise(total_unique_spec = n_distinct(SCIENTIFIC_NAME))
st_write(spec.range.elect.eighty, 
         dsn = "analysed_data/spatial_ops_output/spec.range.elect.eighty.gpkg", 
         layer = 'spec.range.elect.eighty')

spec.range.elect.eighty.aus <- st_intersection(aus, spec.range.elect.eighty) %>%
  st_make_valid()
st_write(spec.range.elect.eighty.aus, 
         dsn = "analysed_data/spatial_ops_output/spec.range.elect.eighty.aus.gpkg", 
         layer = 'spec.range.elect.eighty.aus')

#### spec.endemic.elect - species endemic to each electorate ####

# Filter for species which are endemic to each electorate
spec.endemic.elect <- spec.range.elect %>% 
  filter(percent_range_within == 1) %>% 
  select(-c("State", "Shape_Area", "spec_area_sqm", "Area_SqKm",
            "intersection_area_sqm", "Demographic_class")) %>% 
  st_set_geometry(NULL) %>% 
  inner_join(elects, by = c("Elect_div" = "Elect_div")) %>% 
  st_as_sf() %>% 
  group_by(Elect_div) %>% 
  summarise(total_unique_spec = n_distinct(SCIENTIFIC_NAME))
st_write(spec.endemic.elect, 
         dsn = "analysed_data/spatial_ops_output/spec.endemic.elect.gpkg", 
         layer = 'spec.endemic.elect')

spec.endemic.elect.aus <- st_intersection(aus, spec.endemic.elect) %>%
  st_make_valid()
st_write(spec.endemic.elect.aus, 
         dsn = "analysed_data/spatial_ops_output/spec.endemic.elect.aus.gpkg", 
         layer = 'spec.endemic.elect.aus')

# TO DO: Count no. of endemic species per electorate in wide format?

#### elect.spec.cover - How many electorates does each species's range cover? ####
elect.spec.cover <- elects %>% 
  st_join(species, left = FALSE) %>% 
  group_by(SCIENTIFIC_NAME) %>% 
  summarise(elect_range_covers = n_distinct(Elect_div))

elect.spec.cover.status <- species %>% 
  as.data.frame() %>% 
  select(c("SCIENTIFIC_NAME", "THREATENED_STATUS")) %>% 
  left_join(elect.spec.cover, by = "SCIENTIFIC_NAME") %>% 
  as.data.frame() %>% 
  select(-"geometry")
write.csv(elect.spec.cover.status, 
          file = "analysed_data/spatial_ops_output/elect.spec.cover.status.csv")

#### spec.outside.elect ####
# Two methods here:
## 1. Same procedure as species.range but with st_difference, allows the distinguishing
## of how much of the species's range is outside electorates
elects.union <- st_union(elects, by_feature = FALSE) %>% 
  st_sf()

spec.outside.elect <- st_difference(species.area, elects.union) %>%
  st_make_valid() %>% 
  mutate(outside_area_sqm = st_area(.) %>% as.numeric()) %>% 
  mutate(percent_range_outside = outside_area_sqm / spec_area_sqm) %>% 
  mutate(across(percent_range_outside, round, digits = 2)) %>% 
  filter(percent_range_outside >= .8) # 80% who chose this? Did you? Did I?
st_write(spec.outside.elect, 
         dsn = "analysed_data/spatial_ops_output/spec.outside.elect.gpkg", 
         layer = 'spec.outside.elect')

## 2. Logical vector method
# More from Ryan Peek - https://ryanpeek.org/mapping-in-R-workshop/03_spatial_joins.html
# Make all electorates into the same feature
outside <- sapply(st_intersects(species, elects.union), function(x){
  length(x) == 0
  })
spec.outside.elect.logical <- species[outside, ]
st_write(spec.outside.elect.logical, 
         dsn = "analysed_data/spatial_ops_output/spec.outside.elect.logical.gpkg", 
         layer = 'spec.outside.elect.logical')
