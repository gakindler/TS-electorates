# Back end spatial operations to answer research questions

#### Libraries ####

library(tidyverse)
library(sf)

#### Loading and pre-processing ####

electorates <- st_read("/QRISdata/Q4107/raw_data/AEC_2019_superseded/COM_ELB_region.shp")
species <- st_read("/QRISdata/Q4107/raw_data/SNES_public_1july2021.gdb")
australia <- st_read("/QRISdata/Q4107/raw_data/ASGS_Edition_3_Aust_2021_shapefile/AUS_2021_AUST_GDA94.shp")
demography <- readxl::read_xlsx("/QRISdata/Q4107/raw_data/AEC_demographic-classification-1-january-2019/01-demographic-classification-as-at-1-january-2019.xlsx")

# Filter for only 'likely to occur', and bs columns/rows
specs.ss <- species %>% 
  filter(PRESENCE_RANK == 2) %>% 
  select(c("SCIENTIFIC_NAME", "VERNACULAR_NAME", "THREATENED_STATUS",
           "Shape_Area", "Shape", "REGIONS")) %>% 
  st_make_valid()

# The 'electorates' file has a couple of contractions that do not match 'demography' file
electorates$Elect_div <- gsub("Eden-monaro", "Eden-Monaro", electorates$Elect_div) 
electorates$Elect_div <- gsub("Mcewen", "McEwen", electorates$Elect_div) 
electorates$Elect_div <- gsub("Mcmahon", "McMahon", electorates$Elect_div) 
electorates$Elect_div <- gsub("Mcpherson", "McPherson", electorates$Elect_div) 
electorates$Elect_div <- gsub("O'connor", "O'Connor", electorates$Elect_div) 
elects.ss <- electorates %>% 
  left_join(demography, by = c("Elect_div" = "Electoral division")) %>% 
  select(-c("Numccds", "Actual", "Projected", 
            "Total_Popu", "Australian", "Sortname", "State or territory")) %>% 
  rename(Demographic_class = "Demographic classification") %>% 
  st_make_valid()

# Cutting random bs
aus.ss <- australia %>% 
  select(geometry) %>% 
  slice(1)

# #### spec.per.elect - no. of specs per electorate and concentration ####
# 
# spec.per.elect <- elects.ss %>% 
#   st_join(specs.ss) %>% 
#   group_by(Elect_div) %>% 
#   summarise(total_unique_spec = n_distinct(SCIENTIFIC_NAME)) %>% 
#   mutate(elects_area_sqm = st_area(.) %>% as.numeric()) %>% 
#   mutate(species_concentration = total_unique_spec / elects_area_sqm)
# st_write(spec.per.elect, 
#          dsn = "/QRISdata/Q4107/analysed_data/HPC_spatial_ops_output/spec.per.elect.gpkg", 
#          layer = 'spec.per.elect')
# 
# spec.per.elect.aus <- st_intersection(aus.ss, spec.per.elect) %>% 
#   st_make_valid()
# st_write(spec.per.elect.aus, 
#          dsn = "/QRISdata/Q4107/analysed_data/HPC_spatial_ops_output/spec.per.elect.aus.gpkg", 
#          layer = 'spec.per.elect.aus')
# 
# # Count no. of specs per electorate while maintaining specs list
# spec.per.elect.mutate <- elects.ss %>% 
#   st_join(specs.ss) %>% 
#   group_by(Elect_div) %>% 
#   mutate(total_unique_spec = n_distinct(SCIENTIFIC_NAME))
# st_write(spec.per.elect.mutate, 
#          dsn = "/QRISdata/Q4107/analysed_data/HPC_spatial_ops_output/spec.per.elect.mutate.gpkg", 
#          layer = 'spec.per.elect.mutate')
# 
# #### demo.spec - demography and species ####
# 
# demo.spec <- elects.ss %>% 
#   st_join(specs.ss) %>% 
#   group_by(Demographic_class) %>% 
#   summarise(total_unique_spec = n_distinct(SCIENTIFIC_NAME))
# st_write(demo.spec, 
#          dsn = "/QRISdata/Q4107/analysed_data/HPC_spatial_ops_output/demo.spec.gpkg", 
#          layer = 'demo.spec')
# 
# demo.spec.aus <- st_intersection(aus.ss, demo.spec) %>% 
#   st_make_valid()
# st_write(demo.spec.aus, 
#          dsn = "/QRISdata/Q4107/analysed_data/HPC_spatial_ops_output/demo.spec.aus.gpkg", 
#          layer = 'demo.spec.aus')

#### spec.range.elect - specs range within each electorate ####
# Calculate total area of each species's range
specs.ss.area <- specs.ss %>% 
  mutate(spec_area_sqm = st_area(.) %>% as.numeric())

# Calculate the percentage of species area within each electorate 
spec.range.elect <- st_intersection(specs.ss.area, elects.ss) %>%
  st_make_valid() %>% 
  mutate(intersection_area_sqm = st_area(.) %>% as.numeric()) %>% 
  mutate(percent_range_within = intersection_area_sqm / spec_area_sqm) %>% 
  mutate(across(percent_range_within, round, digits = 2)) # Negates floating point problems (hopefully)

# Filter for species which have >80% of their range within an electorate
spec.range.elect.eighty <- spec.range.elect %>% 
  filter(percent_range_within >= 0.8) %>% 
  select(-c("State", "Shape_Area", "spec_area_sqm", "Area_SqKm",
            "intersection_area_sqm", "Demographic_class")) %>% 
  st_set_geometry(NULL) %>% 
  inner_join(elects.ss, by = c("Elect_div" = "Elect_div")) %>% 
  st_as_sf() %>% 
  group_by(Elect_div) %>% 
  summarise(total_unique_spec = n_distinct(SCIENTIFIC_NAME))
st_write(spec.range.elect.eighty, 
         dsn = "/QRISdata/Q4107/analysed_data/HPC_spatial_ops_output/spec.range.elect.eighty.gpkg", 
         layer = 'spec.range.elect.eighty')

spec.range.elect.eighty.aus <- st_intersection(aus.ss, spec.range.elect.eighty) %>%
  st_make_valid()
st_write(spec.range.elect.eighty.aus, 
         dsn = "/QRISdata/Q4107/analysed_data/HPC_spatial_ops_output/spec.range.elect.eighty.aus.gpkg", 
         layer = 'spec.range.elect.eighty.aus')

#### spec.endemic.elect - specs endemic to each electorate ####

# Filter for species which are endemic to each electorate
spec.endemic.elect <- spec.range.elect %>% 
  filter(percent_range_within == 1) %>% 
  select(-c("State", "Shape_Area", "spec_area_sqm", "Area_SqKm",
            "intersection_area_sqm", "Demographic_class")) %>% 
  st_set_geometry(NULL) %>% 
  inner_join(elects.ss, by = c("Elect_div" = "Elect_div")) %>% 
  st_as_sf() %>% 
  group_by(Elect_div) %>% 
  summarise(total_unique_spec = n_distinct(SCIENTIFIC_NAME))
st_write(spec.endemic.elect, 
         dsn = "/QRISdata/Q4107/analysed_data/HPC_spatial_ops_output/spec.endemic.elect.gpkg", 
         layer = 'spec.endemic.elect')

spec.endemic.elect.aus <- st_intersection(aus.ss, spec.endemic.elect) %>%
  st_make_valid()
st_write(spec.endemic.elect.aus, 
         dsn = "/QRISdata/Q4107/analysed_data/HPC_spatial_ops_output/spec.endemic.elect.aus.gpkg", 
         layer = 'spec.endemic.elect.aus')

# TO DO: Count no. of endemic specs per electorate in wide format?

#### elect.spec.cover - How many electorates does each species's range cover? ####
elect.spec.cover <- elects.ss %>% 
  st_join(specs.ss, left = FALSE) %>% 
  group_by(SCIENTIFIC_NAME) %>% 
  summarise(elect_range_covers = n_distinct(Elect_div))

elect.spec.cover.status <- specs.ss %>% 
  as.data.frame() %>% 
  select(c("SCIENTIFIC_NAME", "THREATENED_STATUS")) %>% 
  left_join(elect.spec.cover, by = "SCIENTIFIC_NAME") %>% 
  as.data.frame() %>% 
  select(-"geometry")
write.csv(elect.spec.cover.status, 
          file = "/QRISdata/Q4107/analysed_data/HPC_spatial_ops_output/elect.spec.cover.status.csv")

#### spec.outside.elect ####
# Two methods here:
## 1. Same procedure as species.range but with st_difference, allows the distinguishing
## of how much of the species's range is outside electorates
elects.ss.union <- st_union(elects.ss, by_feature = FALSE) %>% 
  st_sf()

spec.outside.elect <- st_difference(specs.ss.area, elects.ss.union) %>%
  st_make_valid() %>% 
  mutate(outside_area_sqm = st_area(.) %>% as.numeric()) %>% 
  mutate(percent_range_outside = outside_area_sqm / spec_area_sqm) %>% 
  mutate(across(percent_range_outside, round, digits = 2)) %>% 
  filter(percent_range_outside >= .8) # 80% who chose this? Did you? Did I?
st_write(spec.outside.elect, 
         dsn = "/QRISdata/Q4107/analysed_data/HPC_spatial_ops_output/spec.outside.elect.gpkg", 
         layer = 'spec.outside.elect')

## 2. Logical vector method
# More from Ryan Peek - https://ryanpeek.org/mapping-in-R-workshop/03_spatial_joins.html
# Make all electorates into the same feature
outside <- sapply(st_intersects(specs.ss, elects.ss.union), function(x){
  length(x) == 0
})
spec.outside.elect.logical <- specs.ss[outside, ]
st_write(spec.outside.elect.logical, 
         dsn = "/QRISdata/Q4107/analysed_data/HPC_spatial_ops_output/spec.outside.elect.logical.gpkg", 
         layer = 'spec.outside.elect.logical')
