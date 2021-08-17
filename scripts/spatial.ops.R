# Answering questions from my manuscript and improving MW's tables
# Also using R for GIS, let's see how far I get

#### Libraries ####

library(tidyverse)
library(sf)
library(rmapshaper)
library(ggplot2)
# library(remotes)

#### Loading and subsetting ####

electorates <- st_read("raw_data/AEC_2019_superseded/COM_ELB_region.shp")
species <- st_read("raw_data/SNES_public_1july2021.gdb")
# specs.public <- st_read("raw_data/snes_public_grids_08Aug2019.gdb", layer = "specs_combined")
# st_layers("raw_data/snes_public_grids_08Aug2019.gdb")
australia <- st_read("raw_data/ASGS_Edition_3_Aust_2021_shapefile/AUS_2021_AUST_GDA94.shp")

# What are the unique values of each attribute?
# unique <- lapply(species, unique)

# Check CRS's are the same
st_crs(electorates) == st_crs(species)
st_crs(electorates) == st_crs(australia)

# Filter for only 'likely to occur', and bs columns/rows
specs.ss <- species %>% 
  filter(PRESENCE_RANK == 2) %>% 
  select(c("SCIENTIFIC_NAME", "VERNACULAR_NAME", "THREATENED_STATUS",
          "Shape_Area", "Shape")) %>% 
  st_make_valid()
elects.ss <- electorates %>% 
  select(-c("Numccds", "Actual", "Projected", 
            "Total_Popu", "Australian", "Sortname"))
aus.ss <- australia %>% 
  select(geometry) %>% 
  slice(1)

# Simplify geometry
specs.ss <- st_simplify(specs.ss, dTolerance = 5000) %>% # ___ metres?
  st_make_valid() 
elects.ss <- ms_simplify(elects.ss, keep = 0.01, keep_shape = TRUE) %>%  # __% of original? 
  st_make_valid()
aus.ss <- ms_simplify(aus.ss, keep = 0.01, keep_shape = TRUE) %>%
  select(geometry) %>% 
  st_make_valid()

#### spec.per.elect - no. of specs per electorate ####
# Count no. of specs per electorate, then snap onto Aus
spec.per.elect <- elects.ss %>% 
  st_join(specs.ss) %>% 
  group_by(Elect_div) %>% 
  summarise(total_unique_spec = n_distinct(SCIENTIFIC_NAME))
spec.per.elect.aus <- st_intersection(aus.ss, spec.per.elect) %>% 
  st_make_valid()

# # Count no. of specs per electorate while maintaining specs list
# elect.spec.uniq.spec.exp <- join.intersect %>% 
#   as_tibble() %>% 
#   group_by(Elect_div) %>% 
#   mutate()
#   summarise(total_unique_spec = n_distinct(SCIENTIFIC_NAME))
#   
# exp <- split(specs.sl)

#### spec.range.elect - specs range within each electorate ####
specs.ss.area <- specs.ss %>% 
  slice_sample(n = 20) %>% 
  mutate(spec_area_sqm = st_area(Shape) %>% as.numeric())

# Check for empty geoms
table(st_dimension(specs.ss.area))

spec.range.elect <- st_intersection(elects.ss, specs.ss.area) %>% 
  st_make_valid() %>% 
  mutate(intersection_area_sqm = st_area(geometry)) %>% 
  transform(percent_range_within = intersection_area_sqm / spec_area_sqm)

# PROBLEM: not calucalting percentages correctly

st_geometry(spec.range.elect) <- NULL

plot(spec.range.elect$geometry)
summary(spec.range.elect$percent_range_within)

# Pause for a quick vis
ggplot(spec.range.elect, aes(x = percent_range_within)) +
  geom_histogram(binwidth = .02)

spec.range.elect.eighty <- spec.range.elect %>% 
  filter(percent_range_within >= 0.8) %>% 
  select(-c("State", "Shape_Area")) %>% 
  rename(Elect_div_orig = Elect_div)

# PROBLEM: sometimes the elect_div of pre and post scripts are not matching up 

spec.range.elect.eighty.exp <- elects.ss %>% 
  st_join(spec.range.elect.eighty, left = FALSE)

table(spec.range.elect.eighty$Elect_div == spec.range.elect.eighty$Elect_div_orig)

  group_by(Elect_div) %>%
  summarise(total_unique_spec = n_distinct(SCIENTIFIC_NAME))
spec.range.elect.eighty.aus <- st_intersection(aus.ss, spec.range.elect.eighty) %>%
  st_make_valid()

# Sum area after grouping elect_div and species?

#### spec.endemic.elect - specs endemic to each electorate ####
# Count no. of endemic specs per electorate, then snap onto Aus
spec.endemic.elect <- spec.range.elect %>% 
  filter(percent_range_within == 1) %>% 
  select(-c("Elect_div", "State", "Shape_Area"))

spec.endemic.elect <- elects.ss %>% 
  st_join(spec.endemic.elect, left = FALSE) %>% 
  group_by(Elect_div) %>%
  summarise(total_unique_spec = n_distinct(SCIENTIFIC_NAME))
spec.endemic.elect.aus <- st_intersection(aus.ss, spec.endemic.elect) %>%
  st_make_valid()


spec.endemic.elect <- elects.ss %>% 
  st_join(specs.ss) %>% 
  group_by(SCIENTIFIC_NAME, Elect_div) %>% 
  summarise(elect_range_covers = n_distinct(SCIENTIFIC_NAME))

# Count no. of endemic specs per electorate in wide format?



#### elect.spec.cover - How many electorates does each species's range cover? ####
elect.spec.cover <- elects.ss %>% 
  st_join(specs.ss, left = FALSE) %>% 
  group_by(SCIENTIFIC_NAME) %>% 
  summarise(elect_range_covers = n_distinct(Elect_div))

#### spec.outside.elect - Clipping/antijoin ####
# Couple of methods here:
## 1. Same procedure as endemic but with st_difference
specs.ss.area <- specs.ss %>% 
  slice_sample(n = 20) %>% 
  mutate(spec_area_sqm = st_area(Shape) %>% as.numeric())

spec.outside.elect <- st_difference(specs.ss.area, elects.ss) %>% 
  st_make_valid() %>% 
  mutate(intersection_area_sqm = st_area(Shape)) %>% 
  transform(percent_range_within = intersection_area_sqm / spec_area_sqm)

plot(spec.outside.elect$Shape)
st_geometry(spec.outside.elect) <- NULL
ggplot(spec.outside.elect, aes(x = percent_range_within)) +
  geom_histogram()
summary(spec.outside.elect$percent_range_within)

# PROBLEM: Same as spec.range

spec.range.elect.eighty <- spec.range.elect %>% 
  filter(percent_range_within >= .8) %>% # Their range of less than 20% instead maybe? 
  select(-c("State", "Shape_Area")) %>% 
  rename(Elect_div_orig = Elect_div)

spec.range.elect.eighty.exp <- elects.ss %>% 
  st_join(spec.range.elect.eighty, left = FALSE)

table(spec.range.elect.eighty$Elect_div == spec.range.elect.eighty$Elect_div_orig)

group_by(Elect_div) %>%
  summarise(total_unique_spec = n_distinct(SCIENTIFIC_NAME))
spec.range.elect.eighty.aus <- st_intersection(aus.ss, spec.range.elect.eighty) %>%
  st_make_valid()

## 2. Logical vector method
# Make all electorates into the same feature
elects.ss.union <- st_union(elects.ss, by_feature = FALSE) %>% 
  st_sf()
outside <- sapply(st_intersects(specs.ss, elects.ss.union), function(x){
  length(x) == 0
  })
spec.out <- specs.ss[outside, ]

# Fact check
antijoin.fc <- unique(join.intersect["SCIENTIFIC_NAME"])


# #### Other ####
# 
# st_geometry(intersect) <- NULL
# 
# specs.sl <- st_buffer(specs, 0.0)
# elects.ss <- st_buffer(elects.ss, 0.0)
# 
# specs.sl <- st_buffer(specs.sl[!is.na(valid)], 0.0)
# elects.ss <- st_buffer(elects.ss[!is.na(valid)], 0.0)
# 
# filter <- st_filter(elects.ss, specs.sl, .pred = st_intersects())
# 