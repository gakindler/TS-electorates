# Back end spatial operations to answer the research questions

#### Libraries ####

library(tidyverse)
library(sf)
library(rmapshaper)
# library(remotes)

#### Loading and pre-processing ####

electorates <- st_read("raw_data/AEC_electoral_boundaries_2019/COM_ELB_region.shp")
species <- st_read("raw_data/SNES_public_1july2021.gdb")
# specs.public <- st_read("raw_data/snes_public_grids_08Aug2019.gdb", layer = "specs_combined")
# st_layers("raw_data/snes_public_grids_08Aug2019.gdb")
australia <- st_read("raw_data/ASGS_Edition_3_Aust_2021_shapefile/AUS_2021_AUST_GDA94.shp")
demography <- readxl::read_xlsx("raw_data/AEC_demographic-classification-1-january-2019/01-demographic-classification-as-at-1-january-2019.xlsx")

# What are the unique values of each attribute?
# unique <- lapply(species, unique)

# Check CRS's are the same
st_crs(electorates) == st_crs(species)
st_crs(electorates) == st_crs(australia)

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
  rename(Demographic_class = "Demographic classification")

# Cutting random bs
aus.ss <- australia %>% 
  select(geometry) %>% 
  slice(1)

# Simplify geometry
specs.ss <- st_simplify(specs.ss, 
                        dTolerance = 20000) %>% # units of metres
  st_make_valid() 
elects.ss <- ms_simplify(elects.ss, 
                         keep = 0.01, 
                         keep_shape = TRUE) %>%  # __% of original? 
  st_make_valid()
aus.ss <- ms_simplify(aus.ss,
                      keep = 0.01, 
                      keep_shape = TRUE) %>%
  select(geometry) %>% 
  st_make_valid()

#### spec.per.elect - no. of specs per electorate and concentration ####
# Count no. of specs per electorate, then snap onto Aus
spec.per.elect <- elects.ss %>% 
  st_join(specs.ss) %>% 
  group_by(Elect_div) %>% 
  summarise(total_unique_spec = n_distinct(SCIENTIFIC_NAME)) %>% 
  mutate(elects_area_sqm = st_area(.) %>% as.numeric()) %>% 
  mutate(species_concentration = total_unique_spec / elects_area_sqm)
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

#### demo.spec - How do species relate to AEC's demography? ####

demo.spec <- elects.ss %>% 
  st_join(specs.ss) %>% 
  group_by(Elect_div) %>% 
  mutate(total_unique_spec = n_distinct(SCIENTIFIC_NAME)) %>% 
  ungroup()
demo.spec.aus <- st_intersection(aus.ss, demo.spec) %>% 
  st_make_valid()

#### spec.range.elect - specs range within each electorate ####
# Calculate total area of each species's range
specs.ss.area <- specs.ss %>% 
  filter(REGIONS == "WA") %>% 
  # slice_sample(n = 100) %>% 
  mutate(spec_area_sqm = st_area(.) %>% as.numeric())

# Calculate the percentage of species area within each electorate 
spec.range.elect <- st_intersection(specs.ss.area, elects.ss) %>%
  mutate(intersection_area_sqm = st_area(.) %>% as.numeric()) %>% 
  mutate(percent_range_within = intersection_area_sqm / spec_area_sqm) %>% 
  mutate(across(percent_range_within, round, digits = 2)) # Negates floating point problems (hopefully)

# Filter for species which have >80% of their range within an electorate
spec.range.elect.eighty <- spec.range.elect %>% 
  filter(percent_range_within >= 0.8) %>% 
  select(-c("State", "Shape_Area", "spec_area_sqm", "Area_SqKm",
             "intersection_area_sqm", "Demographic_class")) %>% 
  rename(Elect_div_orig = Elect_div)

spec.range.elect.eighty.exp <- elects.ss %>% 
  st_join(spec.range.elect.eighty, 
          join = st_overlaps, 
          left = FALSE)

spec.range.elect.eighty.exp <- st_join(elects.ss, spec.range.elect.eighty, left = FALSE) 

st_write(spec.range.elect.eighty, dsn = "analysed_data/spec_range_elect_eighty.shp", 
         layer = 'spec.range.elect.eighty')

table(spec.range.elect.eighty.exp$SCIENTIFIC_NAME)

discrep.exp <- spec.range.elect.eighty.exp %>% 
  group_by(SCIENTIFIC_NAME) %>% 
  filter(n() > 1)
  
discrep.exp <- table(spec.range.elect.eighty.exp$SCIENTIFIC_NAME)

discrep <- anti_join(spec.range.elect.eighty.exp, spec.range.elect.eighty,
                     by = c("Elect_div" = "Elect_div_orig", "SCIENTIFIC_NAME" = "SCIENTIFIC_NAME"))

st_write(discrep, dsn = "analysed_data/discrep/discrep.shp", 
         layer = 'discrep')

# PROBLEM: this join is duplicating rows that shouldn't need to be duplicated?
# ANSWER: I did not understand how the intersects predicate works. 
# If you've got geometry that shares the same boundaries i.e. federal electoral boundaries
# And you try and join previously intersection-ed data with that geometry it is
# going to share the boundaries with the geometry that was sliced it
# Therefore if you try to rejoin with this geometry, it's not going to function 
# how I wanted it to which was only capture where the majority of the geometry was contained
# It's going to be captured by the shared borders too, therefore duplicating it
# WHATTA GENIUS

if (nrow(spec.range.elect.eighty) == nrow(spec.range.elect.eighty.exp)){
  spec.range.elect.eighty <- spec.range.elect.eighty %>% 
    group_by(Elect_div) %>% 
    summarise(total_unique_spec = n_distinct(SCIENTIFIC_NAME))
  spec.range.elect.eighty.aus <- st_intersection(aus.ss, spec.eighty.elect.exp) %>%
    st_make_valid()
} else{
  print("This spatial join has duplicated rows")
}

#### spec.endemic.elect - specs endemic to each electorate ####
# Count no. of endemic specs per electorate, then snap onto Aus

# TO DO: Add conditional evaluation to this section

spec.endemic.elect <- spec.range.elect %>% 
  filter(percent_range_within == 1) %>% 
  select(-c("Elect_div", "State", "Shape_Area"))

spec.endemic.elect <- elects.ss %>% 
  st_join(spec.endemic.elect, left = FALSE) %>% 
  group_by(Elect_div) %>%
  summarise(total_unique_spec = n_distinct(SCIENTIFIC_NAME))
spec.endemic.elect.aus <- st_intersection(aus.ss, spec.endemic.elect) %>%
  st_make_valid()

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
  select(-"geometry") %>% 
  drop_na()

#### spec.outside.elect - Clipping/antijoin ####
# Couple of methods here:
## 1. Same procedure as endemic but with st_difference
specs.ss.area <- specs.ss %>% 
  mutate(spec_area_sqm = st_area(Shape) %>% as.numeric())

spec.outside.elect <- st_difference(specs.ss.area, elects.ss) %>% 
  st_make_valid() %>% 
  mutate(intersection_area_sqm = st_area(Shape) %>% as.numeric()) %>% 
  transform(percent_range_within = intersection_area_sqm / spec_area_sqm)

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
# More from Ryan Peek - https://ryanpeek.org/mapping-in-R-workshop/03_spatial_joins.html
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