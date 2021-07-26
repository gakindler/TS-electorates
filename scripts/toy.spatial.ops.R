# Answering questions from my manuscript and improving MW's tables
# Also using R for GIS, let's see how far I get

#### Libraries ####

library(tidyverse)
library(sf)

#### Loading and subsetting ####

electorates <- st_read("raw_data/AEC_2019_superseded/COM_ELB_region.shp")
species <- st_read("raw_data/SNES_public_1july2021.gdb")
species.public <- st_read("raw_data/snes_public_grids_08Aug2019.gdb", layer = "species_combined")
st_layers("raw_data/snes_public_grids_08Aug2019.gdb")
# outline <- st_read("raw_data/aus_outline_nsaasr9nnd_02211a04es_geo/aust_cd66states.shp")

# GET unique values of each column
# st_geometry(species) <- NULL
# unique <- lapply(species, unique)
# unique.exp <- as.data.frame(unique)
# unique.exp <- filter(THREATENED_STATUS, MIGRATORY_STATUS, MARINE, CETACEAN, 
#                      PRESENCE_RANK, PRESENCE_CATEGORY)

# Check CRS's are the same
st_crs(electorates) == st_crs(species)

species.sl <- slice_sample(species, n = 500) %>%
  select(c("SCIENTIFIC_NAME", "VERNACULAR_NAME", "THREATENED_STATUS",
           "Shape_Area", "Shape"))
electorates.ss <- select(electorates, -c("Numccds", "Actual", "Projected",
                                         "Total_Popu", "Australian", "Sortname"))

# Make valid? Wtf does this mean? https://r-spatial.org/r/2017/03/19/invalid.html 
species <- st_make_valid(species)
electorates <- st_make_valid(electorates)

#### Join intersect ####

# 'Electorates' object is the object x as we want to keep this geometry and
# inner join as we only want the intersect, not any disjointed values
join.intersect <- st_join(electorates, species, 
                     join = st_intersects, 
                     left = FALSE)

## Mapping


## Count species within each electorate ##
# Add a column of no of species per electorate
elect.spec.uniq.spec <- join.intersect %>% 
  as_tibble() %>% 
  group_by(Elect_div) %>% 
  mutate(total_unique_spec = n_distinct(SCIENTIFIC_NAME))

# Isolated table of no of species per electorate
elect.spec.uniq.spec.tbl <- join.intersect %>% 
  as_tibble() %>% 
  group_by(Elect_div) %>% 
  summarise(total_unique_spec = n_distinct(SCIENTIFIC_NAME))

## Count electorates on no of species within them ##
# Add a column
elect.spec.uniq.elect <- join.intersect %>% 
  as_tibble() %>% 
  group_by(SCIENTIFIC_NAME) %>% 
  mutate(total_unique_elect = n_distinct(Elect_div))

# Isolated table of no of species per electorate
elect.spec.uniq.elect.tbl <- join.intersect %>% 
  as_tibble() %>% 
  group_by(SCIENTIFIC_NAME) %>% 
  summarise(total_unique_elect = n_distinct(Elect_div))

#### Join antijoin ####

# outside <- sapply(st_intersects(species.sl, electorates.ss), function(x){
#   length(x) == 0
#   })
# 
# inside.elects <- lengths(st_intersects(electorates.ss, species.sl)) > 0
# outside.elects <- !inside.elects

#### Join difference ####

# join.diff <- st_difference(electorates.ss, species.sl)
# join.diff.swap <- st_difference(species.sl, electorates.ss)

#### Join intersect-ion ####

# Intersection join, functions as an inner join
intersection <- st_intersection(electorates, species)

# Make valid again?
intersection <- st_make_valid(intersection)

# Write it
st_write(intersection, dsn = "analysed_data/intersection.gpkg")


# Calculate area of intersection-al polygons (i.e. individual species' range 
# within each of their electorates

# Total area of each species within each electorate?
intersection.calcarea <- intersection %>% 
  mutate(area = st_area(.) %>% as.numeric()) %>% 
  as_tibble() %>%
  group_by(Elect_div, SCIENTIFIC_NAME) %>%
  summarise(area = sum(area))


# st_geometry(intersection.calcarea) <- NULL
# 
# area.nogeom <- area
# intersection <- area.nogeom$AREA_HA == area.nogeom$area
# unique(intersection)
# 
# #### Other ####
# 
# st_geometry(intersect) <- NULL
# 
# species.sl <- st_buffer(species, 0.0)
# electorates.ss <- st_buffer(electorates.ss, 0.0)
# 
# species.sl <- st_buffer(species.sl[!is.na(valid)], 0.0)
# electorates.ss <- st_buffer(electorates.ss[!is.na(valid)], 0.0)
# 
# filter <- st_filter(electorates.ss, species.sl, .pred = st_intersects())
# 
# #### Plotting ####
# 
# tm.elect.spec <- tm_shape(elect.spec) +
#   tm_polygons(col = "lightblue")
# 
# #### Saving ####
# 
# write.csv(elect.spec.ng, file = "analysed_data/elect_spec.csv")
# 
# tmap_save(tm.elect.spec, filename = "plots/tm_elect_spec.png", 
#           width = 600, height = 600)

