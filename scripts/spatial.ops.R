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
# species.public <- st_read("raw_data/snes_public_grids_08Aug2019.gdb", layer = "species_combined")
# st_layers("raw_data/snes_public_grids_08Aug2019.gdb")
australia <- st_read("raw_data/ASGS_Edition_3_Aust_2021_shapefile/AUS_2021_AUST_GDA94.shp")

# What are the unique values of each attribute?
# unique <- lapply(species, unique)
# st_geometry(unique) <- NULL

# Check CRS's are the same
st_crs(electorates) == st_crs(species)
st_crs(electorates) == st_crs(australia)

# Filter for only 'likely to occur', and bs columns/rows
species.ss <- species %>% 
  filter(PRESENCE_RANK == 2) %>% 
  select(c("SCIENTIFIC_NAME", "VERNACULAR_NAME", "THREATENED_STATUS",
          "Shape_Area", "Shape"))
electorates.ss <- electorates %>% 
  select(-c("Numccds", "Actual", "Projected", 
            "Total_Popu", "Australian", "Sortname"))
australia.ss <- australia %>% 
  select(geometry) %>% 
  slice(1)

# # Make valid
species.ss <- st_make_valid(species.ss)
electorates.ss <- st_make_valid(electorates.ss)

# Simplify geometry
species.ss <- st_simplify(species.ss, dTolerance = 20000) # ___ metres?
electorates.ss <- ms_simplify(electorates.ss, keep = 0.01, keep_shape = TRUE) # __% of original?
australia.ss <- ms_simplify(australia.ss, keep = 0.01, keep_shape = TRUE) %>%
  select(geometry)

species.ss <- st_make_valid(species.ss)
electorates.ss <- st_make_valid(electorates.ss)
australia.ss <- st_make_valid(australia.ss)

#### Join intersect ####

# 'Electorates' object is the object x as we want to keep this geometry and
# inner join as we only want the intersect, not any disjointed values
join.intersect <- st_join(electorates.ss, species.ss,
                     join = st_intersects,
                     left = FALSE)

#### Join intersect-ion ####

# Intersection join, functions as an inner join
intersection <- st_intersection(electorates.ss, species.ss)
intersection <- st_make_valid(intersection)

#### Spec.per.elect - no. of species per electorate ####
# Count no. of species per electorate, then snap onto Aus
spec.per.elect <- electorates.ss %>% 
  st_join(species.ss) %>% 
  group_by(Elect_div) %>% 
  summarise(total_unique_spec = n_distinct(SCIENTIFIC_NAME))
spec.per.elect.aus <- st_intersection(australia.ss, spec.per.elect) %>% 
  st_make_valid()

# # Count no. of species per electorate while maintaining species list
# elect.spec.uniq.spec.exp <- join.intersect %>% 
#   as_tibble() %>% 
#   group_by(Elect_div) %>% 
#   mutate()
#   summarise(total_unique_spec = n_distinct(SCIENTIFIC_NAME))
#   
# exp <- split(species.sl)

#### Spec.endemic.per.elect - species endemic to each electorate ####
# Count no. of endemic species per electorate, then snap onto Aus
spec.endemic.elect <- electorates.ss %>% 
  st_join(species.ss) %>% 
  group_by(SCIENTIFIC_NAME, Elect_div) %>% 
  summarise(elect_range_covers = n_distinct(SCIENTIFIC_NAME))

# Count no. of endemic species per electorate in wide format?



# Count no. of electorates each species range covers
spec.range.elect <- join.intersect %>% 
  as_tibble() %>% 
  group_by(SCIENTIFIC_NAME) %>% 
  summarise(elect_range_covers = n_distinct(Elect_div))

#### Clipping/antijoin ####

electorates.ss.union <- st_union(electorates.ss, by_feature = FALSE) %>% 
  st_sf()

# Logical vector method
outside <- sapply(st_intersects(species.ss, electorates.ss.union), function(x){
  length(x) == 0
  })
spec.out <- species.ss[outside, ]

# Fact check
antijoin.fc <- unique(join.intersect["SCIENTIFIC_NAME"])

# Alt method
inside.elects <- lengths(st_intersects(electorates.ss.union, species.ss)) > 0
outside.elects <- !inside.elects



#### Join difference ####

# join.diff <- st_difference(electorates.ss, species.sl)
# join.diff.swap <- st_difference(species.sl, electorates.ss)


# Calculate area of intersection-al polygons (i.e. individual species' range 
# within each of their electorates

#### Spec.range.elect - species range within each electorate ####

electorates.ss.area <- electorates.ss %>% 
  mutate(elect_area_sqm = st_area(geometry) %>% as.numeric())

# PROBLEMO IS HERE - I'm getting species range within the electorate
spec.range.elect <- st_intersection(electorates.ss.area, species.ss) %>% 
  st_make_valid() %>% 
  mutate(intersection_area_sqm = st_area(geometry) %>% as.numeric()) %>% 
  transform(percent_range_within = intersection_area_sqm / elect_area_sqm)

# Pause for a quick vis
ggplot(spec.range.elect, aes(x = percent_range_within)) +
  geom_histogram(binwidth = .02)

spec.range.elect.eighty <- elect.spec.range.cover %>% 
  filter(percent_range_within >= 0.8) %>% 
  select(-c("Elect_div", "State"))

spec.range.elect.eighty <- electorates.ss %>% 
  st_join(spec.range.elect.eighty) %>% 
  group_by(Elect_div) %>% 
  summarise(total_unique_spec = n_distinct(SCIENTIFIC_NAME))
spec.range.elect.eighty.aus <- st_intersection(australia.ss, spec.range.elect.eighty) %>% 
  st_make_valid()



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
# 
# # Write it
# st_write(intersection, dsn = "analysed_data/intersection.gpkg")
