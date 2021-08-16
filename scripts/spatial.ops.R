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
          "Shape_Area", "Shape"))
elects.ss <- electorates %>% 
  select(-c("Numccds", "Actual", "Projected", 
            "Total_Popu", "Australian", "Sortname"))
aus.ss <- australia %>% 
  select(geometry) %>% 
  slice(1)

# # Make valid
specs.ss <- st_make_valid(specs.ss)
elects.ss <- st_make_valid(elects.ss)

# Simplify geometry
specs.ss <- st_simplify(specs.ss, dTolerance = 20000) # ___ metres?
elects.ss <- ms_simplify(elects.ss, keep = 0.01, keep_shape = TRUE) # __% of original?
aus.ss <- ms_simplify(aus.ss, keep = 0.01, keep_shape = TRUE) %>%
  select(geometry)

specs.ss <- st_make_valid(specs.ss)
elects.ss <- st_make_valid(elects.ss)
aus.ss <- st_make_valid(aus.ss)

#### Join intersect ####

# 'Electorates' object is the object x as we want to keep this geometry and
# inner join as we only want the intersect, not any disjointed values
join.intersect <- st_join(elects.ss, specs.ss,
                     join = st_intersects,
                     left = FALSE)

#### Join intersect-ion ####

# Intersection join, functions as an inner join
intersection <- st_intersection(elects.ss, specs.ss)
intersection <- st_make_valid(intersection)

#### Spec.per.elect - no. of specs per electorate ####
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

#### Spec.range.elect - specs range within each electorate ####

# Create toy specs dataset, leave out the simplification step for this
# slice_sample(specs.ss, n = 20)
specs.ss <- specs.ss %>% 
  mutate(spec_area_sqm = st_area(Shape) %>% as.numeric())

# Check for empty geoms
table(st_dimension(specs.ss))

elects.ss.area <- elects.ss %>% 
  mutate(elect_area_sqm = st_area(geometry) %>% as.numeric()) %>% 
  select(-"Area_SqKm")

spec.range.elect <- st_intersection(elects.ss.area, specs.ss) %>% 
  st_make_valid() %>% 
  mutate(intersection_area_sqm = st_area(geometry) %>% as.numeric()) %>% 
  transform(percent_range_within = intersection_area_sqm / spec_area_sqm)

# Pause for a quick vis
ggplot(spec.range.elect, aes(x = percent_range_within)) +
  geom_histogram(binwidth = .02)

spec.range.elect.eighty <- spec.range.elect %>% 
  filter(percent_range_within >= 0.8) %>% 
  select(-c("Elect_div", "State", "Shape_Area"))

# Something is happening here where sometimes the elect_div of pre and post scripts
# here are not matching up - could it be from the simplification step?

spec.range.elect.eighty <- elects.ss %>% 
  st_join(spec.range.elect.eighty, left = FALSE) %>% 
  group_by(Elect_div) %>%
  summarise(total_unique_spec = n_distinct(SCIENTIFIC_NAME))
spec.range.elect.eighty.aus <- st_intersection(aus.ss, spec.range.elect.eighty) %>%
  st_make_valid()


# Total area of each specs within each electorate?
intersection.calcarea <- intersection %>% 
  mutate(area = st_area(.) %>% as.numeric()) %>% 
  as_tibble() %>%
  group_by(Elect_div, SCIENTIFIC_NAME) %>%
  summarise(area = sum(area))

#### Spec.endemic.elect - specs endemic to each electorate ####
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



# Count no. of elects each specs range covers
spec.range.elect <- join.intersect %>% 
  as_tibble() %>% 
  group_by(SCIENTIFIC_NAME) %>% 
  summarise(elect_range_covers = n_distinct(Elect_div))

#### Clipping/antijoin ####

elects.ss.union <- st_union(elects.ss, by_feature = FALSE) %>% 
  st_sf()

# Logical vector method
outside <- sapply(st_intersects(specs.ss, elects.ss.union), function(x){
  length(x) == 0
  })
spec.out <- specs.ss[outside, ]

# Fact check
antijoin.fc <- unique(join.intersect["SCIENTIFIC_NAME"])

# Alt method
inside.elects <- lengths(st_intersects(elects.ss.union, specs.ss)) > 0
outside.elects <- !inside.elects



#### Join difference ####

# join.diff <- st_difference(elects.ss, specs.sl)
# join.diff.swap <- st_difference(specs.sl, elects.ss)


# Calculate area of intersection-al polygons (i.e. individual specs' range 
# within each of their elects



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
# specs.sl <- st_buffer(specs, 0.0)
# elects.ss <- st_buffer(elects.ss, 0.0)
# 
# specs.sl <- st_buffer(specs.sl[!is.na(valid)], 0.0)
# elects.ss <- st_buffer(elects.ss[!is.na(valid)], 0.0)
# 
# filter <- st_filter(elects.ss, specs.sl, .pred = st_intersects())
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
