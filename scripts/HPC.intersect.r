# Intersect HPC script

#### Libraries ####

library(tidyverse)
library(sf)
library(tmap)

#### Loading and subsetting ####

electorates <- st_read("raw_data/AEC_2019_superseded/COM_ELB_region.shp")
species <-  st_read("raw_data/SNES_public_1july2021.gdb")
# outline <- st_read("raw_data/aus_outline_nsaasr9nnd_02211a04es_geo/aust_cd66states.shp")

# Check CRS's are the same
st_crs(electorates) == st_crs(species)

species.ss <- select(c("SCIENTIFIC_NAME", "VERNACULAR_NAME", "THREATENED_STATUS",
           "Shape_Area", "Shape"))
electorates.ss <- select(electorates, -c("Numccds", "Actual", "Projected", 
                                         "Total_Popu", "Australian", "Sortname"))

# Make valid? Wtf does this mean? https://r-spatial.org/r/2017/03/19/invalid.html 
species.ss <- st_make_valid(species.ss)
electorates.ss <- st_make_valid(electorates.ss)

#### Join intersect ####

# 'Electorates' object is the object x as we want to keep this geometry and
# inner join as we only want the intersect, not any disjointed values
join.intersect <- st_join(electorates.ss, species.ss, 
                          join = st_intersects, 
                          left = FALSE)

## Count species within each electorate ##
# Add a column of no of species per electorate
elect.spec.uniq.spec <- intersect %>% 
  as_tibble() %>% 
  group_by(Elect_div) %>% 
  mutate(total_unique_spec = n_distinct(SCIENTIFIC_NAME))

# Isolated table of no of species per electorate
elect.spec.uniq.spec.tbl <- intersect %>% 
  as_tibble() %>% 
  group_by(Elect_div) %>% 
  summarise(total_unique_spec = n_distinct(SCIENTIFIC_NAME))

## Count electorates on no of species within them ##

elect.spec.uniq.elect <- intersect %>% 
  as_tibble() %>% 
  group_by(SCIENTIFIC_NAME) %>% 
  mutate(total_unique_elect = n_distinct(Elect_div))

# Isolated table of no of species per electorate
elect.spec.uniq.elect.tbl <- intersect %>% 
  as_tibble() %>% 
  group_by(SCIENTIFIC_NAME) %>% 
  summarise(total_unique_elect = n_distinct(Elect_div))

#### Join antijoin ####

outside <- sapply(st_intersects(species.sl, electorates.ss), function(x){
  length(x) == 0
})

inside.elects <- lengths(st_intersects(electorates.ss, species.sl)) > 0
outside.elects <- !inside.elects

#### Join difference ####

join.diff <- st_difference(electorates.ss, species.sl)
join.diff.swap <- st_difference(species.sl, electorates.ss)

#### Join intersect-ion ####

# Intersection join, functions as an inner join
intersection <- st_intersection(electorates.ss, species.sl)

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


st_geometry(intersection.calcarea) <- NULL

area.nogeom <- area
intersection <- area.nogeom$AREA_HA == area.nogeom$area
unique(intersection)

#### Other ####

st_geometry(intersect) <- NULL

species.sl <- st_buffer(species.sl, 0.0)
electorates.ss <- st_buffer(electorates.ss, 0.0)

species.sl <- st_buffer(species.sl[!is.na(valid)], 0.0)
electorates.ss <- st_buffer(electorates.ss[!is.na(valid)], 0.0)

filter <- st_filter(electorates.ss, species.sl, .pred = st_intersects())

#### Plotting ####

tm.elect.spec <- tm_shape(elect.spec) +
  tm_polygons(col = "lightblue")

#### Saving ####

write.csv(elect.spec.ng, file = "analysed_data/elect_spec.csv")

tmap_save(tm.elect.spec, filename = "plots/tm_elect_spec.png", 
          width = 600, height = 600)

