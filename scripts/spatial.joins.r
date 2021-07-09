# Answering questions from my manuscript and improving MW's tables
# Also using R for GIS, let's see how far I get

#### Libraries ####

library(tidyverse)
library(sf)
library(tmap)

#### Loading and subsetting ####

electorates <- st_read("raw_data/AEC_2019_superseded/COM_ELB_region.shp")
species <-  st_read("raw_data/SNES_public_1july2021.gdb")
# outline <- st_read("raw_data/aus_outline_nsaasr9nnd_02211a04es_geo/aust_cd66states.shp")

species.sl <- slice_sample(species, n = 1000) %>% 
  select(c("SCIENTIFIC_NAME", "VERNACULAR_NAME", "THREATENED_STATUS", "Shape_Area", "Shape"))
electorates.ss <- select(electorates, -c("Numccds", "Actual", "Projected", 
                                         "Total_Popu", "Australian", "Sortname"))

# Check CRS's are the same
st_crs(electorates) == st_crs(species)

species.sl <- st_buffer(species.sl, 0.0)
electorates.ss <- st_buffer(electorates.ss, 0.0)

species.sl <- st_buffer(species.sl[!is.na(valid)], 0.0)
electorates.ss <- st_buffer(electorates.ss[!is.na(valid)], 0.0)

#### Join intersect ####

# Make valid? Wtf does this mean? https://r-spatial.org/r/2017/03/19/invalid.html 
species.sl <- st_make_valid(species.sl)
electorates.ss <- st_make_valid(electorates.ss)

intersect <- st_join(electorates.ss, species.sl, join = st_intersects)

st_geometry(intersect) <- NULL
# elect.spec.ng <- dplyr::select(as.data.frame(elect.spec), -geometry)

## Count species within each electorate ##

# Add a column of no of species per electorate
elect.spec.uniq.spec <- intersect %>% 
  group_by(Elect_div) %>%
  mutate(total_unique_spec = n_distinct(CURRENT_NAME))

# Isolated table of no of species per electorate
elect.spec.uniq.spec.tbl <- intersect %>% 
  group_by(Elect_div) %>% 
  summarise(total_unique_spec = n_distinct(CURRENT_NAME))

## Count electorates on no of species within them ##

elect.spec.uniq.elect <- intersect %>% 
  group_by(CURRENT_NAME) %>% 
  mutate(total_unique_elect = n_distinct(Elect_div))

# Isolated table of no of species per electorate
elect.spec.uniq.elect.tbl <- intersect %>% 
  group_by(CURRENT_NAME) %>% 
  summarise(total_unique_elect = n_distinct(Elect_div))

#### Join intersect-ion ####

# Make valid? Wtf does this mean? https://r-spatial.org/r/2017/03/19/invalid.html 
species.sl <- st_make_valid(species.sl)
electorates.ss <- st_make_valid(electorates.ss)

# Intersection join
intersection <- st_intersection(species.sl, electorates.ss)

# Calculate area of intersection-al polygons (i.e. individual species' range 
# within each of their electorates

intersection.exp <- intersection %>% 
  mutate(area = st_area(.) %>% as.numeric()) %>% 
  as_tibble() %>%
  group_by(Elect_div, CURRENT_NAME) %>%
  summarise(area = sum(area))

intersection.exp2 <- intersection %>% 
  as_tibble() %>%
  group_by(Elect_div, CURRENT_NAME) %>%
  mutate(area = st_area(.) %>% as.numeric())

area.nogeom <- area
st_geometry(area.nogeom) <- NULL
intersection <- area.nogeom$AREA_HA == area.nogeom$area
unique(intersection)

#### Other ####

filter <- st_filter(electorates.ss, species.sl, .pred = st_intersects())

#### Plotting ####

tm.elect.spec <- tm_shape(elect.spec) +
  tm_polygons(col = "lightblue")

#### Saving ####

write.csv(elect.spec.ng, file = "analysed_data/elect_spec.csv")

tmap_save(tm.elect.spec, filename = "plots/tm_elect_spec.png", 
          width = 600, height = 600)

