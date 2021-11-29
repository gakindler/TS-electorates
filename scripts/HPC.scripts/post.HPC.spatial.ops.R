# Answering questions from my manuscript and improving MW's tables
# Also using R for GIS, let's see how far I get

#### Libraries ####

library(tidyverse)
library(sf)

#### Loading and subsetting ####

join.intersect <- st_read("analysed_data/intersect.gpkg")
# outline <- st_read("raw_data/aus_outline_nsaasr9nnd_02211a04es_geo/aust_cd66states.shp")

#### Join intersect ####
## Count specs within each electorate ##
# Add a column of no of specs per electorate
elect.spec.uniq.spec <- join.intersect %>% 
  as_tibble() %>% 
  group_by(Elect_div) %>% 
  mutate(total_unique_spec = n_distinct(SCIENTIFIC_NAME))

# Isolated table of no of specs per electorate
elect.spec.uniq.spec.tbl <- join.intersect %>% 
  as_tibble() %>% 
  group_by(Elect_div) %>% 
  summarise(total_unique_spec = n_distinct(SCIENTIFIC_NAME))

## Count elects on no of specs within them ##
# Add a column
elect.spec.uniq.elect <- join.intersect %>% 
  as_tibble() %>% 
  group_by(SCIENTIFIC_NAME) %>% 
  mutate(total_unique_elect = n_distinct(Elect_div))

# Isolated table of no of specs per electorate
elect.spec.uniq.elect.tbl <- join.intersect %>% 
  as_tibble() %>% 
  group_by(SCIENTIFIC_NAME) %>% 
  summarise(total_unique_elect = n_distinct(Elect_div))

#### Join antijoin ####


#### Join difference ####


#### Join intersect-ion ####


# Calculate area of intersection-al polygons (i.e. individual specs' range 
# within each of their elects

# Total area of each specs within each electorate?
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

