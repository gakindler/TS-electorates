# Trialling R for GIS as arcGIS was toxic
# Let's see how far I get

#### Libraries ####

library(tidyverse)
library(sf)
library(tmap)

#### Loading and subsetting ####

electorates <- st_read("raw_data/AEC_2019_superseded/COM_ELB_region.shp")
species <-  st_read("raw_data/EPBC_Listed_Species_100m_MW.gdb")
# outline <- st_read("raw_data/aus_outline_nsaasr9nnd_02211a04es_geo/aust_cd66states.shp")

species.sl <- slice_sample(species, n = 50000) %>% 
  select(c("CURRENT_NAME", "COMMON_NAME", "THREATENED_STATUS", "AREA_HA", "SHAPE"))
electorates.ss <- select(electorates, -c("Numccds", "Actual", "Projected", 
                                         "Total_Popu", "Australian", "Sortname"))

# Check CRS's are the same
st_crs(electorates) == st_crs(species)

# Make valid? Wtf does this mean? https://r-spatial.org/r/2017/03/19/invalid.html 
species.sl <- st_make_valid(species.sl)
electorates.ss <- st_make_valid(electorates.ss)

#### Join ####

intersect <- st_join(electorates.ss, species.sl, join = st_intersects)
intersect.exp <- st_join()
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

#### Experimental ####

disjoint <- st_join(electorates.ss, species.sl, join = st_disjoint, left = TRUE)

contains <- st_join(electorates.ss, species.sl, join = st_contains)
st_geometry(contains) <- NULL


#### Plotting ####

tm.elect.spec <- tm_shape(elect.spec) +
  tm_polygons(col = "lightblue")

#### Saving ####

write.csv(elect.spec.ng, file = "analysed_data/elect_spec.csv")

tmap_save(tm.elect.spec, filename = "plots/tm_elect_spec.png", 
          width = 600, height = 600)

