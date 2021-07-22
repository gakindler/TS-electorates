# HPC script for bigger computations

#### Libraries ####

library(dplyr)
library(sf)

#### Loading and subsetting ####

electorates <- st_read("/QRISdata/Q4107/raw_data/AEC_2019_superseded/COM_ELB_region.shp")
electorates.arc <- st_read("/QRISdata/Q4107/raw_data/electorates_arcGIS_repair/electorates_arcGIS_repair.shp")
species <-  st_read("/QRISdata/Q4107/raw_data/SNES_public_1july2021.gdb")

species.arc <-  st_read("/QRISdata/Q4107/raw_data/SNES_public_1july2021_arcGIS_repair/SNES_public_1july2021_arcGIS_repair.shp")

electorates <- select(electorates, c("Elect_div", "State", "Area_SqKm", "geometry"))
electorates_arc <- select(electorates, c("Elect_div", "State", "Area_SqKm", "geometry"))

# Make valid? Wtf does this mean? https://r-spatial.org/r/2017/03/19/invalid.html 
electorates <- st_make_valid(electorates)
electorates.arc <- st_make_valid(electorates)
species <- st_make_valid(species)

species.b <- st_buffer(species[!is.na(valid)], 0.0)
species.b <- st_buffer(species.public, 0.0)

species.invalid <- st_make_valid(species.invalid)
table((st_is_valid(species)))
species.public <- st_make_valid(species.public)
table((st_is_valid(species.public)))
st_is_valid(species.public, reason = TRUE)

species.invalid <- slice(species.public, 1946, 3206, 3247)
table((st_is_valid(species.invalid)))
species.invalid <- species.invalid %>% select(geometry)
st_write(species.invalid, dsn = "analysed_data/invalid.geom.gpkg")


[1213] Loop 167 is not valid: Edge 2591 crosses edge 2595
[1365] Loop 0 edge 0 crosses loop 256 edge 4
[2294] Loop 0 edge 3590 crosses loop 219 edge 0.

#### Join intersect ####

# 'Electorates' object is the object x as we want to keep this geometry and
# inner join as we only want the intersect, not any disjointed values
join.intersect <- st_join(electorates, species, 
                          join = st_intersects, 
                          left = FALSE)

join.intersect <- st_make_valid(join.intersect)

st_write(join.intersect, dsn = "/QRISdata/Q4107/analysed_data/intersect.gpkg")

#### Join intersect-ion ####

# Intersection, functions as an inner join
intersection <- st_intersection(electorates.ss, species.sl)

# Make valid again
intersection <- st_make_valid(intersection)

# Write it
st_write(intersection, dsn = "/QRISdata/Q4107/analysed_data/intersection.gpkg")
