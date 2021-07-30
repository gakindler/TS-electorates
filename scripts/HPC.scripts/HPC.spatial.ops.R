# HPC script for bigger computations

#### Libraries ####

library(dplyr)
library(sf)

#### Loading and subsetting ####

electorates <- st_read("/QRISdata/Q4107/raw_data/AEC_2019_superseded/COM_ELB_region.shp")
species <-  st_read("/QRISdata/Q4107/raw_data/SNES_public_1july2021.gdb")

electorates <- select(electorates, c("Elect_div", "State", "Area_SqKm", "geometry"))

# Now playing around on a planar not geodesic scale
sf_use_s2(FALSE)

species <- st_make_valid(species)
table((st_is_valid(species)))

electorates <- st_make_valid(electorates)
table((st_is_valid(electorates)))

st_crs(electorates) == st_crs(species)

#### Join intersect ####

# 'Electorates' object is the object x as we want to keep this geometry and
# inner join as we only want the intersect, not any disjointed values
join.intersect <- st_join(electorates, species, 
                          join = st_intersects, 
                          left = FALSE)

table((st_is_valid(join.intersect)))

join.intersect <- st_make_valid(join.intersect)
table((st_is_valid(join.intersect)))

st_write(join.intersect, dsn = "/QRISdata/Q4107/analysed_data/intersect.gpkg")

st_geometry(join.intersect) <- NULL

st_write(join.intersect, dsn = "/QRISdata/Q4107/analysed_data/intersect.nogeom.gpkg")

#### Join intersect-ion ####

# Intersection, functions as an inner join
intersection <- st_intersection(electorates, species)

# Make valid again
intersection <- st_make_valid(intersection)

# Write it
st_write(intersection, dsn = "/QRISdata/Q4107/analysed_data/intersection.gpkg")
