# HPC script for bigger computations

#### Libraries ####

library(dplyr)
library(sf)

#### Loading and subsetting ####

elects <- st_read("/QRISdata/Q4107/raw_data/AEC_2019_superseded/COM_ELB_region.shp")
specs <-  st_read("/QRISdata/Q4107/raw_data/SNES_public_1july2021.gdb")

elects <- select(elects, c("Elect_div", "State", "Area_SqKm", "geometry"))

# Now playing around on a planar not geodesic scale
sf_use_s2(FALSE)

specs <- st_make_valid(specs)
table((st_is_valid(specs)))

elects <- st_make_valid(elects)
table((st_is_valid(elects)))

st_crs(elects) == st_crs(specs)

#### Join intersect ####

# 'Electorates' object is the object x as we want to keep this geometry and
# inner join as we only want the intersect, not any disjointed values
join.intersect <- st_join(elects, specs, 
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
intersection <- st_intersection(elects, specs)

# Make valid again
intersection <- st_make_valid(intersection)

# Write it
st_write(intersection, dsn = "/QRISdata/Q4107/analysed_data/intersection.gpkg")
