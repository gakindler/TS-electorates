# sf invalid geoms issue creation file

library(dplyr)
library(sf)

invalid.geom <- st_read(dsn = "analysed_data/invalid.geom.gpkg")

invalid.geom <- st_make_valid(invalid.geom)

st_is_valid(invalid.geom, reason = TRUE)

invalid.geom.b <- st_buffer(invalid.geom, 0.0)

sapply(st_geometry(comm_dstrct_16)[[46]], function(x) nrow(x[[1]]))


geom <- st_geometry(invalid.geom[[1]][[1]])
st_make_valid(geom)



I'm analysing species distributions within Australia. Some of my geometries are invalid, and are not able to be made valid using either `st_make_valid` or `st_buffer`.

I subsetted the troublesome invalid geometry from the large dataset which can be found [here](https://github.com/gakindler/invalid-geom/raw/main/invalid.geom.gpkg).

```
> invalid.geom <- st_read(dsn = "~/invalid.geom.gpkg")
Reading layer `invalid.geom' from data source 
`~\invalid.geom.gpkg' 
  using driver `GPKG'
Simple feature collection with 3 features and 0 fields
Geometry type: MULTIPOLYGON
Dimension:     XY
Bounding box:  xmin: 42.66 ymin: -69.82 xmax: 173.4 ymax: -24
Geodetic CRS:  GDA94
> invalid.geom <- st_make_valid(invalid.geom)
> st_is_valid(invalid.geom, reason = TRUE)
[1] "Loop 35: Edge 387 crosses edge 1104"      "Loop 0 edge 0 crosses loop 266 edge 4"    "Loop 0 edge 3590 crosses loop 371 edge 0"
> invalid.geom.b <- st_buffer(invalid.geom, 0.0)
Error in s2_geography_from_wkb(x, oriented = oriented, check = check) : 
  Evaluation error: Found 3 features with invalid spherical geometry.
[1] Loop 0 is not valid: Edge 2153 crosses edge 2162
[2] Loop 0 edge 0 crosses loop 266 edge 4
[3] Loop 0 edge 3590 crosses loop 371 edge 0.
```
Would you be able to help me dig deeper on this, please?
