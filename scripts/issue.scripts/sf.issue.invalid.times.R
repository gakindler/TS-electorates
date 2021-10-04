# sf invalid geoms issue creation file

library(dplyr)
library(sf)


specs.invalid <- slice(specs.public, 1946, 3206, 3247)
table((st_is_valid(specs.invalid)))
specs.invalid <- specs.invalid %>% select(geometry)
st_write(specs.invalid, dsn = "analysed_data/invalid.geom.gpkg")

invalid.geom <- st_read(dsn = "analysed_data/invalid.geom.gpkg")

invalid.geom <- st_make_valid(invalid.geom)

st_is_valid(invalid.geom, reason = TRUE)

invalid.geom.b <- st_buffer(invalid.geom, 0.0)

sapply(st_geometry(comm_dstrct_16)[[46]], function(x) nrow(x[[1]]))


geom <- st_geometry(invalid.geom[[1]][[1]])
st_make_valid(geom)

