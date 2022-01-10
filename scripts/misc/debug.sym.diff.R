# Debug the sym_diff HPC error

#### Libraries ####

library(tidyverse)
library(sf)
library(rmapshaper) # For installing use 'library(remotes)'

#### main ####

aus.union <- st_read("clean_data/aus.union.clean.gpkg")
elect.union <- st_read("clean_data/elect.union.clean.gpkg")

aus.union <- aus.union %>%
  ms_simplify(
    keep = 0.01,
    keep_shape = TRUE
  ) %>%
  st_make_valid()

elect.union <- elect.union %>%
  ms_simplify(
    keep = 0.01,
    keep_shape = TRUE
  ) %>%
  st_make_valid()

object.size(aus.union)
object.size(elect.union)

elect.aus.union.difference <- elect.union %>%
  mutate(elect_union_sqkm = units::set_units(st_area(.), km^2) %>%
    as.numeric()) %>%
  st_sym_difference(aus.union) %>%
  mutate(aus_union_difference_sqkm = units::set_units(st_area(.), km^2) %>%
    as.numeric())
    # Error in `[[<-.data.frame`(`*tmp*`, attr(x, "sf_column"), value = list( :  replacement has 2 rows, data has 1
    # Error occurs when the data is not simplified, regardless of whether it has been made valid
    st_write(
    "analysed_data/HPC_spatial_ops_output/elect.aus.union.difference.gpkg",
    layer = "elect.aus.union.difference", append = FALSE, delete_dsn = TRUE
  )
