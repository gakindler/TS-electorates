# out of memory carto script

library(tidyverse)
library(sf)
library(tmaptools)

#### Import and simplify data ####

spec.endemic.elect <- st_read(
  "/QRISdata/Q4107/analysed_data/HPC_spatial_ops_output/spec.endemic.elect.gpkg"
)
spec.eighty.elect <- st_read(
  "/QRISdata/Q4107/analysed_data/HPC_spatial_ops_output/spec.eighty.elect.gpkg"
)
elect <- st_read("/QRISdata/Q4107/clean_data/elect.clean.gpkg")

print(object.size(spec.endemic.elect), units = "Kb")
print(object.size(spec.eighty.elect), units = "Kb")
print(object.size(elect), units = "Kb")

spec.endemic.elect <- simplify_shape(
  spec.endemic.elect,
  0.001,
  keep.units = TRUE
)
spec.eighty.elect <- simplify_shape(
  spec.eighty.elect,
  0.001,
  keep.units = TRUE
)
elect <- simplify_shape(
  elect,
  0.001,
  keep.units = TRUE
) %>%
  st_make_valid()

spec.endemic.elect <- ms_simplify(spec.endemic.elect,
  keep = 0.01,
  keep_shape = TRUE
) %>%
  st_make_valid()
spec.eighty.elect <- ms_simplify(spec.eighty.elect,
  keep = 0.01,
  keep_shape = TRUE
) %>%
  st_make_valid()
elect <- ms_simplify(elect,
  keep = 0.01,
  keep_shape = TRUE
) %>%
  st_make_valid()
