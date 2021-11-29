# Converting HPC spatial ops output into dataframes, csvs, etc
# Basic stats calculations that are easier in R comapred to calc

#### Libraries ####

library(tidyverse)
library(sf)
library(jsonlite)

#### Import ####

spec.per.elect <- st_read(dsn = "analysed_data/21-10-28_HPC_spatial_ops_output/spec.per.elect.gpkg")

spec.per.elect.no.geom <- fromJSON("analysed_data/21-10-28_HPC_spatial_ops_output/spec.per.elect.json")
write.csv(spec.per.elect.no.geom,
          "analysed_data/21-10-28_HPC_spatial_ops_output/spec.per.elect.csv")

spec.per.elect.nest <- fromJSON("analysed_data/21-10-28_HPC_spatial_ops_output/spec.per.elect.nest.json")

spec.eighty.elect <- st_read(dsn = "analysed_data/21-10-28_HPC_spatial_ops_output/spec.eighty.elect.gpkg")

spec.eighty.elect.aus <- st_read(dsn = "analysed_data/21-10-28_HPC_spatial_ops_output/spec.eighty.elect.aus.gpkg")

spec.endemic.elect <- st_read(dsn = "analysed_data/21-10-28_HPC_spatial_ops_output/spec.endemic.elect.gpkg")

spec.endemic.elect.aus <- st_read(dsn = "analysed_data/21-10-28_HPC_spatial_ops_output/spec.endemic.elect.aus.gpkg")

elect.spec.cover.status <- fromJSON("analysed_data/21-10-28_HPC_spatial_ops_output/elect.spec.cover.status.json")
write.csv(elect.spec.cover.status,
          "analysed_data/21-10-28_HPC_spatial_ops_output/elect.spec.cover.status.csv")

spec.outside.elect <- st_read(dsn = "analysed_data/21-10-28_HPC_spatial_ops_output/spec.outside.elect.gpkg")

spec.outside.elect.logical <- st_read(dsn = "analysed_data/21-10-28_HPC_spatial_ops_output/spec.outside.elect.logical.gpkg")
