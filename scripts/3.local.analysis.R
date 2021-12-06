# Back end spatial operations to answer research questions

#### Libraries ####

library(tidyverse)
library(sf)
library(rmapshaper) # For installing use 'library(remotes)'
library(jsonlite)
library(magrittr)
library(units)

#### Import clean data - aus, elect, species ####

aus <- st_read("clean_data/aus.clean.gpkg")
elect <- st_read("clean_data/elect.clean.gpkg")
elect.union <- st_read("clean_data/elect.union.clean.gpkg")
species <- st_read("clean_data/species.clean.gpkg")

#### Import and clean - demography, population ####

demo <- readxl::read_xlsx("raw_data/AEC_demographic-classification-1-january-2019/01-demographic-classification-as-at-1-january-2019.xlsx")
demo <- demo %>%
  select(!"State or territory") %>%
  rename(
    Demographic_class = "Demographic classification",
    Elect_div = "Electoral division"
  )

pop <- read.csv("raw_data/AEC_elector_count_2019.csv", skip = 2)
pop <- pop %>%
  rename(Elect_div = Division, Electors = Electors.on.2019.Certified.list) %>%
  select(Elect_div, Electors)
pop$Electors <- as.numeric(gsub(",", "", pop$Electors))
pop$Elect_div <- str_to_title(pop$Elect_div)
pop$Elect_div <- str_squish(pop$Elect_div)
pop$Elect_div <- gsub("Eden-monaro", "Eden-Monaro", pop$Elect_div)
pop$Elect_div <- gsub("Mcewen", "McEwen", pop$Elect_div)
pop$Elect_div <- gsub("Mcmahon", "McMahon", pop$Elect_div)
pop$Elect_div <- gsub("Mcpherson", "McPherson", pop$Elect_div)
pop$Elect_div <- gsub("O'connor", "O'Connor", pop$Elect_div)

#### Import and analyse - HPC data ####

#### spec.per.elect - no. of species per electorates, demography, and concentration ####

spec.per.elect <- st_read(dsn = "analysed_data/21-12-06_HPC_spatial_ops_output/spec.per.elect.gpkg")
spec.per.elect <- spec.per.elect %>%
  inner_join(demo) %>%
  inner_join(pop) %>%
  relocate(
    Elect_div, Demographic_class, Electors, total_unique_spec,
    elect_area_sqkm, species_per_sqkm, geom
  ) %>%
  mutate(Elect_div_abbrev = substr(Elect_div, start = 1, stop = 2)) %T>%
  st_write(
    dsn = "analysed_data/21-12-06_local_analysis_output/spec.per.elect.gpkg",
    layer = "spec.per.elect", append = FALSE
  ) %>%
  st_set_geometry(NULL) %T>%
  write.csv("analysed_data/21-12-06_local_analysis_output/spec.per.elect.csv")

#### spec.per.elect - nested no. of species per electorate ####

# spec.per.elect.nest <- st_read(dsn = "analysed_data/21-12-06_HPC_spatial_ops_output/spec.per.elect.gpkg")
# spec.per.elect.nest <- spec.per.elect.nest %>%
#   st_set_geometry(NULL) %>%
#   inner_join(demo) %>%
#   select(c("Elect_div", "total_unique_spec", ""))
#   nest_by(Elect_div, State, Demographic_class, total_unique_spec) %T>%
#   write_json(path = "analysed_data/21-12-06_local_analysis_output/spec.per.elect.nest.json")

#### spec.range.elect - species range within each electorate ####
spec.eighty.elect <- st_read(dsn = "analysed_data/21-12-06_HPC_spatial_ops_output/spec.eighty.elect.gpkg")

spec.eighty.elect.aus <- st_read(dsn = "analysed_data/21-12-06_HPC_spatial_ops_output/spec.eighty.elect.aus.gpkg")


#### spec.eighty.elect - species range >80% in each electorate ####

spec.endemic.elect <- st_read(dsn = "analysed_data/21-12-06_HPC_spatial_ops_output/spec.endemic.elect.gpkg")

spec.endemic.elect.aus <- st_read(dsn = "analysed_data/21-12-06_HPC_spatial_ops_output/spec.endemic.elect.aus.gpkg")

# TO DO: Count no. of endemic species per electorate in wide format?

#### elect.spec.cover - How many electorates does each species's range cover? ####

elect.spec.cover <- read.csv(
  "analysed_data/21-12-06_HPC_spatial_ops_output/elect.spec.cover.csv"
)

table(elect.spec.cover$elect_range_covers, useNA = "ifany")
table(elect.spec.cover$MIGRATORY_STATUS, useNA = "ifany")

elect.spec.cover$elect_range_covers <-
  elect.spec.cover$elect_range_covers %>%
  replace_na(0)

#### spec.eighty.outside.elect ####

spec.outside.elect <- st_read(
  dsn = "analysed_data/21-12-06_HPC_spatial_ops_output/spec.outside.elect.gpkg"
)

spec.outside.elect.logical <- st_read(
  dsn = "analysed_data/21-12-06_HPC_spatial_ops_output/spec.outside.elect.logical.gpkg"
)
