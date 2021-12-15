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

demo.pop <- demo %>%
  inner_join(pop)

#### Import and analyse - HPC data ####

#### spec.per.elect - no. of species per electorates, demography, and concentration ####

spec.per.elect <- st_read(
  dsn = "analysed_data/21-12-06_HPC_spatial_ops_output/spec.per.elect.gpkg"
)
spec.per.elect <- spec.per.elect %>%
  inner_join(demo.pop) %>%
  mutate(Elect_div_abbrev = substr(Elect_div, start = 1, stop = 2)) %>%
  relocate(
    Elect_div, Elect_div_abbrev, Demographic_class,
    Electors, elect_area_sqkm, total_unique_spec,
    species_per_sqkm, geom
  ) %T>%
  st_write(
    dsn = "analysed_data/21-12-06_local_analysis_output/spec.per.elect.gpkg",
    layer = "spec.per.elect", append = FALSE
  ) %>%
  st_set_geometry(NULL) %T>%
  write.csv(
    "analysed_data/21-12-06_local_analysis_output/spec.per.elect.csv",
    row.names = FALSE
  )

#### spec.per.elect.indiv - indivdual species per electorate ####

spec.per.elect.indiv <- fromJSON(
  dsn = "analysed_data/21-12-06_HPC_spatial_ops_output/spec.per.elect.indiv.json"
)
spec.per.elect.indiv <- spec.per.elect.indiv %>%
  inner_join(demo.pop) %>%
  select(c("Elect_div", "total_unique_spec", "")) %>%
  nest_by(Elect_div, State, Demographic_class, total_unique_spec) %T>%
  write.csv(
    path = "analysed_data/21-12-06_local_analysis_output/spec.per.elect.indiv.csv"
  )

#### spec.endemic.elect/spec.eighty.elect - species range completely/80% within each electorate ####

spec.endemic.elect <- st_read(
  dsn = "analysed_data/21-12-06_HPC_spatial_ops_output/spec.endemic.elect.gpkg"
) %>%
  inner_join(demo.pop) %>%
  rename(total_endemic_unique_spec = total_unique_spec) %T>%
  st_write(
    dsn = "analysed_data/21-12-06_local_analysis_output/spec.endemic.elect.gpkg",
    layer = "spec.endemic.elect", append = FALSE
  ) %>%
  st_set_geometry(NULL)

spec.eighty.elect <- st_read(
  dsn = "analysed_data/21-12-06_HPC_spatial_ops_output/spec.eighty.elect.gpkg"
) %>%
  inner_join(demo.pop) %>%
  rename(total_eighty_unique_spec = total_unique_spec) %T>%
  st_write(
    dsn = "analysed_data/21-12-06_local_analysis_output/spec.eighty.elect.gpkg",
    layer = "spec.eighty.elect", append = FALSE
  ) %>%
  st_set_geometry(NULL)

spec.endemic.eighty.elect <- spec.endemic.elect %>%
  full_join(spec.eighty.elect) %>%
  relocate(
    Elect_div, Demographic_class, Electors,
    total_endemic_unique_spec, total_eighty_unique_spec
  ) %T>%
  write.csv(
    "analysed_data/21-12-06_local_analysis_output/spec.endemic.eighty.elect.csv",
    row.names = FALSE
  )

#### elect.spec.cover - How many electorates does each species's range cover? ####

elect.spec.cover <- fromJSON(
  "analysed_data/21-12-06_HPC_spatial_ops_output/elect.spec.cover.json"
)

elect.spec.cover <- elect.spec.cover %>%
  relocate()

write.csv(
  elect.spec.cover,
  "analysed_data/21-12-06_local_analysis_output/elect.spec.cover.csv",
  row.names = FALSE
)

# elect.spec.cover$elect_range_covers <-
#   elect.spec.cover$elect_range_covers %>%
#   replace_na(0)

#### spec.eighty.outside.elect ####

spec.outside.elect <- st_read(
  dsn = "analysed_data/21-12-06_HPC_spatial_ops_output/spec.outside.elect.gpkg"
)

write.csv(
  spec.outside.elect,
  "analysed_data/21-12-06_local_analysis_output/spec.outside.elect.csv",
  row.names = FALSE
)

spec.outside.elect.logical <- st_read(
  dsn = "analysed_data/21-12-06_HPC_spatial_ops_output/spec.outside.elect.logical.gpkg"
)

write.csv(
  spec.outside.elect.logical,
  "analysed_data/21-12-06_local_analysis_output/spec.outside.elect.logical.csv",
  row.names = FALSE
)