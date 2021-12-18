# Back end spatial operations to answer research questions

#### Libraries ####

library(tidyverse)
library(sf)
library(rmapshaper) # For installing use 'library(remotes)'
library(jsonlite)
library(magrittr)
library(units)

#### Aus, elect, species: Import clean data ####

aus <- st_read("clean_data/aus.clean.gpkg")
elect <- st_read("clean_data/elect.clean.gpkg")
elect.union <- st_read("clean_data/elect.union.clean.gpkg")
species <- st_read("clean_data/species.clean.gpkg")

#### Demography, population: Import and clean ####

demo <- readxl::read_xlsx("raw_data/AEC_demographic-classification-1-january-2019/01-demographic-classification-as-at-1-january-2019.xlsx")
demo <- demo %>%
  rename(
    State_territory = "State or territory",
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

#### Import HPC outputs ####

spec.per.elect.counts <- st_read(
  "analysed_data/21-12-18_HPC_spatial_ops_output/spec.per.elect.counts.gpkg"
)
spec.per.elect.indiv <- fromJSON(
  "analysed_data/21-12-18_HPC_spatial_ops_output/spec.per.elect.indiv.json"
)
spec.eighty.elect <- st_read(
  "analysed_data/21-12-18_HPC_spatial_ops_output/spec.eighty.elect.gpkg"
)
spec.endemic.elect <- st_read(
  "analysed_data/21-12-18_HPC_spatial_ops_output/spec.endemic.elect.gpkg"
)
elect.spec.cover <- fromJSON(
  "analysed_data/21-12-18_HPC_spatial_ops_output/elect.spec.cover.json"
)

#### Elect.info ####

elect.info <- elect %>%
  st_set_geometry(NULL) %>%
  rename(State_territory_abbrev = State) %>%
  inner_join(pop) %>%
  inner_join(demo) %>%
  mutate(Elect_div_abbrev = abbreviate(Elect_div, minlength = 4L))

#### spec.per.elect.counts - no. of species per electorates, demography, and concentration ####

spec.per.elect.counts.elect.info <- spec.per.elect.counts %>%
  inner_join(elect.info) %>%
  mutate(
    species_per_sqkm = total_unique_spec / elect_area_sqkm
  )

#### spec.endemic.elect/spec.eighty.elect - species range completely/80% within each electorate ####

st_write(spec.eighty.elect,
  "analysed_data/21-12-18_local_analysis_output/spec.eighty.elect.gpkg",
  layer = "spec.eighty.elect", append = FALSE
)
st_write(spec.endemic.elect,
  "analysed_data/21-12-18_local_analysis_output/spec.endemic.elect.gpkg",
  layer = "spec.endemic.elect", append = FALSE
)

spec.eighty.elect <- spec.eighty.elect %>%
  st_set_geometry(NULL) %>%
  group_by(Elect_div) %>%
  summarise(total_eighty_unique_spec = n_distinct(SCIENTIFIC_NAME))

spec.endemic.elect <- spec.endemic.elect %>%
  st_set_geometry(NULL) %>%
  group_by(Elect_div) %>%
  summarise(total_endemic_unique_spec = n_distinct(SCIENTIFIC_NAME))
  
spec.per.elect.counts <- spec.per.elect.counts.elect.info %>%
  left_join(spec.eighty.elect) %>%
  left_join(spec.endemic.elect) %>%
  relocate(
    Elect_div, Elect_div_abbrev, State_territory,
    State_territory_abbrev, Demographic_class,
    Electors, elect_area_sqkm, total_unique_spec,
    species_per_sqkm, total_eighty_unique_spec,
    total_endemic_unique_spec, geom
  ) %T>%
  st_write(
    "analysed_data/21-12-18_local_analysis_output/spec.per.elect.counts.gpkg",
    layer = "spec.per.elect.counts", append = FALSE
  ) %>%
  st_set_geometry(NULL) %T>%
  write.csv(
    "analysed_data/21-12-18_local_analysis_output/spec.per.elect.counts.csv",
    row.names = FALSE
  )

#### spec.per.elect.indiv - indivdual species per electorate ####

spec.per.elect.indiv <- spec.per.elect.indiv %T>%
  # left_join(elect.info)
  write.csv(
    "analysed_data/21-12-18_local_analysis_output/spec.per.elect.indiv.csv",
    row.names = FALSE
  )
  # nest_by(Elect_div, State, Demographic_class, total_unique_spec)

# spec.per.elect.indiv.wide <- spec.per.elect.indiv %>%
#   unite(
#     "VERNACULAR_NAME-SCIENTIFIC_NAME",
#     VERNACULAR_NAME:SCIENTIFIC_NAME,
#     sep = "-"
#   ) %>%
#   select(!c("", ""))
#   pivot_wider(
#     names_from = Elect_div,
#     values_from = "VERNACULAR_NAME-SCIENTIFIC_NAME"
#   ) %T>%
#   write.csv(
#     "analysed_data/21-12-18_local_analysis_output/spec.per.elect.indiv.wide.csv",
#     row.names = FALSE
#   )

#### elect.spec.cover - How many electorates does each species's range cover? ####

elect.spec.cover <- elect.spec.cover %>%
  relocate(SCIENTIFIC_NAME, VERNACULAR_NAME, THREATENED_STATUS,
  TAXON_GROUP, VERNACULAR_NAME, MIGRATORY_STATUS,
  elect_range_covers) %T>%
  write.csv(
  "analysed_data/21-12-18_local_analysis_output/elect.spec.cover.csv",
  row.names = FALSE
)

# elect.spec.cover$elect_range_covers <-
#   elect.spec.cover$elect_range_covers %>%
#   replace_na(0)

# #### spec.eighty.outside.elect ####

# write.csv(
#   spec.outside.elect,
#   "analysed_data/21-12-18_local_analysis_output/spec.outside.elect.csv",
#   row.names = FALSE
# )



# write.csv(
#   spec.outside.elect.logical,
#   "analysed_data/21-12-18_local_analysis_output/spec.outside.elect.logical.csv",
#   row.names = FALSE
# )