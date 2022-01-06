# Back end spatial operations to answer research questions

#### Libraries ####

library(tidyverse)
library(sf)
library(rmapshaper) # For installing use 'library(remotes)'
library(jsonlite)
library(magrittr)
library(units)

#### Import HPC outputs ####

elect.aus.union.difference <- st_read(
  "analysed_data/HPC_spatial_ops_output/elect.aus.union.difference.dpkg"
)
spec.per.elect.counts <- st_read(
  "analysed_data/HPC_spatial_ops_output/spec.per.elect.counts.gpkg"
)
spec.per.elect.indiv <- fromJSON(
  "analysed_data/HPC_spatial_ops_output/spec.per.elect.indiv.json"
)
elect.spec.cover.counts <- fromJSON(
  "analysed_data/HPC_spatial_ops_output/elect.spec.cover.counts.json"
)
elect.spec.cover.indiv <- fromJSON(
  "analysed_data/HPC_spatial_ops_output/elect.spec.cover.indiv.json"
)
spec.range.elect <- st_read(
  "analysed_data/HPC_spatial_ops_output/spec.range.elect.gpkg"
)
spec.outside.elect <- st_read(
  "analysed_data/HPC_spatial_ops_output/spec.outside.elect.gpkg"
)

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

#### Elect.info ####

elect.info <- elect %>%
  st_set_geometry(NULL) %>%
  rename(State_territory_abbrev = State) %>%
  inner_join(pop) %>%
  inner_join(demo) %>%
  mutate(Elect_div_abbrev = abbreviate(Elect_div, minlength = 4L))

#### spec.per.elect ####

spec.per.elect.counts.elect.info <- spec.per.elect.counts %>%
  inner_join(elect.info) %>%
  mutate(
    species_per_sqkm = total_unique_spec / elect_area_sqkm
  )

spec.per.elect.indiv.wide <- spec.per.elect.indiv %>%
  unite(
    "VERNACULAR_NAME-SCIENTIFIC_NAME",
    VERNACULAR_NAME:SCIENTIFIC_NAME,
    sep = "-"
  ) %>%
  select(!c("", ""))
  pivot_wider(
    names_from = Elect_div,
    values_from = "VERNACULAR_NAME-SCIENTIFIC_NAME"
  )

#### elect.spec.cover ####

elect.spec.cover.counts <-

elect.spec.cover.indiv <-
pivot wider?

#### spec.range.elect ####

spec.range.elect.wide <- spec.range.elect %>%
  st_set_geometry(NULL) %>%
  pivot_wider(names_from = Elect_div)

# Filter for species which have >80% of their range within an electorate
spec.eighty.elect <- spec.range.elect %>%
  filter(percent_range_within >= 0.8) %>%
  inner_join(elect) %>% # could use st_join here, but it duplicated col names
  st_as_sf() %>%
  st_set_geometry(NULL) %>%
  group_by(Elect_div) %>%
  mutate(total_eighty_unique_spec = n_distinct(SCIENTIFIC_NAME))

# Filter for species which are endemic to each electorate
spec.endemic.elect <- spec.range.elect %>%
  filter(percent_range_within == 1) %>%
  inner_join(elect) %>%
  st_as_sf() %>%
  st_set_geometry(NULL) %>%
  group_by(Elect_div) %>% # DO WE NEED THIS
  mutate(total_endemic_unique_spec = n_distinct(SCIENTIFIC_NAME))

spec.range.elect.elect.info <-

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
    "analysed_data/local_analysis_output/spec.per.elect.counts.gpkg",
    layer = "spec.per.elect.counts", append = FALSE
  ) %>%
  st_set_geometry(NULL) %T>%
  write.csv(
    "analysed_data/local_analysis_output/spec.per.elect.counts.csv",
    row.names = FALSE
  )

#### elect.spec.cover - How many electorates does each species's range cover? ####

elect.spec.cover <- elect.spec.cover %>%
  relocate(SCIENTIFIC_NAME, VERNACULAR_NAME, THREATENED_STATUS,
  TAXON_GROUP, VERNACULAR_NAME, MIGRATORY_STATUS,
  elect_range_covers)
    left_join(spec.per.elect.indiv)
  write.csv(
  "analysed_data/21-12-18_local_analysis_output/elect.spec.cover.csv",
  row.names = FALSE
)

elect.spec.cover$elect_range_covers <-
  elect.spec.cover$elect_range_covers %>%
  replace_na(0)

# #### spec.eighty.outside.elect ####

spec.outside.elect <-
  filter(percent_range_outside >= .8) # 80%, who chose this? Did you? Did I?
write.csv(
  "analysed_data/21-12-18_local_analysis_output/spec.outside.elect.csv",
  row.names = FALSE
)
