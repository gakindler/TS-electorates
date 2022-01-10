# Differneces between the 2019 and 2021-released electoral boundaries

#### Libraries ####

library(tidyverse)
library(sf)

#### Electorates: Import/clean ####

elect2019 <- st_read("raw_data/AEC_electoral_boundaries_2019/COM_ELB_region.shp") %>%
  st_set_geometry(NULL)

elect2021 <- st_read("raw_data/2021-Cwlth_electoral_boundaries_ESRI/2021_ELB_region.shp") %>%
  st_set_geometry(NULL)

#### Check ####

names(elect2019)
names(elect2021)

#### Clean ####

elect2019 <- elect2019 %>%
    select(Elect_div)

elect2021 <- elect2021 %>%
    select(Elect_div)

#### Test ####

elect.difference <- elect2019 %>% anti_join(elect2021)
