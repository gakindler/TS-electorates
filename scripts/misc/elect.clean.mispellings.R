# cleaning the 2021 elect data for contractions and such, so it matches the other files

#### Libraries ####

library(tidyverse)
library(sf)

#### Import ####

elect.2019 <- st_read("raw_data/AEC_electoral_boundaries_2019/COM_ELB_region.shp") %>%
  st_set_geometry(NULL)

elect.2021 <- st_read("raw_data/2021-Cwlth_electoral_boundaries_ESRI/2021_ELB_region.shp") %>%
  st_set_geometry(NULL)

demo.2019 <- readxl::read_xlsx("raw_data/AEC_demographic-classification-1-january-2019/01-demographic-classification-as-at-1-january-2019.xlsx")

demo.2021 <- readxl::read_xlsx("raw_data/demographic-classification-as-at-2-august-2021.xlsx")

#### Clean ####

names(demo.2019)
names(demo.2021)

demo.2019 <- demo.2019 %>%
  rename(
    State_territory = "State or territory",
    Demographic_class = "Demographic classification",
    Elect_div = "Electoral division"
  )

demo.2021 <- demo.2021 %>%
  rename(
    State_territory = "State or territory",
    Demographic_class = "Demographic classification",
    Elect_div = "Electoral division"
  )

difference <- elect.2021 %>% anti_join(demo.2021)

difference <- demo.2019 %>% anti_join(demo.2021)
