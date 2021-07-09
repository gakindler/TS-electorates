# sf issue creation file

library(tidyverse)
library(sf)

species.sl <- slice_sample(species, n = 25) %>% 
  select(c("SCIENTIFIC_NAME", "Shape")) %>% 
  rename(rain = SCIENTIFIC_NAME, geometry = Shape) %>% 
  st_make_valid()
  
electorates.ss <- slice_sample(electorates, n = 25) %>% 
  select(c("Elect_div", "geometry")) %>% 
  rename(field = Elect_div) %>% 
  st_make_valid()


