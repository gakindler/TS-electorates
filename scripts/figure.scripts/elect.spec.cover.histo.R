# Creating plots using the spatial.ops data

#### Libraries ####

library(tidyverse)
library(viridis)

#### Data ####
aus <- st_read("raw_data/ASGS_Edition_3_Aust_2021_shapefile/AUS_2021_AUST_GDA94.shp")
aus <- aus %>%
  select(geometry) %>%
  slice(1) %>%
  st_crop(xmin = 113, ymin = -43.740482, # drop those pesky islands
          xmax = 154, ymax = -9.219937)

species <- st_read("raw_data/SNES_public_1july2021.gdb")
species <- species %>%
  filter(PRESENCE_RANK == 2) %>%
  filter(!is.na(THREATENED_STATUS)) %>%
  filter(THREATENED_STATUS %in% c(
    "Vulnerable",
    "Endangered",
    "Critically Endangered",
    "Extinct in the wild")) %>% # as Ward_database_2021(?)
  filter(!MARINE %in% "Listed") %>%
  # what about c("Listed", "Listed - overfly marine area")?
  filter(!CETACEAN %in% "Cetacean") %>%
  select(c("SCIENTIFIC_NAME", "VERNACULAR_NAME", "THREATENED_STATUS",
           "MIGRATORY_STATUS", "TAXON_GROUP", "Shape_Area", "Shape")) %>%
  st_make_valid() %>%
  # Merge species at the broad taxa as there are duplicate polygons
  # Probably attributable to subspecies populations, but still ¯\_(ツ)_/¯
  group_by(SCIENTIFIC_NAME, VERNACULAR_NAME, THREATENED_STATUS,
           MIGRATORY_STATUS, TAXON_GROUP) %>%
  summarise() %>%
  ungroup() %>%
  st_make_valid() %>%
  st_intersection(aus) %>% # Crop species to terrestrial Australia
  st_make_valid()

elect.spec.cover.status <- species %>%
  as.data.frame() %>%
  select(c("SCIENTIFIC_NAME", "THREATENED_STATUS", "MIGRATORY_STATUS", "TAXON_GROUP")) %>%
  inner_join(elect.spec.cover.status) # is this actually robust???

# Changes things?
elect.spec.cover.status$elect_range_covers <- elect.spec.cover.status$elect_range_covers %>% 
  replace_na(0)


#### Occurance of TS within demographies ####

elect.spec.cover.status$THREATENED_STATUS <- factor(
  elect.spec.cover.status$THREATENED_STATUS, 
  levels = c("Extinct in the wild", "Conservation Dependent", 
             "Critically Endangered", "Endangered", "Vulnerable",
             "Unknown"))

elect.spec.cover.status.histogram <- ggplot(elect.spec.cover.status) +
  aes(x = elect_range_covers) +
  geom_histogram(binwidth = 1) +
  geom_vline(aes(xintercept = median(elect_range_covers)),
             col = "red",
             size = 0.8,
             linetype = "dashed") +
  labs(x = "Electorates within species's range",
       y = "Number of threatened species") +
  theme_classic()

# elect.spec.cover.status.histogram <-
ggplot(elect.spec.cover.status) +
  aes(x = elect_range_covers, 
      fill = THREATENED_STATUS) +
  geom_histogram(binwidth = 1) + 
  geom_vline(aes(xintercept = median(elect_range_covers)), 
             col = 'red',
             size = 0.6,
             linetype = "dashed") +
  scale_fill_viridis_d(direction = -1,
                       name = "Threatened status") +
  labs(x = "Electorates within species's range",
       y = "Number of threatened species") +
  theme_classic()

ggsave("figures/elect_spec_cover_status_histogram.png", elect.spec.cover.status.histogram)

# ggplot(elect.spec.cover.status, aes(elect_range_covers, fill = THREATENED_STATUS)) +
#   geom_histogram(binwidth = 1) +
#   scale_y_continuous(trans='log10')
# 
# geom_point(show.legend = FALSE) +
#   scale_colour_viridis_d() 
# labels = scales::comma
# 
# # elect.spec.cover.status <- 
# ggplot(elect.spec.cover.status) +
#   aes(x = elect_range_covers, y = THREATENED_STATUS, fill = elect_range_covers) +
#   geom_point(aes(group = THREATENED_STATUS))
#                
#  
#   #               , scale = "width", draw_quantiles = c(0.25, 0.5, 0.75)) +
#   # scale_fill_manual(values = c("#4CB04A83", "#E61C1C83", "#377FBA80")) +
#   # theme_set(theme_bw()) +
#   # theme(panel.grid = element_blank()) +
#   # coord_cartesian(xlim = c(-5e+06, 3e+07)) +
#   # labs(title = "Senate", 
#   #      x = "\nTotal amount party received ($)", y = "Agreement score\n")


#### Calculations ####

summary(elect.spec.cover.status)

prop.table(table(elect.spec.cover.status$elect_range_covers))
0.4499478624 + 0.1892596455 + 0.0740354536 + 0.0464025026

elect.spec.cover.status.4.greater.cover <-  elect.spec.cover.status %>% 
  filter(elect_range_covers > 4)

elect.spec.cover.status.4.less.cover <-  elect.spec.cover.status %>% 
  filter(elect_range_covers < 5)

table(elect.spec.cover.status$MIGRATORY_STATUS)
table(elect.spec.cover.status.4.greater.cover$MIGRATORY_STATUS)
table(elect.spec.cover.status.4.less.cover$MIGRATORY_STATUS)



table(elect.spec.cover.status.4.cover$MIGRATORY_STATUS)


