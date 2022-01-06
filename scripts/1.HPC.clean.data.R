# Processing and cleaning the data

#### Libraries ####

library(tidyverse)
library(sf)
library(jsonlite)
library(magrittr)
library(units)

# To get the right bounding box of the spatial data we needed to trim
# the annoying islands, this meant loading the data into QGIS, single part
# features, then selecting, exporting, then reading into sf, st_bbox,
# yet this didn't work so I spread the box by 1 in every direction
# for some unknown reason, the ymax of qgis bounding box
# measure was still cutting the top off of Aus
# xmin       ymin       xmax       ymax
# 112.921114 -43.740510 153.638727  -9.115517

#### Aus boundary: Import/clean ####

aus <- st_read("/QRISdata/Q4107/TS_electorates/raw_data/ASGS_Edition_3_Aust_2021_shapefile/AUS_2021_AUST_GDA94.shp")
aus <- aus %>%
        select(geometry) %>%
        slice(1) %>%
        st_crop(
                xmin = 112.921114, ymin = -43.740510, # drop those pesky islands
                xmax = 153.638727, ymax = -9.115517
        ) %T>%
        st_write(
                dsn = "/QRISdata/Q4107/TS_electorates/clean_data/aus.clean.gpkg",
                layer = "aus.clean", append = FALSE, delete_dsn = TRUE
        )

aus.union <- aus %>%
  st_union(by_feature = FALSE) %>%
  st_sf()

#### Electorates: Import/clean ####

elect <- st_read("/QRISdata/Q4107/TS_electorates/raw_data/AEC_electoral_boundaries_2019/COM_ELB_region.shp")
# The 'elect' file has a couple of contractions that do not match 'demo' file
elect$Elect_div <- gsub("Eden-monaro", "Eden-Monaro", elect$Elect_div)
elect$Elect_div <- gsub("Mcewen", "McEwen", elect$Elect_div)
elect$Elect_div <- gsub("Mcmahon", "McMahon", elect$Elect_div)
elect$Elect_div <- gsub("Mcpherson", "McPherson", elect$Elect_div)
elect$Elect_div <- gsub("O'connor", "O'Connor", elect$Elect_div)
elect <- elect %>%
        select(-c(
                "Numccds", "Actual", "Projected", "Area_SqKm",
                "Total_Popu", "Australian", "Sortname"
        )) %>%
        st_make_valid() %>%
        st_crop(
                xmin = 111.921114, ymin = -44.740510,
                xmax = 154.638727, ymax = -8.115517
                # remove the islands
        ) %>%
        mutate(
                elect_area_sqkm = units::set_units(st_area(.), km^2) %>%
                        as.numeric()
        ) %T>%
        st_write(
                dsn = "/QRISdata/Q4107/TS_electorates/clean_data/elect.clean.gpkg",
                layer = "aus.clean", append = FALSE, delete_dsn = TRUE
        )

elect.union <- elect %>%
        st_union(by_feature = FALSE) %>%
        st_sf() %T>%
        st_write(
                dsn = "/QRISdata/Q4107/TS_electorates/clean_data/elect.union.clean.gpkg",
                layer = "elect.union.clean", append = FALSE, delete_dsn = TRUE
        )

#### Species: Import/clean ####

species <- st_read("/QRISdata/Q4107/TS_electorates/raw_data/SNES_public_1july2021.gdb")
species <- species %>%
        filter(PRESENCE_RANK == 2) %>%
        filter(!is.na(THREATENED_STATUS)) %>%
        filter(THREATENED_STATUS %in% c(
                "Vulnerable",
                "Endangered",
                "Critically Endangered",
                "Extinct in the wild"
        )) %>% # as in wardNationalscaleDatasetThreats2021
        filter(!MARINE %in% "Listed") %>%
        filter(!SCIENTIFIC_NAME %in% c(
                "Brachionichthys hirsutus", # Spotted Handfish
                "Brachiopsilus ziebelli", # Ziebell's Handfish, Waterfall Bay Handfish
                "Carcharias taurus (east coast population)", # Grey Nurse Shark (east coast population)
                "Carcharias taurus (west coast population)", # Grey Nurse Shark (west coast population)
                "Carcharodon carcharias", # White Shark, Great White Shark
                "Epinephelus daemelii", # Black Rockcod, Black Cod, Saddled Rockcod
                "Glyphis garricki", # Northern River Shark, New Guinea River Shark
                "Glyphis glyphis", # Speartooth Shark
                "Pristis clavata", # Dwarf Sawfish, Queensland Sawfish
                "Rhincodon typus", # Whale Shark
                "Thymichthys politus", # Red Handfish
                "Zearaja maugeana" # Maugean Skate, Port Davey Skate
        )) %>%
        # some marine species were not being filtered out, remove all marine species, keep inland freshwater species
        filter(!SCIENTIFIC_NAME %in% c(
                "Calidris canutus", # Red Knot, Knot
                "Calidris ferruginea", # Curlew Sandpiper
                "Calidris tenuirostris", # Great Knot
                "Hirundapus caudacutus" # White-throated Needletail
        )) %>%
        # MARINE == "Listed - overfly marine area"
        filter(!CETACEAN %in% "Cetacean") %>%
        select(c(
                "SCIENTIFIC_NAME", "VERNACULAR_NAME", "THREATENED_STATUS",
                "MIGRATORY_STATUS", "TAXON_GROUP", "Shape"
        )) %>%
        st_make_valid() %>%
        # Merge species at the broad taxa as there are duplicate polygons
        # Probably attributable to subspecies populations, but still ¯\_(ツ)_/¯
        group_by(
                SCIENTIFIC_NAME, VERNACULAR_NAME, THREATENED_STATUS,
                MIGRATORY_STATUS, TAXON_GROUP
        ) %>%
        summarise() %>%
        ungroup() %>%
        st_make_valid() %>%
        mutate(
                species_area_sqkm = units::set_units(st_area(.), km^2) %>%
                        as.numeric()
        ) %T>%
        st_write(
                dsn = "/QRISdata/Q4107/TS_electorates/clean_data/species.clean.gpkg",
                layer = "species.clean", append = FALSE, delete_dsn = TRUE
        )
