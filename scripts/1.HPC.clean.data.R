# Processing and cleaning the data

#### Libraries ####

library(tidyverse)
library(sf)
library(jsonlite)
library(magrittr)
library(units)

# To get the right bounding box of the spatial data we needed to trim
# the annoying islands, this meant loading the data into QGIS, single part
# features, then selecting, exporting, then reading into sf, st_bbox

#### Aus boundary: Import/clean ####

aus <- st_read("/QRISdata/Q4107/raw_data/ASGS_Edition_3_Aust_2021_shapefile/AUS_2021_AUST_GDA94.shp")
aus <- aus %>%
        select(geometry) %>%
        slice(1) %>%
        st_crop(
                xmin = 112.921114, ymin = -43.740510, # drop those pesky islands
                xmax = 153.638727, ymax = -9.115517
        ) %T>%
        st_write(
                dsn = "/QRISdata/Q4107/clean_data/aus.clean.gpkg",
                layer = "aus.clean", append = FALSE
        )

#### Electorates: Import/clean ####

elect <- st_read("/QRISdata/Q4107/raw_data/AEC_electoral_boundaries_2019/COM_ELB_region.shp")
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
                xmin = 112.921114, ymin = -43.740510, # drop those pesky islands
                xmax = 153.638727, ymax = -9.115517
        ) %T>%
        st_write(
                dsn = "/QRISdata/Q4107/clean_data/elect.clean.gpkg",
                layer = "aus.clean", append = FALSE
        )

elect.union <- st_union(elect, by_feature = FALSE) %>%
        st_sf() %T>%
        st_write(
                dsn = "/QRISdata/Q4107/clean_data/elect.union.clean.gpkg",
                layer = "elect.union.clean", append = FALSE
        )

#### Species: Import/clean/crop ####

species <- st_read("/QRISdata/Q4107/raw_data/SNES_public_1july2021.gdb")
species <- species %>%
        filter(PRESENCE_RANK == 2) %>%
        filter(!is.na(THREATENED_STATUS)) %>%
        filter(THREATENED_STATUS %in% c(
                "Vulnerable",
                "Endangered",
                "Critically Endangered",
                "Extinct in the wild"
        )) %>% # as Ward_database_2021(?)
        filter(!MARINE %in% "Listed") %>%
        filter(!SCIENTIFIC_NAME %in% c(
                "Calidris canutus", "Calidris ferruginea",
                "Calidris tenuirostris", "Hirundapus caudacutus"
        )) %>%
        # 1 - Red Knot, Knot; 2 - Curlew Sandpiper;
        # 4 - Great Knot; 6 - White-throated Needletail
        # of the MARINE == "Listed - overfly marine area"
        filter(!CETACEAN %in% "Cetacean") %>%
        select(c(
                "SCIENTIFIC_NAME", "VERNACULAR_NAME", "THREATENED_STATUS",
                "MIGRATORY_STATUS", "TAXON_GROUP", "Shape_Area", "Shape"
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
        st_make_valid() %T>%
        st_write(
                dsn = "/QRISdata/Q4107/clean_data/species.clean.gpkg",
                layer = "species.clean", append = FALSE
        )