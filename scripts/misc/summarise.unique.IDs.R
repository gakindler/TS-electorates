# Checkng what happens when I dissolve along unique identifiers?

#### Libraries ####

library(tidyverse)
library(sf)
library(units)

#### Species: Import/clean ####

species <- st_read("raw_data/SNES_public_1july2021.gdb")
species <- species %>%
    filter(PRESENCE_RANK == 2) %>%
    filter(!is.na(THREATENED_STATUS)) %>%
    filter(THREATENED_STATUS %in% c(
        "Vulnerable",
        "Endangered",
        "Critically Endangered"
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
    st_set_geometry(NULL)

#### Species: Import/clean ####

species.dissolve <- st_read("raw_data/SNES_public_1july2021.gdb")
species.dissolve <- species.dissolve %>%
    filter(PRESENCE_RANK == 2) %>%
    filter(!is.na(THREATENED_STATUS)) %>%
    filter(THREATENED_STATUS %in% c(
        "Vulnerable",
        "Endangered",
        "Critically Endangered"
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
    st_set_geometry(NULL)

#### Discrepancy ####

species.duplicated <- species %>%
    group_by(
        SCIENTIFIC_NAME, VERNACULAR_NAME, THREATENED_STATUS,
        MIGRATORY_STATUS, TAXON_GROUP
    ) %>%
    filter(n() > 1) %T>%
    write.csv(
        "analysed_data/species.duplicated.csv",
        row.names = FALSE
    )
