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

#### Import & clean: Aus ####

aus <- st_read("/QRISdata/Q4107/TS_electorates/raw_data/ASGS_Edition_3_Aust_2021_shapefile/AUS_2021_AUST_GDA94.shp")

aus.clean <- aus %>%
    select(geometry) %>%
    slice(1) %>%
    st_crop(
        xmin = 112.921114, ymin = -43.740510,
        xmax = 153.638727, ymax = -9.115517
        # remove external islands
    ) %T>%
    st_write(
        dsn = "/QRISdata/Q4107/TS_electorates/clean_data/aus.clean.gpkg",
        layer = "aus.clean", append = FALSE, delete_dsn = TRUE
    )

aus.union.clean <- aus.clean %>%
    st_union(by_feature = FALSE) %>%
    st_sf() %>%
    st_make_valid() %T>%
    st_write(
        dsn = "/QRISdata/Q4107/TS_electorates/clean_data/aus.union.clean.gpkg",
        layer = "aus.union.clean", append = FALSE, delete_dsn = TRUE
    )

#### Import & clean: Electorates ####

elect <- st_read("/QRISdata/Q4107/TS_electorates/raw_data/2021-Cwlth_electoral_boundaries_ESRI/2021_ELB_region.shp")

elect.clean <- elect %>%
    select(
        Elect_div, geometry
    ) %>%
    rename(
        electorate = Elect_div
    ) %>%
    st_make_valid() %>%
    st_crop(
        xmin = 111.921114, ymin = -44.740510,
        xmax = 154.638727, ymax = -8.115517
        # remove the islands
    ) %>%
    mutate(
        electorate_area_sqkm = units::set_units(st_area(.), km^2) %>%
            as.numeric()
    ) %>%
    mutate(
        across(electorate_area_sqkm, signif, digits = 3)
    ) %>%
    relocate(
        electorate,
        electorate_area_sqkm,
        geometry
    ) %T>%
    st_write(
        dsn = "/QRISdata/Q4107/TS_electorates/clean_data/elect.clean.gpkg",
        layer = "elect.clean", append = FALSE, delete_dsn = TRUE
    )

elect.union.clean <- elect.clean %>%
    st_union(by_feature = FALSE) %>%
    st_sf() %>%
    st_make_valid() %T>%
    st_write(
        dsn = "/QRISdata/Q4107/TS_electorates/clean_data/elect.union.clean.gpkg",
        layer = "elect.union.clean", append = FALSE, delete_dsn = TRUE
    )

#### Import & clean: Species ####

species <- st_read("/QRISdata/Q4107/TS_electorates/raw_data/SNES_public_1july2021.gdb")

species.clean.first <- species %>%
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
    rename(
        scientific_name = SCIENTIFIC_NAME,
        vernacular_name = VERNACULAR_NAME,
        threatened_status = THREATENED_STATUS,
        migratory_status = MIGRATORY_STATUS,
        taxon_group = TAXON_GROUP,
        geometry = Shape
    ) %>%
    st_make_valid() %>%
    # Merge species at the broad taxa as there are duplicate polygons
    # Likely attributable to subspecies populations
    group_by(
        scientific_name,
        vernacular_name,
        threatened_status,
        migratory_status,
        taxon_group
    ) %>%
    summarise() %>%
    ungroup() %>%
    st_make_valid()

species.clean.unclipped <- species.clean.first %>%
    mutate(
        species_range_area_sqkm = units::set_units(st_area(.), km^2) %>%
            as.numeric()
    ) %>%
    mutate(
        across(species_range_area_sqkm, signif, digits = 3)
    ) %T>%
    st_write(
        dsn = "/QRISdata/Q4107/TS_electorates/clean_data/species.clean.unclipped.gpkg",
        layer = "species.clean.unclipped", append = FALSE, delete_dsn = TRUE
    )

species.clean.coastal <- species.clean.first %>%
    filter(
        !scientific_name %in% c(
            "Fregetta grallaria grallaria",
            "Diomedea antipodensis gibsoni",
            "Sternula nereis nereis",
            "Thalassarche bulleri platei",
            "Pachyptila turtur subantarctica",
            "Pristis pristis",
            "Pristis zijsron",
            "Neophema chrysogaster",
            "Pterodroma heraldica"
        )
    ) %>%
    st_intersection(
        elect.union.clean
    ) %>%
    st_make_valid() %>%
    mutate(
        species_range_area_sqkm = units::set_units(st_area(.), km^2) %>%
            as.numeric()
    )

species.clean.coastal.not <- species.clean.first %>%
    filter(
        scientific_name %in% c(
            "Fregetta grallaria grallaria",
            "Diomedea antipodensis gibsoni",
            "Sternula nereis nereis",
            "Thalassarche bulleri platei",
            "Pachyptila turtur subantarctica",
            "Pristis pristis",
            "Pristis zijsron",
            "Neophema chrysogaster",
            "Pterodroma heraldica"
        )
    ) %>%
    mutate(
        species_range_area_sqkm = units::set_units(st_area(.), km^2) %>%
            as.numeric()
    )

species.clean <- species.clean.coastal %>%
    mutate(
        across(species_range_area_sqkm, signif, digits = 3)
    ) %>%
    bind_rows(species.clean.coastal.not) %>%
    relocate(
        scientific_name, vernacular_name,
        threatened_status, migratory_status,
        taxon_group, species_range_area_sqkm, geometry
    ) %T>%
    st_write(
        dsn = "/QRISdata/Q4107/TS_electorates/clean_data/species.clean.gpkg",
        layer = "species.clean", append = FALSE, delete_dsn = TRUE
    )

#### Import & clean: Demography ####

demo <- readxl::read_xlsx(
    "/QRISdata/Q4107/TS_electorates/raw_data/demographic-classification-as-at-2-august-2021.xlsx"
)

demo.clean <- demo %>%
    rename(
        state_territory = "State or territory",
        demographic_class = "Demographic classification",
        electorate = "Electoral division"
    ) %>%
    mutate(
        state_territory = replace(
            state_territory, state_territory == "ACT", "Australian Capital Territory"
        )
    ) %>%
    mutate(
        state_territory = replace(
            state_territory, state_territory == "NT", "Northern Territory"
        )
    ) %>%
    mutate(
        state_territory_abbrev = case_when(
            state_territory == "Australian Capital Territory" ~ "ACT",
            state_territory == "New South Wales" ~ "NSW",
            state_territory == "Northern Territory" ~ "NT",
            state_territory == "Queensland" ~ "QLD",
            state_territory == "South Australia" ~ "SA",
            state_territory == "Tasmania" ~ "TAS",
            state_territory == "Victoria" ~ "VIC",
            state_territory == "Western Australia" ~ "WA"
        )
    ) %>%
    mutate(
        demographic_class = str_to_sentence(demographic_class)
    ) %>%
    mutate(
        electorate_abbrev = abbreviate(
            electorate,
            minlength = 2L
        )
    ) %>%
    mutate(
        electorate_abbrev = replace(
            electorate_abbrev, electorate_abbrev == "E-", "E-M"
        )
    ) %>%
    mutate(
        electorate_abbrev = replace(
            electorate_abbrev, electorate_abbrev == "O'", "O'C"
        )
    ) %>%
    relocate(
        electorate_abbrev,
        .after = electorate
    ) %T>%
    write.csv(
        "/QRISdata/Q4107/TS_electorates/clean_data/demo.clean.csv",
        row.names = FALSE
    )

#### Import & clean: States ####

# TODO: wrong! Let's just use standard state boundaries
# state.clean <- elect.clean %>%
#   full_join(demo.clean) %>%
#   group_by(
#     state_territory, state_territory_abbrev
#   ) %>%
#   summarise() %T>%
#   st_write(
#     dsn = "/QRISdata/Q4107/TS_electorates/clean_data/state.clean.gpkg",
#     layer = "state.clean", append = FALSE, delete_dsn = TRUE
#   )