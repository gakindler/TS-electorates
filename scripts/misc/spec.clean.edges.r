#### spec.cleaning ####

spec.within.logical <- species %>%
  st_is_within_distance(
    elect.union,
    dist = 10000,
    sparse = FALSE
  )

spec.within <- species[spec.within.logical, ]

st_write(spec.within,
  "analysed_data/HPC_spatial_ops_output/spec.within.gpkg",
  layer = "spec.within",
  append = FALSE, delete_dsn = TRUE
)

# Went into qgis and selected the species that were beyond the boundaries of the elects

species.edge.clean <- spec.within %>%
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
  ) %T>%
  st_write(
  "analysed_data/HPC_spatial_ops_output/species.edge.clean.gpkg",
  layer = "species.edge.clean",
  append = FALSE, delete_dsn = TRUE
)

species.elect.clip <- species.edge.clean %>%
  st_intersection(elect.union)
