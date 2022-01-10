# Comparing my results with previous (Michelle's and Carto map)


## Durack ##
new.durack <- join.intersect %>%
  filter(Elect_div == "Durack")
st_geometry(new.durack) <- NULL
new.durack <- new.durack %>%
  distinct(Elect_div, SCIENTIFIC_NAME, .keep_all = TRUE) %>%
  select(SCIENTIFIC_NAME, VERNACULAR_NAME)
old.durack <- read.csv("analysed_data/old_durack.csv")
old.durack <- rename(old.durack, SCIENTIFIC_NAME = Scientific.Name, VERNACULAR_NAME = Common.Name)
join.durack <- anti_join(new.durack, old.durack, by = "SCIENTIFIC_NAME")
