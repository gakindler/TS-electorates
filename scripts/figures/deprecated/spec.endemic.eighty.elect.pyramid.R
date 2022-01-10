# Mapping for demo.spec
# https://ggplot2.tidyverse.org/reference/facet_grid.html

#### Libraries ####

library(tidyverse)
library(viridis)
library(grid)
library(httpgd)

#### Import ####

spec.endemic.eighty.elect <- read.csv(
  "analysed_data/21-12-06_local_analysis_output/spec.endemic.eighty.elect.csv"
)

#### Pyramid plot ####

# spec.endemic.eighty.elect.pyramid <-
ggplot(spec.endemic.eighty.elect) +
  aes(
    x = Elect_div,
    y = total_endemic_unique_spec
  ) +
  geom_bar(
    stat = "identity"
  ) +
  coord_flip()

# spec.endemic.eighty.elect.pyramid <-
ggplot(spec.endemic.eighty.elect) +
  aes(
    x = Elect_div,
    y = total_eighty_unique_spec
  ) +
  geom_bar(
    stat = "identity"
  ) +
  coord_flip()
