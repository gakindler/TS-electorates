# Mapping for demo.spec

#### Libraries ####

library(tidyverse)
library(sf)
library(viridis)
library(grid)
library(cartogram)
library(httpgd)
library(jsonlite)

#### Import ####

print(object.size(spec.per.elect), units = "Kb")

#### Correlation plot ####

spec.per.elect.point <- ggplot(spec.per.elect) +
  aes(x = elect_area_sqkm,
     y = total_unique_spec,
     fill = Elect_div,
     size = Electors) +
  geom_point(aes(colour = Demographic_class),
     alpha = 0.6,
     show.legend = c(size = FALSE,
          fill = FALSE)) +
  geom_smooth(se = FALSE,
              fullrange = TRUE,
              show.legend = FALSE) +
  scale_size(range = c(.5, 10)) +
  scale_colour_viridis(discrete = TRUE,
     option = "H") +
  scale_x_continuous(trans='log10') +
  # annotation_logticks(sides = "b") +
  labs(x = bquote("Electorate area"~(km^2)),
     y = "Number of threatened species") +
  guides(colour = guide_legend(title = "Demographic class", 
                               override.aes = list(size=5))) +
  theme_classic()

# Same but with smooth line
spec.per.elect.point.smooth <- ggplot(spec.per.elect) +
  aes(x = elect_area_sqkm,
      y = total_unique_spec) +
  geom_point(aes(colour = Demographic_class),
             alpha = 0.6,
             show.legend = c(size = FALSE,
                             fill = FALSE),
             size = 3) +
  geom_smooth(method = "loess",
              show.legend = FALSE,
              colour = "black") +
  scale_colour_viridis(discrete = TRUE,
                       option = "H") +
  scale_x_continuous(trans='log10', 
                     labels = scales::comma) +
  scale_shape_manual(values = c(1, 2)) +
  # annotation_logticks(sides = "b") +
  labs(x = bquote("Electorate area"~(km^2)),
       y = "Number of threatened species") +
  guides(colour = guide_legend(title = "Demographic class",
                               override.aes = list(size = 3))) +
  theme_classic()

ggsave("figures/spec_per_elect_point.png", spec.per.elect.point)
ggsave("figures/spec_per_elect_point_smooth.png", spec.per.elect.point.smooth)

#### Calcualtions ####

summary(spec.per.elect)

prop.table(table(spec.per.elect$Demographic_class))

spec.per.elect %>%
  group_by(Demographic_class) %>%
  summarise(sum(total_unique_spec))

spec.per.elect %>%
  summarise(sum(total_unique_spec))
