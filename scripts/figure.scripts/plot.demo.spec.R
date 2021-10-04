# Mapping for demo.spec

#### Libraries ####

library(tidyverse)
library(sf)
library(tmap)
library(leaflet)
library(viridis)
library(grid)
library(cartogram)
library(rmapshaper)

#### Import and simplify data ####

demo.spec <- st_read(dsn = "analysed_data/HPC_spatial_ops_output/demo.spec.gpkg")
print(object.size(demo.spec), units = "Kb")

demo.spec <- ms_simplify(demo.spec,
                              keep = 0.1,
                              keep_shape = TRUE) 

demo.spec <- demo.spec %>% 
  st_make_valid() %>% 
  st_crop(xmin = 113, ymin = -43.740482, # drop those pesky islands
          xmax = 154, ymax = -9.219937)

print(object.size(demo.spec), units = "Kb")

st_geometry(demo.spec) <- NULL

#### Histogram ####

demo.spec$Demographic_class <- factor(
  demo.spec$Demographic_class, 
  levels = c("Inner Metropolitan", "Outer Metropolitan", 
             "Provincial", "Rural"))


demo.spec.col <- ggplot(demo.spec) +
  aes(x = Demographic_class, 
      y = total_unique_spec,
      fill = Demographic_class) +
  geom_col(show.legend = FALSE) +
  scale_fill_viridis_d(direction = -1) +
  coord_flip() +
  labs(x = "Demographic classification", 
       y = "Number of threatened species") +
  theme_classic() +
  theme(axis.title.y = element_blank())

ggsave("plots/demo_spec_col.png", demo.spec.col)


#### Mapping ####

# chloropleth.demo.spec <- 
tm_shape(demo.spec, 
         bbox = st_bbox(c(xmin = 113, 
                          ymin = -43.740482, 
                          xmax = 154, 
                          ymax = -9.219937), 
                        crs = st_crs(demo.spec))) +
  tm_fill("total_unique_spec", 
          style = "jenks", 
          title = "Number of threatened species",
          palette = "-plasma") + 
  tm_text("Demographic_class", size = "AREA") + 
  tm_borders(alpha = 0.3) +
  tm_compass(position = c("left", "bottom")) +
  tm_scale_bar(position = c("left", "bottom"), 
               width = 0.2) +
  tm_layout(frame = FALSE)
