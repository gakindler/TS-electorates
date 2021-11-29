# Creating maps of Australia using spatial.ops data

# https://jforbes14.github.io/eechidna/articles/plotting-electorates.html

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

spec.per.elect <- st_read(dsn = "analysed_data/HPC_spatial_ops_output/spec.per.elect.gpkg")

print(object.size(spec.per.elect), units = "Kb")

spec.per.elect <- ms_simplify(spec.per.elect,
                              keep = 0.1,
                              keep_shape = TRUE) 

spec.per.elect <- spec.per.elect %>% 
  st_make_valid() %>% 
  st_crop(xmin = 113, ymin = -43.740482, # drop those pesky islands
          xmax = 154, ymax = -9.219937) %>% 
  mutate(Elect_div_abbrev = substr(Elect_div, start = 1, stop = 2))

print(object.size(spec.per.elect), units = "Kb")

st_geometry(spec.per.elect) <- NULL

#### tmap dorling cartogram ####
# https://geocompr.robinlovelace.net/adv-map.html
tm_shape(spec.per.elect, 
         bbox = st_bbox(c(xmin = 113, 
                          ymin = -43.740482,
                          xmax = 154, 
                          ymax = -9.219937), 
                        crs = st_crs(spec.per.elect))) +
  tm_polygons() +
  tm_symbols(col = "grey", size = "total_unique_spec")

tm_fill("total_unique_spec", 
        style = "jenks", 
        title = "Number of vulnerable specs",
        palette = "-viridis") + 
  tm_text("Elect_div", size = "AREA") + 
  tm_borders(alpha = 0.3) +
  tm_compass(position = c("left", "bottom")) +
  tm_scale_bar(position = c("left", "bottom"), 
               width = 0.2) +
  tm_layout(frame = FALSE)

#### ggplot dorling cartogram ####
# https://r-charts.com/spatial/cartogram-ggplot2/#dorling
# https://rpubs.com/frankhecker/434695
# https://rud.is/rpubs/hello-dorling.html

# Pass to dorling cartogram function
spec.per.elect.dorl <- st_transform(spec.per.elect, 3112) %>% 
  cartogram_dorling(weight = "total_unique_spec")

spec.per.elect.dorl <- ggplot(spec.per.elect.dorl) +
  geom_sf(
    aes(fill = total_unique_spec), 
    color = "grey50"
  ) +
  scale_fill_viridis(
    option = "C", 
    direction = -1,
    n.breaks = 10,
    guide_colorbar(
      barwidth = 50,
      barheight = 50,
      title = "Number of threatened species",
      title.position = "right",
      title.vjust = 0.1,
      ticks = FALSE)
  ) +
  geom_sf_text(
    aes(label = Elect_div, size = total_unique_spec), 
    show.legend = FALSE
  ) +
  theme_void()

ggsave("plots/spec_per_elect_dorl.png", spec.per.elect.dorl)




theme(legend.position = "top",
      legend.margin = margin(t = 5, b = 0),
      legend.title = element_text(size = 7),
      legend.text = element_text(angle = 45,
                                 margin = margin(t = 5)))




ggplot(spec.per.elect.dorl) +
  geom_sf(
    aes(fill = total_unique_spec), 
    color = "grey50") +
  # geom_sf_text(aes(label = Elect_div),
  #              check_overlap = TRUE) +
  scale_fill_viridis(
    direction = -1,
    n.breaks = 10,
    guide_colorbar(
      barwidth = 50,
      barheight = 50,
      title = "Number of vulnerable species",
      title.position = "right",
      title.vjust = 0.1,
      ticks = FALSE)) +
  theme_void() +
  theme(legend.position = "top",
        legend.margin = margin(t = 5, b = 0),
        legend.title = element_text(size = 7),
        legend.text = element_text(angle = 45,
                                   margin = margin(t = 5)))



ggsave("spec.per.elect.plot.png", spec.per.elect.plot)

#### ggplot proportional symbol map ####

elect_centroid <- st_centroid(spec.per.elect, of_largest_polygon = TRUE)

ggplot() +
  geom_sf(data = spec.per.elect, fill = "grey95") +
  geom_sf(data = elect_centroid, 
          aes(fill = total_unique_spec)) +
  geom_sf(data = elect_centroid, 
          aes(size = total_unique_spec), 
          show.legend = FALSE) +
  scale_fill_viridis(direction = -1) +
  # scale_size(range = c(1, 9),
  #            guide = guide_legend()) +
  theme_void() +
  theme(legend.position = "top")

#### ggplot non-contiguous cartogram ####

#### other ####

# Non-continental Australia
# Norfolk
tm_shape(spec.per.elect.aus, 
         bbox = st_bbox(c(xmin = 167.863479, 
                          ymin = -29.164764,
                          xmax = 168.042694, 
                          ymax = -28.964304), 
                        crs = st_crs(4283))) +
  tm_fill("total_unique_spec", 
          style = "jenks", 
          title = "Number of vulnerable specs",
          palette = "-viridis") + 
  tm_text("Elect_div", size = "AREA") + 
  tm_borders(alpha = 0.3) +
  tm_compass(position = c("left", "bottom")) +
  tm_scale_bar(position = c("left", "bottom")) +
  tm_layout(frame = FALSE)

# Lord Howe
tm_shape(spec.per.elect.aus, 
         bbox = st_bbox(c(xmin = 159.010215, 
                          ymin = -31.618085,
                          xmax = 159.133983, 
                          ymax = -31.469301), 
                        crs = st_crs(4283))) +
  tm_fill("total_unique_spec", 
          style = "jenks", 
          title = "Number of vulnerable specs",
          palette = "-viridis") + 
  tm_text("Elect_div", size = "AREA") + 
  tm_borders(alpha = 0.3) +
  tm_compass(position = c("left", "bottom")) +
  tm_scale_bar(position = c("left", "bottom")) +
  tm_layout(frame = FALSE)

# Christmas Island
tm_shape(spec.per.elect.aus, 
         bbox = st_bbox(c(xmin = 105.470347, 
                          ymin = -10.618522,
                          xmax = 105.773844, 
                          ymax = -10.35723), 
                        crs = st_crs(4283))) +
  tm_fill("total_unique_spec", 
          style = "jenks", 
          title = "Number of vulnerable specs",
          palette = "-viridis") + 
  tm_text("Elect_div", size = "AREA") + 
  tm_borders(alpha = 0.3) +
  tm_compass(position = c("left", "bottom")) +
  tm_scale_bar(position = c("left", "bottom")) +
  tm_layout(frame = FALSE)

# Cocos islands
tm_shape(spec.per.elect.aus, 
         bbox = st_bbox(c(xmin = 96.736216, 
                          ymin = -12.265961, 
                          xmax = 96.986155, 
                          ymax = -11.781081), 
                        crs = st_crs(4283))) +
  tm_fill("total_unique_spec", 
          style = "jenks", 
          title = "Number of vulnerable specs",
          palette = "-viridis") + 
  tm_text("Elect_div", size = "AREA") + 
  tm_borders(alpha = 0.3) +
  tm_compass(position = c("left", "bottom")) +
  tm_scale_bar(position = c("left", "bottom")) +
  tm_layout(frame = FALSE, legend.position = center)



