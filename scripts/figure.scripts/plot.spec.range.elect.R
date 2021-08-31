# plotting and mapping of spec.range.elect

#### spec.range.elect ####

tm_shape(spec.range.elect.eighty.aus, 
         bbox = st_bbox(c(xmin = 113, 
                          ymin = -43.740482,
                          xmax = 154, 
                          ymax = -9.219937), 
                        crs = st_crs(4283))) +
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

tmap_save(tm1, file = "plots/draft_spec.range.elect.eighty.png")



