# GIS StackExchange Q

library(sf)

# North Carolina - lets' pretend its an island...
nc = st_read(system.file("shape/nc.shp", package="sf"))

# Turn it into a rough metre-length system by projection
nc = st_transform(nc, 3857)

# scatter 1000 points around
pts = st_jitter(st_sample(nc, 1000), factor=0.2)

land = lengths(st_intersects(pts, nc)) > 0
ocean = !land

plot(nc$geom)
plot(pts[ocean,], pch=19, col="blue", add=TRUE)
plot(pts[land,], pch=19, col="green", add=TRUE)

plot(olinda)
plot(storms)

https://gis.stackexchange.com/questions/394954/r-using-st-intersects-to-classify-points-inside-outside-and-within-a-buffer 

s <- rbind(c(1, 1), c(10, 1), c(10, 10), c(1, 10), c(1, 1)) %>% 
  list %>% 
  st_polygon %>% 
  st_sfc

outside <- sapply(st_intersects(nc_point, sample_polygons),function(x){length(x)==0})

