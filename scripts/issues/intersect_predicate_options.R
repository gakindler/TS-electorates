library(tidyverse)
library(sf)

a <- st_polygon(list(rbind(c(-1, -1), c(1, -1), c(1, 1), c(-1, -1))))
b <- st_polygon(list(rbind(c(-1, -1), c(-1, 1), c(1, 1), c(-1, -1))))
c <- st_polygon(list(rbind(c(-0.5, -0.5), c(0.5, 0.5), c(-0.5, 0.5), c(-0.5, -0.5))))
poly <- st_as_sf(c(a, b, c))

plot(poly)


# create points
p_matrix = matrix(c(0.5, 1, -1, 0, 0, 1, 0.5, 1), ncol = 2)
p_multi = st_multipoint(x = p_matrix)
p = st_cast(st_sfc(p_multi), "POINT")