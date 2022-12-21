Sys.setenv(LANGUAGE = "en")
source("plot_map.R")

library(sf)
library(tidyverse)
library(stringr)
library(ggmap)
library(osmdata)
library(units)
library(nngeo)


# czy drogi są poligonami? tak

main_streets$osm_polygons

# https://gis.stackexchange.com/questions/225102/calculate-distance-between-points-and-nearest-polygon-in-r 


# copy-paste from above

library(sp)
library(geosphere)
library(maptools)

# some country polygons to try on
data(wrld_simpl, package = "maptools")
wrld_subset <- wrld_simpl[wrld_simpl@data$ISO2 %in% c("RO","HU","AT","DE","FR"),]

# Generate random points (in and out)
set.seed(2017)
pts <- sp::makegrid(wrld_subset, n = 5)

# compute the shortest distance between points and polygons
# (from ?dist2Line): "returns matrix with distance and lon/lat of the nearest point" & 
# "the ID (index) of (one of) the nearest objects"; distance is in meters (default)
dist.mat <- geosphere::dist2Line(p = pts, line = wrld_subset)

# bind results with original points
pts.wit.dist <- cbind(pts, dist.mat)
pts.wit.dist[1:3,]

pts.sp <- sp::SpatialPoints(coords      = pts[,c("x1","x2")], # order matters
                            proj4string = wrld_subset@proj4string)
plot(pts.sp, col = "red")
plot(wrld_subset, add = TRUE)
# plot arrows to indicate the direction of the great-circle-distance
for (i in 1:nrow(pts.wit.dist)) {
  arrows(x0 = pts.wit.dist[i,1], 
         y0 = pts.wit.dist[i,2], 
         x1 = pts.wit.dist[i,4], 
         y1 = pts.wit.dist[i,5],
         length = 0.1,
         col = "green")
}

# on my data

d_lon_lat <- d %>% select(ec5_uuid, longitude, latitude) %>% 
  filter(!is.na(longitude))

d_4326 <- st_as_sf(x = d_lon_lat,
                        coords = c("longitude", "latitude"),
                        crs = st_crs(main_streets$osm_polygons))

# mainst_dist.mat <- geosphere::dist2Line(p = d_lon_lat, line = main_streets)
# nie działa

class(main_streets$osm_polygons) # złe, ma być jak class(wrld_subset)

?dist2Line

# mainst_dist.mat <- geosphere::dist2Line(p = d_lon_lat, line = main_streets$osm_polygons)
# nie działa

# mainst_dist.mat <- geosphere::dist2Line(p = d_lon_lat, line = main_streets$osm_multipolygons)
# nie działa

# trzeba przejść z sp na st 

?st_distance

st_crs(d_4326)
st_crs(main_streets$osm_polygons)

class(d_4326)
class(main_streets$osm_polygons)

dist_poly <- sf::st_distance(x = d_4326, y = main_streets$osm_polygons)

nrow(dist_poly) == nrow(d_4326) # policzyłem odległości, 

# ale dlaczego cztery zmienne? 
dim(dist_poly)

# odległość od każdej z dróg? bo są cztery wiersze w poligonach

dim(main_streets$osm_polygons)

ggplot() + 
  geom_sf(data = main_streets$osm_polygons, color = "red", size = 1) +
  geom_sf(data = main_streets$osm_lines, color = "darkgray") +
  coord_sf(datum = NA,
           xlim = c(19.33, 19.40), 
           ylim = c(51.77, 51.855))

# nie wiem co oznaczają te poligony
# może zatem linie? 

dist_line <- sf::st_distance(x = d_4326, y = main_streets$osm_lines)

dim(dist_line)
dim(main_streets$osm_lines)

dim(dist_line)[2] == dim(main_streets$osm_lines)[1]

main_streets$osm_lines[, 2] # nazwy ulic, powtarzają się

dist_nngeo_lines_ret <- nngeo::st_nn(d_4326, main_streets$osm_lines, returnDist = TRUE)
unlist(dist_nngeo_lines_ret$dist)
# ta funkcja zwraca po jednej wartości odległości dla najbliższego sąsiada

tail(dist_poly)

dist_poly_nngeo <- nngeo::st_nn(d_4326, main_streets$osm_polygons, returnDist = TRUE)

dist_poly_df <- data.frame(dist_poly)

dist_poly_df$row_minimum = apply(dist_poly_df[,-1], 1, min)

table(dist_poly_df$row_minimum == unlist(dist_poly_nngeo$dist))

connect_points_and_poly <- nngeo::st_connect(d_4326, main_streets$osm_polygons)

ggplot() + 
  geom_sf(data = main_streets$osm_polygons, color = "red", size = 1) +
  geom_sf(data = main_streets$osm_lines, color = "darkgray") +
  geom_sf(data = connect_points_and_poly, color = "blue") # działa pięknie!!!
 

###### dystansy do najbliższych na czysto ################

dist_main_streets <- nngeo::st_nn(d_4326, main_streets$osm_lines, returnDist = TRUE)
connect_to_main_streets <- nngeo::st_connect(d_4326, main_streets$osm_lines)

ggplot() + 
  geom_sf(data = d_4326, color = "red", size = 2) +
  geom_sf(data = main_streets$osm_lines, color = "darkgray") +
  geom_sf(data = connect_to_main_streets, color = "blue", alpha = 0.5) # rewelacja.

dist_streets <- nngeo::st_nn(d_4326, streets$osm_lines, returnDist = TRUE)
connect_to_streets <- nngeo::st_connect(d_4326, streets$osm_lines)

srodmiescie_bbox <- st_bbox(dzielnice_4326[4, ])

ggplot() + 
  geom_sf(data = main_streets$osm_lines, color = "darkgray") +
  geom_sf(data = streets$osm_lines, color = "gray") + 
  geom_sf(data = d_4326, color = "black", size = 2) +
  geom_sf(data = connect_to_main_streets, color = "blue", alpha = 0.5) +
  geom_sf(data = connect_to_streets, color = "red") +
  coord_sf(xlim = c(srodmiescie_bbox[[1]], srodmiescie_bbox[[3]]),
           ylim = c(srodmiescie_bbox[[2]], srodmiescie_bbox[[4]]))


##### badanie danych o odległościach od ulic ###############
dms <- unlist(dist_main_streets$dist)
ds <- unlist(dist_streets$dist)

hist(x = dms)
hist(x = ds)

summary(dms)
summary(ds)

table(dms < 1)
table(ds < 10)

plot(dms, ds)
cor(dms, ds)

save.image(file = "test_point_to_polygon_dist.RData")
# pomysł - policzyć odległości między każdym a każdym punktem
# zidentyfikować te najbliżej położone
# sprawdzić po id jakościowo czym się różnią
# czy ta sama osoba je dodała
# czy zdjęcia są podobne 



