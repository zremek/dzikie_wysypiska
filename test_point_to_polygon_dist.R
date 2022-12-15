Sys.setenv(LANGUAGE = "en")
source("plot_map.R")
# install.packages("sf")
library(sf)
library(tidyverse)
library(stringr)
library(ggmap)
library(osmdata)
library(units)

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

d_lon_lat <- d %>% select(longitude, latitude)

# mainst_dist.mat <- geosphere::dist2Line(p = d_lon_lat, line = main_streets)
# nie działa

class(main_streets$osm_polygons) # złe, ma być jak class(wrld_subset)

?dist2Line

# mainst_dist.mat <- geosphere::dist2Line(p = d_lon_lat, line = main_streets$osm_polygons)
# nie działa

# mainst_dist.mat <- geosphere::dist2Line(p = d_lon_lat, line = main_streets$osm_multipolygons)
# nie działa

# TODO trzeba przejść z sp na st 
