library(sp)
library(geosphere)

# 51.77353, 19.466746 z sierpnia
# 51.77361, 19.466826 z września

j_1908_lon <- 51.77353
j_1908_lat <- 19.466746
j_2709_lon <- 51.77361
j_2709_lat <- 19.466826

# https://stackoverflow.com/questions/32363998/function-to-calculate-geospatial-distance-between-two-points-lat-long-using-r 

sp::spDistsN1(pts = matrix(c(j_1908_lat, j_1908_lon), ncol = 2),
              pt = c(j_2709_lat, j_2709_lon), longlat = FALSE) * 1000

sp::spDistsN1(pts = matrix(c(j_1908_lat, j_1908_lon), ncol = 2),
              pt = c(j_2709_lat, j_2709_lon), longlat = TRUE) * 1000

geosphere::distm(c(j_1908_lat, j_1908_lon),
                c(j_2709_lat, j_2709_lon), fun = distGeo)

geosphere::distm(c(j_1908_lat, j_1908_lon),
                c(j_2709_lat, j_2709_lon), fun = distCosine)

geosphere::distm(c(j_1908_lat, j_1908_lon),
                 c(j_2709_lat, j_2709_lon), fun = distHaversine)

geosphere::distm(c(j_1908_lat, j_1908_lon),
                c(j_2709_lat, j_2709_lon), fun = distVincentySphere)

geosphere::distm(c(j_1908_lat, j_1908_lon),
                c(j_2709_lat, j_2709_lon), fun = distVincentyEllipsoid)

# TODO - w jaki sposób mam zakodowane punkty: crs 4326? EPSG:4326 WGS 84?
# https://epsg.io/4326 to jeszcze nie jest dla mnie jasne

# z E5 https://docs.epicollect.net/formbuilder/location-questions#mobile-app 
# latitude and longitude in 
# signed degrees format, with 6 decimal places 
# to pinpoint a location within 11cm.

# accuracy in meters, 
# Meters, refers to how close the device's calculated position is
# from the truth, expressed as a radius. Consumer smartphones devices can get
# a maximum accuracy of 3 or 4 meters.

# i jakiego sposobu trzeba użyć na liczenie tej odległości? 

# najlepszy sposób: haversine #### 

geosphere::distm(c(j_1908_lat, j_1908_lon),
                 c(j_2709_lat, j_2709_lon), fun = distHaversine)

# The shortest distance between two points (i.e., the 'great-circle-distance'
# or 'as the crow flies'), according to the 'haversine method'. This method
# assumes a spherical earth, ignoring ellipsoidal effects.

distHaversine(c(j_1908_lat, j_1908_lon),
              c(j_2709_lat, j_2709_lon))
