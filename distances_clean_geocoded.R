Sys.setenv(LANGUAGE = "en")
# source("plot_map.R")

library(sf)
library(tidyverse)
library(stringr)
library(ggmap)
library(osmdata)
library(units)
library(nngeo)

###### dystansy do main streets i streets ################

dgeoc <- read_rds("d_clean_geocoded.rds")

d_lon_lat <- dgeoc %>% select(ecuuid, longitude, latitude) %>% 
  filter(!is.na(longitude))

nrow(dgeoc) == nrow(d_lon_lat) # mamy lokalizacje dla wszystkich rekordów

d_4326 <- st_as_sf(x = d_lon_lat,
                   coords = c("longitude", "latitude"),
                   crs = st_crs(main_streets$osm_polygons))

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
  geom_sf(data = d_4326, color = "red", size = 2) +
  geom_sf(data = connect_to_main_streets, color = "blue", alpha = 0.5) +
  geom_sf(data = connect_to_streets, color = "green") +
  coord_sf(xlim = c(srodmiescie_bbox[[1]], srodmiescie_bbox[[3]]),
           ylim = c(srodmiescie_bbox[[2]], srodmiescie_bbox[[4]]))


## mamy punkty poza Łodzią i one mają źle policzone odległości
## bo nie mamy pobranych ulic spoza Łodzi

# pobieramy ulice z większego kwadratu, obejmującego wszystkie punkty ###########

opq(st_bbox(d_4326))

main_streets_2 <- opq(st_bbox(d_4326)) %>%
  add_osm_feature(key = "highway", 
                  value = main_st) %>%
  osmdata_sf()

streets_2 <- opq(st_bbox(d_4326)) %>%
  add_osm_feature(key = "highway", 
                  value = st) %>%
  osmdata_sf() 

# x-axis is longitude = East-West, długość, południki, zerowy Greenwich
# y-axis is latitude = North-South, szerokość, równoleżniki, zerowy równik

# odległości od ulic - większy bbox ###### 

dist_main_streets_2 <- nngeo::st_nn(d_4326, main_streets_2$osm_lines, returnDist = TRUE)
connect_to_main_streets_2 <- nngeo::st_connect(d_4326, main_streets_2$osm_lines)

ggplot() + 
  geom_sf(data = d_4326, color = "red", size = 2) +
  geom_sf(data = main_streets_2$osm_lines, color = "darkgray") +
  geom_sf(data = connect_to_main_streets_2, color = "blue", alpha = 0.5) # rewelacja.

dist_streets_2 <- nngeo::st_nn(d_4326, streets_2$osm_lines, returnDist = TRUE)

##### badanie danych o odległościach od ulic ###############
dms <- unlist(dist_main_streets$dist)
ds <- unlist(dist_streets$dist)
dms2 <- unlist(dist_main_streets_2$dist)
ds2 <- unlist(dist_streets_2$dist)

hist(x = dms)
hist(x = dms2)
hist(x = ds)

hist(x = ds2)

summary(dms)
summary(dms2)
summary(ds)
summary(ds2)

table(dms < 100)
table(dms2 < 100)
table(ds < 10)
table(ds2 < 10)

plot(dms, ds)
cor(dms, ds)

plot(dms2, ds2)
cor(dms2, ds2)

# pobieramy zieleń ###########

# tutaj pro artykuł jak to zrobić, do cytowania 2018 i 2019
# https://youtu.be/znE3LmdrihM?t=222

# teraz wersja szybka tags = {‘leisure’: ‘park’, ‘landuse’: ‘grass’}
# https://towardsdatascience.com/fetching-green-areas-from-osm-data-a6ff835c40dc 

park <- opq(st_bbox(d_4326)) %>%
  add_osm_feature(key = "leisure", 
                  value = "park") %>%
  osmdata_sf()

grass <- opq(st_bbox(d_4326)) %>%
  add_osm_feature(key = "landuse", 
                  value = "grass") %>%
  osmdata_sf() 

ggplot() + 
  geom_sf(data = main_streets_2$osm_lines, color = "darkgray") + 
  geom_sf(data = park$osm_polygons, fill = "darkgreen") + 
  geom_sf(data = grass$osm_polygons, fill = "green")

# odległości od zieleni ###### 

dist_park <- nngeo::st_nn(d_4326, park$osm_polygons, returnDist = TRUE)
connect_to_park <- nngeo::st_connect(d_4326, park$osm_polygons)

ggplot() + 
  geom_sf(data = d_4326, color = "red", size = 2) +
  geom_sf(data = park$osm_polygons, color = "darkgreen") +
  geom_sf(data = connect_to_park, color = "blue", alpha = 0.5) # rewelacja.

dist_grass <- nngeo::st_nn(d_4326, grass$osm_polygons, returnDist = TRUE)

dpark <- unlist(dist_park$dist)
dgrass <- unlist(dist_grass$dist)

# pobieramy landuse z OSM

# złoto: https://rspatialdata.github.io/osm.html 

landuse <- opq(st_bbox(d_4326)) %>%
  add_osm_feature(key = "landuse", 
                  value = osmdata::available_tags("landuse")) %>%
  osmdata_sf() 

d_landuse <- st_intersection(d_4326, landuse$osm_polygons)

table(d_landuse$landuse, useNA = "always")

# fajnie ale dlaczego nie mam wszystkich punktów? 

tibble(ecuuid = d_landuse$ecuuid) %>% count(ecuuid) # są 94 punkty
tibble(ecuuid = d_landuse$ecuuid) %>% count(ecuuid) %>% count(n)
# fajnie ale pewne punkty są więcej niż raz

# może te landuse nakładają się na siebie? 
# tak czy siak muszę dotrzeć do tych ecuuid, co nie ma w ramce

a = nrow(d_4326) # 208 wszystkich
b = nrow(tibble(ecuuid = d_landuse$ecuuid) %>% count(ecuuid)) # 94 mamy landuse (w tym NA)
c = nrow(d_4326 %>% filter(!ecuuid %in% d_landuse$ecuuid)) # 114 nie mamy 

a - b == c # good

d_landuse2 <- st_intersection(d_4326 %>% filter(!ecuuid %in% d_landuse$ecuuid),
                             landuse$osm_polygons)

nrow(d_landuse2) # zero, więc dalej dla 114 nie mamy 

tibble(ecuuid = d_landuse$ecuuid, 
       landuse = d_landuse$landuse) %>%
  count(landuse) %>% arrange(-n)

save.image("distances_clead_geocoded.RData")
# load("distances_clead_geocoded.RData")
