Sys.setenv(LANGUAGE = "en")
source("wykres_liczba_wpisow_dzikie.R")
# install.packages("sf")
library(sf)
library(tidyverse)
library(stringr)
library(ggmap)
library(osmdata)
library(units)

# ŁÓDŹ - Mapa dzielnic Łodzi - https://www.google.com/maps/d/viewer?mid=1JSrhdHx4LcvdLDDV-oJLiSg_iU4&usp=sharing 

# st_drivers()

# for human reading
# https://gis-support.pl/baza-wiedzy-2/dane-do-pobrania/granice-administracyjne/ 

# downloading
# download.file("https://www.gis-support.pl/downloads/Jednostki_ewidencyjne.zip",
#               "Jednostki_ewidencyjne.zip")
# 
# unzip("Jednostki_ewidencyjne.zip")

# shape 
shp <- sf::st_read(dsn = ".",layer = "Jednostki_ewidencyjne")

shp %>% pull(JPT_NAZWA_) %>% tibble() %>% rename(., nazwa = `.`) %>% 
  filter(str_detect(nazwa, "ŁÓDŹ"))

lodz_dzielnice <- shp %>% filter(str_detect(JPT_NAZWA_, "ŁÓDŹ"))

ggplot() + geom_sf(data = lodz_dzielnice, aes(fill = JPT_NAZWA_)) +
  labs(title = "Dziękuję Kochana Olu :)")

lodz_coord <- c(left = 19, bottom = 51.5, right = 20, top = 52)

# map_lodz_stanen <- get_stamenmap(lodz_coord,
#                                  zoom = 11,
#                                  maptype = "toner-lite")
# 
# map_lodz_osm <- get_openstreetmap(lodz_coord, scale = 171395)
# 
# ggmap(map_lodz_stanen) + 
#   geom_sf(data = lodz_dzielnice, aes(fill = JPT_NAZWA_)) +
#   labs(title = "Dziękuję Kochana Olu :)")
# inne odwzorowanie kuli i nie działa
# rezygnuję z pakietu ggmap
# TODO - odłączyć swoje konto google od map API 

# osm data
# https://rforjournalists.com/2020/12/15/how-to-access-open-street-map-in-r/ 

lodz_osm <- "Lodz" %>% opq()

#build different types of streets
main_st <- data.frame(type = c("motorway","trunk",
                               "primary","motorway_junction",
                               "trunk_link","primary_link","motorway_link"))
st <- data.frame(type = available_tags('highway'))
st <- subset(st, !type %in% main_st$type)
path <- data.frame(type = c("footway","path","steps","cycleway"))
st <- subset(st, !type %in% path$type)
st <- as.character(st$type)
main_st <- as.character(main_st$type)
path <- as.character(path$type)

#query OSM
main_streets <- lodz_osm %>%
  add_osm_feature(key = "highway", 
                  value = main_st) %>%
  osmdata_sf()

streets <- lodz_osm %>%
  add_osm_feature(key = "highway", 
                  value = st) %>%
  osmdata_sf() 

# x-axis is longitude = East-West, długość, południki, zerowy Greenwich
# y-axis is latitude = North-South, szerokość, równoleżniki, zerowy równik

d <- d %>% mutate(latitude = as.numeric(`5_Tutaj_zlokalizowan.latitude`),
                  longitude = as.numeric(`5_Tutaj_zlokalizowan.longitude`))

ggplot() + 
  geom_sf(data = main_streets$osm_lines, color = "darkgray", size = 0.5) + 
  geom_sf(data = streets$osm_lines, size = 0.25, color = "gray") +
  geom_sf(data = lodz_dzielnice, aes(fill = JPT_NAZWA_), 
          alpha = 0.5, colour = NA) +
  coord_sf(datum = NA,
           xlim = c(19.33, 19.63), 
           ylim = c(51.69, 51.855)) +
  geom_point(data = d, aes(x = longitude, y = latitude), 
             alpha = 0.3)
  labs(title = "Dziękuję Kochana Olu :)") +
  theme_void()

# count points in polygons

dzielnice_4326 <- st_transform(lodz_dzielnice %>% 
                                   select(JPT_NAZWA_, geometry), 4326) %>%
  mutate(dzielnica = str_remove(JPT_NAZWA_, "ŁÓDŹ-") %>% 
           stringi::stri_trans_totitle())

punkty_4326 <- st_as_sf(x = d %>% 
                          filter(!is.na(longitude)) %>% 
                          select(volunteer_id, longitude, latitude),
                 coords = c("longitude", "latitude"),
                 crs = st_crs(dzielnice_4326))


punkty_dzielnice <- st_intersection(x = dzielnice_4326, y = punkty_4326)

punkty_dzielnice %>% count(JPT_NAZWA_)

manual_fill <- c(Bałuty = "blue", 
                 Górna = "orange", 
                 Polesie = "purple", 
                 Śródmieście = "seagreen", 
                 Widzew = "red",
                 "gray")

ggplot() + 
  geom_sf(data = main_streets$osm_lines, color = "darkgray", size = 0.5) + 
  geom_sf(data = streets$osm_lines, size = 0.25, color = "gray") +
  geom_sf(data = dzielnice_4326, aes(fill = dzielnica), 
          alpha = 0.5, colour = NA) +
  geom_sf(data = punkty_4326, alpha = 0.5, size = 3) + 
  coord_sf(datum = NA,
           xlim = c(19.33, 19.63), 
           ylim = c(51.69, 51.855)) + 
  scale_fill_manual(values = manual_fill) +
  guides(fill = "none") +
  labs(title = "Lokalizacja dzikich wysypisk śmieci w Łodzi i okolicach", 
subtitle = 
  "Jeden punkt na mapie oznacza jeden wpis", 
caption = "Źródło: badania własne\ndzikiewysypiska.uni.lodz.pl")

ggsave(filename = paste(date_max, "_mapa_dzielnice.jpg", sep = ""),
       scale = 1, units = "in", width = 5, height = 5)


# wychodzi 163 punktów, a mam 167 wszystkie (bez 28.02. Anny) 
# 4 mam poza granicami dzielnic Łodzi 

# jak policzyć też autmatycznie punkty, które nie są w Łodzi?
# i jak zakomunikować ludziom, że 
## jest 178 wpisów, 167 mają lokalizację, 163 w mieście? 

# zrobić słupkowy flip z podziałem na dzielnice
# i dodać słup "brak współrzędnych wpisu", "punkt poza granicami m. Łodzi"

punkty_dzielnice %>% count(dzielnica) %>% tibble() %>% select(-geometry) %>% 
  add_case(dzielnica = c("brak współrzędnych wpisu",
                         "punkt poza granicami Łodzi"),
           n = c(11, 4)) %>% 
  mutate(dzielnica = fct_reorder(dzielnica, n, max)) %>% 
  ggplot(aes(x = dzielnica, 
             y = n, fill = dzielnica)) + 
  geom_col(alpha = 0.6) + 
  coord_flip() +
  scale_fill_manual(values = manual_fill) +
  guides(fill = "none") + 
  labs(title = "Liczba dzikich wysypisk śmieci\nw Łodzi według dzielnicy", 
       subtitle = paste0("Stan na ", date_max), 
       caption = "Źródło: badania własne\ndzikiewysypiska.uni.lodz.pl", 
       x = NULL,
       y = "liczba wpisów") + 
  theme_minimal()

ggsave(filename = paste(date_max, "_slupki_dzielnice.jpg", sep = ""),
       scale = 1, units = "in", width = 5, height = 3)

(dzielnice_4326 <- dzielnice_4326 %>% mutate(
  area = st_area(dzielnice_4326)))

st_join(punkty_dzielnice, dzielnice_4326) %>% count(dzielnica.x, area) %>% 
  mutate(`liczba wpisów przez powierzchnię dzielnicy` = n / area,
         dzielnica.x = fct_reorder(dzielnica.x, 
                                   `liczba wpisów przez powierzchnię dzielnicy`,
                                   max)) %>% 
  tibble() %>% select(-geometry) %>% 
  ggplot(aes(x = dzielnica.x, 
             y = `liczba wpisów przez powierzchnię dzielnicy`, 
             fill = dzielnica.x)) + 
  geom_col(alpha = 0.6) + 
  coord_flip() +
  scale_fill_manual(values = manual_fill) +
  guides(fill = "none") + 
  labs(title = "Liczba dzikich wysypisk śmieci w Łodzi,\nna metr kwadratowy powierzchni,\nwedług dzielnicy", 
       subtitle = paste0("Stan na ", date_max, ". Tylko wpisy w granicach Łodzi"), 
       caption = "Źródło: badania własne\ndzikiewysypiska.uni.lodz.pl", 
       x = NULL) + 
  theme_minimal()

ggsave(filename = paste(date_max, "_mkw_slupki_dzielnice.jpg", sep = ""),
       scale = 1, units = "in", width = 5, height = 3)
