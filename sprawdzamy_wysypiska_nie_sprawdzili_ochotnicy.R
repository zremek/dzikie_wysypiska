Sys.setenv(LANGUAGE = "en")

library(sf)
library(tidyverse)
library(stringr)
library(ggmap)
library(osmdata)
library(units)
library(nngeo)
library(jsonlite)

###### dystansy pomiędzy wszystkimi punktami stan na 11. lipca 2023 ################

# load("distances_clead_geocoded.RData")

# pobieram ręcznie dzisiejszy json z e5 i wczytuję ####

l_spr <- jsonlite::fromJSON(txt = "json_data/2023_07_11_dzikie.json",
                            flatten = TRUE)

d_spr <- l_spr[["data"]]

glimpse(d_spr)

table(d_spr$`9_Czy_sprawdzasz_sta`, useNA = "always")

82 / 213 # 82 wpisy jako "sprawdzam - tak" 
# to prawie 40% wpisów pierwotnych tzn. zanim wprowadziłem możliwość sprawdzania
213 - 82 # pozostaje do sprawdzenia 131 wpisów 
# to dużo, może należałoby pozbyć się z tego punktów,
# które wpisano poza zasadnicznym terminem badań? 
# na razie zostawiam jak jest

# sklejam dzisiejszy json z geokodowaniem #### 

d_geocoded <- readRDS("d_clean_geocoded.rds")

summary(d_geocoded)

## ma być 307 obs z lat long, datą, eeuid i info czy sprawdzone

spr <- d_spr %>% select(ec5_uuid,
                        created_at,
                        `9_Czy_sprawdzasz_sta`,
                        `5_Tutaj_zlokalizowan.latitude`,
                        `5_Tutaj_zlokalizowan.longitude`)


spr <- spr %>% rename(
  ecuuid = ec5_uuid,
  czy_sprawdzasz = `9_Czy_sprawdzasz_sta`,
  latitude = `5_Tutaj_zlokalizowan.latitude`,
  longitude = `5_Tutaj_zlokalizowan.longitude`
)

glimpse(spr)
glimpse(d_geocoded %>% select("ecuuid", "created_at", "latitude", "longitude"))


spr <- spr %>% mutate(
  latitude = as.numeric(latitude), 
  longitude = as.numeric(longitude), 
  created_at = as.POSIXct(created_at)
)


joined_spr <- left_join(x = spr,
                        y = d_geocoded %>%
                          select("ecuuid",
                                 "latitude",
                                 "longitude"), 
                        by = "ecuuid")


summary(joined_spr)


table(is.na(joined_spr$latitude.x), 
      is.na(joined_spr$latitude.y)) # będzie jeden case do usunięcia

joined_spr <- joined_spr %>% mutate(
  latitude = ifelse(is.na(latitude.x),
                    yes = latitude.y, 
                    no = latitude.x), 
  longitude = ifelse(is.na(longitude.x), 
                     yes = longitude.y, 
                     no = longitude.x)
)


joined_spr %>% 
  filter(is.na(longitude)) %>% 
  View()

joined_spr_306 <- joined_spr %>% 
  filter(!is.na(longitude)) %>% 
  select(-longitude.x, -longitude.y, -latitude.x, -latitude.y)

  
glimpse(joined_spr_306)

# po sklejeniu mam 306 rekordów z lat long i liczę odległości ###### 

joined_spr_306_4326 <- st_as_sf(x = joined_spr_306,
                       coords = c("longitude", "latitude"), 
                       crs = 4326)

st_crs(joined_spr_306_4326) # 4326 czyli WGS 84

dist_spr_306 <- nngeo::st_nn(joined_spr_306_4326,
                             joined_spr_306_4326,
                             returnDist = TRUE)

unlist(dist_spr_306$dist) 
# wychodzą same zera czyli trzeba chyba dać więcej odstępu? 
# teraz każdy punkt wychodzi że jest najbliżej sam siebie... 

# użyję funkcji st_distance

dist_spr_306_d <- sf::st_distance(joined_spr_306_4326,
                                     joined_spr_306_4326,
                                     by_element = FALSE)

dist_spr_306_d %>% View() # policzyło się :) mam macież 

dim(dist_spr_306_d)

# jest OK ale jak teraz z tego skorzystać? 

# pomysły: zrobić z wide to long 
# może najpierw ograniczyć x do punktów które to niby są sprawdzeniem? 
# potem wykluczyć te, których sprawdzenie nie dotyczyło? 




