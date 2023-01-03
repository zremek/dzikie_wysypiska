library(osmdata)
library(tidyverse)
library(sf)
Sys.setenv(LANGUAGE = "en")

# geocodin with osm data - osmdata::getbb()

?osmdata::getbb

load("test_point_to_polygon_dist.RData")

d_clean <- read_rds("d_clean.rds")

no_loc <- 
  d_clean %>% filter(is.na(latitude)) %>%
  select(ecuuid, lct_comment, place_comment, volunteer_id, starts_with("phot"))

osmdata::getbb(place_name = d_clean %>%
                 filter(ecuuid == "af27cbb2-bf45-4cb8-96d1-ba48755b3e7f") %>% 
                 select(lct_comment))

osmdata::getbb(place_name = d_clean %>% 
                 filter(ecuuid == "79c11420-aafc-11ec-ad03-cd8cdd6e83f9") %>% 
                 select(lct_comment))
# daje to granice bbox - wszystkie NA
# może ręcznie przepisać nazwę? 

d_clean %>% 
  filter(ecuuid == "79c11420-aafc-11ec-ad03-cd8cdd6e83f9") %>% 
  select(lct_comment)

s5 <- "Senatorska 5, Lodz, Poland"

bbs5 <- getbb(s5, featuretype = "street")

sfbbs5 <- bbs5 %>% t() %>% data.frame() %>% st_as_sf(coords = c("x", "y"), 
                                                     crs = st_crs(main_streets$osm_lines))

ggplot() + 
  geom_sf(data = streets$osm_lines, color = "darkgray") +
  geom_sf(data = sfbbs5, color = "red", size = 1) +
  coord_sf(datum = NA,
           xlim = c(19.4681, 19.4684), 
           ylim = c(51.7461, 51.7465))

# to będzie ręczne geocoding case by case
geocoded <- tibble(ecuuid = NA, 
                   latitude = NA, 
                   longitude = NA)

# lat long reminder ######
# latitude = y axis = szerokość geograficzna = North-South (Łódź około 51)
## latitide zazwyczaj podawane jest jako pierwsze np. w mapach google
## szerokość wyznaczają równoleżniki, zerowy to równik
# longitude = x axis = długość geograficzna = East-West (Łódź około 19)
## longitude zazwyczaj podawana jako druga
## długość wyznaczają południki, zerowy to Greenwich

# case 1. Warszawska 77 ###########

browseURL(pull(no_loc[1, 4]))
# zdjęcia nie przesłane na serwer więc biorę koordynaty adresu z google maps
# https://goo.gl/maps/38943RNpcdbBPH457 
# 51.80858922841398, 19.475885229161815
# z dokładnością do sześciu cyfr dla ujednolicenia 
# 51.808589, 19.475885

geocoded <- 
  add_case(geocoded, 
           ecuuid = pull(no_loc[1, 1]), 
           latitude = 51.808589, 
           longitude = 19.475885)

# case 2. Zacna, koordynaty w komentarzu ######## 
# 51.704266,19.470457 

geocoded <- 
  add_case(geocoded, 
           ecuuid = pull(no_loc[2, 1]), 
           latitude = 51.704266, 
           longitude = 19.470457)

# case 3. Spokojna, koordynaty w komentarzu ####### 
# 51.705055,19.469440 

geocoded <- 
  add_case(geocoded, 
           ecuuid = pull(no_loc[3, 1]), 
           latitude = 51.705055, 
           longitude = 19.469440)

# case 4. na chodniku na Narutowicza 93 i 95 ###### 

browseURL(pull(no_loc[4, 7]))

# na zdjęciach śmieci przy płocie do pustej posesji na rogu Narutowicza / Solskiego
# leżą od strony chodnika. 
# rozpoznaję drzewo z widoku satelity google street view i wybieram punkt 
# https://goo.gl/maps/3gQXNTJBbQXd2DCP8
# 51.773116, 19.485332 

geocoded <- 
  add_case(geocoded, 
           ecuuid = pull(no_loc[4, 1]), 
           latitude = 51.773116, 
           longitude = 19.485332)

# case 5. Grabowa nr 23-27 

browseURL(pull(no_loc[5, 7]))

# pusta posesja, mur i betonowy parkan widoczne na zdjęciach
# https://goo.gl/maps/FJmiXUtGWk9kUC4A7
# 51.745875, 19.473201 
# na google street view także widoczne śmieci na posesji  

geocoded <- add_case(
  geocoded, 
  ecuuid = pull(no_loc[5, 1]), 
  latitude = 51.745875, 
  longitude = 19.473201
)

# case 6. Tymienieckiego 15. anna_krzynowek #######
# anna_krzynowek ma wszystkie swoje wpisy bez lokalizacji
# komentarz w place_comment, nie w lct comment 

d_clean %>% filter(volunteer_id == "anna_krzynowek") %>% 
  select(volunteer_id, latitude)

browseURL(pull(no_loc[6, 7]))

pull(no_loc[6, 3])

# na podstawie zdjęć punkt na parkingu z widocznością na
# tabliczkę z nazwą parku Kilińskiego 
# https://goo.gl/maps/WeSrXaH8JGPQdKL58
# 51.752090, 19.475652 

geocoded <- 
  add_case(
    geocoded, 
    ecuuid = pull(no_loc[6, 1]), 
    latitude = 51.752090, 
    longitude = 19.475652
  )

# case 7. Senatorska 5 #############

# w przypadku Senatorskiej zdjęcia są przy parkanie betonowymi koło ściany
# na google street view widzę podobny parkan nieco dalej od rogu Brzozowej,
# w kierunku Sosnowej (na zachód), czyli 
# właściwie lewy, górny róg mojego bboxa Senatorska 5
# ostatecznie pobieram koordynaty z google maps 51.746524, 19.468599
# https://www.google.com/maps/place/51%C2%B044'47.5%22N+19%C2%B028'07.0%22E/@51.7465248,19.4680518,150m/data=!3m2!1e3!4b1!4m6!3m5!1s0x0:0x17edc9d7200f2f6c!7e2!8m2!3d51.7465244!4d19.4685988 
# jak w opisie - pusta działka otoczona kamienicami



geocoded <- 
  add_case(geocoded,
         ecuuid = pull(no_loc[7, 1]), 
         latitude = 51.746524, 
         longitude = 19.468599)


# case 8. plac Reymonta, dawne kino Rekord

browseURL(pull(no_loc[8, 5]))

# http://www.polskaniezwykla.pl/web/place/47181,lodz-budynek-dawnego-kina-rekord.html
# https://literacka.lodz.pl/tropem-fabuly/punkty/info/180 
# z goole maps na Rzgowskiej 2 
# (budynek jest na street view i mapie, starałem się wybrać punkt jak na zdjęciu) 
# 51.742224857959954, 19.46352411862308 
# 51.742224, 19.463524

geocoded <- add_case(
  geocoded, 
  ecuuid = pull(no_loc[8, 1]), 
  latitude = 51.742224, 
  longitude = 19.463524
)

# case 9. ul. Dąbrowskiego, teren dawnej zajezdni MPK #######

browseURL(pull(no_loc[9, 6]))

# kierując się zdjęciem z rurami wybieram punkt blisko działek
# https://goo.gl/maps/sF4GH9UgdwGXgGtu8 
# 51.739290, 19.475741

geocoded <- 
  add_case(
    geocoded, 
    ecuuid = pull(no_loc[9, 1]),
    latitude = 51.739290, 
    longitude = 19.475741
  )

# case 10. darkerone bez komentarzy ##########

browseURL(pull(no_loc[10, 5]))

# brak wskazówek w jednynym zdjęciu 

d_clean %>% filter(volunteer_id == "darkerone") %>% View()
# to jest pierwszy wpis, ma inne z lokalizacją 
# sprawdzam je w google maps

dko <- d_clean %>% filter(volunteer_id == "darkerone")
sapply(dko$photo_4, browseURL)

# żadne zdjęcie nie powtarza się z tym pierwszym, 
# ale są podobne co do roślinności 
# zakładam, że lokalizacja jest podobna do 51.77864 19.50390
# czyli 1b588313-980f-48a9-abda-846a5df9a3ce
# ponieważ zdjęcia wpisy zostały przesłane w odstępie 

f <- "214b73fc-99cf-48a5-a64b-f56b9a240557"
s <- "1b588313-980f-48a9-abda-846a5df9a3ce"

ft <- d_clean %>% filter(ecuuid == f) %>% select(uploaded_at) %>% pull()
st <- d_clean %>% filter(ecuuid == s) %>% select(uploaded_at) %>% pull()

difftime(st, ft)

# w odstępie niecałych trzech minut 
# jednak się waham, bo na street view nie widać budynków jak na zdjęciu f
# miejsce s jest przy torach tramwajowych
# poszukałem miejsca ze zdjęcia f na street view pozostałych miejsc darkerone
# nie widzę żadnej wskazówki
# to jakieś inne miejsce w okolicy
# czy iść w teren? 

# wizyta w terenie 29.12. wskazuje na okolicę
# 51.779663, 19.505233

geocoded <- add_case(
  geocoded, 
  ecuuid = pull(no_loc[10, 1]),
  latitude = 51.779663, 
  longitude = 19.505233
)

# case 11. przy dawnym Tesco na ulicy Franciszkańsk ######

browseURL(pull(no_loc[11, 6]))

# skwerek przy Franciszkańskiej 31A
# https://goo.gl/maps/BScWb6LZ4HnSn1V56
# 51.784140, 19.460951

geocoded <- add_case(
  geocoded, 
  ecuuid = pull(no_loc[11, 1]),
  latitude = 51.784140, 
  longitude = 19.460951
)

# case 12. Hanuszkiewicza 100 m do Brzezińskiej #######

browseURL(pull(no_loc[12, 6]))
# https://goo.gl/maps/bjGMmHpYhay8MHDQ8
# 51.799015, 19.557424 

geocoded <- add_case(
  geocoded, 
  ecuuid = pull(no_loc[12, 1]), 
  latitude = 51.799015, 
  longitude = 19.557424
)

geocoded$is_geocoded <- "yes"

d_clean_geocoded <- left_join(d_clean, geocoded, by = "ecuuid")

d_clean_geocoded <- 
  d_clean_geocoded %>% mutate(
    latitude = if_else(condition = is.na(latitude.x), true = latitude.y, false = latitude.x), 
    longitude = if_else(condition = is.na(longitude.x), true = longitude.y, false = longitude.x)
  )

table(is.na(d_clean_geocoded$latitude))

summary(d_clean_geocoded$latitude)

write_rds(d_clean_geocoded, "d_clean_geocoded.rds")
