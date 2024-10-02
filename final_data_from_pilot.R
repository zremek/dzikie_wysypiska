library(tidyverse)
library(readxl)
library(DataExplorer)
library(sjPlot)
library(sjmisc)
library(sf)

# this script makes final dataset from pilot study


d_geocoded <- read_rds("d_clean_geocoded.rds")

d_inputed <- read_excel("e5_data_clean.xlsm", sheet = 4)

summary(d_inputed)

tibble(geocoded = names(d_geocoded),
       inputed = c(names(d_inputed), 1:3)) %>%
  mutate(same = geocoded == inputed) %>%
  filter(same == FALSE)



# fin <- left_join(d_inputed %>% select(-time_user), d_geocoded %>% select(-contains(".")))
#
# summary(fin)
#
# create_report(fin)
#
# # dlaczego is_geocoded jest 100% NA?
#
# table(d_geocoded$is_geocoded)
# table(fin$is_geocoded)
#
# create_report(d_geocoded)
#
#
# fin2 <- left_join(d_inputed, d_geocoded, by = "ecuuid")


table(d_geocoded$district, d_geocoded$is_geocoded) #TODO uzupełnić dzielnice


fin3 <- left_join(
  d_inputed %>% select(-latitude, -longitude),
  d_geocoded %>% select(ecuuid, latitude, longitude, is_geocoded)
)



# create_report(fin3)
# view_df(fin3, show.frq = TRUE)

#
# to powinny być fct
#
# more_dump
# area
# character
# span
# place
# visible
# first_time_user
# more_you
# volunteer_id
# district
# more_dumps_input
# is_geocoded
#


fctors <- c(
  "more_dump",
  "area",
  "character",
  "span",
  "place",
  "visible",
  "first_time_user",
  "more_you",
  "volunteer_id",
  "district",
  "more_dumps_input",
  "is_geocoded"
)

fin3 <- fin3 %>% mutate_at(fctors, as_factor)



fin3 %>% select(all_of(fctors)) %>% frq()


# TODO kolejność kategorii w faktorach

levels(fin3$span)

fin3 <-
  fin3 %>%
  mutate(
    area = fct_relevel(area, levels(area)[2]),
    area = fct_relevel(area, levels(area)[4], after = Inf),
    span = fct_relevel(span, levels(span)[3]),
    span = fct_relevel(span, levels(span)[5], after = Inf),
    span = fct_relevel(span, levels(span)[4], after = Inf),
    volunteer_id = fct_infreq(volunteer_id),
    more_dumps_input = fct_relevel(more_dumps_input, levels(more_dumps_input)[2]),
    place = fct_infreq(place),
    place = fct_relevel(place, "inne", after = Inf)
  )

# sjmisc::shorten_string() może użyć?

#
# należałoby też przekodować type na yes / no z uwzględnieniem NA



fin3 %>%
  select(visible, type_glass) %>%
  mutate(glass = case_when(
    !is.na(visible) & nchar(type_glass) > 0 ~ "Wybrano",
    !is.na(visible) & is.na(type_glass) ~ "Nie wybrano",
    is.na(visible) ~ NA
  )) %>% View()


types_multi <- fin3 %>% select(starts_with("type_")) %>% names()


fin3 <- fin3 %>%
  mutate(across(types_multi[1:19],
                function(x)
                  case_when(
                    !is.na(visible) & nchar(x) > 0 ~ "Wybrano",
                    !is.na(visible) & is.na(x) ~ "Nie wybrano",
                    is.na(visible) ~ NA
                  )))


fin3 <- fin3 %>% mutate_at(types_multi[1:19], as_factor)

fin3 %>% select_if(is.factor) %>% sjmisc::frq()




#
# należałoby wszystko przetłumaczyć
#
# zaktualizować słowniki danych
#
# etykietki zmiennych i wartości opisać


# TODO district doliczyć dla geokodowanych wartości


shp <- sf::st_read(dsn = ".", layer = "Jednostki_ewidencyjne")

lodz_dzielnice <- shp %>% filter(str_detect(JPT_NAZWA_, "ŁÓDŹ"))

dzielnice_4326 <- st_transform(lodz_dzielnice %>%
                                 select(JPT_NAZWA_, geometry), 4326) %>%
  mutate(dzielnica = str_remove(JPT_NAZWA_, "ŁÓDŹ-") %>%
           stringi::stri_trans_totitle())

punkty_4326 <- st_as_sf(
  x = fin3 %>%
    select(ecuuid, longitude, latitude),
  coords = c("longitude", "latitude"),
  crs = st_crs(dzielnice_4326)
)


punkty_dzielnice <-
  st_intersection(x = dzielnice_4326, y = punkty_4326)

punkty_dzielnice %>% count(JPT_NAZWA_)

punkty_dzielnice %>% count(JPT_NAZWA_) %>% pull(n) %>% sum()


fin4 <-
  left_join(fin3, punkty_dzielnice %>% select(ecuuid, dzielnica))

tab_xtab(fin4$district, fin4$dzielnica, show.na = TRUE)

fin4 <- fin4 %>%
  mutate(district = fct_na_value_to_level(dzielnica, "poza granicami m. Łodzi")) %>%
  select(-dzielnica)




# TODO is_geocoded ogarnąć albo usunąć, obecnie samo "yes"


fin4 <- fin4 %>%
  mutate(is_geocoded = fct_na_value_to_level(is_geocoded, "no"))

frq(fin4$is_geocoded)

# TODO type_count pokazuje wartości przed imputem, ogarnąć albo usunąć


fin5 <-
  sjmisc::row_count(
    x = fin4,
    type_glass:type_paints_in,
    count = "Wybrano",
    var = "sum_type",
    append = TRUE
  )

table(fin5$type_sum == fin5$sum_type, useNA = "always")

# TODO time zone stała?


table(fin3$lct_zone, useNA = "always")
