# goal: make clean, user-friendly excel file including photos 
# one in Polish, one in English

# eng - academic papers
# pol - book / report for local audience 

# var names in eng only! 

knitr::knit("data_exploration.Rmd")

d_clean <- d %>% rename(photo_1 = `1_Prosimy_o_zdjcie_z`,
                        photo_2 = `2_Jeli_chcesz_dodaj_`, 
                        photo_3 = `3_Jeli_chcesz_dodaj_`, 
                        photo_4 = `4_Jeli_chcesz_dodaj_`)

d_clean <- rename_with(d_clean, ~tolower(str_remove_all(., "\\d+_")))

d_clean <- d_clean %>% select(-jakie_rodzaje_odp) %>% as_tibble()

d_clean <- type_convert(d_clean)

## 3. dołączyć wielokrotny wybór rodzaje_long jako wide 

rodzaje_long <- rodzaje_long %>% 
  mutate(value = str_trim(value))

rodzaje_eng <- read_csv("rodzaje_eng.csv")

rodzaje_eng <- rodzaje_eng %>% 
  mutate(value = str_trim(value),
         val_eng = str_trim(val_eng))

rodzaje_long_en <- left_join(x = rodzaje_long, 
                             y = rodzaje_eng)

rodzaje_long_en <- rodzaje_long_en %>% select(-value) %>% 
  rename(value = val_eng)

print(paste0("class of rodzaje_long_en: ", class(rodzaje_long_en)))

rodzaje_wide_pl <- pivot_wider(data = rodzaje_long,
                               id_cols = L1,
                               names_from = value,
                               names_prefix = "rodz_")

rodzaje_wide_pl <- rodzaje_wide_pl %>% rename(ecuuid = L1)

rodzaje_wide_en <- pivot_wider(data = rodzaje_long_en,
                               id_cols = L1,
                               names_from = value,
                               names_prefix = "type_")

rodzaje_wide_en <- rodzaje_wide_en %>% rename(ecuuid = L1)

colnames(rodzaje_wide_en) <- 
  colnames(rodzaje_wide_en) %>% 
  stringr::str_squish(string = .) %>% 
  stringr::str_trunc(string = ., width = 15, side = "right", ellipsis = "") %>% 
  gsub("(", "", ., fixed = TRUE) %>%
  gsub(")", "", ., fixed = TRUE) %>% 
  gsub("/", "", ., fixed = TRUE) %>% 
  gsub(",", "", ., fixed = TRUE) %>% 
  gsub(" ", "_", ., fixed = TRUE)

# rodzaje_wide_en stays for eng version

## make eng var names for pl data

colnames(rodzaje_wide_pl) <- colnames(rodzaje_wide_en)

d_clean <- left_join(d_clean, rodzaje_wide_pl)

## 4. ustalić co jest NA - załatwione w type_convert

## 5. ułożyć wygodną kolejność zmiennych wg. kwestionariusza

# summary(d_clean$dzisiejsza_data__p) tą zmienną pomijam,
# mam wyczyszczone date_user

d_clean <- d_clean %>% select(1:8, 25:26, 9, 31, 11:19, 35:53, 20:24, 27:30, 32:34)

### jakie zmienne doliczyć?
# liczba zdjęć
# liczba rodzajów odpadów (NA kiedy nie podano szczegółów)

d_clean <- 
  d_clean %>% mutate(
  photo_sum = rowSums(!is.na(select(., starts_with("photo_")))),
  type_sum = rowSums(!is.na(select(., starts_with("type_")))),
  type_sum = if_else(condition = czy_chcesz_nam_pow == "Tak",
                     true = type_sum,
                     false = NA_real_)
  )

## zrobić czytelne nazwy zmiennych 

nazwy_zmiana <- names(d_clean)[c(9:11, 13:21, 41:49)] %>% as_tibble()

nowe_nazwy <- c("lct_lat", 
                "lct_long",
                "lct_comment", 
                "time_user", 
                "more_dump",
                "area", 
                "character", 
                "character_comment", 
                "span", 
                "place", 
                "place_comment",
                "visible", 
                "type_comment", 
                "first_time_user", 
                "new_name_user", 
                "old_name_user",
                "more_you", 
                "lct_accu",
                "lct_northing",
                "lct_easting", 
                "lct_zone")

nazwy_zmiana$nowe_nazwy <- nowe_nazwy

names(d_clean)[c(9:11, 13:21, 41:49)] <- nowe_nazwy

names(d_clean) %>% tibble() %>% print(n = 100)

## dodać dzielnicę

districts <- 
  punkty_dzielnice %>% as_tibble() %>% select(3, 2, -geometry) %>% 
  rename(district = dzielnica, 
         ecuuid = ec5_uuid)

d_clean <- left_join(d_clean, districts)

d_clean <-
  d_clean %>%
  mutate(district_tmp = ifelse(is.na(lct_zone),
                           "brak współrzędnych wpisu",
                           ifelse(is.na(district), 
                                  "punkt poza granicami m. Łodzi", 
                                  district)))

# table(d_clean$district, d_clean$lct_zone, useNA = "always")
# table(d_clean$district_tmp, d_clean$lct_zone, useNA = "always")

d_clean <- d_clean %>% 
  select(-district) %>% 
  rename(district = district_tmp)

## zapisać 6 cyfr dokładności po kropce lat / long ################

d_clean %>% 
  select(ecuuid, created_at, lct_lat, latitude) %>% 
  filter(ecuuid == "14a666a7-9c5b-4d5e-83a6-3621d7c199c7") %>% 
  View()

d %>% 
  select(ec5_uuid, latitude) %>% 
  filter(ec5_uuid == "14a666a7-9c5b-4d5e-83a6-3621d7c199c7") %>% 
  pull(latitude)

# mam 51.78125 (8 znaków, 5 cyfr po kropce) 
# a w JSON źródłowym 51.781247 (9 znaków, 6 cyfr po kropce)

d_clean %>% mutate(lat_digits = nchar(latitude)) %>% pull(lat_digits) %>% table(useNA = "always")

d %>% mutate(lat_digits = nchar(latitude)) %>% pull(lat_digits) %>% table(useNA = "always")

# w większości przypadków jest jednak 9, rzadko 8, czasem 7,

table(nchar(l$data$`5_Tutaj_zlokalizowan.latitude`)) # to samo!

# to samo czytanie JSONa jest złe?
# https://stackoverflow.com/questions/36038349/significant-digits-when-converting-json-using-fromjson-in-jsonlite 

# sprawdzę jeszcze print

d %>% 
  select(ec5_uuid, latitude) %>% 
  filter(ec5_uuid == "14a666a7-9c5b-4d5e-83a6-3621d7c199c7") %>% 
  pull(latitude) %>% 
  print(digits = 15)
# wszystko gra, to tylko kwestia wyświetlania... właśnie się czegoś nauczyłem!

####

# l_digits <- jsonlite::fromJSON(txt = data_json, 
#                                options(digits = 22, flatten = TRUE)) # to są global options :D
# 
# options(digits = 7) # Valid values are 1...22 with default 7.


# nchar(l_digits[["data"]][[1]][["5_Tutaj_zlokalizowan"]][["latitude"]])

# nchar daje 8, wypisana liczba to 51.77353000000000093905 ?



# działa dziwnie, lista wychodzi case by case
# a wartości lat / lon mają po 

d_clean %>% mutate(lat_digits = nchar(latitude)) %>% 
  filter(lct_accu < 10000) %>% 
  ggplot(aes(factor(lat_digits), lct_accu)) + 
  geom_boxplot()

d_clean %>% mutate(lat_digits = nchar(latitude)) %>% 
  filter(lct_accu < 1000) %>%
  group_by(lat_digits) %>% summarise(
    me_accu = mean(lct_accu, na.rm = TRUE),
    md_accu = median(lct_accu, na.rm = TRUE),
    sd_accu = sd(lct_accu, na.rm = TRUE), 
    min_accu = min(lct_accu, na.rm = TRUE),
    max_accu = max(lct_accu, na.rm = TRUE),
    n = n()
  )

# przy 7 cyfrach dokładność jest najwyższa
# przy 8 średnio najniższa dokładność, ale tu jest jednen outlier
# 9 największa rozpiętość, największa średnia i wariancja jeśli odciąć do 1000

################################ wszystko było prawidłowo ####################

## zapisać Rdata

# write_rds(d_clean, "d_clean.rds")




