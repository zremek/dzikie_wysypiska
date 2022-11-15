# goal: make clean, user-friendly excel file including photos 
# one in Polish, one in English

knitr::knit("data_exploration.Rmd")

## zrobione 1. wyczyścić nazwy zmiennych w d, wzorując się na d_cechy i cechy_tak

# summary(d$date_user)

d_clean <- d %>% rename(zdjecie_1 = `1_Prosimy_o_zdjcie_z`,
                        zdjecie_2 = `2_Jeli_chcesz_dodaj_`, 
                        zdjecie_3 = `3_Jeli_chcesz_dodaj_`, 
                        zdjecie_4 = `4_Jeli_chcesz_dodaj_`)

d_clean <- rename_with(d_clean, ~tolower(str_remove_all(., "\\d+_")))

## 2. ustalić typy danych, pozbyć się listy w środku

d_clean <- d_clean %>% select(-jakie_rodzaje_odp) %>% as_tibble()

d_clean <- type_convert(d_clean)

## 3. dołączyć wielokrotny wybór rodzaje_long jako wide 

RODZAJE <- pivot_wider(data = rodzaje_long, id_cols = L1, names_from = value, names_prefix = "rodz_")
RODZAJE <- RODZAJE %>% rename(ecuuid = L1)

colnames(RODZAJE) <- 
  colnames(RODZAJE) %>% 
  stringr::str_squish(string = .) %>% 
  stringr::str_trunc(string = ., width = 18, side = "right", ellipsis = "") %>% 
  gsub("(", "", ., fixed = TRUE) %>%
  gsub(")", "", ., fixed = TRUE) %>% 
  gsub("/", "", ., fixed = TRUE) %>% 
  gsub(",", "", ., fixed = TRUE) %>% 
  gsub(" ", "_", ., fixed = TRUE)

d_clean <- left_join(d_clean, RODZAJE)

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
  zdjecia_sum = rowSums(!is.na(select(., starts_with("zdjecie_")))),
  rodz_sum = rowSums(!is.na(select(., starts_with("rodz_")))),
  rodz_sum = if_else(condition = czy_chcesz_nam_pow == "Tak",
                     true = rodz_sum,
                     false = NA_real_)
  )

## 6. zapisać Rdata

write_rds(d_clean, "d_clean.rds")

## 7. wybrać zmienne do zapisu do excela i zapisać excela 



