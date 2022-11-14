# goal: make clean, user-friendly excel file including photos 
# one in Polish, one in English

knitr::knit("data_exploration.Rmd")

## zrobione 1. wyczyścić nazwy zmiennych w d, wzorując się na d_cechy i cechy_tak

summary(d$date_user)

d_clean <- d %>% rename(zdjecie_1 = `1_Prosimy_o_zdjcie_z`,
                        zdjecie_2 = `2_Jeli_chcesz_dodaj_`, 
                        zdjecie_3 = `3_Jeli_chcesz_dodaj_`, 
                        zdjecie_4 = `4_Jeli_chcesz_dodaj_`)

d_clean <- rename_with(d_clean, ~tolower(str_remove_all(., "\\d+_")))

## 2. ustalić typy danych, pozbyć się listy w środku

d_clean %>% select(-jakie_rodzaje_odp) %>% str()

## 3. dołączyć wielokrotny wybór rodzaje_long jako wide 
## 4. ustalić co jest NA 

