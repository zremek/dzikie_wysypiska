library(tidyverse)
library(writexl)
Sys.setenv(LANGUAGE = "en")

d_clean <- read_rds("d_clean.rds")

## w osobnej ramce słownik z etykietami pol / eng 
## to idzie jako kolejna karta w excelu 

e5_data_dictionary_pl <- tibble(var_names = names(d_clean))

e5_data_dictionary_pl$var_labels_pl <- NA

e5_data_dictionary_pl$var_labels_pl[1:21] <- c(
  "Identyfikator techniczny wpisu w E5",
  "Data i godzina zamknięcia formularza (UTC)",
  "Data i godzina przesłania formularza (UTC)",
  "Identyfikator czytelny wpisu w E5 (data i godzina wskazana przez użytkownika, nazwa użytkownika jeżeli pierwszy wpis)",
  "Zdjęcie znalezionego dzikiego wysypiska (1)", 
  "Zdjęcie znalezionego dzikiego wysypiska (2)",
  "Zdjęcie znalezionego dzikiego wysypiska (3)",
  "Zdjęcie znalezionego dzikiego wysypiska (4)",
  "Tutaj zlokalizowane jest dzikie wysypisko (szerokość geogr. wskazana przez użytkownika)",
  "Tutaj zlokalizowane jest dzikie wysypisko (długość geogr. wskazana przez użytkownika)",
  "Jeżeli masz uwagi do lokalizacji znalezionego dzikiego wysypiska, prosimy o wpisanie",
  "Dzisiejsza data - prosimy o potwierdzenie (wskazana przez użytkownika)",
  "Aktualny czas - prosimy o potwierdzenie (wskazana przez użytkownika)", 
  "Czy chcesz nam powiedzieć coś więcej o znalezionym wysypisku?",
  "Jak oceniasz powierzchnię znalezionego dzikiego wysypiska?",
  "Jaki jest charakter znalezionego wysypiska?",
  "Opisz charakter znalezionego wysypiska",
  "Czy możesz ocenić czas istnienia znalezionego wysypiska?",
  "W jakim miejscu jest znalezione wysypisko?",
  "Opisz miejsce znalezionego wysypiska",
  "Czy znalezione wysypisko jest widoczne dla przechodniów?"
)

t <- d_clean %>% 
        select(1, 22:40) %>%
        pivot_longer(!ecuuid) %>%
        filter(!is.na(value)) %>%
        group_by(name) %>%
        count(value) %>% 
        select(-n) %>% 
        mutate(value = paste0(value, ": Jakie rodzaje odpadów (materiałów) znajdują się w wysypisku?")) %>% 
        rename(var_names = name, 
               var_labels_pl = value)

e5_data_dictionary_pl <- left_join(e5_data_dictionary_pl, t, by = "var_names")

e5_data_dictionary_pl <- 
  e5_data_dictionary_pl %>%
  unite("var_labels_pl", var_labels_pl.x, var_labels_pl.y, sep = "", na.rm = TRUE)


e5_data_dictionary_pl$var_labels_pl[41:55] <- c(
  "Opisz rodzaj znalezionych odpadów (materiałów)", 
  "Czy po raz pierwszy przesyłasz nam jakiekolwiek zdjęcie dzikiego wysypiska?",
  "Wymyśl swoją nazwę użytkownika (nowo nadana nazwa przy pierwszym wpisie)",
  "Wpisz swoją nazwę użytkownika (dotychczasowa nazwa przy kolejnych wpisach, wprowadzana ręcznie)",
  "Czy chcesz powiedzieć nam coś więcej o sobie? (wyświetla link do zewnętrznego formularza LimeSurvey, pytanie zadane tylko przy pierwszym wpisie)",
  "Dokładność lokalizacji znalezionego dzikiego wysypiska (promień w metrach)",
  "UTM Northing (szerokość geogr. wskazana przez użytkownika)",
  "UTM Easting (długość geogr. wskazana przez użytkownika)",
  "UTM Zone (strefa lokalizacji wskazana przez użytkownika)",
  "Identyfikator użytkownika (pochodna new_name_user i old_name_user: nowo nadana lub dotychczasowa nazwa użytkownika, ujednolicona)",
  "Stopnie dziesiętne do 0.000 001 (szerokość geogr. wskazana przez użytkownika)",
  "Stopnie dziesiętne do 0.000 001 (długość geogr. wskazana przez użytkownika)",
  "Suma przesłanych przez użytkownika zdjęć dzikiego wysypiska (pochodna: photo_1...photo_4)", 
  "Suma podanych przez użytkownika rodzajów odpadów znajdujących się w wysypisku (pochodna: type_glass...type_paints_in)", 
  "Dzielnica m. Łodzi, w której zlokalizowane jest znalezione wysypisko (pochodna: latitude, longitude, jednostki ewidencyjne Głównego Urzędu Geodezji i Kartografii 2019-08-08)"
)



# write_xlsx(list(d_clean = d_clean, e5_data_dictionary_pl = e5_data_dictionary_pl),
#            "e5_data_clean.xlsx")

# write_csv(d_clean, "e5_data_clean.csv") 
# for kepler.gl test

# w samym excelu przygotować oglądanie zdjęć w komórkach 
# https://www.extendoffice.com/documents/excel/4212-excel-insert-image-from-url.html
# to VBA działa na macu :) 
