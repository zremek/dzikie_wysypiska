Sys.setenv(LANGUAGE = "en")

library(jsonlite)
library(tidyverse)
library(lubridate)

data_json <- "2022-06-30-form-1__dzikie-wysypiska.json"

l <- jsonlite::fromJSON(txt = data_json,
                        flatten = TRUE)

d <- l[["data"]]

head(d$`7_Dzisiejsza_data__p`)

d <- d %>% mutate(date_user = parse_date(x = `7_Dzisiejsza_data__p`,
                                         format = "%d/%m/%Y")) %>% 
  filter(date_user >= "2022-03-01")

summary(d$date_user)

# plot #####

count_date_user <- d %>% count(date_user)
n_max <- count_date_user %>% pull(n) %>% max()
n_sum <- count_date_user %>% pull(n) %>% sum()

# zmiana ręczna daty max, bo jest mniejsza niż koniec miesiąca
# TODO może oddzielny skrypt na takie zmienne?

# date_max <- max(d$date_user)

date_max <- "2022-06-30"

count_date_user %>% 
  ggplot(aes(x = date_user, y = n)) +
  geom_col(width = 0.1, fill = "seagreen") +
  geom_point(colour = "seagreen", size = 1) + 
  scale_y_continuous(breaks = 0:n_max, minor_breaks = NULL, limits = c(0, n_max),
                     name = "liczba wpisów") + 
  scale_x_date(date_minor_breaks = "1 day",
               name = "data dodania wpisu",
               date_breaks = "7 days", 
               date_labels = "%d.%m") + 
  theme_minimal() + 
  labs(title = "Wpisy na łódzkiej mapie dzikich wysypisk", 
       subtitle = paste0("Do ", 
                         date_max, 
                         " przesłaliście nam ",
                         n_sum, 
                         " wpisów, dziękujemy!"), 
       caption = "Źródło: badania własne\ndzikiewysypiska.uni.lodz.pl")

# ggsave(filename = paste(Sys.Date(), "liczba_wpisow_E5.png"),
#        units = "cm", height = 5, width = 7) źle działa!!!

ggsave(filename = paste(date_max, "_liczba_wpisow_E5.jpg", sep = ""),
       scale = 1, units = "in", width = 8, height = 4)


# wpisy w agregatach czasu

count_date_user %>% group_by(month = floor_date(date_user, "month")) %>%
  summarize(n = sum(n)) %>% 
  ggplot(aes(x = as.character(month), y = n)) +
  geom_col(width = 0.01, fill = "seagreen") +
  geom_point(colour = "seagreen", size = 4) +
  scale_y_continuous(name = "liczba wpisów") + 
  scale_x_discrete(name = "miesiąc dodania wpisu", 
                   labels = c("marzec", "kwiecień", "maj", "czerwiec")) +
  theme_minimal() + 
  labs(title = "Wpisy na łódzkiej mapie dzikich wysypisk", 
       subtitle = 
         "Liczba przesyłanych wpisów jest coraz niższa
w kolejnych miesiącach realizacji badania", 
       caption = "Źródło: badania własne\ndzikiewysypiska.uni.lodz.pl")

ggsave(filename = paste(date_max, "_miesięczna_liczba_wpisow_E5.jpg", sep = ""),
       scale = 1, units = "in", width = 4, height = 4)

# ile zdjęć w sumie? #####

d %>% select(5:8) %>%
  mutate(across(.cols = everything(), .fns = ~ str_starts(.x, "https"))) %>%  
  mutate(ile = rowSums(.)) %>% pull(ile) %>% sum()

# jaki czas odpowiedzi? 

d %>% mutate(czas_odp_sekundy = as.numeric(
  parse_datetime(uploaded_at) - parse_datetime(created_at))) %>% 
  pull(czas_odp_sekundy) %>% summary()

# ochotnicy

d <- d %>% mutate(volunteer_id = paste0(`20_Wymyl_swoj_nazw_u`,
                                   `21_Wpisz_swoj_nazw_u`) %>% 
               tolower() %>% 
               stringr::str_trim(., side = "both"))


d %>% count(volunteer_id) %>% dim()

# liczba ochotników dołączająca miesięcznie

d %>% group_by(volunteer_id) %>% 
  summarise(min_date = min(date_user)) %>% 
  count(min_date) %>% group_by(month = floor_date(min_date, "month")) %>%
  summarize(n = sum(n)) %>% 
  ggplot(aes(x = as.character(month), y = n)) +
  geom_col(width = 0.01, fill = "orange") +
  geom_point(colour = "orange", size = 4) +
  scale_y_continuous(name = "liczba dołączających osób") + 
  scale_x_discrete(name = "miesiąc dołączenia pierwszego wpisu", 
                   labels = c("marzec", "kwiecień", "maj", "czerwiec")) +
  theme_minimal() + 
  labs(title = "Ochotniczki / ochotnicy na łódzkiej mapie
dzikich wysypisk", 
       subtitle = 
         "Liczba dołączających osób jest coraz niższa
w kolejnych miesiącach realizacji badania", 
caption = "Źródło: badania własne\ndzikiewysypiska.uni.lodz.pl")

ggsave(filename = paste(date_max, "_miesięczna_liczba_osób_E5.jpg", sep = ""),
       scale = 1, units = "in", width = 4, height = 4)

# uwaga, Bogusia dołączyła w czerwcu a wpis dała z kwietnia!!! ######
# na stronę www daje to jako dołączenie w kwietniu, bo liczę daty 
# zatwierdzone przez ludzi, a nie daty serwera 
