library(jsonlite)
library(tidyverse)

l <- jsonlite::fromJSON(txt = "2022-03-21-form-1__dzikie-wysypiska.json",
                        flatten = TRUE)

d <- l[["data"]]

head(d$`7_Dzisiejsza_data__p`)

d <- d %>% mutate(date_user = parse_date(x = `7_Dzisiejsza_data__p`, format = "%d/%m/%Y"))

summary(d$date_user)

# TODO dodać to do wykresu paste() #####
# d %>% count(date_user) %>% pull(n) %>% sum() 


d %>% count(date_user) %>% ggplot(aes(x = date_user, y = n)) +
  geom_col(width = 0.1, fill = "seagreen") +
  geom_point(colour = "seagreen", size = 3) + 
  scale_y_continuous(breaks = 0:6, minor_breaks = NULL, limits = c(0, 6),
                     name = "liczba wpisów") + 
  scale_x_date(date_minor_breaks = "1 day",
               name = "data dodania wpisu",
               date_breaks = "3 days", 
               date_labels = "%d.%m") + 
  theme_minimal() + 
  labs(title = "Wpisy na łódzkiej mapie dzikich wysypisk", 
       subtitle = "Od 1. do 20. marca 2022 przesłaliście nam 50 wpisów, dziękujemy!", 
       caption = "Źródło: badania własne\ndzikiewysypiska.uni.lodz.pl")

# ggsave(filename = paste(Sys.Date(), "liczba_wpisow_E5.png"),
#        units = "cm", height = 5, width = 7) źle działa!!!

ggsave(filename = paste(Sys.Date(), "liczba_wpisow_E5.jpg", sep = ""),
       scale = 1, units = "in", width = 5, height = 3)


# ile zdjęć w sumie? #####

d %>% select(5:8) %>%
  mutate(across(.cols = everything(), .fns = ~ str_starts(.x, "https"))) %>%  
  mutate(ile = rowSums(.)) %>% pull(ile) %>% sum()

# jaki czas odpowiedzi? 

d %>% mutate(czas_odp = as.numeric(
  parse_datetime(uploaded_at) - parse_datetime(created_at))
  ) %>% 
  pull(czas_odp) %>% summary()

d %>% mutate(czas_odp_sekundy = as.numeric(
  parse_datetime(uploaded_at) - parse_datetime(created_at))
) %>% 
  pull(czas_odp_sekundy) %>% summary()
