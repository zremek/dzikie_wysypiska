library(jsonlite)
library(tidyverse)

data_json <- "2022-04-29-form-1__dzikie-wysypiska.json"

l <- jsonlite::fromJSON(txt = data_json,
                        flatten = TRUE)

d <- l[["data"]]

head(d$`7_Dzisiejsza_data__p`)

d <- d %>% mutate(date_user = parse_date(x = `7_Dzisiejsza_data__p`, format = "%d/%m/%Y"))

summary(d$date_user)

# plot #####

count_date_user <- d %>% count(date_user)
n_max <- count_date_user %>% pull(n) %>% max()
n_sum <- count_date_user %>% pull(n) %>% sum()
date_max <- max(d$date_user)

count_date_user %>% ggplot(aes(x = date_user, y = n)) +
  geom_col(width = 0.1, fill = "seagreen") +
  geom_point(colour = "seagreen", size = 3) + 
  scale_y_continuous(breaks = 0:n_max, minor_breaks = NULL, limits = c(0, n_max),
                     name = "liczba wpisów") + 
  scale_x_date(date_minor_breaks = "1 day",
               name = "data dodania wpisu",
               date_breaks = "3 days", 
               date_labels = "%d.%m") + 
  theme_minimal() + 
  labs(title = "Wpisy na łódzkiej mapie dzikich wysypisk", 
       subtitle = paste0("Do ", 
                         date_max, 
                         " przesłaliście nam ",
                         n_sum, 
                         " wpisy, dziękujemy!"), 
       caption = "Źródło: badania własne\ndzikiewysypiska.uni.lodz.pl")

# ggsave(filename = paste(Sys.Date(), "liczba_wpisow_E5.png"),
#        units = "cm", height = 5, width = 7) źle działa!!!

ggsave(filename = paste(date_max, "_liczba_wpisow_E5.jpg", sep = ""),
       scale = 1, units = "in", width = 8, height = 4)


# ile zdjęć w sumie? #####

d %>% select(5:8) %>%
  mutate(across(.cols = everything(), .fns = ~ str_starts(.x, "https"))) %>%  
  mutate(ile = rowSums(.)) %>% pull(ile) %>% sum()

# jaki czas odpowiedzi? 

d %>% mutate(czas_odp_sekundy = as.numeric(
  parse_datetime(uploaded_at) - parse_datetime(created_at))) %>% 
  pull(czas_odp_sekundy) %>% summary()
