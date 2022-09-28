# source("data_exploration.Rmd")

# liczba dzielnice #### 

theme_set(theme_minimal(base_size = 12))

punkty_dzielnice_gg_eng <- punkty_dzielnice_gg

punkty_dzielnice_gg_eng$dzielnica[6] <- "no geolocation"
punkty_dzielnice_gg_eng$dzielnica[7] <- "outside the city of Lodz"

punkty_dzielnice_gg_eng %>% 
  mutate(dzielnica = fct_reorder(dzielnica, n, max)) %>% 
  ggplot(aes(x = dzielnica, 
             y = n, fill = dzielnica)) + 
  geom_col(alpha = 0.6) + 
  # geom_text(aes(label = n, hjust = "right")) +
  coord_flip() +
  scale_fill_manual(values = manual_fill) +
  scale_y_continuous(breaks = seq(0, 70, 10), minor_breaks = NULL) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +
  guides(fill = "none") + 
  labs(x = "district",
       y = "records count [n = 208]")

ggsave("count_distr.png", width = 7.5, height = 6.5, units = "cm")

# powierzchnia ##### 

dzielnice_4326 <- dzielnice_4326 %>% mutate(
  area = st_area(dzielnice_4326))

st_join(punkty_dzielnice, dzielnice_4326) %>% count(dzielnica.x, area) %>% 
  mutate(n_area = n / area,
         dzielnica.x = fct_reorder(dzielnica.x, 
                                   n_area,
                                   max)) %>% 
  tibble() %>% select(-geometry) %>% 
  ggplot(aes(x = dzielnica.x, 
             y = n_area, 
             fill = dzielnica.x)) + 
  geom_col(alpha = 0.6) + 
  coord_flip() +
  scale_fill_manual(values = manual_fill) +
  guides(fill = "none") + 
  labs(x = "district",
       y = "records per district area")

ggsave("area_count_distr.png", width = 8.5, height = 6.5, units = "cm")


# skład n = 64 #### 

names(cechy_tak$Jakie_rodzaje_odp) <- cechy_tak$ecuuid

rodzaje_long <- reshape2::melt(cechy_tak$Jakie_rodzaje_odp)

rodzaje_wide <- bind_rows(lapply(cechy_tak$Jakie_rodzaje_odp,
                                 as.data.frame.list))

# sum(!is.na(rodzaje_wide)) == sum(!is.na(rodzaje_long$value))

# write.csv(rodzaje_long, "rodzaje_long.csv")

rodzaje_long <- rodzaje_long %>% 
  mutate(value = str_trim(value))

rodzaje_eng <- read_csv("rodzaje_eng.csv")

rodzaje_eng <- rodzaje_eng %>% 
  mutate(value = str_trim(value),
         val_eng = str_trim(val_eng))

rodzaje_long_en <- left_join(x = rodzaje_long, 
                             y = rodzaje_eng)

rodzaje_long_en %>%
  select(-value) %>% 
  ggplot(aes(x = fct_rev(fct_infreq(val_eng)))) + 
  geom_bar(alpha = 0.6) + 
  geom_hline(yintercept = podano_cechy[2] / 2,
             colour = "blue", lty = 2, alpha = 0.4) +
  annotate("text", x = 5, y = podano_cechy[2] / 2,
           label = "half of the records", colour = "blue") +
  labs(y = paste0("records count [n = ", podano_cechy[2], "]"),
       x = "garbage type [multiple choice question]") + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 30)) +
  theme_minimal(base_size = 12) +
  coord_flip()

ggsave("garb_type.png", width = 15, height = 13, units = "cm")
