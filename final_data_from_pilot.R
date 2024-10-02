library(tidyverse)
library(readxl)
library(DataExplorer)
library(sjPlot)

# this script makes final dataset from pilot study 


d_geocoded <- read_rds("d_clean_geocoded.rds")

d_inputed <- read_excel("e5_data_clean.xlsm", sheet = 4)

summary(d_inputed)

tibble(geocoded = names(d_geocoded), inputed = c(names(d_inputed), 1:3)) %>% 
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


fin3 <- left_join(d_inputed %>% select(-latitude, -longitude), 
                  d_geocoded %>% select(ecuuid, latitude, longitude, is_geocoded))



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


fin3 <- fin3 %>% mutate_at(c(
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
), as_factor)


fin3 %>% select_if(is.factor) %>% sjmisc::frq()

# TODO kolejność kategorii w faktorach 

# 
# należałoby też przekodować type na yes / no z uwzględnieniem NA


fin3 %>% 
  mutate(across(starts_with("type_"),
                       function(x) nchar(x) > 0)) %>%
  View()


# 
# należałoby wszystko przetłumaczyć 
# 
# zaktualizować słowniki danych 
# 
# etykietki zmiennych i wartości opisać 






