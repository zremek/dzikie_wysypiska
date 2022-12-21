# 
library(tableone)
library(tidyverse)
Sys.setenv(LANGUAGE = "en")

d_clean <- read_rds("d_clean.rds")

t1 <-
  tableone::CreateTableOne(vars = colnames(d_clean[, c(9:10, 12:16, 18:19, 21:40, 42, 45:55)]),
                           data = d_clean,
                           strata = "visible")

t1$ContTable
t1$CatTable
tableone::kableone(x = t1, format = "html")

######### napisać mailowo do Ani o tym uzupełnieniu danych: 
prop.table(table(d_clean$more_dump)) # do ręcznego zakodowania charakterystyk śmietnisk mamy prawie 70% wpisów
prop.table(table(!is.na(d_clean$latitude))) # do ręcznego zakodowania koordynat 6% wpisów 
# ponadto do ręcznego zakodowania które zdjęcia są "puste" tzn. samo logo e5
# z tych rekordów raczej nie będzie ręcznego kodowania charaktreystyk 
# bo charakterystyki weźmiemy ze zdjęć
# można kogoś wynająć do tego kodowania, lub sami, lub parę osób i porównać kody

library(ggmosaic)

ggplot(data = d_clean %>% filter(more_dump == "Tak")) + 
  ggmosaic::geom_mosaic(aes(x = product(visible), fill = area))
