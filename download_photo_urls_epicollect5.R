library(jsonlite)

l <- jsonlite::fromJSON(txt = "form-1-dzikie-wysypiska-4.json",
              flatten = TRUE)

d <- l[["data"]]

## photos here:
# 1_Prosimy_o_zdjcie_z
# 2_Jeli_chcesz_dodaj_ 

d1 <- d[d$`1_Prosimy_o_zdjcie_z` != "",]
d2 <- d[d$`2_Jeli_chcesz_dodaj_` != "",]

# https://stackoverflow.com/questions/32174306/download-url-links-using-r 

for (i in 1:length(d1$`1_Prosimy_o_zdjcie_z`)) {
  download.file(d1$`1_Prosimy_o_zdjcie_z`[i],
                destfile = paste0("photos/d1_", d1$ec5_uuid[i], ".jpg"),
                method = "auto")
}

for (i in 1:length(d2$`2_Jeli_chcesz_dodaj_`)) {
  download.file(d2$`2_Jeli_chcesz_dodaj_`[i],
                destfile = paste0("photos/d2_", d2$ec5_uuid[i], ".jpg"),
                method = "auto")
}
