library(jsonlite)

# step 0: manually login and download json file #####
# from https://five.epicollect.net/project/dzikie-wysypiska/data 

# 1: make df with photo urls ####

my_fresh_file <- "2022-05-23-form-1__dzikie-wysypiska.json"

l <- jsonlite::fromJSON(txt = my_fresh_file,
                        flatten = TRUE)

d <- l[["data"]]

d1 <- d[d$`1_Prosimy_o_zdjcie_z` != "",]
d2 <- d[d$`2_Jeli_chcesz_dodaj_` != "",]
d3 <- d[d$`3_Jeli_chcesz_dodaj_` != "",]
d4 <- d[d$`4_Jeli_chcesz_dodaj_` != "",]

# 2: loop for download #### 
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

for (i in 1:length(d3$`3_Jeli_chcesz_dodaj_`)) {
  download.file(d3$`3_Jeli_chcesz_dodaj_`[i],
                destfile = paste0("photos/d3_", d3$ec5_uuid[i], ".jpg"),
                method = "auto")
}

for (i in 1:length(d4$`4_Jeli_chcesz_dodaj_`)) {
  download.file(d4$`4_Jeli_chcesz_dodaj_`[i],
                destfile = paste0("photos/d4_", d4$ec5_uuid[i], ".jpg"),
                method = "auto")
}
