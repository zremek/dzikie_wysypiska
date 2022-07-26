# install.packages("webshot")
# webshot::install_phantomjs()

library(webshot)
library(rmarkdown)

rmdshot("data_exploration.Rmd", "data_exploration.pdf")

# efekt słaby - jedna, długa strona pdf. wydruk z firefoxa lepszy
