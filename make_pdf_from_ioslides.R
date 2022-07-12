install.packages("webshot")
webshot::install_phantomjs()

library(webshot)
library(rmarkdown)

rmdshot("slajdy_fgi_15_lipca.Rmd", "slajdy_fgi_15_lipca.pdf")
