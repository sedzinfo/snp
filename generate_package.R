##########################################################################################
# DIRECTORIES
##########################################################################################
# R CMD check standard.normal.plot
# R CMD Rd2pdf standard.normal.plot
# R CMD build standard.normal.plot --resave-data
library(devtools)
library(roxygen2)
setwd("/mnt/WD500")

usethis::create_package("snp")
dir.create(file.path("data"),showWarnings=FALSE)
setwd("/mnt/WD500/snp/")
document()
