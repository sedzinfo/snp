##########################################################################################
# DIRECTORIES
##########################################################################################
# R CMD check snp
# R CMD Rd2pdf snp
# R CMD build snp --resave-data
library(devtools)
library(roxygen2)
usethis::create_package("snp")
document()
