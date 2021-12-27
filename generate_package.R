##########################################################################################
# DIRECTORIES
##########################################################################################
# R CMD check snp
# R CMD Rd2pdf snp
# R CMD build snp --resave-data
library(devtools)
library(roxygen2)
# setwd("/mnt/WD500/repositories/snp/")
# usethis::create_package("snp")
document()
install()
  
