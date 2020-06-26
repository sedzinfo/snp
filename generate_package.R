##########################################################################################
# DIRECTORIES
##########################################################################################
# R CMD check snp
# R CMD Rd2pdf snp
# R CMD build snp --resave-data
library(devtools)
library(roxygen2)
# setwd("/mnt/WDRED_REMOTE/repositories/snp/")
# setwd("/mnt/WD500/public_rstatistics/")
# usethis::create_package("snp")
document()
install()
  
