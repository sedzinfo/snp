##########################################################################################
# DIRECTORIES
##########################################################################################
# R CMD check snp
# R CMD Rd2pdf snp
# R CMD build snp --resave-data
library(devtools)
library(roxygen2)
rm(list = ls())
graphics.off()
cat("\014")
##########################################################################################
# FUNCTIONS PACKAGE
##########################################################################################
directory <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(directory)
# usethis::create_package("snp")
document()
if ("snp" %in% rownames(installed.packages())) remove.packages("snp")
install()
library(snp)

pkgdown::build_site(getwd())
system("R CMD Rd2pdf . --force --no-preview --output=snp.pdf")
if (.Platform$OS.type == "windows") shell.exec("snp.pdf") else system("evince snp.pdf")
system("R CMD build . --resave-data")
