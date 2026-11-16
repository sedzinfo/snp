##########################################################################################
# DIRECTORIES
##########################################################################################
# R CMD check snp
# .rs.restartR()
library(devtools)
rm(list = ls())
graphics.off()
cat("\014")
##########################################################################################
# FUNCTIONS PACKAGE
##########################################################################################
directory <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(directory)
document()
if ("snp" %in% loadedNamespaces()) unloadNamespace("snp") # an old version still loaded would make the reinstalled one look corrupt
if ("snp" %in% rownames(installed.packages())) remove.packages("snp")
install()
library(snp)

pkgdown::build_site(getwd())
system("R CMD Rd2pdf . --force --no-preview --output=snp.pdf")
if (.Platform$OS.type == "windows") shell.exec("snp.pdf") else system("evince snp.pdf")
system("R CMD build . --resave-data")

plot_normal()
