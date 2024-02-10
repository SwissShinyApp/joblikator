## SCRIPT INSTALLS PACKAGES AND LOADS LIBRARIES AND DEPENDENCIES.
# renv::init()
# install.packages("komaletter")
# install.packages('R.cache')
# install.packages('R.methodsS3')
# install.packages('R.oo')
# install.packages('R.utils')
# install.packages('RcppArmadillo')
# install.packages('RcppEigen')
# install.packages('bib2df')
# install.packages('bibtex')
# install.packages('ggf')
# install.packages('RefManageR')
# install.packages('vitae')
# install.packages('ggforce')
# install.packages('ggraph')
# install.packages('graphlayouts')
# install.packages('polyclip')
# install.packages('scholar')
# install.packages('viridis')
# install.packages("reticulate")
# install.packages("usethis")
# install.packages('scholar') not
# install.packages("colourpicker")

# Always happens this: 
#   RROR: compilation failed for package ‘igraph’
# * removing ‘/Users/michaelrubin/Library/Mobile Documents/com~apple~CloudDocs/DEV STUDIO/JOBlikatoR/renv/staging/1/igraph’
# ------------------------------------------------------------------------------ 
#   R was unable to find the gfortran binary.
# gfortran is required for the compilation of FORTRAN source files.
# Please check that gfortran is installed and available on the PATH.
# Please see https://stackoverflow.com/q/35999874 for more information.
# 
# Reason(s):
#   - 'make: /opt/gfortran/bin/gfortran: No such file or directory'
# Error: install of package 'igraph' failed [error code 1]

# install.packages('tinytex')
# tinytex::install_tinytex()
# Matrix [1.5-4.1] igraph
# renv::init()
# renv::snapshot()
# renv::restore()
# renv::install()
# 
# renv::init()
# renv::restore()
# renv::snapshot()
# renv::status()
#library(Matrix)
library(tidyverse)
library(here)
library(readr)
library(rmarkdown)
library(shiny)
library(shinyjs)
library(shinyWidgets)
library(shinyFiles)
library(jsonlite)
library(colourpicker)
library(DT)
library(shinybusy)
library(purrr)


# 
# install.packages(c("rvest", "stringr", "purrr"))
# library(rvest)
# library(stringr)
# library(purrr)
# 
# # The URL of the website you want to scrape
# url <- "https://claires.wd12.myworkdayjobs.com/de-DE/Claires/job/Bern-Bern/Verkuferin_JR147172?q=Switzerland&codes=Indeed"
# 
# # Read the HTML of the website
# webpage <- read_html(url)
# 
# 
# 
# # Extract CSS
# css <- html_nodes(webpage, "style")
# 
# 
# 
# library(stringr)
# library(xml2)
# 
# # assuming `css` is an xml_nodeset
# css_char <- as.character(css)
# 
# colors <- str_extract_all(css_char, "#[A-Fa-f0-9]{6}")
# 
# # Extract color codes
# colors <- str_extract_all(css, "#[A-Fa-f0-9]{6}")
# 
# # Find the most frequently used color
# main_color <- colors %>% unlist() %>% table() %>% sort(decreasing = TRUE)
# 
