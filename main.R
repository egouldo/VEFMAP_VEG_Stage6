# Analysis of VEFMAP vegetation monitoring data

# See README.md for details of the VEFMAP program and this project and see
#    analysis/preregistration_template.Rmd for details of the analysis
#    implemented in this script

# Authors: Jian Yen, Elliot Gould, Henry Wootton
# Contact: jdl.yen [at] gmail.com

# Date created: 16 November 2023
# Last updated: 16 November 2023

# load some packages (preliminary list for renv tracking)
library(here)
library(readr)
library(readxl)
library(dplyr)
library(tidyr)
library(mgcv)
library(performance)
library(ggplot2)

# load helper functions
# source("R/...")

# load data, subsetted to pilot data set (Campaspe)
