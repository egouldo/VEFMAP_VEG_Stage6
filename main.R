# Analysis of VEFMAP vegetation monitoring data

# See README.md for details of the VEFMAP program and this project and see
#    analysis/preregistration_template.Rmd for details of the analysis
#    implemented in this script

# Authors: Jian Yen, Elliot Gould, Henry Wootton
# Contact: jdl.yen [at] gmail.com

# Date created: 16 November 2023
# Last updated: 21 November 2023

# load some packages (preliminary list for renv tracking)
library(here)
library(qs)
library(readr)
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(mgcv)
library(performance)
library(ggplot2)

# set project root
here::i_am()

# load helper functions
source("R/data.R")

# load site info
coords <- load_coordinates()
site_info <- load_metadata()


## LOAD DATA, CLASSIFY SITES INTO ZONES (ABOVE/BELOW BS/SPR THRESHOLDS, PLUS MARGINAL ZONE)
##    BUILD MODELS BY ZONE, EVENT (surveys 1 and 2 and pre/post spring event
##           and 3 and 4 are pre/post summer event), with other factors

## Modle should have (TERM | PFG / SPECIES) in it

## DISPLAY OUTPUTS IN NEAT WAY.


