# Analysis of VEFMAP vegetation monitoring data

# See README.md for details of the VEFMAP program and this project and see
#    analysis/preregistration_template.Rmd for details of the analysis
#    implemented in this script

# Authors: Jian Yen, Elliot Gould, Henry Wootton
# Contact: jdl.yen [at] gmail.com

# Date created: 16 November 2023
# Last updated: 22 November 2023

# load some packages (include all dependencies for renv tracking)
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

# load helper functions
source("R/data.R")

# load site info
coords <- load_coordinates()
site_info <- load_metadata()

# read in veg survey data and species information for
#   pilot data set (Campaspe River)
veg <- load_points(system = "Campaspe", pilot = TRUE)

# check missing transects and species
# TODO

# filter out non-target species
# TODO

# add the thresholds and sub-transect AHD measurements and
#    calculate flow zones
coords <- coords |> 
  mutate(coords_available = 1, metres = as.numeric(metres)) |>
  left_join(
    site_info |> mutate(thresholds_available = 1),
    by = c("system", "waterbody", "site", "transect")
  ) |>
  mutate(
    zone = ifelse(
      height_ahd < baseflow_m_ahd, "below_baseflow",
      ifelse(height_ahd < springfresh_m_ahd, "baseflow_to_springfresh",
             "above_springfresh")
    )
  )

# add this info into the veg data set
veg <- veg |>
  left_join(coords, by = c("system", "waterbody", "site", "transect", "metres"))

# filter out the data without PFG or zone info

##    BUILD MODELS BY ZONE, EVENT (surveys 1 and 2 and pre/post spring event
##           and 3 and 4 are pre/post summer event), with other factors

## Modle should have (TERM | PFG / SPECIES) in it

## DISPLAY OUTPUTS IN NEAT WAY.


