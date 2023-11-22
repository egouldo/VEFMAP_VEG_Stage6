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
# TODO: work out difference calculations to align true elev with flow elev
site_info <- load_metadata(recompile = TRUE)

# read in veg survey data and species information for
#   pilot data set (Campaspe River)
veg_cover <- load_cover(system = "Campaspe", pilot = TRUE, recompile = TRUE)
veg_richness <- load_richness(system = "Campaspe", pilot = TRUE, recompile = TRUE)

# check missing transects and species
# TODO
# (won't work with veg directly because currently summing over species)
# veg |> 
#   distinct(wpfg, species) |> 
#   arrange(wpfg, species) |>
#   mutate(include = ifelse(wpfg %in% wpfg_list, TRUE, FALSE))

# add this info into the veg data set
veg <- veg |>
  left_join(site_info, by = c("waterbody", "site", "transect", "metres"))

# load flow data and merge summary metrics with veg

# filter out the data without PFG or zone info

##    BUILD MODELS BY ZONE, EVENT (surveys 1 and 2 and pre/post spring event
##           and 3 and 4 are pre/post summer event), with other factors
## HITS out of 40, but two obs exceed 40 (work out what to do with these)

## Modle should have (TERM | PFG / SPECIES) in it
mod <- mgcv::gamm((hits / npoint) ~ wpfg * )

Cov_event_mod <- gam(plant_hits/point_samples ~ 
                       Year*Period*Zone*Origin +  	# fixed effects for year (1-4), period (before/after), treatment zone (bank elev), and origin (native/exotic) and their interactions
                       Grazing + 			# Fixed effect of grazing (binary, pres/absent)
                       s(Transect, bs='re') +		# Random effect for Transect
                       s(Site, bs='re'),      		# Random effect for Site
                     #s(System, bs='re') +		# Random effect for System, not to be implemented in pilot analysis
                     data=Data_springfresh_subset, family=binomial("logit"), method='REML' )


## DISPLAY OUTPUTS IN NEAT WAY.


