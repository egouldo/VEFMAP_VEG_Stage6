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
library(lubridate)
library(stringr)
library(aae.hydro)
library(mgcv)
library(performance)
library(ggplot2)

# load helper functions
source("R/utils.R")
source("R/data.R")

# load site info
# TODO: work out difference calculations to align true elev with flow elev
site_info <- load_metadata(recompile = FALSE)

# read in veg survey data and species information for
#   pilot data set (Campaspe River)
veg_cover <- load_cover(system = "Campaspe", pilot = TRUE, recompile = FALSE)
veg_richness <- load_richness(system = "Campaspe", pilot = TRUE, recompile = FALSE)
veg_cover_ar <- load_cover(system = "Campaspe", pilot = TRUE, recompile = FALSE, ar = TRUE)

# TODO: check missing species
# veg |> 
#   distinct(wpfg, species) |> 
#   arrange(wpfg, species) |>
#   mutate(include = ifelse(wpfg %in% wpfg_list, TRUE, FALSE))

# load flow data and merge summary metrics with veg
flow <- load_flow(system = "Campaspe", pilot = TRUE, recompile = FALSE)
metrics <- calculate_metrics(flow, site_info)

# add site and flow info into the veg data set, removing plots with missing
#    values
# TODO: check missing site info with CJ
veg_cover_ar <- veg_cover_ar |>
  left_join(site_info, by = c("waterbody", "site", "transect", "metres")) |>
  filter(!is.na(zone)) |>
  left_join(
    metrics,
    by = c("system", "waterbody", "site", "survey_year", "period")
  )
veg_richness <- veg_richness |>
  left_join(site_info, by = c("waterbody", "site", "transect", "metres")) |>
  filter(!is.na(zone)) |>
  left_join(
    metrics,
    by = c("system", "waterbody", "site", "survey_year", "period")
  )

##    BUILD MODELS BY ZONE, EVENT (surveys 1 and 2 and pre/post spring event
##           and 3 and 4 are pre/post summer event), with other factors
## HITS out of 40, but two obs exceed 40 (work out what to do with these)

## TODO: consider including exotic cover as a predictor OR include
##   random int/slopes for origin and look at correlations to assess
##   how natives and exotics interact

stan_seed <- 2023-11-26


## Modle should have (TERM | PFG / SPECIES) in it
veg_cover_ar <- veg_cover_ar |>
  mutate(
    log_hits_tm1 = log(hits_tm1 + 1),
    days_above_baseflow_std = scale(days_above_baseflow)[, 1],
    days_above_springfresh_std = scale(days_above_springfresh)[, 1],
    days_above_baseflow_std_sq = days_above_baseflow_std ^ 2,
    days_above_springfresh_std_sq = days_above_springfresh_std ^ 2
  ) |>
  filter(!is.na(days_above_springfresh))  # temporary due to incomplete flow data
cover_ar1_mod <- mgcv::gamm(
  hits ~ log_hits_tm1 +
    s(days_above_baseflow_std) + s(days_above_springfresh_std) +
    zone * period +
    grazing +
    offset(npoint),
  random = list(
    # origin = ~ zone + period + grazing,
    waterbody = ~ 1,
    site = ~ 1,
    transect = ~ 1,
    survey_year = ~ 1,
    wpfg = ~ 1
    # wpfg = ~ days_above_baseflow_std + 
    #   days_above_springfresh_std +
    #   zone * origin * period,
    # species = ~ days_above_baseflow_std + 
    #   days_above_springfresh_std +
    #   zone * origin * period
  ),
  family = poisson(),
  data = veg_cover_ar |> filter(wpfg %in% c("ATl", "Sk", "ARp"))
)


cover_ar1_mod <- brms::brm(
  hits ~ log_hits_tm1 +
    days_above_baseflow_std + days_above_springfresh_std +
    days_above_baseflow_std_sq + days_above_springfresh_std_sq +
    zone * period +
    grazing +
    offset(npoint) +
    (1 | waterbody) + 
    (1 | site) +
    (1 | transect) +
    (1 | survey_year) +
    (1 | wpfg),
    # origin = ~ zone + period + grazing,
    # wpfg = ~ days_above_baseflow_std + 
    #   days_above_springfresh_std +
    #   zone * origin * period,
    # species = ~ days_above_baseflow_std + 
    #   days_above_springfresh_std +
    #   zone * origin * period
  family = poisson(),
  data = veg_cover_ar |> filter(wpfg %in% c("ATl", "Sk", "ARp"))
)

veg_richness <- veg_richness |>
  mutate(
    days_above_baseflow_std = scale(days_above_baseflow)[, 1],
    days_above_springfresh_std = scale(days_above_springfresh)[, 1],
    days_above_baseflow_std_sq = days_above_baseflow_std ^ 2,
    days_above_springfresh_std_sq = days_above_springfresh_std ^ 2
  ) |>
  filter(!is.na(days_above_springfresh))  # temporary due to incomplete flow data

richness_mod <- brms::brm(
  richness ~ days_above_baseflow_std + days_above_springfresh_std +
    days_above_baseflow_std_sq + days_above_springfresh_std_sq +
    zone * period +
    grazing +
    (1 | waterbody) + 
    (1 | site) +
    (1 | transect) +
    (1 | survey_year) +
    (1 | wpfg),
  # origin = ~ zone + period + grazing,
  # wpfg = ~ days_above_baseflow_std + 
  #   days_above_springfresh_std +
  #   zone * origin * period,
  # species = ~ days_above_baseflow_std + 
  #   days_above_springfresh_std +
  #   zone * origin * period
  data = veg_richness,
  family = poisson(),
  chains = 4,
  cores = 4,
  seed = stan_seed,
  iter = 2000,
  warmup = 1000,
  control = list(adapt_delta = 0.8, max_treedepth = 10),
  backend = "rstan",
  refresh = 100,
  silent = 0,
  threads = brms::threading(3)
)

# TODO: update pre-reg if required, update git for latest model changes
# TODO: assess Bayesian models? (maybe too large; glmmTMB instead?)

# Works quickly, r2 near 0.5
# cover_ar1_mod <- mgcv::gam(
#   hits ~ log_hits_tm1 +
#     s(days_above_baseflow_std) + s(days_above_springfresh_std) +
#     zone * period +
#     grazing + offset(npoint),
#   family = poisson(),
#   data = veg_cover_ar |> filter(wpfg %in% c("ATl", "Sk", "ARp"))
# )

Cov_event_mod <- gam(plant_hits/point_samples ~ 
                       Year*Period*Zone*Origin +  	# fixed effects for year (1-4), period (before/after), treatment zone (bank elev), and origin (native/exotic) and their interactions
                       Grazing + 			# Fixed effect of grazing (binary, pres/absent)
                       s(Transect, bs='re') +		# Random effect for Transect
                       s(Site, bs='re'),      		# Random effect for Site
                     #s(System, bs='re') +		# Random effect for System, not to be implemented in pilot analysis
                     data=Data_springfresh_subset, family=binomial("logit"), method='REML' )


## DISPLAY OUTPUTS IN NEAT WAY.


