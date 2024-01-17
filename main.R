# Analysis of VEFMAP vegetation monitoring data

# See README.md for details of the VEFMAP program and this project and see
#    analysis/preregistration_template.Rmd for details of the analysis
#    implemented in this script

# Authors: Jian Yen, Elliot Gould, Henry Wootton
# Contact: jdl.yen [at] gmail.com

# Date created: 16 November 2023
# Last updated: 10 January 2024

# Clear environment
rm(list = ls()) 

# renv notes:
#  Check renv is up to date with renv::status()
#  Whenever changing packages, make sure the renv lockfile is updated
#     with renv::snapshot()

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
library(lme4)
library(mgcv)
library(performance)
library(ggplot2)
library(glmmTMB)
library(performance)
library(see)
library(patchwork)
library(jtools)
library(parameters) # extract model params for plotting

# load helper functions
source("R/utils.R")
source("R/data.R")

# load site info
# TODO: work out difference calculations to align true elev with flow elev
site_info <- load_metadata(recompile = FALSE)

# read in veg survey data and species information for
#   pilot data set (Campaspe River)
veg_richness <- load_richness(system = "Campaspe", pilot = TRUE, recompile = FALSE)
veg_cover_ar <- load_cover(system = "Campaspe", pilot = TRUE, recompile = FALSE, ar = TRUE)

# take a quick look at the data
summary(veg_richness)
unique(veg_richness$waterbody)
#[1] "Campaspe"
unique(veg_richness$site)
#[1] "Bryants"     "Campbells"   "Doaks"       "English"     "Spencer"     "Strathallan"
unique(veg_richness$transect)
#[1] "1"  "10" "2"  "3"  "4"  "5"  "6"  "7"  "8"  "9" 
unique(veg_richness$period)
#[1] "after_spring"  "after_summer"  "before_spring"
unique(veg_richness$origin)
#[1] "exotic"  "native"  "unknown"



## IGNORE for now, JY to follow up
# TODO: check missing species
# veg |> 
#   distinct(wpfg, species) |> 
#   arrange(wpfg, species) |>
#   mutate(include = ifelse(wpfg %in% wpfg_list, TRUE, FALSE))

# TODO: sum cover over all species within each wpfg
veg_cover_ar_sum <- veg_cover_ar |>
  group_by(
    waterbody, site, transect, metres, survey, survey_year,
    period, origin, wpfg
  ) |>
  summarise(
    hits = sum(hits),
    npoint = unique(npoint),  # each point can have multiple overlapping veg records,
                              #   so it potentially makes more sense to treat it as
                              #   40 points with possibility of > 100% cover.
    hits_tm1 = sum(hits_tm1),
    npoint_tm1 = unique(npoint_tm1)
  )


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
veg_cover_ar_sum <- veg_cover_ar_sum |>
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

## IGNORE FOR NOW
##    BUILD MODELS BY ZONE, EVENT (surveys 1 and 2 and pre/post spring event
##           and 3 and 4 are pre/post summer event), with other factors
## HITS out of 40, but two obs exceed 40 (work out what to do with these)

## IGNORE FOR NOW
## TODO: consider including exotic cover as a predictor OR include
##   random int/slopes for origin and look at correlations to assess
##   how natives and exotics interact

## TODO: generate npoint_tm1 variable to calculate correct log_pr_cover_tm1 variable - also talk to Jian about log(+1 of this var)
# standardise predictors and remove rows with missing flow info - note we need to add a small value to everything? 

veg_cover_ar$days_above_baseflow_std <- scale(veg_cover_ar$days_above_baseflow)[, 1]
veg_cover_ar$days_above_springfresh_std <- scale(veg_cover_ar$days_above_springfresh)[, 1]

veg_cover_ar <- veg_cover_ar |>
  mutate(
    pr_cover = hits/npoint,
    log_hits_tm1 = log(hits_tm1 + 1),
    #log_pr_cover_tm1 = log((hits_tm1/npoint) + 1), 
   # days_above_baseflow_std = scale(days_above_baseflow)[, 1],
   # days_above_springfresh_std = scale(days_above_springfresh)[, 1],
    days_above_baseflow_std_sq = days_above_baseflow_std ^ 2,
    days_above_springfresh_std_sq = days_above_springfresh_std ^ 2
  ) |>
  filter(!is.na(days_above_springfresh))  # temporary due to incomplete flow data

veg_cover_ar_sum$days_above_baseflow_std <- scale(veg_cover_ar_sum$days_above_baseflow)[, 1]
veg_cover_ar_sum$days_above_springfresh_std <- scale(veg_cover_ar_sum$days_above_springfresh)[, 1]

veg_cover_ar_sum <- veg_cover_ar_sum |>
  mutate(
    pr_cover = hits/npoint,
    log_hits_tm1 = log(hits_tm1 + 1),
    #log_pr_cover_tm1 = log((hits_tm1/npoint) + 1), 
   # days_above_baseflow_std = scale(days_above_baseflow)[, 1],
   # days_above_springfresh_std = scale(days_above_springfresh)[, 1],
    days_above_baseflow_std_sq = days_above_baseflow_std ^ 2,
    days_above_springfresh_std_sq = days_above_springfresh_std ^ 2
  ) |>
  filter(!is.na(days_above_springfresh))  # temporary due to incomplete flow data

# find the minimum proportion cover score for each dataset

min(veg_cover_ar$pr_cover[veg_cover_ar$pr_cover > 0])
min(veg_cover_ar_sum$pr_cover[veg_cover_ar_sum$pr_cover > 0])

# add half this minumum value to all repsonse values as an added constant for use in lognormal model (as per JY methodology)

veg_cover_ar <- veg_cover_ar |>
  mutate(
    pr_cover_tf = pr_cover + (.025*.5),
    log_pr_cover_tm1_tf = log((hits_tm1/npoint_tm1) + (.025*.5)), 
  )

veg_cover_ar_sum <- veg_cover_ar_sum |>
  mutate(
    pr_cover_tf = pr_cover + (.025*.5),
    log_pr_cover_tm1_tf = log((hits_tm1/npoint_tm1) + (.025*.5)), 
  )

# look at this data - come back to this its hard to even look at

unique(veg_richness$zone)
#[1] "baseflow_to_springfresh" "above_springfresh"       "below_baseflow"   

hist(veg_cover_ar$hits)
plot(density(veg_cover_ar$hits))

hist(veg_cover_ar_sum$hits)
plot(density(veg_cover_ar_sum$hits))

hist(veg_cover_ar$pr_cover_tf)
plot(density(veg_cover_ar$pr_cover_tf))

hist(veg_cover_ar_sum$pr_cover_tf)
plot(density(veg_cover_ar_sum$pr_cover_tf))

veg_cover_ar_sum %>% 
  group_by(wpfg, site, origin, period, metres, transect, survey) %>%
  summarise(no_rows = length(hits)) %>% print(n=50)

ggplot(veg_cover_ar_sum, aes(x = metres, y = hits, group = wpfg, colour = site) ) + geom_point() + facet_grid(.~period)
ggplot(veg_cover_ar_sum[which(veg_cover_ar_sum$metres == 0),], aes(x = period, y = pr_cover_tf, group = wpfg, colour = site) ) + geom_line() + facet_grid(wpfg~transect)



## Some errors caused by categeroies with all 0 or all 1 values
## Need to remove these if they occur.
## http://bbolker.github.io/mixedmodels-misc/glmmFAQ.html#penalizationhandling-complete-separation

# GENERAL MODEL STRUCTURE BASED ON CHRIS'S INITIAL MODEL
# Cov_event_mod <- gam(plant_hits/point_samples ~ 
#                        Year*Period*Zone*Origin +  	# fixed effects for year (1-4), period (before/after), treatment zone (bank elev), and origin (native/exotic) and their interactions
#                        Grazing + 			# Fixed effect of grazing (binary, pres/absent)
#                        s(Transect, bs='re') +		# Random effect for Transect
#                        s(Site, bs='re'),      		# Random effect for Site
#                      #s(System, bs='re') +		# Random effect for System, not to be implemented in pilot analysis
#                      data=Data_springfresh_subset, family=binomial("logit"), method='REML' )
# 
# JY RECOMMENDATION: build up to this from a simple base, don't try really high
#    level interactions to start
#
# For richness, consider flow metrics, zone, period, grazing, origin, wpfg,
#    maybe work towards zone * period interactions and zone * flow_metrics
#    interactions with random effects for waterbody, site, transect, survey_year
# 
# For cover, use a similar structure but add hits_tm1 as an additional fixed predictor
#    to set up an autoregressive structure (see examples below)
#
# Consider TMBB models, or whatever works best given data amount
#
# Model distributions:
#   1. Richness: start with Poisson, consider Negative Binomial or zero-inflated Poisson if issues
#   2. Cover (autoregressive): consider Poisson with offset(npoint) (shown below),
#        but could also calculate proportional cover (hits / npoint) and use
#        a zero-inflated lognormal. Need to include hits_tm1 on the link scale 
#        in the same model (see below, becomes log_hits_tm1 for poisson, would be
#        log_proportional_cover_tm1 in lognormal model)

# GLMER version (linear mixed model)
cover_ar1_mod <- lme4::glmer(
  hits ~ log_hits_tm1 +
    days_above_baseflow_std + days_above_springfresh_std +
    days_above_baseflow_std_sq + days_above_springfresh_std_sq +
#    zone * period +
    zone + period +
    grazing +
    offset(npoint) +
    # (1 | waterbody) + # only one for now
    # (1 | site) +
    # (1 | transect) +
    # (1 | survey_year) +
    (1 | wpfg),
  # origin = ~ zone + period + grazing,
  # wpfg = ~ days_above_baseflow_std + 
  #   days_above_springfresh_std +
  #   zone * origin * period,
  # species = ~ days_above_baseflow_std + 
  #   days_above_springfresh_std +
  #   zone * origin * period
  family = poisson(),
  data = veg_cover_ar #|> filter(wpfg %in% c("ATl", "Sk", "ARp"))
)

# GAMM version (additive mixed model)
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

# above is Chris' and Jian's model structures. Lets first attempt to fit simple models 
# and build complexity in once we start to understand how the models are behaving.

# we will first fit a zero inflated lognormal model. This will be proportional cover
# (hits/npoints) where some values are greater than 100% cover due to 
# The model needs a autoregressive fixed effect on the link scale of cover in the previous timestep. 

# lets first fit a series of additive models using the glmmTMB package. 
# this allows us to easily view model fit diagnostics using the xx function.

summary(veg_cover_ar_sum)

# glmmTMB version (linear mixed model)
# note that fitting lognormal models of proportionate cover performed poorly so we instead move towards poisson or negative binomial models
# these include an offset of npoints to account for the number of points (effectivly becomes a proportion response?)

# lets fit a simple model without fixed effects to look at fit of random effects
# poisson with simple zero inflation formula
cover_ar1_TMBmod <- glmmTMB::glmmTMB(
  hits ~ log_hits_tm1 +
  #  days_above_baseflow_std + days_above_springfresh_std +
   # days_above_baseflow_std_sq + days_above_springfresh_std_sq +
    #    zone * period +
  #  zone + period +
  #  grazing +
    (1 | site / transect) +
    (1 | site / period) +
    (1 | metres) +
    (1 | survey_year) +
    offset(npoint),
  family = poisson(),
  ziformula=~1,
  data = veg_cover_ar_sum #|> filter(wpfg %in% c("ATl", "Sk", "ARp"))
)

summary(cover_ar1_TMBmod)

# old school model fit diagnostic plots

plot(fitted(cover_ar1_TMBmod), cover_ar1_TMBmod$frame$hits)
plot(fitted(cover_ar1_TMBmod) + 1, cover_ar1_TMBmod$frame$hits + 1, log = "xy")

# use performance package to test model predictions - ignore homogeneity of variance and normality of residuals

check_model(cover_ar1_TMBmod)

check_predictions(cover_ar1_TMBmod) 
# looks ok

check_collinearity(cover_ar1_TMBmod)
# no predictors

check_overdispersion(cover_ar1_TMBmod)
# massively overdispersed

check_zeroinflation(cover_ar1_TMBmod)
# underfitting zeros

check_singularity(cover_ar1_TMBmod)
# false

# now try a model with a different zformula. This one uses a zone by functional group interaction (accounts for differing amounts of zeros per function group per zone)

cover_ar2_TMBmod <- glmmTMB::glmmTMB(
  hits ~ log_hits_tm1 +
    #  days_above_baseflow_std + days_above_springfresh_std +
    # days_above_baseflow_std_sq + days_above_springfresh_std_sq +
    #    zone * period +
    #  zone + period +
    #  grazing +
    (1 | site / transect) +
    (1 | site / period) +
    (1 | metres) +
    (1 | survey_year) +
    offset(npoint),
  family = poisson(),
  ziformula=~wpfg*zone,
  data = veg_cover_ar_sum #|> filter(wpfg %in% c("ATl", "Sk", "ARp"))
)

summary(cover_ar2_TMBmod)
# old school model fit diagnostic plots

plot(fitted(cover_ar2_TMBmod), cover_ar2_TMBmod$frame$hits)
plot(fitted(cover_ar2_TMBmod) + 1, cover_ar2_TMBmod$frame$hits + 1, log = "xy")

# use performance package to test model predictions - ignore homogeneity of variance and normality of residuals

check_model(cover_ar2_TMBmod)

check_predictions(cover_ar2_TMBmod) 
# looks even better

check_collinearity(cover_ar2_TMBmod)
# no predictors yet

check_overdispersion(cover_ar2_TMBmod)
# doesnt work but looks overdispersed from the check_model call

check_zeroinflation(cover_ar2_TMBmod)
# better but still underfitting zeros

check_singularity(cover_ar2_TMBmod)
# singular

# now try a model with a different zformula. This one uses random effects in the z formula which make it more flexible

cover_ar3_TMBmod <- glmmTMB::glmmTMB(
  hits ~ log_hits_tm1 +
    #  days_above_baseflow_std + days_above_springfresh_std +
    # days_above_baseflow_std_sq + days_above_springfresh_std_sq +
    #    zone * period +
    #  zone + period +
    #  grazing +
    (1 | site / transect) +
    (1 | site / period) +
    (1 | metres) +
    (1 | survey_year) +
    offset(npoint),
  family = poisson(),
  ziformula=~ (1 | site) + (1 | site:transect) + wpfg * zone,
  data = veg_cover_ar_sum #|> filter(wpfg %in% c("ATl", "Sk", "ARp"))
)

summary(cover_ar3_TMBmod)
# old school model fit diagnostic plots

plot(fitted(cover_ar3_TMBmod), cover_ar3_TMBmod$frame$hits)
plot(fitted(cover_ar3_TMBmod) + 1, cover_ar3_TMBmod$frame$hits + 1, log = "xy")

# use performance package to test model predictions - ignore homogeneity of variance and normality of residuals

check_model(cover_ar3_TMBmod)

check_predictions(cover_ar3_TMBmod) 
# looks ok again

check_collinearity(cover_ar3_TMBmod)
# no predictors yet

check_overdispersion(cover_ar3_TMBmod)
# doenst work but above plot looks overdispersed

check_zeroinflation(cover_ar3_TMBmod)
# underfitting zeros

check_singularity(cover_ar3_TMBmod)
# singular

# now lets try fitting models with negative binomial dispersion parameters. This should deal better with overdispersion. 
# simple z formula
cover_ar4_TMBmod <- glmmTMB::glmmTMB(
  hits ~ log_hits_tm1 +
    #  days_above_baseflow_std + days_above_springfresh_std +
    # days_above_baseflow_std_sq + days_above_springfresh_std_sq +
    #    zone * period +
    #  zone + period +
    #  grazing +
    (1 | site / transect) +
    (1 | site / period) +
    (1 | metres) +
    (1 | survey_year) +
    offset(npoint),
  family = nbinom2,
  ziformula=~ 1 ,
  data = veg_cover_ar_sum #|> filter(wpfg %in% c("ATl", "Sk", "ARp"))
)

summary(cover_ar4_TMBmod)
# old school model fit diagnostic plots

plot(fitted(cover_ar4_TMBmod), cover_ar4_TMBmod$frame$hits)
plot(fitted(cover_ar4_TMBmod) + 1, cover_ar4_TMBmod$frame$hits + 1, log = "xy")
# looks to be overpredicting cover, esp at small values

# use performance package to test model predictions - ignore homogeneity of variance and normality of residuals

check_model(cover_ar4_TMBmod)

check_predictions(cover_ar4_TMBmod) 
# ok but was better before

check_collinearity(cover_ar4_TMBmod)
# no predictors yet

check_overdispersion(cover_ar4_TMBmod)
# overdispersed

check_zeroinflation(cover_ar4_TMBmod)
# does ok

check_singularity(cover_ar4_TMBmod)
# not singular

# zformula with interaction of functional group and zone
cover_ar5_TMBmod <- glmmTMB::glmmTMB(
  hits ~ log_hits_tm1 +
    #  days_above_baseflow_std + days_above_springfresh_std +
    # days_above_baseflow_std_sq + days_above_springfresh_std_sq +
    #    zone * period +
    #  zone + period +
    #  grazing +
    (1 | site / transect) +
    (1 | site / period) +
    (1 | metres) +
    (1 | survey_year) +
    offset(npoint),
  family = nbinom2,
  ziformula=~ wpfg*zone ,
  data = veg_cover_ar_sum #|> filter(wpfg %in% c("ATl", "Sk", "ARp"))
)

summary(cover_ar5_TMBmod)
# convergence issues

# old school model fit diagnostic plots

plot(fitted(cover_ar5_TMBmod), cover_ar5_TMBmod$frame$hits)
plot(fitted(cover_ar5_TMBmod) + 1, cover_ar5_TMBmod$frame$hits + 1, log = "xy")
# looks to be overpredicting cover, esp at small values

# use performance package to test model predictions - ignore homogeneity of variance and normality of residuals

check_model(cover_ar5_TMBmod)

check_predictions(cover_ar5_TMBmod) 
# ok but was better before

check_collinearity(cover_ar5_TMBmod)
# no predictors yet

check_overdispersion(cover_ar5_TMBmod)
# error and plot above looks weird

check_zeroinflation(cover_ar5_TMBmod)
# does ok

check_singularity(cover_ar5_TMBmod)
# not singular

# zformula with interaction of functional group and zone, as well as zformula random effects 
cover_ar6_TMBmod <- glmmTMB::glmmTMB(
  hits ~ log_hits_tm1 +
    #  days_above_baseflow_std + days_above_springfresh_std +
    # days_above_baseflow_std_sq + days_above_springfresh_std_sq +
    #    zone * period +
    #  zone + period +
    #  grazing +
    (1 | site / transect) +
    (1 | site / period) +
    (1 | metres) +
    (1 | survey_year) +
    offset(npoint),
  family = nbinom2,
  ziformula=~ (1 | site) + (1 | site:transect) + wpfg*zone ,
  dispformula = ~ wpfg,
  data = veg_cover_ar_sum #|> filter(wpfg %in% c("ATl", "Sk", "ARp"))
)

summary(cover_ar6_TMBmod)
# old school model fit diagnostic plots

plot(fitted(cover_ar6_TMBmod), cover_ar6_TMBmod$frame$hits)
plot(fitted(cover_ar6_TMBmod) + 1, cover_ar6_TMBmod$frame$hits + 1, log = "xy")
# looks to be overpredicting cover, esp at small values

# use performance package to test model predictions - ignore homogeneity of variance and normality of residuals

check_model(cover_ar6_TMBmod)

check_predictions(cover_ar6_TMBmod) 
# ok but was better before

check_collinearity(cover_ar6_TMBmod)
# no predictors yet

check_overdispersion(cover_ar6_TMBmod)
# doenst work and above looked weird

check_zeroinflation(cover_ar6_TMBmod)
# does ok

check_singularity(cover_ar6_TMBmod)
# not singular

# lets try some more zformulas
# first a couple of simple ones

# zformula of functional group alone
cover_ar7_TMBmod <- glmmTMB::glmmTMB(
  hits ~ log_hits_tm1 +
    #  days_above_baseflow_std + days_above_springfresh_std +
    # days_above_baseflow_std_sq + days_above_springfresh_std_sq +
    #    zone * period +
    #  zone + period +
    #  grazing +
    (1 | site / transect) +
    (1 | site / period) +
    (1 | metres) +
    (1 | survey_year) +
    offset(npoint),
  family = nbinom2,
  ziformula=~ wpfg,
  data = veg_cover_ar_sum #|> filter(wpfg %in% c("ATl", "Sk", "ARp"))
)

summary(cover_ar7_TMBmod)
# old school model fit diagnostic plots

plot(fitted(cover_ar7_TMBmod), cover_ar7_TMBmod$frame$hits)
plot(fitted(cover_ar7_TMBmod) + 1, cover_ar7_TMBmod$frame$hits + 1, log = "xy")
# looks to be overpredicting cover, esp at small values

# use performance package to test model predictions - ignore homogeneity of variance and normality of residuals

check_model(cover_ar7_TMBmod)

check_predictions(cover_ar7_TMBmod) 
# ok but was better before

check_collinearity(cover_ar7_TMBmod)
# no predictors yet

check_overdispersion(cover_ar7_TMBmod)
# doenst work and above looked weird

check_zeroinflation(cover_ar7_TMBmod)
# does ok

check_singularity(cover_ar7_TMBmod)
# singular

# zformula of zone alone
cover_ar8_TMBmod <- glmmTMB::glmmTMB(
  hits ~ log_hits_tm1 +
    #  days_above_baseflow_std + days_above_springfresh_std +
    # days_above_baseflow_std_sq + days_above_springfresh_std_sq +
    #    zone * period +
    #  zone + period +
    #  grazing +
    (1 | site / transect) +
    (1 | site / period) +
    (1 | metres) +
    (1 | survey_year) +
    offset(npoint),
  family = nbinom2,
  ziformula=~ zone,
  data = veg_cover_ar_sum #|> filter(wpfg %in% c("ATl", "Sk", "ARp"))
)

summary(cover_ar8_TMBmod)
# old school model fit diagnostic plots

plot(fitted(cover_ar8_TMBmod), cover_ar8_TMBmod$frame$hits)
plot(fitted(cover_ar8_TMBmod) + 1, cover_ar8_TMBmod$frame$hits + 1, log = "xy")
# looks to be overpredicting cover, esp at small values

# use performance package to test model predictions - ignore homogeneity of variance and normality of residuals

check_model(cover_ar8_TMBmod)

check_predictions(cover_ar8_TMBmod) 
# ok but was better before

check_collinearity(cover_ar8_TMBmod)
# no predictors yet

check_overdispersion(cover_ar8_TMBmod)
# overdispersed

check_zeroinflation(cover_ar8_TMBmod)
# does ok

check_singularity(cover_ar8_TMBmod)
# not singular

# zformula of additive functional group and zone
cover_ar9_TMBmod <- glmmTMB::glmmTMB(
  hits ~ log_hits_tm1 +
    #  days_above_baseflow_std + days_above_springfresh_std +
    # days_above_baseflow_std_sq + days_above_springfresh_std_sq +
    #    zone * period +
    #  zone + period +
    #  grazing +
    (1 | site / transect) +
    (1 | site / period) +
    (1 | metres) +
    (1 | survey_year) +
    offset(npoint),
  family = nbinom2,
  ziformula=~ wpfg + zone,
  data = veg_cover_ar_sum #|> filter(wpfg %in% c("ATl", "Sk", "ARp"))
)

summary(cover_ar9_TMBmod)
# old school model fit diagnostic plots

plot(fitted(cover_ar9_TMBmod), cover_ar9_TMBmod$frame$hits)
plot(fitted(cover_ar9_TMBmod) + 1, cover_ar9_TMBmod$frame$hits + 1, log = "xy")
# looks to be overpredicting cover, esp at small values

# use performance package to test model predictions - ignore homogeneity of variance and normality of residuals

check_model(cover_ar9_TMBmod)

check_predictions(cover_ar9_TMBmod) 
# ok but was better before

check_collinearity(cover_ar9_TMBmod)
# no predictors yet

check_overdispersion(cover_ar9_TMBmod)
# doenst work and above didnt look great but better than before

check_zeroinflation(cover_ar9_TMBmod)
# does ok

check_singularity(cover_ar9_TMBmod)
# singular

# zformula of additive functional group and zone and ziformula random effects
cover_ar10_TMBmod <- glmmTMB::glmmTMB(
  hits ~ log_hits_tm1 +
    #  days_above_baseflow_std + days_above_springfresh_std +
    # days_above_baseflow_std_sq + days_above_springfresh_std_sq +
    #    zone * period +
    #  zone + period +
    #  grazing +
    (1 | site / transect) +
    (1 | site / period) +
    (1 | metres) +
    (1 | survey_year) +
    offset(npoint),
  family = nbinom2,
  ziformula=~ (1 | site) + (1 | site:transect) + wpfg + zone,
  data = veg_cover_ar_sum #|> filter(wpfg %in% c("ATl", "Sk", "ARp"))
)

summary(cover_ar10_TMBmod)
# lots of convergence issues

# old school model fit diagnostic plots

plot(fitted(cover_ar10_TMBmod), cover_ar10_TMBmod$frame$hits)
plot(fitted(cover_ar10_TMBmod) + 1, cover_ar10_TMBmod$frame$hits + 1, log = "xy")
# looks to be overpredicting cover, esp at small values

# use performance package to test model predictions - ignore homogeneity of variance and normality of residuals

check_model(cover_ar10_TMBmod)

check_predictions(cover_ar10_TMBmod) 
# that just made it worse

check_collinearity(cover_ar10_TMBmod)
# no predictors yet

check_overdispersion(cover_ar10_TMBmod)
# overdispersed

check_zeroinflation(cover_ar10_TMBmod)
# does ok

check_singularity(cover_ar10_TMBmod)
# singular









# model with wpfg as a fixed effect, and random effects of site, transect an survey_year
# flow metrics, zone, period, grazing, origin, wpfg,

cover_ar4_TMBmod <- glmmTMB::glmmTMB(
  pr_cover_tf ~ log_pr_cover_tm1_tf +
    days_above_baseflow_std + days_above_springfresh_std +
    days_above_baseflow_std_sq + #days_above_springfresh_std_sq +
    #    zone * period +
    zone + period +
    grazing + wpfg + origin +
    # (1 | waterbody) + # only one for now
     (1 | site) +
     (1 | transect) +
     (1 | survey_year) ,
    #(1 | wpfg),
  family = lognormal(),
  ziformula=~zone,
  data = veg_cover_ar_sum |> filter(wpfg %in% c("ATl", "Sk", "ARp"))
)

summary(cover_ar4_TMBmod) # something wrong here
# old school model fit diagnostic plots

plot(fitted(cover_ar1_TMBmod), cover_ar1_TMBmod$frame$hits)
plot(fitted(cover_ar1_TMBmod) + 1, cover_ar1_TMBmod$frame$hits + 1, log = "xy")

# use performance package to test model predictions - ignore homogeneity of variance and normality of residuals

check_model(cover_ar4_TMBmod)

check_predictions(cover_ar4_TMBmod) 
# doesnt work

check_collinearity(cover_ar4_TMBmod)

check_overdispersion(cover_ar4_TMBmod)
# needs to be poisson family

check_zeroinflation(cover_ar4_TMBmod)
# needs to be poisson or binomial family

check_singularity(cover_ar4_TMBmod)

# now fit with interactive ziformula of zone and wpfg
cover_ar5_TMBmod <- glmmTMB::glmmTMB(
  pr_cover_tf ~ log_pr_cover_tm1_tf +
    days_above_baseflow_std + days_above_springfresh_std +
    days_above_baseflow_std_sq + #days_above_springfresh_std_sq +
    #    zone * period +
    zone + period +
    grazing + wpfg + origin +
    # (1 | waterbody) + # only one for now
    (1 | site) +
    (1 | transect) +
    (1 | survey_year) ,
  #(1 | wpfg),
  family = lognormal(),
  ziformula=~ (1 | site) + (1 | site:transect) + zone*wpfg,
  data = veg_cover_ar_sum |> filter(wpfg %in% c("ATl", "Sk", "ARp"))
)

summary(cover_ar5_TMBmod) 
# old school model fit diagnostic plots

plot(fitted(cover_ar1_TMBmod), cover_ar1_TMBmod$frame$hits)
plot(fitted(cover_ar1_TMBmod) + 1, cover_ar1_TMBmod$frame$hits + 1, log = "xy")

# now fit with Poisson response
cover_ar_jian_TMBmod <- glmmTMB::glmmTMB(
  hits ~ log_hits_tm1 +
    # days_above_baseflow_std + days_above_springfresh_std +
    # days_above_baseflow_std_sq + #days_above_springfresh_std_sq +
    #    zone * period +
    # zone + period +
    # grazing + wpfg + origin +
    # (1 | waterbody) + # only one for now
    (1 | site / transect) +
    (1 | site / period) +
    (1 | metres) +
    (1 | survey_year) +
    offset(npoint),
  #(1 | wpfg),
  family = poisson(), # negbin1()/negbin2(),
  ziformula=~ (1 | site) + (1 | site:transect) + zone*wpfg,
  data = veg_cover_ar_sum #|> filter(wpfg %in% c("ATl", "Sk", "ARp"))
)
plot(fitted(cover_ar_jian_TMBmod), cover_ar_jian_TMBmod$frame$hits)
plot(fitted(cover_ar_jian_TMBmod) + 1, cover_ar_jian_TMBmod$frame$hits + 1, log = "xy")
check_model(cover_ar_jian_TMBmod)

# predict(cover_ar_jian_TMBmod, newdata = data.frame(TO_BE_SPECIFIED))

## COULD try binomial family (response is binomial (count) with npoint trials)

# use performance package to test model predictions - ignore homogeneity of variance and normality of residuals

check_model(cover_ar5_TMBmod)

check_predictions(cover_ar5_TMBmod) 
# doesnt work

check_collinearity(cover_ar5_TMBmod)

check_overdispersion(cover_ar5_TMBmod)
# needs to be poisson family

check_zeroinflation(cover_ar5_TMBmod)
# needs to be poisson or binomial family

check_singularity(cover_ar5_TMBmod)

# now fit with additive ziformula of zone and wpfg
cover_ar6_TMBmod <- glmmTMB::glmmTMB(
  pr_cover_tf ~ log_pr_cover_tm1_tf +
    days_above_baseflow_std + days_above_springfresh_std +
    days_above_baseflow_std_sq + #days_above_springfresh_std_sq +
    #    zone * period +
    zone + period +
    grazing + wpfg + origin +
    # (1 | waterbody) + # only one for now
    (1 | site) +
    (1 | transect) +
    (1 | survey_year) ,
  #(1 | wpfg),
  family = lognormal(),
  ziformula=~zone+wpfg,
  data = veg_cover_ar_sum |> filter(wpfg %in% c("ATl", "Sk", "ARp"))
)

summary(cover_ar6_TMBmod) # something wrong here

# use performance package to test model predictions - ignore homogeneity of variance and normality of residuals

check_model(cover_ar6_TMBmod)

check_predictions(cover_ar6_TMBmod) 
# doesnt work

check_collinearity(cover_ar6_TMBmod)

check_overdispersion(cover_ar6_TMBmod)
# needs to be poisson family

check_zeroinflation(cover_ar6_TMBmod)
# needs to be poisson or binomial family

check_singularity(cover_ar6_TMBmod)

# play with model predictions

effect_plot(cover_ar6_TMBmod, pred = days_above_baseflow_std  , interval = TRUE, partial.residuals = TRUE)

effect_plot(cover_ar6_TMBmod, pred = wpfg  , interval = TRUE, partial.residuals = TRUE)

effect_plot(cover_ar6_TMBmod, pred = period  , interval = TRUE, partial.residuals = TRUE)



# try really simplifying into a few fixed effects
cover_ar7_TMBmod <- glmmTMB::glmmTMB(
  pr_cover_tf ~ log_pr_cover_tm1_tf +
 #   days_above_baseflow_std + days_above_springfresh_std +
  #  days_above_baseflow_std_sq + #days_above_springfresh_std_sq +
    #    zone * period +
    zone + period + wpfg +
   # grazing + wpfg + origin +
    # (1 | waterbody) + # only one for now
    (1 | site) +
    (1 | transect) +
    (1 | survey_year) ,
  #(1 | wpfg),
  family = lognormal(),
  ziformula=~zone+wpfg,
  data = veg_cover_ar_sum |> filter(wpfg %in% c("ATl", "Sk", "ARp"))
)

summary(cover_ar7_TMBmod) # something wrong here

# use performance package to test model predictions - ignore homogeneity of variance and normality of residuals

check_model(cover_ar7_TMBmod)

check_predictions(cover_ar7_TMBmod) 
# doesnt work

check_collinearity(cover_ar7_TMBmod)

check_overdispersion(cover_ar7_TMBmod)
# needs to be poisson family

check_zeroinflation(cover_ar7_TMBmod)
# needs to be poisson or binomial family

check_singularity(cover_ar7_TMBmod)

# play with model predictions

plot(model_parameters(cover_ar7_TMBmod))

effect_plot(cover_ar7_TMBmod, pred = zone  , interval = TRUE, partial.residuals = TRUE)

effect_plot(cover_ar7_TMBmod, pred = wpfg  , interval = TRUE, partial.residuals = TRUE)

effect_plot(cover_ar7_TMBmod, pred = period  , interval = TRUE, partial.residuals = TRUE)

# plot raw data 





# now lets model vegetation richness

veg_richness <- veg_richness |>
  mutate(
    days_above_baseflow_std = scale(days_above_baseflow)[, 1],
    days_above_springfresh_std = scale(days_above_springfresh)[, 1],
    days_above_baseflow_std_sq = days_above_baseflow_std ^ 2,
    days_above_springfresh_std_sq = days_above_springfresh_std ^ 2
  ) |>
  filter(!is.na(days_above_springfresh))  # temporary due to incomplete flow data


## DON'T DO THIS ONE, WILL BE TOO SLOW, left here so you can see model
##   structure in possibly closer-to-TMBB syntax
# richness_mod <- brms::brm(
#   richness ~ days_above_baseflow_std + days_above_springfresh_std +
#     days_above_baseflow_std_sq + days_above_springfresh_std_sq +
#     zone * period +
#     grazing +
#     (1 | waterbody) + 
#     (1 | site) +
#     (1 | transect) +
#     (1 | survey_year) +
#     (1 | wpfg),
#   # origin = ~ zone + period + grazing,
#   # wpfg = ~ days_above_baseflow_std + 
#   #   days_above_springfresh_std +
#   #   zone * origin * period,
#   # species = ~ days_above_baseflow_std + 
#   #   days_above_springfresh_std +
#   #   zone * origin * period
#   data = veg_richness,
#   family = poisson(),
#   chains = 4,
#   cores = 4,
#   seed = stan_seed,
#   iter = 2000,
#   warmup = 1000,
#   control = list(adapt_delta = 0.8, max_treedepth = 10),
#   backend = "rstan",
#   refresh = 100,
#   silent = 0,
#   threads = brms::threading(3)
# )

# IGNORE FOR NOW
# TODO: update pre-reg if required, update git for latest model changes

# Fall-back option: GAM without random terms
# Works quickly, r2 near 0.5
# cover_ar1_mod <- mgcv::gam(
#   hits ~ log_hits_tm1 +
#     s(days_above_baseflow_std) + s(days_above_springfresh_std) +
#     zone * period +
#     grazing + offset(npoint),
#   family = poisson(),
#   data = veg_cover_ar |> filter(wpfg %in% c("ATl", "Sk", "ARp"))
# )


## TODO: update pre-reg doc based on final model structure
## TODO: run for all systems (JY to assist with data cleaning)
## TODO: generate outputs for reporting:
##    1. estimates of flow effects or zone/period effects
##    2. plots of veg richness and cover as a function of zone/period/transect
