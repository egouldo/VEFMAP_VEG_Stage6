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
library(interactions)
library(ggforce)
library(effects)
library(ggtext)
if (!"mixedup" %in% installed.packages()) withr::with_envvar(c(R_REMOTES_NO_ERRORS_FROM_WARNINGS = "true"), remotes::install_github('m-clark/mixedup'))
library(mixedup)
library(brms)
library(rstanarm)
library(assertthat)
library(purrr)


# load helper functions
source("R/utils.R")
source("R/data.R")

# load site info
# TODO: work out difference calculations to align true elev with flow elev
site_info <- load_metadata(recompile = T)

# pilot analysis Campaspe ####

# read in veg survey data and species information for
#   pilot data set (Campaspe River)
#debugonce(load_richness)
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

## IGNORE for now, JY to follow up - see also issue #53 on GitHub `gh issue view 53 -w`
# TODO: check missing species in .wpfg_list

veg_cover_ar %>% 
  pointblank::col_vals_in_set(columns = wpfg, 
                              set = .wpfg_list, 
                              actions = 
                                pointblank::action_levels(
                                  warn_at = 1,
                                  fns = list(warn = ~ 
                                               cli_warn("Some species with no `wpfg` in `.wpfg_list`"))))

wpfg_spp_missing <- 
  veg_cover_ar |>
  distinct(wpfg, species) |>
  mutate(include = ifelse(wpfg %in% .wpfg_list, TRUE, FALSE)) %>% 
  arrange(wpfg, species) |>
  filter(is.na(wpfg) | include == FALSE) # species with no wpfg in .wpfg_list or NA


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
  ) %>% 
  pointblank::col_vals_equal(npoint, 40) # validate npoint == 40

# check the values of hits

summary(veg_cover_ar_sum)

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

# create factor that captures differing combinations of plant functional group and origin (native or exotic)

veg_cover_ar$wpfg_ori <- as.factor(paste(veg_cover_ar$wpfg,veg_cover_ar$origin, sep = "_"))

veg_cover_ar_sum$wpfg_ori <- as.factor(paste(veg_cover_ar_sum$wpfg,veg_cover_ar_sum$origin, sep = "_"))

# look at this data 

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

veg_cover_ar_sum %>% 
  group_by(wpfg,  origin) %>%
  summarise(no_rows = length(hits)) %>% print(n=50)


ggplot(veg_cover_ar_sum, aes(x = metres, y = hits, group = wpfg, colour = site) ) + geom_point() + facet_grid(.~period)
ggplot(veg_cover_ar_sum[which(veg_cover_ar_sum$metres == 0),], aes(x = period, y = pr_cover_tf, group = wpfg, colour = site) ) + geom_line() + facet_grid(wpfg~transect)
ggplot(veg_cover_ar_sum, aes(x = days_above_springfresh, y = hits, group = wpfg, colour = wpfg) ) + geom_point() 

# finally create a daraframe of data for plotting that removes nuisance factor levels found in modelling below
## TODO: chase up the 'Atl_native' and 'Ate_native' levels to see if they are typos

Plotdata <- veg_cover_ar_sum |> filter(!wpfg_ori %in% c("Atl_native", "Ate_native", "Tda_unknown"))
Plotdata$zone <- ordered(Plotdata$zone, levels = c( "below_baseflow", "baseflow_to_springfresh", "above_springfresh"))
Plotdata$period <- ordered(Plotdata$period, levels = c( "before_spring", "after_spring", "after_summer"))

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

summary(veg_cover_ar_sum)

# pilot analysis ####
# glmmTMB version (linear mixed model)
# note that fitting lognormal models of proportionate cover performed poorly so we instead move towards poisson or negative binomial models
# these include an offset of npoints to account for the number of points (effectively becomes a proportion response?)

# fitting negative binomial and poisson models led us to adopt a poisson model with a zformula of ~ wpfg. Fitting dispersion parameters 
# did not help the fit so we leave it out. We dont fit an offset of npoint as they are all the same value (40), and fitting it seemed to effect fits. Note also that we exclude two 
# plant functional groups that may be incorrectly specified data (typos). 

sum(veg_cover_ar_sum$hits %in% 0 ) / nrow(veg_cover_ar_sum) # 86% zeros

# lets attempt to fit the hydrology model first -
cover_ar_TMBmod_1 <- glmmTMB::glmmTMB(
  hits ~ log_hits_tm1 +
    days_above_baseflow_std*wpfg*origin + days_above_springfresh_std*wpfg*origin +
   # days_above_baseflow_std^2 + days_above_springfresh_std^2 +
    #   zone * period +
  #  zone + period +
  #  grazing + wpfg  +
    (1 | site / transect) +
    #(1 | site / period) +
    (1 | metres) +
    (1 | survey_year),
  # offset(npoint),
  family = poisson,
  ziformula=~ wpfg,
  #dispformula =~ wpfg ,
  data = veg_cover_ar_sum |> filter(!wpfg_ori %in% c("Atl_native", "Ate_native", "Tda_unknown"))
)


summary(cover_ar_TMBmod_1)
# 

# old school model fit diagnostic plots

plot(fitted(cover_ar_TMBmod_1), cover_ar_TMBmod_1$frame$hits)
plot(fitted(cover_ar_TMBmod_1) + 1, cover_ar_TMBmod_1$frame$hits + 1, log = "xy")
# 

# use performance package to test model predictions - ignore homogeneity of variance and normality of residuals

check_model(cover_ar_TMBmod_1)

model_performance(cover_ar_TMBmod_1)

check_predictions(cover_ar_TMBmod_1) 
# 

check_collinearity(cover_ar_TMBmod_1)
# a problem but to be expected in interactions

check_overdispersion(cover_ar_TMBmod_1)
# still overdispersed

check_zeroinflation(cover_ar_TMBmod_1)
# slightly underfitting but pretty good

check_singularity(cover_ar_TMBmod_1)
# false

# forrest plot of estiamtes

plot(model_parameters(cover_ar_TMBmod_1))

effect_plot(cover_ar_TMBmod_1, pred = log_hits_tm1 , interval = TRUE, partial.residuals = TRUE) + scale_y_continuous(limits=c(0, 60))

effect_plot(cover_ar_TMBmod_1, pred = wpfg , interval = TRUE, partial.residuals = TRUE) + scale_y_continuous(limits=c(0, 60))

effect_plot(cover_ar_TMBmod_1, pred = days_above_baseflow_std , interval = TRUE, partial.residuals = T, plot.points = T) + scale_y_continuous(limits=c(0, 100))

effect_plot(cover_ar_TMBmod_1, pred = days_above_springfresh_std  , interval = TRUE, partial.residuals = TRUE) + scale_y_continuous(limits=c(0, 60))

effect_plot(cover_ar_TMBmod_1, pred = days_above_baseflow_std_sq  , interval = TRUE, partial.residuals = TRUE) + scale_y_continuous(limits=c(0, 60))

interact_plot(cover_ar_TMBmod_1, pred = days_above_baseflow_std, modx = wpfg, interval = T) + scale_y_continuous(limits=c(0, 5))

interact_plot(cover_ar_TMBmod_1, pred = days_above_baseflow_std, modx = wpfg, interval = T, plot.points = T) + scale_y_continuous(limits=c(0, 20))

interact_plot(cover_ar_TMBmod_1, pred = days_above_springfresh_std, modx = wpfg_ori, plot.points = T) + scale_y_continuous(limits=c(0, 20))

# extract and plot model predictions

HydroPredictDaysabovebaseFunc<- as.data.frame(Effect(c('days_above_baseflow_std', 'wpfg'),cover_ar_TMBmod_1,xlevels=20))

c<-mean(veg_cover_ar_sum$days_above_baseflow) 
d<-sd(veg_cover_ar_sum$days_above_baseflow)
HydroPredictDaysabovebaseFunc$days_above_baseflow<-(HydroPredictDaysabovebaseFunc$days_above_baseflow_std*d+c)

HydroPredictDaysabovebaseFunc$hits <- HydroPredictDaysabovebaseFunc$fit

HydroPredictDaysabovebaseFuncPlot<-ggplot(HydroPredictDaysabovebaseFunc, aes(days_above_baseflow, hits, colour = wpfg, group = wpfg)) +
  geom_line(linewidth = 2)+
  geom_ribbon(aes(ymin = lower, ymax = upper, fill= wpfg),  alpha= 0.1)+
  geom_point(data= Plotdata,aes(x=days_above_baseflow, y= hits, colour = wpfg), alpha = 0.2)+
  coord_cartesian(ylim = c(0, 50))+
  labs(x = "Days above baseflow", y = "Hits")+ theme_bw() +# coord_cartesian(ylim = c(0.5, 1)) + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), legend.position = "right") 

HydroPredictDaysabovebaseFuncPlot

HydroPredictDaysabovebaseFuncOri<- as.data.frame(Effect(c('days_above_baseflow_std', 'wpfg','origin'),cover_ar_TMBmod_1,xlevels=20))

c<-mean(veg_cover_ar_sum$days_above_baseflow) 
d<-sd(veg_cover_ar_sum$days_above_baseflow)
HydroPredictDaysabovebaseFuncOri$days_above_baseflow<-(HydroPredictDaysabovebaseFuncOri$days_above_baseflow_std*d+c)

HydroPredictDaysabovebaseFuncOri$hits <- HydroPredictDaysabovebaseFuncOri$fit

HydroPredictDaysabovebaseFuncoriPlot<-ggplot(HydroPredictDaysabovebaseFuncOri, aes(days_above_baseflow, hits, colour = wpfg, group = wpfg)) +
  geom_line(linewidth = 2)+
  #geom_ribbon(aes(ymin = lower, ymax = upper, fill= wpfg),  alpha= 0.1)+
  geom_point(data= Plotdata ,aes(x=days_above_baseflow, y= hits, colour = wpfg), alpha = 0.2)+
  coord_cartesian(ylim = c(0, 50))+
  labs(x = "Days above baseflow", y = "Hits")+ theme_bw() + facet_grid(.~origin) +# coord_cartesian(ylim = c(0.5, 1)) 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), legend.position = "right") 

HydroPredictDaysabovebaseFuncoriPlot


HydroPredictDaysabovespringFunc<- as.data.frame(Effect(c('days_above_springfresh_std', 'wpfg','origin'),cover_ar_TMBmod_1,xlevels=20))
c<-mean(veg_cover_ar_sum$days_above_springfresh) 
d<-sd(veg_cover_ar_sum$days_above_springfresh)
HydroPredictDaysabovespringFunc$days_above_springfresh<-(HydroPredictDaysabovespringFunc$days_above_springfresh_std*d+c)

HydroPredictDaysabovespringFunc$hits <- HydroPredictDaysabovespringFunc$fit

HydroPredictDaysabovespringFuncPlot<-ggplot(HydroPredictDaysabovespringFunc, aes(days_above_springfresh, hits, colour = wpfg, group = wpfg)) +
  geom_line(linewidth = 2)+
  #geom_ribbon(aes(ymin = lower, ymax = upper),  fill = "brown1", alpha= 0.1)+
  geom_point(data= Plotdata,aes(x=days_above_springfresh, y= hits, colour = wpfg), alpha = 0.2)+
  coord_cartesian(ylim = c(0, 50))+
  labs(x = "Days above spring fresh", y = "Hits")+ theme_bw()  + facet_grid(.~origin) +# coord_cartesian(ylim = c(0.5, 1)) + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), legend.position = "right") 

HydroPredictDaysabovespringFuncPlot


# now lets fit the 'spatial' or 'flow event' model
# 
# first look at flow effects across zone

cover_ar_TMBmod_2 <- glmmTMB::glmmTMB(
  hits ~ log_hits_tm1 +
    #days_above_baseflow_std*wpfg*origin + days_above_springfresh_std*wpfg*origin +
    # days_above_baseflow_std^2 + #days_above_springfresh_std_sq +
    zone*period +
     origin + wpfg +
    grazing +
    (1 | site / transect) +
    #(1 | site / period) +
    (1 | metres) +
    (1 | survey_year),
  # offset(npoint),
  family = poisson,
  ziformula=~ wpfg,
  #dispformula =~ wpfg ,
  data = veg_cover_ar_sum |> filter(!wpfg_ori %in% c("Atl_native", "Ate_native", "Tda_unknown"))
)


summary(cover_ar_TMBmod_2)
# 

# old school model fit diagnostic plots

plot(fitted(cover_ar_TMBmod_2), cover_ar_TMBmod_2$frame$hits)
plot(fitted(cover_ar_TMBmod_2) + 1, cover_ar_TMBmod_2$frame$hits + 1, log = "xy")
# 

# use performance package to test model predictions - ignore homogeneity of variance and normality of residuals

check_model(cover_ar_TMBmod_2)

model_performance(cover_ar_TMBmod_2)

check_predictions(cover_ar_TMBmod_2) 
# 

check_collinearity(cover_ar_TMBmod_2)
# good

check_overdispersion(cover_ar_TMBmod_2)
# still overdispersed

check_zeroinflation(cover_ar_TMBmod_2)
# slightly underfitting zeros

check_singularity(cover_ar_TMBmod_2)
# False

# forrest plot of estiamtes

plot(model_parameters(cover_ar_TMBmod_2))

effect_plot(cover_ar_TMBmod_2, pred = zone  , interval = TRUE, partial.residuals = TRUE) + scale_y_continuous(limits=c(0, 40))

effect_plot(cover_ar_TMBmod_2, pred = wpfg  , interval = TRUE, partial.residuals = TRUE) + scale_y_continuous(limits=c(0, 60))

effect_plot(cover_ar_TMBmod_2, pred = period  , interval = TRUE, partial.residuals = TRUE) + scale_y_continuous(limits=c(0, 60))

effect_plot(cover_ar_TMBmod_2, pred = survey_year  , interval = TRUE, partial.residuals = TRUE) + scale_y_continuous(limits=c(0, 60))

cat_plot(cover_ar_TMBmod_2, pred = zone, modx = period, plot.points = T) + scale_y_continuous(limits=c(0, 60))

# plot model estimates 

EventsPredictZonePeriod<- as.data.frame(Effect(c('zone', 'period'),cover_ar_TMBmod_2,xlevels=20))
EventsPredictZonePeriod$hits <- EventsPredictZonePeriod$fit
EventsPredictZonePeriod$zone <- ordered(EventsPredictZonePeriod$zone, levels = c( "below_baseflow", "baseflow_to_springfresh", "above_springfresh"))
EventsPredictZonePeriod$period <- ordered(EventsPredictZonePeriod$period, levels = c( "before_spring", "after_spring", "after_summer"))

EventsPredictZonePeriodPlot<-ggplot(EventsPredictZonePeriod, aes(zone, hits, colour = period, group = period)) +
  geom_point(size = 5, position= position_dodge(0.5))+
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.3,  size= 1, position= position_dodge(0.5))+
  geom_point(data= Plotdata,aes(x=zone, y= hits, colour = period), alpha = 0.2,position= position_dodge(0.5))+
  coord_cartesian(ylim = c(0, 20))+
  labs(x = "Zone", y = "Hits")+ theme_bw() +# coord_cartesian(ylim = c(0.5, 1)) + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), legend.position = "right") 

EventsPredictZonePeriodPlot

EventsPredictZonePeriodPlot2<-ggplot(EventsPredictZonePeriod, aes(period, hits, colour = period)) +
  geom_point(size = 5)+
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.3,  size= 1)+
  geom_sina(data= Plotdata, alpha = 0.05)+
  coord_cartesian(ylim = c(0, 20))+
  labs(x = "Zone", y = "Hits")+ theme_bw() + facet_grid(~zone, switch="x" ) +# coord_cartesian(ylim = c(0.5, 1)) + 
  theme(axis.text.x = element_blank(),      # hide iv.y labels
        axis.ticks.x = element_blank(),#strip.background = element_blank(), 
        panel.spacing.x = unit(0, "mm"), panel.border = element_blank(), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"), legend.position = "right") 

EventsPredictZonePeriodPlot2
  

# now look at flow effects across functional groups

cover_ar_TMBmod_3 <- glmmTMB::glmmTMB(
  hits ~ log_hits_tm1 +
   # days_above_baseflow_std*wpfg*origin + days_above_springfresh_std*wpfg*origin +
    # days_above_baseflow_std^2 + #days_above_springfresh_std_sq +
    zone + wpfg * period + 
     origin + 
    grazing +
    (1 | site / transect) +
    #(1 | site / period) +
    (1 | metres) +
    (1 | survey_year),
  # offset(npoint),
  family = poisson,
  ziformula=~ wpfg,
  #dispformula =~ wpfg ,
  data = veg_cover_ar_sum |> filter(!wpfg_ori %in% c("Atl_native", "Ate_native", "Tda_unknown"))
)

summary(cover_ar_TMBmod_3)
# 

# old school model fit diagnostic plots

plot(fitted(cover_ar_TMBmod_3), cover_ar_TMBmod_3$frame$hits)
plot(fitted(cover_ar_TMBmod_3) + 1, cover_ar_TMBmod_3$frame$hits + 1, log = "xy")
# 

# use performance package to test model predictions - ignore homogeneity of variance and normality of residuals

check_model(cover_ar_TMBmod_3)

model_performance(cover_ar_TMBmod_3)

check_predictions(cover_ar_TMBmod_3) 
# 

check_collinearity(cover_ar_TMBmod_3)
# good

check_overdispersion(cover_ar_TMBmod_3)
# still overdispersed

check_zeroinflation(cover_ar_TMBmod_3)
# slightly underfitting zeros

check_singularity(cover_ar_TMBmod_3)
# True

# forrest plot of estiamtes

plot(model_parameters(cover_ar_TMBmod_3))

effect_plot(cover_ar_TMBmod_3, pred = zone  , interval = TRUE, partial.residuals = TRUE) + scale_y_continuous(limits=c(0, 40))

effect_plot(cover_ar_TMBmod_3, pred = wpfg  , interval = TRUE, partial.residuals = TRUE) + scale_y_continuous(limits=c(0, 60))

effect_plot(cover_ar_TMBmod_3, pred = period  , interval = TRUE, partial.residuals = TRUE) + scale_y_continuous(limits=c(0, 60))

effect_plot(cover_ar_TMBmod_3, pred = survey_year  , interval = TRUE, partial.residuals = TRUE) + scale_y_continuous(limits=c(0, 60))

cat_plot(cover_ar_TMBmod_3, pred = wpfg, modx =  period, plot.points = T) + scale_y_continuous(limits=c(0, 10))

# plot model estimates 

EventsPredictFuncPeriod<- as.data.frame(Effect(c('period', 'wpfg'),cover_ar_TMBmod_3,xlevels=20))
EventsPredictFuncPeriod$hits <- EventsPredictFuncPeriod$fit
EventsPredictFuncPeriod$period <- ordered(EventsPredictFuncPeriod$period, levels = c( "before_spring", "after_spring", "after_summer"))

EventsPredictFuncPeriodPlot<-ggplot(EventsPredictFuncPeriod, aes(period, hits, colour = wpfg, group = wpfg)) +
  geom_point(size = 5, position= position_dodge(0.5))+
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.3,  size= 1, position= position_dodge(0.5))+
  geom_point(data= Plotdata,aes(x=period, y= hits, colour = wpfg), alpha = 0.2,position= position_dodge(0.5))+
  coord_cartesian(ylim = c(0, 20))+
  labs(x = "Period", y = "Hits")+ theme_bw() + facet_grid(~wpfg) +# coord_cartesian(ylim = c(0.5, 1)) + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), legend.position = "right") 

EventsPredictFuncPeriodPlot

EventsPredictFuncPeriodPlot2<-ggplot(EventsPredictFuncPeriod, aes(period, hits, colour = wpfg)) +
  geom_point(size = 5)+
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.3,  size= 1)+
  geom_sina(data= Plotdata, alpha = 0.1)+
  coord_cartesian(ylim = c(0, 20))+
  labs(x = "Period", y = "Hits")+ theme_bw() + facet_grid(~wpfg, switch="x" ) +# coord_cartesian(ylim = c(0.5, 1)) + 
  theme(#axis.text.x = element_blank(),      # hide iv.y labels
        #axis.ticks.x = element_blank(),#strip.background = element_blank(), 
        panel.spacing.x = unit(0, "mm"), panel.border = element_blank(), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"), legend.position = "right") 

EventsPredictFuncPeriodPlot2



# now lets model vegetation richness

veg_richness <- veg_richness |>
  mutate(
    days_above_baseflow_std = scale(days_above_baseflow)[, 1],
    days_above_springfresh_std = scale(days_above_springfresh)[, 1],
    days_above_baseflow_std_sq = days_above_baseflow_std ^ 2,
    days_above_springfresh_std_sq = days_above_springfresh_std ^ 2
  ) |>
  filter(!is.na(days_above_springfresh))  # temporary due to incomplete flow data

# create factor that captures differing combinations of plant functional group and origin (native or exotic)

veg_richness$wpfg_ori <- as.factor(paste(veg_richness$wpfg,veg_richness$origin, sep = "_"))

# create plot data for raw datapoints
PlotdataRich <- veg_richness |> filter(!wpfg_ori %in% c("Atl_native", "Ate_native", "Tda_unknown"))
PlotdataRich$zone <- ordered(PlotdataRich$zone, levels = c( "below_baseflow", "baseflow_to_springfresh", "above_springfresh"))
PlotdataRich$period <- ordered(PlotdataRich$period, levels = c( "before_spring", "after_spring", "after_summer"))

# lets attempt to fit the additive model to look at model fits under differing distributions
# tried poisson and nbinom with dispersion and ziformulas of wpfg but all had convergence issues so landed again on poisson distributions.

# Regime model
richness_ar_TMBmod_1 <- glmmTMB::glmmTMB(
  richness ~ 
    days_above_baseflow_std*wpfg*origin + days_above_springfresh_std*wpfg*origin +
    # days_above_baseflow_std^2 + days_above_springfresh_std^2 +
    #   zone * period +
    #zone *period + zone*wpfg + wpfg*period +
   # grazing + origin +
    (1 | site / transect) +
    #(1 | site / period) +
    (1 | metres) +
    (1 | survey_year),
  # offset(npoint),
  family = poisson,
  #family = nbinom2,
  #ziformula=~ wpfg,
  # dispformula =~ wpfg ,
  data = veg_richness |> filter(!wpfg_ori %in% c("Atl_native", "Ate_native", "Tda_unknown"))
)


summary(richness_ar_TMBmod_1)
# 

# old school model fit diagnostic plots

plot(fitted(richness_ar_TMBmod_1), richness_ar_TMBmod_1$frame$hits)
#plot(fitted(richness_ar_TMBmod_1) + 1, richness_ar_TMBmod_1$frame$hits + 1, log = "xy")
# 

# use performance package to test model predictions - ignore homogeneity of variance and normality of residuals

check_model(richness_ar_TMBmod_1)

model_performance(richness_ar_TMBmod_1)

check_predictions(richness_ar_TMBmod_1) 
# 

check_collinearity(richness_ar_TMBmod_1)
# 

check_overdispersion(richness_ar_TMBmod_1)
# Not overdispersed

check_zeroinflation(richness_ar_TMBmod_1)
# ok

check_singularity(richness_ar_TMBmod_1)
# false

# forrest plot of estiamtes

plot(model_parameters(richness_ar_TMBmod_1))

effect_plot(richness_ar_TMBmod_1, pred = log_hits_tm1 , interval = TRUE, partial.residuals = TRUE) + scale_y_continuous(limits=c(0, 60))

effect_plot(richness_ar_TMBmod_1, pred = wpfg , interval = TRUE, partial.residuals = TRUE) + scale_y_continuous(limits=c(0, 60))

effect_plot(richness_ar_TMBmod_1, pred = days_above_baseflow_std , interval = TRUE, partial.residuals = T, plot.points = T) + scale_y_continuous(limits=c(0, 100))

effect_plot(richness_ar_TMBmod_1, pred = days_above_springfresh_std  , interval = TRUE, partial.residuals = TRUE) + scale_y_continuous(limits=c(0, 60))

effect_plot(richness_ar_TMBmod_1, pred = days_above_baseflow_std_sq  , interval = TRUE, partial.residuals = TRUE) + scale_y_continuous(limits=c(0, 60))

cat_plot(richness_ar_TMBmod_1, pred = zone, modx = period, interval = T, plot.points = T) #+ scale_y_continuous(limits=c(0, 5))

interact_plot(richness_ar_TMBmod_1, pred = days_above_baseflow_std, modx = wpfg, interval = T, plot.points = T) + scale_y_continuous(limits=c(0, 20))

interact_plot(richness_ar_TMBmod_1, pred = days_above_springfresh_std, modx = wpfg_ori, plot.points = T) + scale_y_continuous(limits=c(0, 20))



# plot model estimates 

RichPredictDaysabovebaseFunc<- as.data.frame(Effect(c('days_above_baseflow_std', 'wpfg'),richness_ar_TMBmod_1,xlevels=20))

c<-mean(veg_cover_ar_sum$days_above_baseflow) 
d<-sd(veg_cover_ar_sum$days_above_baseflow)
RichPredictDaysabovebaseFunc$days_above_baseflow<-(RichPredictDaysabovebaseFunc$days_above_baseflow_std*d+c)

RichPredictDaysabovebaseFunc$richness <- RichPredictDaysabovebaseFunc$fit

RichPredictDaysabovebaseFuncPlot<-ggplot(RichPredictDaysabovebaseFunc, aes(days_above_baseflow, richness, colour = wpfg, group = wpfg)) +
  geom_line(linewidth = 2)+
  geom_ribbon(aes(ymin = lower, ymax = upper, fill= wpfg),  alpha= 0.1)+
  geom_point(data= PlotdataRich,aes(x=days_above_baseflow, y= richness, colour = wpfg), alpha = 0.2)+
  coord_cartesian(ylim = c(0, 7))+
  labs(x = "Days above baseflow", y = "Species richness")+ theme_bw() +# coord_cartesian(ylim = c(0.5, 1)) + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), legend.position = "right") 

RichPredictDaysabovebaseFuncPlot

RichPredictDaysabovebaseFuncOri<- as.data.frame(Effect(c('days_above_baseflow_std', 'wpfg','origin'),richness_ar_TMBmod_1,xlevels=20))

c<-mean(veg_cover_ar_sum$days_above_baseflow) 
d<-sd(veg_cover_ar_sum$days_above_baseflow)
RichPredictDaysabovebaseFuncOri$days_above_baseflow<-(RichPredictDaysabovebaseFuncOri$days_above_baseflow_std*d+c)

RichPredictDaysabovebaseFuncOri$richness <- RichPredictDaysabovebaseFuncOri$fit

RichPredictDaysabovebaseFuncoriPlot<-ggplot(RichPredictDaysabovebaseFuncOri, aes(days_above_baseflow, richness, colour = wpfg, group = wpfg)) +
  geom_line(linewidth = 2)+
  #geom_ribbon(aes(ymin = lower, ymax = upper, fill= wpfg),  alpha= 0.1)+
  geom_point(data= PlotdataRich ,aes(x=days_above_baseflow, y= richness, colour = wpfg), alpha = 0.2)+
  coord_cartesian(ylim = c(0, 7))+
  labs(x = "Days above baseflow", y = "Species richness")+ theme_bw() + facet_grid(.~origin) +# coord_cartesian(ylim = c(0.5, 1)) 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), legend.position = "right") 

RichPredictDaysabovebaseFuncoriPlot


RichPredictDaysabovespringFunc<- as.data.frame(Effect(c('days_above_springfresh_std', 'wpfg','origin'),richness_ar_TMBmod_1,xlevels=20))
c<-mean(veg_cover_ar_sum$days_above_springfresh) 
d<-sd(veg_cover_ar_sum$days_above_springfresh)
RichPredictDaysabovespringFunc$days_above_springfresh<-(RichPredictDaysabovespringFunc$days_above_springfresh_std*d+c)

RichPredictDaysabovespringFunc$richness <- RichPredictDaysabovespringFunc$fit

RichPredictDaysabovespringFuncPlot<-ggplot(RichPredictDaysabovespringFunc, aes(days_above_springfresh, richness, colour = wpfg, group = wpfg)) +
  geom_line(linewidth = 2)+
  #geom_ribbon(aes(ymin = lower, ymax = upper),  fill = "brown1", alpha= 0.1)+
  geom_point(data= PlotdataRich,aes(x=days_above_springfresh, y= richness, colour = wpfg), alpha = 0.2)+
  coord_cartesian(ylim = c(0, 7))+
  labs(x = "Days above spring fresh", y = "Species richness")+ theme_bw()  + facet_grid(.~origin) +# coord_cartesian(ylim = c(0.5, 1)) + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), legend.position = "right") 

RichPredictDaysabovespringFuncPlot

# Flow event model

richness_ar_TMBmod_2 <- glmmTMB::glmmTMB(
  richness ~ 
    #days_above_baseflow_std + days_above_springfresh_std +
    # days_above_baseflow_std^2 + days_above_springfresh_std^2 +
    #   zone * period +
     zone *period + zone*wpfg + wpfg*period +
      grazing + origin +
    (1 | site / transect) +
    #(1 | site / period) +
    (1 | metres) +
    (1 | survey_year),
  # offset(npoint),
 family = poisson,
  #family = nbinom2,
  #ziformula=~ wpfg,
 # dispformula =~ wpfg ,
  data = veg_richness |> filter(!wpfg_ori %in% c("Atl_native", "Ate_native", "Tda_unknown"))|> filter(!wpfg %in% c("Sk", "Se"))
)


summary(richness_ar_TMBmod_2)
# 

# old school model fit diagnostic plots

plot(fitted(richness_ar_TMBmod_2), richness_ar_TMBmod_2$frame$hits)
#plot(fitted(richness_ar_TMBmod_2) + 1, richness_ar_TMBmod_2$frame$hits + 1, log = "xy")
# 

# use performance package to test model predictions - ignore homogeneity of variance and normality of residuals

check_model(richness_ar_TMBmod_2)

model_performance(richness_ar_TMBmod_2)

check_predictions(richness_ar_TMBmod_2) 
# 

check_collinearity(richness_ar_TMBmod_2)
# 

check_overdispersion(richness_ar_TMBmod_2)
# Not overdispersed

check_zeroinflation(richness_ar_TMBmod_2)
# ok

check_singularity(richness_ar_TMBmod_2)
# false

# forrest plot of estiamtes

plot(model_parameters(richness_ar_TMBmod_2))

effect_plot(richness_ar_TMBmod_2, pred = log_hits_tm1 , interval = TRUE, partial.residuals = TRUE) + scale_y_continuous(limits=c(0, 60))

effect_plot(richness_ar_TMBmod_2, pred = wpfg , interval = TRUE, partial.residuals = TRUE) + scale_y_continuous(limits=c(0, 60))

effect_plot(richness_ar_TMBmod_2, pred = days_above_baseflow_std , interval = TRUE, partial.residuals = T, plot.points = T) + scale_y_continuous(limits=c(0, 100))

effect_plot(richness_ar_TMBmod_2, pred = days_above_springfresh_std  , interval = TRUE, partial.residuals = TRUE) + scale_y_continuous(limits=c(0, 60))

effect_plot(richness_ar_TMBmod_2, pred = days_above_baseflow_std_sq  , interval = TRUE, partial.residuals = TRUE) + scale_y_continuous(limits=c(0, 60))

cat_plot(richness_ar_TMBmod_2, pred = zone, modx = period, interval = T, plot.points = T) #+ scale_y_continuous(limits=c(0, 5))

interact_plot(richness_ar_TMBmod_2, pred = days_above_baseflow_std, modx = wpfg, interval = T, plot.points = T) + scale_y_continuous(limits=c(0, 20))

interact_plot(richness_ar_TMBmod_2, pred = days_above_springfresh_std, modx = wpfg_ori, plot.points = T) + scale_y_continuous(limits=c(0, 20))



# plot model estimates 

RichPredictZonePeriod<- as.data.frame(Effect(c('period', 'zone'),richness_ar_TMBmod_2,xlevels=20))
RichPredictZonePeriod$richness <- RichPredictZonePeriod$fit
RichPredictZonePeriod$period <- ordered(RichPredictZonePeriod$period, levels = c( "before_spring", "after_spring", "after_summer"))
RichPredictZonePeriod$zone <- ordered(RichPredictZonePeriod$zone, levels = c( "below_baseflow", "baseflow_to_springfresh", "above_springfresh"))

RichPredictZonePeriodPlot<-ggplot(RichPredictZonePeriod, aes(zone, richness, colour = period, group = period)) +
  geom_point(size = 5, position= position_dodge(0.5))+
 # geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.3,  size= 1, position= position_dodge(0.5))+
  geom_point(data= PlotdataRich,aes(x=zone, y= richness, colour = period), alpha = 0.2,position= position_dodge(0.5))+
  coord_cartesian(ylim = c(0, 7))+
  labs(x = "Zone", y = "Species richness")+ theme_bw() + facet_grid(~period) +# coord_cartesian(ylim = c(0.5, 1)) + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), legend.position = "right") 

RichPredictZonePeriodPlot

RichPredictZonePeriodPlot2<-ggplot(RichPredictZonePeriod, aes(period, richness, colour = period)) +
  geom_point(size = 5)+
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.3,  size= 1)+
  geom_sina(data= PlotdataRich, alpha = 0.05)+
  coord_cartesian(ylim = c(0, 7))+
  labs(x = "Zone", y = "Species richness")+ theme_bw() + facet_grid(~zone, switch="x" ) +# coord_cartesian(ylim = c(0.5, 1)) + 
  theme(#axis.text.x = element_blank(),      # hide iv.y labels
    #axis.ticks.x = element_blank(),#strip.background = element_blank(), 
    panel.spacing.x = unit(0, "mm"), panel.border = element_blank(), 
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
    axis.line = element_line(colour = "black"), legend.position = "right") 

RichPredictZonePeriodPlot2


RichPredictZonewpfg<- as.data.frame(Effect(c('zone', 'wpfg'),richness_ar_TMBmod_2,xlevels=20))
RichPredictZonewpfg$richness <- RichPredictZonewpfg$fit
RichPredictZonewpfg$zone <- ordered(RichPredictZonewpfg$zone, levels = c( "below_baseflow", "baseflow_to_springfresh", "above_springfresh"))

RichPredictZonewpfgPlot<-ggplot(RichPredictZonewpfg, aes(zone, richness, colour = wpfg, group = wpfg)) +
  geom_point(size = 5, position= position_dodge(0.5))+
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.3,  size= 1, position= position_dodge(0.5))+
  geom_point(data= PlotdataRich,aes(x=zone, y= richness, colour = wpfg), alpha = 0.2,position= position_dodge(0.5))+
  coord_cartesian(ylim = c(0, 7))+
  labs(x = "Zone", y = "Species richness")+ theme_bw() + facet_grid(~wpfg) +# coord_cartesian(ylim = c(0.5, 1)) + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), legend.position = "right") 

RichPredictZonewpfgPlot

RichPredictZonewpfgPlot2<-ggplot(RichPredictZonewpfg, aes(zone, richness, colour = zone)) +
  geom_point(size = 5)+
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.3,  size= 1)+
  geom_sina(data= PlotdataRich, alpha = 0.05)+
  coord_cartesian(ylim = c(0, 7))+
  labs(x = "Zone", y = "Species richness")+ theme_bw() + facet_grid(~wpfg, switch="x" ) +# coord_cartesian(ylim = c(0.5, 1)) + 
  theme(axis.text.x = element_blank(),      # hide iv.y labels
    axis.ticks.x = element_blank(),#strip.background = element_blank(), 
    panel.spacing.x = unit(0, "mm"), panel.border = element_blank(), 
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
    axis.line = element_line(colour = "black"), legend.position = "right") 

RichPredictZonewpfgPlot2

RichPredictPeriodwpfg<- as.data.frame(Effect(c('period', 'wpfg'),richness_ar_TMBmod_2,xlevels=20))
RichPredictPeriodwpfg$richness <- RichPredictPeriodwpfg$fit
RichPredictPeriodwpfg$period <- ordered(RichPredictPeriodwpfg$period, levels = c( "before_spring", "after_spring", "after_summer"))

RichPredictPeriodwpfgPlot<-ggplot(RichPredictPeriodwpfg, aes(period, richness, colour = wpfg, group = wpfg)) +
  geom_point(size = 5, position= position_dodge(0.5))+
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.3,  size= 1, position= position_dodge(0.5))+
  geom_point(data= PlotdataRich,aes(x=period, y= richness, colour = wpfg), alpha = 0.2,position= position_dodge(0.5))+
  coord_cartesian(ylim = c(0, 7))+
  labs(x = "Period", y = "Species richness")+ theme_bw() + facet_grid(~wpfg) +# coord_cartesian(ylim = c(0.5, 1)) + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), legend.position = "right") 

RichPredictPeriodwpfgPlot

RichPredictPeriodwpfgPlot2<-ggplot(RichPredictPeriodwpfg, aes(period, richness, colour = period)) +
  geom_point(size = 5)+
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.3,  size= 1)+
  geom_sina(data= PlotdataRich, alpha = 0.05)+
  coord_cartesian(ylim = c(0, 7))+
  labs(x = "Period", y = "Species richness")+ theme_bw() + facet_grid(~wpfg, switch="x" ) +# coord_cartesian(ylim = c(0.5, 1)) + 
  theme(axis.text.x = element_blank(),      # hide iv.y labels
    axis.ticks.x = element_blank(),#strip.background = element_blank(), 
    panel.spacing.x = unit(0, "mm"), panel.border = element_blank(), 
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
    axis.line = element_line(colour = "black"), legend.position = "right") 

RichPredictPeriodwpfgPlot2

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


#  ----------- full analysis: all sites  --------

## ----------- Load Data --------

# now lets move onto the full analysis
# first we will attempt to fit a model with all sites included. 
# we will take the same approach to fitting as we did in the pilot analysis

### ----------- Extract cover data from raw data files for each site ----------

veg_cover_ar_full <- map2(.x = .system_list, 
                          .y = c(TRUE, FALSE, TRUE, FALSE, FALSE, FALSE, TRUE) %>% 
                            set_names(.system_list),
                          ~ load_cover(system = .x, 
                                       pilot = FALSE, 
                                       recompile = TRUE, 
                                       ar = TRUE, 
                                       s6s7 = .y)) %>% 
  list_rbind(names_to = "system")

# veg_cover_ar_full %>% filter(system == "Campaspe") %>% distinct(site, survey_year, period) %>% arrange(survey_year, period) %>% print(n=90)


### ----------- Extract richness data from raw data files for each site --------

veg_richness_full <- map2(.x = .system_list, 
                          .y = c(TRUE, FALSE, TRUE, FALSE, FALSE, FALSE, TRUE) %>% 
                            set_names(.system_list),
                          ~ load_richness(system = .x, 
                                          pilot = FALSE, 
                                          recompile = TRUE, #TODO Check if we need to recompile? seems unnecessary if recompiling full dataset in a couple of steps
                                          s6s7 = .y)) %>% 
  list_rbind(names_to = "system")

# Check Campaspe
# veg_richness_full %>% filter(system == "Campaspe") %>% distinct(site, survey_year, period) %>% arrange(survey_year, period)

# now lets combine datasets
  
# collect information on the number of species at sites etc. within the raw dataset (not the ar dataset as this removes the first year)

veg_cover_full <- map2(.x = .system_list, 
                          .y = c(TRUE, FALSE, TRUE, FALSE, FALSE, FALSE, TRUE) %>% 
                            set_names(.system_list),
                          ~ load_cover(system = .x, 
                                       pilot = FALSE, 
                                       recompile = TRUE, 
                                       ar = FALSE, 
                                       s6s7 = .y)) %>% 
  list_rbind(names_to = "system") %>% 
  unite("unique_transect", site, transect, remove = FALSE)



# filter for rows hits greater than 0
veg_cover_full_summary_hits <- veg_cover_full_summary %>% filter(hits > 0)

veg_cover_full_summary$unique_transect <- as.factor(paste(veg_cover_full_summary$site,veg_cover_full_summary$transect, sep = "_"))

veg_cover_full_summary %>% group_by(waterbody) %>% summarise(n=length(unique(site))) %>% print(n=80) #%>% summarise(ntot=sum((n)))

veg_cover_full_summary %>% group_by(waterbody) %>% distinct((survey_year)) %>% arrange(waterbody) %>% print(n=80)

veg_cover_full_summary %>% distinct((survey_year))  %>% print(n=80)

veg_cover_full_summary %>% group_by(waterbody, site) %>% summarise(n=length(unique(unique_transect))) %>% group_by(waterbody) %>% summarise(ntot=sum((n))) #%>% summarise(ntot2=sum((ntot)))

veg_cover_full_summary %>% group_by(waterbody,survey_year) %>% summarise(n=length(unique(survey))) %>% group_by(waterbody) %>% summarise(ntot=sum((n))) %>% print(n=28) %>% summarise(ntot2=sum((ntot)))

veg_cover_full_summary_hits  %>% group_by(waterbody, rec_group) %>% summarise(n=length(unique(species)))%>% print(n=80) #%>% group_by(rec_group)%>% summarise(ntot=sum((n)))

veg_cover_full_summary_hits %>% group_by(rec_group) %>% summarise(n=length(unique(species)))%>% print(n=80) 


## IGNORE for now, JY to follow up
# TODO: check missing species
# veg |> 
#   distinct(wpfg, species) |> 
#   arrange(wpfg, species) |>
#   mutate(include = ifelse(wpfg %in% wpfg_list, TRUE, FALSE))

# sum cover over all species within each wpfg
  veg_cover_ar_full_sum <- veg_cover_ar_full |>
  group_by(
    waterbody, site, transect, metres, survey, survey_year,
    period, origin, rec_group
  ) |>
  summarise(
    hits = sum(hits),
    npoint = unique(npoint),  # each point can have multiple overlapping veg records,
    #   so it potentially makes more sense to treat it as
    #   40 points with possibility of > 100% cover.
    hits_tm1 = sum(hits_tm1),
    npoint_tm1 = unique(npoint_tm1)
  )

 
# sum richness across wpfg to get correct score for rec_group
  
  veg_richness_full_sum <- veg_richness_full |>
    group_by(
      waterbody, site, transect, metres, survey, survey_year,
      period, origin, rec_group
    ) |>
    summarise(
      richness = sum(richness),
          )


# load flow data and merge summary metrics with veg
flow_full <- 
  map(.system_list,
      load_flow,
      pilot = FALSE, 
      recompile = TRUE) %>% 
  list_rbind()


# TODO: investigate raw-data files for low amounts of data
# note that we have excluded some sites for now (see within the 'load_flow' function for site list)

# calculate metrics
metrics_full <- calculate_metrics(flow_full, site_info)

# TODO: check thresholds are correct for each site
# note that sites should have positive values for mean_days_above_spring and mean_days_above_baseflow

# add site and flow info into the veg data set, removing plots with missing
#    values
# TODO: check missing site info with CJ - note that there are about 10 sites with missing info that the filter for zone below removes

# remove duplicate entries
site_info <- site_info |> distinct(system, waterbody, site, transect, metres, zone, grazing) 

veg_cover_ar_full <- veg_cover_ar_full |>
  left_join(site_info, by = c("waterbody", "site", "transect", "metres")) |>
  filter(!is.na(zone)) |>  
  left_join(
    metrics_full,
    by = c("system", "waterbody", "site", "survey_year", "period")
  )

veg_cover_ar_full_sum <- veg_cover_ar_full_sum |>
  left_join(site_info, by = c("waterbody", "site", "transect", "metres")) |>
  filter(!is.na(zone)) |>
  left_join(
    metrics_full,
    by = c("system", "waterbody", "site", "survey_year", "period")
  )

veg_richness_full <- veg_richness_full |>
  left_join(site_info, by = c("waterbody", "site", "transect", "metres")) |>
  filter(!is.na(zone)) |>
  left_join(
    metrics_full,
    by = c("system", "waterbody", "site", "survey_year", "period")
  )

veg_richness_full_sum <- veg_richness_full_sum |>
  left_join(site_info, by = c("waterbody", "site", "transect", "metres")) |>
  filter(!is.na(zone)) |>
  left_join(
    metrics_full,
    by = c("system", "waterbody", "site", "survey_year", "period")
  )

## IGNORE FOR NOW
## TODO: consider including exotic cover as a predictor OR include
##   random int/slopes for origin and look at correlations to assess
##   how natives and exotics interact

## TODO: generate npoint_tm1 variable to calculate correct log_pr_cover_tm1 variable - also talk to Jian about log(+1 of this var)
# standardise predictors and remove rows with missing flow info - note we need to add a small value to everything? 

veg_cover_ar_full$days_above_baseflow_std <- scale(veg_cover_ar_full$days_above_baseflow)[, 1]
veg_cover_ar_full$days_above_springfresh_std <- scale(veg_cover_ar_full$days_above_springfresh)[, 1]

veg_cover_ar_full <- veg_cover_ar_full |>
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

veg_cover_ar_full_sum$days_above_baseflow_std <- scale(veg_cover_ar_full_sum$days_above_baseflow)[, 1]
veg_cover_ar_full_sum$days_above_springfresh_std <- scale(veg_cover_ar_full_sum$days_above_springfresh)[, 1]

veg_cover_ar_full_sum <- veg_cover_ar_full_sum |>
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

min(veg_cover_ar_full$pr_cover[veg_cover_ar_full$pr_cover > 0])
min(veg_cover_ar_full_sum$pr_cover[veg_cover_ar_full_sum$pr_cover > 0])

# add half this minumum value to all response values as an added constant for use in lognormal model (as per JY methodology)

veg_cover_ar_full <- veg_cover_ar_full |>
  mutate(
    pr_cover_tf = pr_cover + (.025*.5),
    log_pr_cover_tm1_tf = log((hits_tm1/npoint_tm1) + (.025*.5)), 
  )

veg_cover_ar_full_sum <- veg_cover_ar_full_sum |>
  mutate(
    pr_cover_tf = pr_cover + (.025*.5),
    log_pr_cover_tm1_tf = log((hits_tm1/npoint_tm1) + (.025*.5)), 
  )

# now look at this data

unique(veg_richness_full$zone)
#[1] "baseflow_to_springfresh" "above_springfresh"       "below_baseflow"   

hist(veg_richness_full$richness)

hist(veg_richness_full_sum$richness)

hist(veg_cover_ar_full$hits)
plot(density(veg_cover_ar_full$hits))

hist(veg_cover_ar_full_sum$hits)
plot(density(veg_cover_ar_full_sum$hits))

hist(veg_cover_ar_full$pr_cover_tf)
plot(density(veg_cover_ar_full$pr_cover_tf))

hist(veg_cover_ar_full_sum$pr_cover_tf)
plot(density(veg_cover_ar_full_sum$pr_cover_tf))

veg_cover_ar_full_sum %>% 
  group_by(rec_group,  origin) %>%
  summarise(no_rows = length(hits)) %>% print(n=50) # a bunch of unknown origins here but lets leave in as a factor level for now

veg_cover_ar_full_sum %>% filter(system == "Campaspe") %>% group_by(period, survey_year) %>%
  distinct( period, survey_year) %>% arrange(period) %>% print(n=90)

ggplot(veg_cover_ar_full_sum, aes(x = period, y = hits, group = rec_group, colour =rec_group) ) + geom_point(position=position_dodge(.5)) + facet_grid(.~zone)
ggplot(veg_cover_ar_full_sum, aes(x = period, y = hits, group = rec_group, colour = rec_group) ) + geom_jitter(position=position_dodge(.5)) #+ facet_grid(wpfg~transect)
ggplot(veg_cover_ar_full_sum, aes(x = days_above_springfresh, y = hits, group = rec_group, colour = rec_group) ) + geom_point(position=position_dodge(.5)) 
ggplot(veg_cover_ar_full_sum, aes(x = days_above_baseflow, y = hits, group = rec_group, colour = rec_group) ) + geom_point(position=position_dodge(.5)) 

# full analysis ####

# now move onto fitting the full models

# calculate the proportion of zeros in the data

sum(veg_cover_ar_full_sum$hits %in% 0 ) / nrow(veg_cover_ar_full_sum) # 76% zeros

# modify the wpfg var to investigate the use of rec_group var in its place

veg_cover_ar_full_sum$wpfg <- veg_cover_ar_full_sum$rec_group

# cover ####
# first we will attempt to fit the full model with all systems and sites included.

cover_ar_TMBmod_full_1 <- glmmTMB::glmmTMB(
  hits ~ log_hits_tm1 +
   days_above_baseflow_std*wpfg+ days_above_springfresh_std*wpfg + # original regime model does not converge
    I(days_above_baseflow_std^2)*wpfg + I(days_above_springfresh_std^2)*wpfg + origin +
    (1 | site / transect) +
    (1 | metres) +
    (1 | survey_year)+
    (1| system),
  # offset(npoint), # offset not needed as all sites have 40 points
  family = poisson(),
  ziformula=~ wpfg,
  data = veg_cover_ar_full_sum
)

summary(cover_ar_TMBmod_full_1)
# 

# old school model fit diagnostic plots

plot(fitted(cover_ar_TMBmod_full_1), cover_ar_TMBmod_full_1$frame$hits)
plot(fitted(cover_ar_TMBmod_full_1) + 1, cover_ar_TMBmod_full_1$frame$hits + 1, log = "xy")
# 

# use performance package to test model predictions - ignore homogeneity of variance and normality of residuals

check_model(cover_ar_TMBmod_full_1)

model_performance(cover_ar_TMBmod_full_1)

# r^2
cor(fitted(cover_ar_TMBmod_full_1), cover_ar_TMBmod_full_1$frame$hits) ^ 2

check_predictions(cover_ar_TMBmod_full_1)
# 

check_collinearity(cover_ar_TMBmod_full_1)
# high correlations but to be expected in interactions

check_overdispersion(cover_ar_TMBmod_full_1)
# overdispersed

check_zeroinflation(cover_ar_TMBmod_full_1)
# underfitting but we rely on check_predictions output above for this check

check_singularity(cover_ar_TMBmod_full_1)
# false

# extract and plot random effects from the model using the 'mixedup' package

cover_ar_TMBmod_full_1_re <- extract_random_effects(cover_ar_TMBmod_full_1) 

cover_ar_TMBmod_full_1_re <- cover_ar_TMBmod_full_1_re %>% filter(group_var == "system")

cover_ar_TMBmod_full_1_re$group <- ordered(cover_ar_TMBmod_full_1_re$group, levels = c( "Campaspe", "Glenelg", "Loddon", "Moorabool", "WGippsland", "Wimmera", "Yarra"))

ggplot(cover_ar_TMBmod_full_1_re , aes(group, value)) +
  geom_point(size = 2)+
  geom_errorbar(aes(ymin = lower_2.5, ymax = upper_97.5), width = 0.1,  size= 1)+
  labs(x = "System", y = "Value")+ theme_bw() +
  theme(#axis.text.x = element_blank(),      # hide iv.y labels
    #axis.ticks.x = element_blank(),#strip.background = element_blank(), 
    panel.spacing.x = unit(0, "mm"), #panel.border = element_blank(), 
    #panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
    axis.line = element_line(colour = "black"), legend.position = "right", text = element_text(size = 20)) +labs(color='System')  # 1200 x 800

# extract and plot vc components

cover_ar_TMBmod_full_1_vc <- extract_vc(cover_ar_TMBmod_full_1) 

cover_ar_TMBmod_full_1_vc$group <- ordered(cover_ar_TMBmod_full_1_vc$group, levels = c( "system", "site", "transect:site", "metres", "survey_year"))

cover_ar_TMBmod_full_1_vc$group <- recode_factor(cover_ar_TMBmod_full_1_vc$group, system = "System", 
                                                       site = "Site", 'transect:site' = "Site(transect)", metres = "Metres", survey_year = "Survey year")

ggplot(cover_ar_TMBmod_full_1_vc , aes(group, sd)) +
  geom_point(size = 2)+
  geom_errorbar(aes(ymin = sd_2.5, ymax = sd_97.5), width = 0.1,  size= 1)+
  labs(x = "Effect", y = "Standard deviation")+ theme_bw() +
  theme(#axis.text.x = element_blank(),      # hide iv.y labels
    #axis.ticks.x = element_blank(),#strip.background = element_blank(), 
    panel.spacing.x = unit(0, "mm"), #panel.border = element_blank(), 
    #panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
    axis.line = element_line(colour = "black"), legend.position = "none", text = element_text(size = 20)) +labs(color='System')  # 1200 x 800


# forest plot of estimates

plot(model_parameters(cover_ar_TMBmod_full_1))

# predicted effects plots

# effect of days above baseflow

HydroPredictDaysabovebaseFuncOri_full<- as.data.frame(Effect(c('days_above_baseflow_std', 'wpfg'),cover_ar_TMBmod_full_1,xlevels=20))

c<-mean(veg_cover_ar_full_sum$days_above_baseflow) 
d<-sd(veg_cover_ar_full_sum$days_above_baseflow)
HydroPredictDaysabovebaseFuncOri_full$days_above_baseflow<-(HydroPredictDaysabovebaseFuncOri_full$days_above_baseflow_std*d+c)

HydroPredictDaysabovebaseFuncOri_full$hits <- HydroPredictDaysabovebaseFuncOri_full$fit

HydroPredictDaysabovebaseFuncoriPlot_full<-ggplot(HydroPredictDaysabovebaseFuncOri_full, aes(days_above_baseflow, hits/40*100, colour = wpfg, group = wpfg)) +
  geom_line(linewidth = 2)+
  geom_ribbon(aes(ymin=lower/40*100, ymax=upper/40*100,fill=wpfg),alpha=0.1 , color=NA, show.legend = F) +
  coord_cartesian(ylim = c(0, 17))+
  labs(x = "Days above baseflow", y = "Predicted % cover")+ theme_bw() +# facet_grid(.~origin) +# coord_cartesian(ylim = c(0.5, 1)) 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), legend.position = "right", text = element_text(size = 20)) +labs(color='Functional group') 

HydroPredictDaysabovebaseFuncoriPlot_full

HydroPredictDaysabovebaseFuncoriPlot_full2<-ggplot(HydroPredictDaysabovebaseFuncOri_full, aes(days_above_baseflow, hits/40*100, colour = wpfg, group = wpfg)) +
  geom_line(linewidth = 2)+
  geom_ribbon(aes(ymin=lower/40*100, ymax=upper/40*100,fill=wpfg),alpha=0.1 , color=NA, show.legend = F) +
  coord_cartesian(ylim = c(0, 100))+
  labs(x = "Days above baseflow", y = "Predicted % cover")+ theme_bw() +# facet_grid(.~origin) +# coord_cartesian(ylim = c(0.5, 1)) 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), legend.position = "right", text = element_text(size = 20)) +labs(color='Functional group') 

HydroPredictDaysabovebaseFuncoriPlot_full2

# effect of days above springfresh

HydroPredictDaysabovespringFunc_full<- as.data.frame(Effect(c('days_above_springfresh_std', 'wpfg'),cover_ar_TMBmod_full_1,xlevels=60))
c<-mean(veg_cover_ar_full_sum$days_above_springfresh) 
d<-sd(veg_cover_ar_full_sum$days_above_springfresh)
HydroPredictDaysabovespringFunc_full$days_above_springfresh<-(HydroPredictDaysabovespringFunc_full$days_above_springfresh_std*d+c)

HydroPredictDaysabovespringFunc_full$hits <- HydroPredictDaysabovespringFunc_full$fit

HydroPredictDaysabovespringFuncPlot_full<-ggplot(HydroPredictDaysabovespringFunc_full, aes(days_above_springfresh, hits/40*100, colour = wpfg, group = wpfg)) +
  geom_line(linewidth = 2)+
  coord_cartesian(ylim = c(0, 17))+
  geom_ribbon(aes(ymin=lower/40*100, ymax=upper/40*100,fill=wpfg),alpha=0.1 , color=NA, show.legend = F) +
  labs(x = "Days above spring fresh", y = "Predicted % cover")+ theme_bw()  + #facet_grid(.~origin) +# coord_cartesian(ylim = c(0.5, 1)) + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), legend.position = "right", text = element_text(size = 20)) +labs(color='Functional group') 

HydroPredictDaysabovespringFuncPlot_full

HydroPredictDaysabovespringFuncPlot_full<-ggplot(HydroPredictDaysabovespringFunc_full, aes(days_above_springfresh, hits/40*100, colour = wpfg, group = wpfg)) +
  geom_line(linewidth = 2)+
  coord_cartesian(ylim = c(0, 100))+
  geom_ribbon(aes(ymin=lower/40*100, ymax=upper/40*100,fill=wpfg),alpha=0.1 , color=NA, show.legend = F) +
  labs(x = "Days above spring fresh", y = "Predicted % cover")+ theme_bw()  + #facet_grid(.~origin) +# coord_cartesian(ylim = c(0.5, 1)) + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), legend.position = "right", text = element_text(size = 20)) +labs(color='Functional group') 

HydroPredictDaysabovespringFuncPlot_full

# effect of origin

HydroPredictOrigin_full<- as.data.frame(Effect(c('origin'),cover_ar_TMBmod_full_1))

HydroPredictOrigin_full$origin <- ordered(HydroPredictOrigin_full$origin, levels = c( "native", "exotic"))

HydroPredictOrigin_full$origin <- recode_factor(HydroPredictOrigin_full$origin, native = "Native", 
                                                 exotic = "Exotic")


ggplot(HydroPredictOrigin_full%>% filter(origin != "unknown"), aes(origin , fit/40*100)) +
  geom_point(size = 2)+
  geom_errorbar(aes(ymin = lower/40*100, ymax = upper/40*100), width = 0.1,  size= 1)+
  coord_cartesian(ylim = c(0, 25))+
  labs(x = "Origin", y = "Predicted % cover")+ theme_bw() +
  theme(#axis.text.x = element_blank(),      # hide iv.y labels
    #axis.ticks.x = element_blank(),#strip.background = element_blank(), 
    panel.spacing.x = unit(0, "mm"), #panel.border = element_blank(), 
    #panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
    axis.line = element_line(colour = "black"), legend.position = "right", text = element_text(size = 20)) +labs(color='Origin') 


# now lets fit the 'spatial' or 'flow event' model
# 
# we need to modify the period factor for a few sites that do not recieve Eflows and as such we will call this a no_event level

veg_cover_ar_full_sum <-  veg_cover_ar_full_sum %>% dplyr::mutate(
 period = ifelse(site == "Peuckers" & period == "after_spring", "no_event", ifelse(site == "Peuckers" & period == "after_summer", "no_event", 
                                                                                   ifelse(site == "McInnes" & period == "after_spring", "no_event",  
                                                                                          ifelse(site == "McInnes" & period == "after_summer", "no_event", period)))))
veg_cover_ar_full_sum %>% 
  group_by( waterbody, site, period) %>%
  summarise(no_rows = length(hits)) %>% print(n=100)


# first look at flow effects across zone and period for each functional group

cover_ar_TMBmod_full_2 <- glmmTMB::glmmTMB(
  hits ~ log_hits_tm1 +
     zone * wpfg * period + origin +  grazing + 
    (1 | site / transect) +
    #(1 | site / period) +
    (1 | metres) +
    (1 | survey_year)+
    (1| system),
  # offset(npoint), # offset not needed as all sites have 40 points
  family = poisson(),
  ziformula=~ wpfg,
   dispformula =~ wpfg*zone ,
  data = veg_cover_ar_full_sum |> filter(!site %in% c("Peuckers", "McInnes")) |> filter(!origin %in% c("unknown"))
)


summary(cover_ar_TMBmod_full_2)
# 

# old school model fit diagnostic plots

plot(fitted(cover_ar_TMBmod_full_2), cover_ar_TMBmod_full_2$frame$hits)
plot(fitted(cover_ar_TMBmod_full_2) + 1, cover_ar_TMBmod_full_2$frame$hits + 1, log = "xy")
# 

# use performance package to test model predictions - ignore homogeneity of variance and normality of residuals

check_model(cover_ar_TMBmod_full_2)

model_performance(cover_ar_TMBmod_full_2)

# r^2
cor(fitted(cover_ar_TMBmod_full_2), cover_ar_TMBmod_full_2$frame$hits) ^ 2

check_predictions(cover_ar_TMBmod_full_2)
# 

check_collinearity(cover_ar_TMBmod_full_2)
# high correlations but to be expected in interactions

check_overdispersion(cover_ar_TMBmod_full_2)
# overdispersed

check_zeroinflation(cover_ar_TMBmod_full_2)
# underfitting but we rely on check_predictions output above for this check

check_singularity(cover_ar_TMBmod_full_2)
# false

# extract and plot random effects from the model using the 'mixedup' package

cover_ar_TMBmod_full_2_re <- extract_random_effects(cover_ar_TMBmod_full_2) 

cover_ar_TMBmod_full_2_re <- cover_ar_TMBmod_full_2_re %>% filter(group_var == "system")

cover_ar_TMBmod_full_2_re$group <- ordered(cover_ar_TMBmod_full_2_re$group, levels = c( "Campaspe", "Glenelg", "Loddon", "Moorabool", "WGippsland", "Wimmera", "Yarra"))

ggplot(cover_ar_TMBmod_full_2_re %>% filter(group_var == "system"), aes(group, value)) +
  geom_point(size = 2)+
  geom_errorbar(aes(ymin = lower_2.5, ymax = upper_97.5), width = 0.1,  size= 1)+
  labs(x = "System", y = "Value")+ theme_bw() +
  theme(#axis.text.x = element_blank(),      # hide iv.y labels
    #axis.ticks.x = element_blank(),#strip.background = element_blank(), 
    panel.spacing.x = unit(0, "mm"), #panel.border = element_blank(), 
    #panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
    axis.line = element_line(colour = "black"), legend.position = "none", text = element_text(size = 20)) +labs(color='System')  # 1200 x 800

# extract and plot vc components

cover_ar_TMBmod_full_2_vc <- extract_vc(cover_ar_TMBmod_full_2) 

cover_ar_TMBmod_full_2_vc$group <- ordered(cover_ar_TMBmod_full_2_vc$group, levels = c( "system", "site", "transect:site", "metres", "survey_year"))

cover_ar_TMBmod_full_2_vc$group <- recode_factor(cover_ar_TMBmod_full_2_vc$group, system = "System", 
                                                 site = "Site", 'transect:site' = "Site(transect)", metres = "Metres", survey_year = "Survey year")

ggplot(cover_ar_TMBmod_full_2_vc , aes(group, sd)) +
  geom_point(size = 2)+
  geom_errorbar(aes(ymin = sd_2.5, ymax = sd_97.5), width = 0.1,  size= 1)+
  labs(x = "Effect", y = "Standard deviation")+ theme_bw() +
  theme(#axis.text.x = element_blank(),      # hide iv.y labels
    #axis.ticks.x = element_blank(),#strip.background = element_blank(), 
    panel.spacing.x = unit(0, "mm"), #panel.border = element_blank(), 
    #panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
    axis.line = element_line(colour = "black"), legend.position = "none", text = element_text(size = 20)) +labs(color='System')  # 1200 x 800

# forrest plot of parameter estimates

plot(model_parameters(cover_ar_TMBmod_full_2))

# predicted effects plots

EventsPredictFuncPeriodZone_full<- as.data.frame(Effect(c('period', 'wpfg', 'zone'),cover_ar_TMBmod_full_2,xlevels=20))
EventsPredictFuncPeriodZone_full$hits <- EventsPredictFuncPeriodZone_full$fit
EventsPredictFuncPeriodZone_full$period <- ordered(EventsPredictFuncPeriodZone_full$period, levels = c( "before_spring", "after_spring", "after_summer", "no_event"))

EventsPredictFuncPeriodZone_full$period <- recode_factor(EventsPredictFuncPeriodZone_full$period, before_spring = "Before spring", 
                                                         after_spring = "After spring", after_summer = "After summer", no_event = "No event")
EventsPredictFuncPeriodZone_full$zone <- recode_factor(EventsPredictFuncPeriodZone_full$zone, above_springfresh = "Above springfresh", 
                                                       baseflow_to_springfresh = "Baseflow to springfresh", below_baseflow = "Below baseflow")

# now calculate records for the correct combination of factors
Cover_records <- veg_cover_ar_full_sum %>% filter(!site %in% c("Peuckers", "McInnes")) %>% filter(!origin %in% c("unknown")) %>% group_by(wpfg, zone) %>% summarise(n=sum(hits>0)) 

Cover_records$period <- "After spring"
Cover_records$zone <- recode_factor(Cover_records$zone, above_springfresh = "Above springfresh", 
                                       baseflow_to_springfresh = "Baseflow to springfresh", below_baseflow = "Below baseflow")


   ggplot(EventsPredictFuncPeriodZone_full, aes(period, hits/40*100)) +
  geom_point(size = 2)+
  geom_errorbar(aes(ymin = lower/40*100, ymax = upper/40*100), width = 0.1,  size= 1)+
  coord_cartesian(ylim = c(0, 65))+
  labs(x = "Period", y = "Predicted % cover")+ theme_bw() + facet_grid(zone~wpfg) + 
     ggtext::geom_textbox(data= Cover_records, aes(y= 50, label = paste("n = ", n)), size = 4, halign = 0,  hjust = .45,  
                          # fill = "white",
                 # box.colour = "white"
                 width = .2, show.legend = FALSE)+
  theme(#axis.text.x = element_blank(),      # hide iv.y labels
    #axis.ticks.x = element_blank(),#strip.background = element_blank(), 
   panel.spacing.x = unit(0, "mm"), #panel.border = element_blank(), 
    #panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
    axis.line = element_line(colour = "black"), legend.position = "none", text = element_text(size = 20), axis.text=element_text(size=12)) +labs(color='Functional group') # 1600 x 800

  
# effect of origin 
  
EventsPredictOrigin_full<- as.data.frame(Effect(c('origin'),cover_ar_TMBmod_full_2))
   
EventsPredictOrigin_full$origin <- ordered(EventsPredictOrigin_full$origin, levels = c( "native", "exotic"))

EventsPredictOrigin_full$origin <- recode_factor(EventsPredictOrigin_full$origin, native = "Native",  exotic = "Exotic")

ggplot(EventsPredictOrigin_full, aes(origin, fit/40*100)) +
 geom_point(size = 2)+
 geom_errorbar(aes(ymin = lower/40*100, ymax = upper/40*100), width = 0.1,  size= 1)+
 coord_cartesian(ylim = c(0, 20))+
  labs(x = "Origin", y = "Predicted % cover")+ theme_bw() +
  theme(#axis.text.x = element_blank(),      # hide iv.y labels
    #axis.ticks.x = element_blank(),#strip.background = element_blank(), 
    panel.spacing.x = unit(0, "mm"), #panel.border = element_blank(), 
    #panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
   axis.line = element_line(colour = "black"), legend.position = "right", text = element_text(size = 20)) +labs(color='Origin') 

# effect of grazing 
   
EventsPredictgrazing_full<- as.data.frame(Effect(c('grazing'),cover_ar_TMBmod_full_2))

EventsPredictgrazing_full$grazing <- recode_factor(EventsPredictgrazing_full$grazing, N = "No",   Y = "Yes")

ggplot(EventsPredictgrazing_full, aes(grazing, fit/40*100)) +
  geom_point(size = 2)+
  geom_errorbar(aes(ymin = lower/40*100, ymax = upper/40*100), width = 0.1,  size= 1)+
   coord_cartesian(ylim = c(0, 20))+
  labs(x = "Grazing", y = "Predicted % cover")+ theme_bw() +
  theme(#axis.text.x = element_blank(),      # hide iv.y labels
    #axis.ticks.x = element_blank(),#strip.background = element_blank(), 
    panel.spacing.x = unit(0, "mm"), #panel.border = element_blank(), 
    #panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
   axis.line = element_line(colour = "black"), legend.position = "none", text = element_text(size = 20)) +labs(color='Grazing') 
   
# richness ####
# now lets model vegetation richness

# modify the wpfg var to investigate the use of rec_group var in its place
   
veg_richness_full_sum$wpfg <- veg_richness_full_sum$rec_group
PlotdataRich_full$wpfg <- PlotdataRich_full$rec_group
   
veg_cover_ar_full_sum %>% group_by(origin, wpfg) %>% distinct(origin, wpfg) %>% arrange(origin) %>% print(n=50)
   
veg_richness_full_sum$days_above_baseflow_std <-  scale(veg_richness_full_sum$days_above_baseflow)[, 1]
veg_richness_full_sum$days_above_springfresh_std <-  scale(veg_richness_full_sum$days_above_springfresh)[, 1]

veg_richness_full_sum <- veg_richness_full_sum |>
  mutate(
       days_above_baseflow_std_sq = days_above_baseflow_std ^ 2,
       days_above_springfresh_std_sq = days_above_springfresh_std ^ 2
     ) |>
     filter(!is.na(days_above_springfresh))  # temporary due to incomplete flow data
   
# create factor that captures differing combinations of plant functional group and origin (native or exotic)
   
veg_richness_full_sum$wpfg_ori <- as.factor(paste(veg_richness_full_sum$wpfg,veg_richness_full_sum$origin, sep = "_"))
  
summary(veg_richness_full_sum)

veg_richness_full_sum <-  veg_richness_full_sum %>% dplyr::mutate(
  period = ifelse(site == "Peuckers" & period == "after_spring", "no_event", ifelse(site == "Peuckers" & period == "after_summer", "no_event", 
                                  ifelse(site == "McInnes" & period == "after_spring", "no_event",   ifelse(site == "McInnes" & period == "after_summer", "no_event", period)))))

veg_richness_full_sum %>% 
  group_by( waterbody, site, period) %>%
  summarise(no_rows = length(richness)) %>% print(n=100)

# full model

richness_ar_TMBmod_full_1 <- glmmTMB::glmmTMB(
     richness ~ 
       days_above_baseflow_std*wpfg + days_above_springfresh_std*wpfg + origin +
       I(days_above_baseflow_std^2) + I(days_above_springfresh_std^2) +
       zone * wpfg * period  + grazing +
       (1 | site / transect) +
       (1 | metres) +
       (1 | survey_year)+
       (1 | system),
      family = poisson,
     #ziformula=~ wpfg,
     # dispformula =~ wpfg ,
     data = veg_richness_full_sum |> filter(!site %in% c("Peuckers", "McInnes"))
   )
   
   
summary(richness_ar_TMBmod_full_1)
# 
   
# old school model fit diagnostic plots
   
plot(fitted(richness_ar_TMBmod_full_1), richness_ar_TMBmod_full_1$frame$richness)
plot(fitted(richness_ar_TMBmod_full_1) + 1, richness_ar_TMBmod_full_1$frame$richness + 1, log = "xy")
# 
   
# use performance package to test model predictions - ignore homogeneity of variance and normality of residuals

check_model(richness_ar_TMBmod_full_1)

model_performance(richness_ar_TMBmod_full_1)

# r^2
cor(fitted(richness_ar_TMBmod_full_1), richness_ar_TMBmod_full_1$frame$hits) ^ 2

check_predictions(richness_ar_TMBmod_full_1)
# 

check_collinearity(richness_ar_TMBmod_full_1)
# high correlations but to be expected in interactions

check_overdispersion(richness_ar_TMBmod_full_1)
# overdispersed

check_zeroinflation(richness_ar_TMBmod_full_1)
# this check ok

check_singularity(richness_ar_TMBmod_full_1)
# false

# extract and plot random effects from the model using the 'mixedup' package

richness_ar_TMBmod_full_1_re <- extract_random_effects(richness_ar_TMBmod_full_1) 

richness_ar_TMBmod_full_1_re <- richness_ar_TMBmod_full_1_re %>% filter(group_var == "system")

richness_ar_TMBmod_full_1_re$group <- ordered(richness_ar_TMBmod_full_1_re$group, levels = c( "Campaspe", "Glenelg", "Loddon", "Moorabool", "WGippsland", "Wimmera", "Yarra"))

ggplot(richness_ar_TMBmod_full_1_re %>% filter(group_var == "system"), aes(group, value)) +
  geom_point(size = 2)+
  geom_errorbar(aes(ymin = lower_2.5, ymax = upper_97.5), width = 0.1,  size= 1)+
  labs(x = "System", y = "Value")+ theme_bw() +
  theme(#axis.text.x = element_blank(),      # hide iv.y labels
    #axis.ticks.x = element_blank(),#strip.background = element_blank(), 
    panel.spacing.x = unit(0, "mm"), #panel.border = element_blank(), 
    #panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
    axis.line = element_line(colour = "black"), legend.position = "none", text = element_text(size = 20)) +labs(color='System')  # 1200 x 800

# extract and plot vc components

richness_ar_TMBmod_full_1_vc <- extract_vc(richness_ar_TMBmod_full_1) 

richness_ar_TMBmod_full_1_vc$group <- ordered(richness_ar_TMBmod_full_1_vc$group, levels = c( "system", "site", "transect:site", "metres", "survey_year"))

richness_ar_TMBmod_full_1_vc$group <- recode_factor(richness_ar_TMBmod_full_1_vc$group, system = "System", 
                                                 site = "Site", 'transect:site' = "Site(transect)", metres = "Metres", survey_year = "Survey year")

ggplot(richness_ar_TMBmod_full_1_vc , aes(group, sd)) +
  geom_point(size = 2)+
  geom_errorbar(aes(ymin = sd_2.5, ymax = sd_97.5), width = 0.1,  size= 1)+
  labs(x = "Effect", y = "Standard deviation")+ theme_bw() +
  theme(#axis.text.x = element_blank(),      # hide iv.y labels
    #axis.ticks.x = element_blank(),#strip.background = element_blank(), 
    panel.spacing.x = unit(0, "mm"), #panel.border = element_blank(), 
    #panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
    axis.line = element_line(colour = "black"), legend.position = "none", text = element_text(size = 20)) +labs(color='System')  # 1200 x 800


# forest plot of estimates
   
plot(model_parameters(richness_ar_TMBmod_full_1))
   
# plot model estimates 

# effect of days above baseflow on species richness

RichPredictDaysabovebaseFunc<- as.data.frame(Effect(c('days_above_baseflow_std', 'wpfg'),richness_ar_TMBmod_full_1,xlevels=20))
   
c<-mean(veg_cover_ar_full_sum$days_above_baseflow) 
d<-sd(veg_cover_ar_full_sum$days_above_baseflow)
RichPredictDaysabovebaseFunc$days_above_baseflow<-(RichPredictDaysabovebaseFunc$days_above_baseflow_std*d+c)
   
RichPredictDaysabovebaseFunc$richness <- RichPredictDaysabovebaseFunc$fit
   
RichPredictDaysabovebaseFuncPlot<-ggplot(RichPredictDaysabovebaseFunc, aes(days_above_baseflow, richness, colour = wpfg, group = wpfg)) +
geom_line(linewidth = 2)+
geom_ribbon(aes(ymin = lower, ymax = upper, fill= wpfg), colour = NA, alpha= 0.1, show.legend = F)+
coord_cartesian(ylim = c(0, 1.5))+
labs(x = "Days above baseflow", y = "Species richness")+ theme_bw() +# coord_cartesian(ylim = c(0.5, 1)) + 
theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), legend.position = "right", text = element_text(size = 20)) +labs(color='Functional group') 
   
RichPredictDaysabovebaseFuncPlot
   
# effect of days above springfresh on species richness
   
RichPredictDaysabovespringFunc<- as.data.frame(Effect(c('days_above_springfresh_std', 'wpfg'),richness_ar_TMBmod_full_1,xlevels=20))
c<-mean(veg_cover_ar_full_sum$days_above_springfresh) 
d<-sd(veg_cover_ar_full_sum$days_above_springfresh)
RichPredictDaysabovespringFunc$days_above_springfresh<-(RichPredictDaysabovespringFunc$days_above_springfresh_std*d+c)
   
RichPredictDaysabovespringFunc$richness <- RichPredictDaysabovespringFunc$fit
   
RichPredictDaysabovespringFuncPlot<-ggplot(RichPredictDaysabovespringFunc, aes(days_above_springfresh, richness, colour = wpfg, group = wpfg)) +
geom_line(linewidth = 2)+
geom_ribbon(aes(ymin = lower, ymax = upper, fill=wpfg),  colour=NA, alpha= 0.1, show.legend = F)+
coord_cartesian(ylim = c(0, 1.5))+
labs(x = "Days above spring fresh", y = "Species richness")+ theme_bw()  + #facet_grid(.~origin) +# coord_cartesian(ylim = c(0.5, 1)) + 
theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), legend.position = "right", text = element_text(size = 20)) +labs(color='Functional group') 
   
RichPredictDaysabovespringFuncPlot
   
# three way plot for the effects of period, functional group and zone on species richness

RichPredictFuncPeriodZone_full<- as.data.frame(Effect(c('period', 'wpfg', 'zone'),richness_ar_TMBmod_full_1,xlevels=20))
RichPredictFuncPeriodZone_full$richness <- RichPredictFuncPeriodZone_full$fit
RichPredictFuncPeriodZone_full$period <- ordered(RichPredictFuncPeriodZone_full$period, levels = c( "before_spring", "after_spring", "after_summer"))

RichPredictFuncPeriodZone_full$period <- recode_factor(RichPredictFuncPeriodZone_full$period, before_spring = "Before spring", 
                                                         after_spring = "After spring", after_summer = "After summer")
RichPredictFuncPeriodZone_full$zone <- recode_factor(RichPredictFuncPeriodZone_full$zone, above_springfresh = "Above springfresh", 
                                                       baseflow_to_springfresh = "Baseflow to springfresh", below_baseflow = "Below baseflow")


# now calculate records for the correct combination of factors
Richness_records <- veg_richness_full_sum %>% filter(!site %in% c("Peuckers")) %>% group_by(wpfg, zone) %>% summarise(n=sum(richness>0)) 

Richness_records$period <-"After spring" 
Richness_records$zone <- recode_factor(Richness_records$zone, above_springfresh = "Above springfresh", 
                                                     baseflow_to_springfresh = "Baseflow to springfresh", below_baseflow = "Below baseflow")


ggplot(RichPredictFuncPeriodZone_full, aes(period, richness)) +
  geom_point(size = 2)+
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1,  size= 1)+
  coord_cartesian(ylim = c(0, 1.5))+ ggtext::geom_textbox(data= Richness_records, aes(y= 1.25, label = paste("n = ", n)),
                                                          size = 4,
                                                          halign = 0, 
                                                          hjust = .4,
                                                         # fill = "white",
                                                         # box.colour = "white"
                                                         width = .2,
                                                         show.legend = FALSE
                                                          )+
  labs(x = "Period", y = "Species richness")+ theme_bw() + facet_grid(zone~wpfg) +
  theme(#axis.text.x = element_blank(),      # hide iv.y labels
    #axis.ticks.x = element_blank(),#strip.background = element_blank(), 
    panel.spacing.x = unit(0, "mm"), #panel.border = element_blank(), 
    #panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
    axis.line = element_line(colour = "black"), legend.position = "none", text = element_text(size = 20), axis.text=element_text(size=12)) +labs(color='Functional group') +guides(size=FALSE) # 1600 x 800

