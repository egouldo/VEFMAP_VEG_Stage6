# Reprex:

# Model Fails
cover_ar_TMBmod_1 <- glmmTMB::glmmTMB(
  +   hits ~ log_hits_tm1 +
    +     days_above_baseflow_std*wpfg*origin + days_above_springfresh_std*wpfg*origin +
    +    # days_above_baseflow_std^2 + days_above_springfresh_std^2 +
    +     #   zone * period +
    +   #  zone + period +
    +   #  grazing + wpfg  +
    +     (1 | site / transect) +
    +     #(1 | site / period) +
    +     (1 | metres) +
    +     (1 | survey_year),
  +   # offset(npoint),
    +   family = poisson,
  +   ziformula=~ wpfg,
  +   #dispformula =~ wpfg ,
    +   data = veg_cover_ar_sum |> filter(!wpfg_ori %in% c("Atl_native", "Ate_native", "Tda_unknown"))
  + )
#> Error in `contrasts<-`(`*tmp*`, value = contr.funs[1 + isOF[nn]]) : 
  contrasts can be applied only to factors with 2 or more levels