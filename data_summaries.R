# Veg Report Checks pulled out of main.R

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

# check the values of hits

summary(veg_cover_ar_sum)