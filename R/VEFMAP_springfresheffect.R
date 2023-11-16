### Data analysis for the effect of spring freshes on vegetation for the DELWP report and paper 2020
### 

# Campaspe veg data are from Chris Jones through VEFMAP Stage 6

# Instructions for the analysis:
# Before and after periods for Campaspe Riv
#-	Aug 2017 (S4) and Jan 2018 (S5)
#-	Aug/Sep 2018 (S7) and Nov 2018 (S8)
#-	Sep 2019 (S10) and Dec 2019 (S11)




###### Packages
# library(tidyverse)
# library(ggplot2)
# library(mgcv)   # for the gamm model
# library(vcd)    # for model checking
# library(lme4)  # for glmer
# library(emmeans)
# library(RColorBrewer)

###### Data

# Veg data
veg_points <- read.csv("./raw_data/veg_data/VEFMAPS6_Campaspe_2017_2018_2019_2020_Point.csv")
veg_master <- read.csv("./raw_data/veg_data/VEFMAP_species_master.csv")

gps_data <- read.csv("./raw_data/site_data/GPS_All.csv")
meta_data <- read.csv("./raw_data/site_data/VEFMAPS6_Site_Metadata.csv")


###### Data formatting 
veg_points$DATE <- as.Date(as.character(veg_points$DATE), "%d/%m/%Y")

###### Data merging

# Merge species master data with point data
veg_points <- veg_points[,-8] # Remove ORIGIN column from veg_points which may have errors

veg_points <- left_join(veg_points,veg_master)

# Merge gps data with point data
gps_data$TRANSECT <- as.factor(gps_data$TRANSECT)
veg_points$METRES <- as.factor(veg_points$METRES)
gps_data <- gps_data %>% select(SYSTEM, WATERWAY, SITE, TRANSECT, METRES, HEIGHT_AHD)

veg_points <- left_join(veg_points,gps_data)

# Merge metadata with point data
#meta_data <- meta_data %>% select(SYSTEM, WATERWAY, SITE, TRANSECT, METRES, HEIGHT_AHD)

veg_points <- left_join(veg_points,meta_data)



###### Grouping veg data by ...

veg_points$Zone <- "1"
#veg_points["Zone"][veg_points["HEIGHT_AHD"] > veg_points["BASEFLOW_m_AHD"] & veg_points["HEIGHT_AHD"] <= veg_points["IVTFLOW"]] <- "2"
veg_points["Zone"][veg_points["HEIGHT_AHD"] > veg_points["BASEFLOW_m_AHD"] & veg_points["HEIGHT_AHD"] <= veg_points["SPRINGFRESH_m_AHD"]] <- "2"
veg_points["Zone"][veg_points["HEIGHT_AHD"] > veg_points["SPRINGFRESH_m_AHD"]] <- "3"

veg_points$Period <- "Before"
veg_points["Period"][veg_points["SURVEY"] == "5" | veg_points["SURVEY"] == "8" | veg_points["SURVEY"] == "11"] <- "After"

veg_points$Fresh_subset <- "False"
veg_points["Fresh_subset"][veg_points["SURVEY"] == "4" | veg_points["SURVEY"] == "7" | veg_points["SURVEY"] == "10" |
                           veg_points["SURVEY"] == "5" | veg_points["SURVEY"] == "8" | veg_points["SURVEY"] == "11"] <- "True"

veg_points$Year <- "2017/18"
veg_points["Year"][veg_points["SURVEY"] == "7" | veg_points["SURVEY"] == "8"] <- "2018/19"
veg_points["Year"][veg_points["SURVEY"] == "10" | veg_points["SURVEY"] == "11"] <- "2019/20"

veg_points$Period <- factor(veg_points$Period, levels = c("Before","After"))  # Treatment order

veg_points_subset <- filter(veg_points, Fresh_subset=="True")
veg_points_subset$Zone <- as.factor(veg_points_subset$Zone)



###### Plotting data

# Select sites
veg_points_subset_site <- filter(veg_points_subset, SITE=="English" | SITE=="Spencer" | SITE=="Strathallan" | SITE=="Campbells")
veg_points_subset_site <- filter(veg_points_subset, SITE=="Spencer")
veg_points_subset_site <- filter(veg_points_subset, SITE=="English")
veg_points_subset_site <- filter(veg_points_subset, SITE=="Strathallan")
veg_points_subset_site <- filter(veg_points_subset, SITE=="Campbells")


# Claculate hits per zone per period and year
veg_points_sum <- group_by(veg_points_subset_site,SITE, TRANSECT, Year, SURVEY, Period, Zone, fct_explicit_na(ORIGIN), fct_explicit_na(CLASSIFICATION)) %>% 
  summarise( HITS_TOT = sum(HITS)) 

veg_points_sum <- veg_points_sum %>% rename(ORIGIN = "fct_explicit_na(ORIGIN)")
veg_points_sum <- veg_points_sum %>% rename(CLASSIFICATION = "fct_explicit_na(CLASSIFICATION)")

veg_points_sum <- filter(veg_points_sum, ORIGIN=="exotic" | ORIGIN=="native")
veg_points_sum <- filter(veg_points_sum, 
                         CLASSIFICATION=="Emergent" | 
                           CLASSIFICATION=="Fringing_low" |
                           CLASSIFICATION=="Fringing_high" |
                           CLASSIFICATION=="Terrestrial" )

veg_points_sum_wide <- spread(veg_points_sum, CLASSIFICATION, HITS_TOT)
veg_points_sum_wide[c(8:11)][is.na(veg_points_sum_wide[c(8:11)])] <- 0
veg_points_sum <- gather(veg_points_sum_wide, "CLASSIFICATION", "HITS_TOT", 8:11) # Use spread and then gather to add zeros

veg_points_sum$CLASSIFICATION <- factor(veg_points_sum$CLASSIFICATION, 
                                        levels = c("Aquatic","Emergent","Fringing_low","Fringing_high","Terrestrial"))  # Treatment order

veg_points_sum$ORIGIN <- factor(veg_points_sum$ORIGIN, levels = c("native","exotic"))
veg_points_sum$SURVEY <- factor(veg_points_sum$SURVEY, levels = c("4","5","7","8","10","11"))  # Treatment order


# Single site version
ggplot(veg_points_sum, aes(x=SURVEY,y=HITS_TOT+1, fill=Zone)) +
  geom_boxplot() +
  theme_bw() + 
  ggtitle("Campbell's") +
  scale_y_log10() +
  xlab("Survey") +
  ylab("Total number of hits") +
  scale_fill_manual(values=c("dodgerblue1", "sienna2", "grey","gold1")) +
  facet_grid(vars(CLASSIFICATION), vars(ORIGIN)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),  
        axis.line = element_line(colour = "black"))




# All site version
ggplot(veg_points_sum, aes(x=Year,y=HITS_TOT, fill=Period)) +
  geom_boxplot() +
  theme_bw() + 
  scale_y_log10() +
  xlab("Elevation (mAHD)") +
  ylab("Cover %") +
  #geom_rect(data=data_base, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill="blue", alpha=0.1, inherit.aes = FALSE) +
  #geom_rect(data=data_fresh, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill="red", alpha=0.1, inherit.aes = FALSE) +
  facet_grid(vars(SITE), vars(CLASSIFICATION)) +
  #geom_vline(xintercept = baseflow_level, colour = "blue", lty = 2) +
  #geom_vline(xintercept = springfresh_level, colour = "red", lty = 2) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),  
        axis.line = element_line(colour = "black"),axis.text.x = element_text(angle = 90))



# Colour by month version
ggplot(veg_points_sum, aes(x=HEIGHT_AHD,y=COVER, color=MONTH)) +
  geom_point() +
  theme_bw() + 
  # ylim(0,600) +
  xlab("Elevation (mAHD)") +
  ylab("Cover %") +
  facet_grid(vars(ORIGIN), vars(CLASSIFICATION)) +
  geom_vline(xintercept = baseflow_level, colour = "blue", lty = 2) +
  geom_vline(xintercept = springfresh_level, colour = "red", lty = 2) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),  axis.line = element_line(colour = "black"))







##### CLEAN MODEL VERSION

# Select sites
veg_points_subset_site <- veg_points_subset
veg_points_subset_site <- filter(veg_points_subset, SITE=="English" | SITE=="Spencer" | SITE=="Strathallan" | SITE=="Campbells")
veg_points_subset_site <- filter(veg_points_subset, SITE=="Spencer")
veg_points_subset_site <- filter(veg_points_subset, SITE=="English")
veg_points_subset_site <- filter(veg_points_subset, SITE=="Strathallan")
veg_points_subset_site <- filter(veg_points_subset, SITE=="Campbells")
veg_points_subset_site <- filter(veg_points_subset, SITE=="English" | SITE=="Spencer" | SITE=="Strathallan" | SITE=="Campbells" | SITE=="Bryants" | SITE=="Doaks")


# Claculate hits per zone per period and year
veg_points_sum <- group_by(veg_points_subset_site,SITE, TRANSECT, METRES, Year, SURVEY, Period, Zone, fct_explicit_na(ORIGIN), fct_explicit_na(CLASSIFICATION)) %>% 
  summarise( HITS_TOT = sum(HITS)) 

veg_points_sum <- veg_points_sum %>% rename(ORIGIN = "fct_explicit_na(ORIGIN)")
veg_points_sum <- veg_points_sum %>% rename(CLASSIFICATION = "fct_explicit_na(CLASSIFICATION)")

veg_points_sum_wide <- spread(veg_points_sum, CLASSIFICATION, HITS_TOT)
veg_points_sum_wide[c(9:17)][is.na(veg_points_sum_wide[c(9:17)])] <- 0
veg_points_sum <- gather(veg_points_sum_wide, "CLASSIFICATION", "HITS_TOT", 9:17) # Use spread and then gather to add zeros

veg_points_sum <- filter(veg_points_sum, ORIGIN=="exotic" | ORIGIN=="native")

veg_points_sum <- filter(veg_points_sum, 
                         CLASSIFICATION=="Emergent" | 
                           CLASSIFICATION=="Fringing_low" |
                           CLASSIFICATION=="Fringing_high" |
                           CLASSIFICATION=="Terrestrial" )


veg_points_sum$CLASSIFICATION <- factor(veg_points_sum$CLASSIFICATION, 
                                        levels = c("Emergent","Fringing_low","Fringing_high","Terrestrial"))  # Treatment order

veg_points_sum$ORIGIN <- factor(veg_points_sum$ORIGIN, levels = c("native","exotic"))

# Subset data to target group
veg_points_sum_model <- filter(veg_points_sum, CLASSIFICATION=="Emergent")
veg_points_sum_model <- filter(veg_points_sum, CLASSIFICATION=="Fringing_low" & HITS_TOT<200)
veg_points_sum_model <- filter(veg_points_sum, CLASSIFICATION=="Fringing_high")
veg_points_sum_model <- filter(veg_points_sum, CLASSIFICATION=="Terrestrial")

#veg_points_sum_model <- filter(veg_points_sum, speciesName=="Sum_All.Aquatic")

veg_points_sum_model$Year <- as.factor(veg_points_sum_model$Year)
veg_points_sum_model$TRANSECT <- as.factor(veg_points_sum_model$TRANSECT)
veg_points_sum_model$SITE <- as.factor(veg_points_sum_model$SITE)


# Model

hist(veg_points_sum_model$HITS_TOT)
hist(sqrt(veg_points_sum_model$HITS_TOT))
hist(log(veg_points_sum_model$HITS_TOT))

distplot(veg_points_sum_model$HITS_TOT, type="poisson")
distplot(veg_points_sum_model$HITS_TOT, type="nbinom")  # you are looking for dots in a line




Mixmod_Camp <- gam( HITS_TOT+1 ~ Year*Period*Zone*ORIGIN + # fixed effects for period and treatment and their interaction
                      #ORIGIN +
                      s(SITE, bs='re') +
                      s(TRANSECT, bs='re'),      # Random effect for species 
                    data=veg_points_sum_model , family=nb(), method='REML' )


#checking model fit

Mixmod_Camp$outer.info ## check convergence

res <- residuals(Mixmod_Camp, type="response")
plot((predict(Mixmod_Camp)), res)
plot(log(predict(Mixmod_Camp)), res)

anova(Mixmod_Camp,test="Chisq")

qqnorm(residuals(Mixmod_Camp))
abline(0,1)



# post-hoc tests for contrasts

em <- emmeans( Mixmod_Camp, pairwise ~ Period | Year | Zone | ORIGIN)
ds_em <- em$emmeans %>% data.frame()
ds_contrasts <- em$contrasts %>% data.frame()


em <- emmeans( Mixmod_Camp, pairwise ~ Zone |Period | Year  | ORIGIN)
ds_em <- em$emmeans %>% data.frame()
ds_contrasts <- em$contrasts %>% data.frame()


ds_contrasts$Effect <- (1-exp(ds_contrasts$estimate))*100
ds_contrasts$CI_LB <- (1-exp(ds_contrasts$estimate-2*ds_contrasts$SE))*100
ds_contrasts$CI_UB <- (1-exp(ds_contrasts$estimate+2*ds_contrasts$SE))*100



# Plot effects and CIs

PlotD <-   ggplot(ds_contrasts, aes(y=Zone,x=Effect,fill=Year))+
  geom_point(aes(colour=Year), position=position_dodge(0.5)) +
  ggtitle("Terrestrial") +
  ylab("Bank Zone") +
  theme_bw() +
  xlab("Multiplicative effect (%)") +
  #ylim(0,100) +
  facet_grid(cols=vars(ORIGIN)) +
  geom_vline(xintercept = 0, lty = 2) +
  geom_errorbar(aes(xmin=CI_LB, xmax=CI_UB, colour=Year), width=.2,position=position_dodge(0.5)) +
  theme(legend.position="right", panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))



cowplot::plot_grid(PlotA, PlotB, PlotC, PlotD ,ncol=2, labels=paste0(LETTERS[1:4],')' ), align = c('hv') )


