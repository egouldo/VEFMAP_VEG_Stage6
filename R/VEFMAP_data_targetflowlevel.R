# Data processing for VEFMAP paper, evaluation of vegetation against target flow points
# 

###### Packages
library(tidyr)
library(dplyr)
library(forcats)
library(ggplot2)
library(lubridate)

###### Data
setwd("~/../GIT2/VEFMAP_VEG_Stage6/data")  # This will only work for certain computer setups, setwd to data folder.

# Veg data
veg_points <- read.csv("./raw_data/veg_data/VEFMAPS6_Campaspe_2017_2018_2019_2020_Point.csv")

veg_master <- read.csv("./raw_data/veg_data/VEFMAP_species_master.csv")


# Site data
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




###### Plotting data

### single plot

# Doaks
veg_points_doaks <- filter(veg_points, SITE=="Doaks")

veg_points_doaks_sum <- group_by(veg_points_doaks, TRANSECT, DATE, SUB_TRANS, METRES, HEIGHT_AHD, fct_explicit_na(ORIGIN), fct_explicit_na(CLASSIFICATION)) %>% 
  summarise( COVER = sum(HITS)/40*100) 

veg_points_doaks_sum$MONTH <- month(veg_points_doaks_sum$DATE,label=T)

veg_points_doaks_sum <- veg_points_doaks_sum %>% rename(ORIGIN = "fct_explicit_na(ORIGIN)")
veg_points_doaks_sum <- veg_points_doaks_sum %>% rename(CLASSIFICATION = "fct_explicit_na(CLASSIFICATION)")

veg_points_doaks_sum <- filter(veg_points_doaks_sum, ORIGIN=="exotic" | ORIGIN=="native")
veg_points_doaks_sum <- filter(veg_points_doaks_sum, 
                                 CLASSIFICATION=="Aquatic" | 
                                 CLASSIFICATION=="Emergent" | 
                                 CLASSIFICATION=="Fringing_low" |
                                 CLASSIFICATION=="Fringing_high" |
                                 CLASSIFICATION=="Terrestrial" )

veg_points_doaks_sum$CLASSIFICATION <- factor(veg_points_doaks_sum$CLASSIFICATION, 
              levels = c("Aquatic","Emergent","Fringing_low","Fringing_high","Terrestrial"))  # Treatment order


veg_points_doaks_sum$ORIGIN <- factor(veg_points_doaks_sum$ORIGIN, 
                                              levels = c("native","exotic"))

baseflow_level <- mean(veg_points_doaks$BASEFLOW_m_AHD)
springfresh_level <- mean(veg_points_doaks$SPRINGFRESH_m_AHD)

  ggplot(veg_points_doaks_sum, aes(x=HEIGHT_AHD,y=COVER, color=MONTH)) +
  geom_point() +
  theme_bw() + 
 # ylim(0,600) +
  xlab("Elevation (mAHD)") +
  ylab("Cover %") +
  facet_grid(vars(ORIGIN), vars(CLASSIFICATION)) +
  geom_vline(xintercept = baseflow_level, colour = "blue", lty = 2) +
  geom_vline(xintercept = springfresh_level, colour = "red", lty = 2) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),  axis.line = element_line(colour = "black"))



  # Bryants
  veg_points_bryants <- filter(veg_points, SITE=="Bryants")
  
  veg_points_bryants_sum <- group_by(veg_points_bryants, TRANSECT, SUB_TRANS, METRES, HEIGHT_AHD, fct_explicit_na(ORIGIN), fct_explicit_na(CLASSIFICATION)) %>% 
    summarise( COVER = sum(HITS)/40*100)  
  
  veg_points_bryants_sum <- veg_points_bryants_sum %>% rename(ORIGIN = "fct_explicit_na(ORIGIN)")
  veg_points_bryants_sum <- veg_points_bryants_sum %>% rename(CLASSIFICATION = "fct_explicit_na(CLASSIFICATION)")
  
  veg_points_bryants_sum <- filter(veg_points_bryants_sum, ORIGIN=="exotic" | ORIGIN=="native")
  veg_points_bryants_sum <- filter(veg_points_bryants_sum, 
                                 CLASSIFICATION=="Aquatic" | 
                                   CLASSIFICATION=="Emergent" | 
                                   CLASSIFICATION=="Fringing_low" |
                                   CLASSIFICATION=="Fringing_high" |
                                   CLASSIFICATION=="Terrestrial" )
  
  veg_points_bryants_sum$CLASSIFICATION <- factor(veg_points_bryants_sum$CLASSIFICATION, 
                                                levels = c("Aquatic","Emergent","Fringing_low","Fringing_high","Terrestrial"))  # Treatment order
  
  veg_points_bryants_sum$ORIGIN <- factor(veg_points_bryants_sum$ORIGIN, 
                                        levels = c("native","exotic"))
  
  
  baseflow_level <- mean(veg_points_bryants$BASEFLOW_m_AHD)
  springfresh_level <- mean(veg_points_bryants$SPRINGFRESH_m_AHD)
  
  ggplot(veg_points_bryants_sum, aes(x=HEIGHT_AHD,y=COVER)) +
    geom_point() +
    theme_bw() + 
    # ylim(0,600) +
    xlab("Elevation (mAHD)") +
    ylab("Cover %") +
    facet_grid(vars(ORIGIN), vars(CLASSIFICATION)) +
    geom_vline(xintercept = baseflow_level, colour = "blue", lty = 2) +
    geom_vline(xintercept = springfresh_level, colour = "red", lty = 2) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),  axis.line = element_line(colour = "black"))
  
  
