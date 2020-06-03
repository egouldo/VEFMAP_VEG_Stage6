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
veg_points2 <- read.csv("./raw_data/veg_data/VEFMAPS6_Wimmera_2017_2018_2019_Point.csv")
veg_points3 <- read.csv("./raw_data/veg_data/VEFMAPS6_Glenelg_2018_2019_Point.csv")
veg_points4 <- read.csv("./raw_data/veg_data/VEFMAPS6_Loddon_2017_2018_Point.csv")
veg_points5 <- read.csv("./raw_data/veg_data/VEFMAPS6_Moorabool_2017_2018_Final_Point.csv")
veg_points6 <- read.csv("./raw_data/veg_data/VEFMAPS6_ThomsonMacalister_2018_2019_Point.csv")
veg_points7 <- read.csv("./raw_data/veg_data/VEFMAPS6_Yarra_2018_2019_Point.csv")

veg_points2 <- veg_points2[,-c(13:14)]
veg_points3 <- veg_points3[,-c(13:14)]
veg_points4 <- veg_points4[,-c(13:14)]
veg_points5 <- veg_points5[,-c(13:14)]
veg_points6 <- veg_points6[,-c(13:14)]

veg_points <- rbind(veg_points,veg_points2,veg_points3,veg_points4,veg_points5,veg_points6,veg_points7)

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

### single site plot

# Select site
veg_points_subset <- filter(veg_points, SITE=="Doaks")
veg_points_subset <- filter(veg_points, SITE=="Campbells")

veg_points_sum <- group_by(veg_points_subset, TRANSECT, DATE, METRES, HEIGHT_AHD, fct_explicit_na(ORIGIN), fct_explicit_na(CLASSIFICATION)) %>% 
  summarise( COVER = sum(HITS)/40*100) 

veg_points_sum$MONTH <- month(veg_points_sum$DATE,label=T)

veg_points_sum <- veg_points_sum %>% rename(ORIGIN = "fct_explicit_na(ORIGIN)")
veg_points_sum <- veg_points_sum %>% rename(CLASSIFICATION = "fct_explicit_na(CLASSIFICATION)")

veg_points_sum <- filter(veg_points_sum, ORIGIN=="exotic" | ORIGIN=="native")
veg_points_sum <- filter(veg_points_sum, 
                                 CLASSIFICATION=="Aquatic" | 
                                 CLASSIFICATION=="Emergent" | 
                                 CLASSIFICATION=="Fringing_low" |
                                 CLASSIFICATION=="Fringing_high" |
                                 CLASSIFICATION=="Terrestrial" )

veg_points_sum_wide <- spread(veg_points_sum, CLASSIFICATION, COVER)
veg_points_sum_wide[c(8:12)][is.na(veg_points_sum_wide[c(8:12)])] <- 0
veg_points_sum <- gather(veg_points_sum_wide, "CLASSIFICATION", "COVER", 8:12) # Use spread and then gather to add zeros

veg_points_sum$CLASSIFICATION <- factor(veg_points_sum$CLASSIFICATION, 
              levels = c("Aquatic","Emergent","Fringing_low","Fringing_high","Terrestrial"))  # Treatment order

veg_points_sum$ORIGIN <- factor(veg_points_sum$ORIGIN, levels = c("native","exotic"))

baseflow_level <- median(veg_points_subset$BASEFLOW_m_AHD)
springfresh_level <- median(veg_points_subset$SPRINGFRESH_m_AHD)

baseflow_max <- max(veg_points_subset$BASEFLOW_m_AHD)
baseflow_min <- min(veg_points_subset$BASEFLOW_m_AHD)

fresh_max <- max(veg_points_subset$SPRINGFRESH_m_AHD)
fresh_min <- min(veg_points_subset$SPRINGFRESH_m_AHD)

max_cov <- max(na.omit(veg_points_sum$COVER))+5

data_base <- data.frame (xmin=baseflow_min, xmax=baseflow_max, ymin=-5, ymax=max_cov)
data_fresh <- data.frame (xmin=fresh_min, xmax=fresh_max, ymin=-5, ymax=max_cov)


# Plain version
ggplot(veg_points_sum, aes(x=HEIGHT_AHD,y=COVER)) +
  geom_point() +
  theme_bw() + 
  # ylim(0,600) +
  xlab("Elevation (mAHD)") +
  ylab("Cover %") +
  geom_rect(data=data_base, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill="blue", alpha=0.1, inherit.aes = FALSE) +
  geom_rect(data=data_fresh, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill="red", alpha=0.1, inherit.aes = FALSE) +
  facet_grid(vars(ORIGIN), vars(CLASSIFICATION)) +
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



### single species plot
  
# Select site and species
  veg_points_subset <- filter(veg_points, SITE=="Spencer" & SPECIES=="Oxalis pes-caprae" |
                                SITE=="Spencer" & SPECIES=="Poa labillardierei var. labillardierei" |
                                SITE=="Spencer" & SPECIES=="Phragmites australis" |
                                SITE=="Spencer" & SPECIES=="Alternanthera denticulata" |
                                SITE=="Spencer" & SPECIES=="Bare" | 
                                SITE=="Spencer" & SPECIES=="Litter" | 
                                SITE=="Spencer" & SPECIES=="Water")
  
  veg_points_sum <- group_by(veg_points_subset, TRANSECT, DATE, METRES, HEIGHT_AHD, SPECIES) %>% 
    summarise( COVER = sum(HITS)/40*100) 
  
  veg_points_sum$MONTH <- month(veg_points_sum$DATE,label=T)
  
  veg_points_sum_wide <- spread(veg_points_sum, SPECIES, COVER)
  veg_points_sum_wide[c(6:12)][is.na(veg_points_sum_wide[c(6:12)])] <- 0
  veg_points_sum <- gather(veg_points_sum_wide, "SPECIES", "COVER", 6:12) # Use spread and then gather to add zeros
  
veg_points_sum <- filter(veg_points_sum, SPECIES!="Bare" & SPECIES!="Litter" & SPECIES!="Water")

veg_points_sum$SPECIES <- factor(veg_points_sum$SPECIES, 
            levels = c("Phragmites australis","Alternanthera denticulata","Poa labillardierei var. labillardierei","Oxalis pes-caprae"))  # Treatment order

  
  baseflow_level <- median(veg_points_subset$BASEFLOW_m_AHD)
  springfresh_level <- median(veg_points_subset$SPRINGFRESH_m_AHD)
  
  baseflow_max <- max(veg_points_subset$BASEFLOW_m_AHD)
  baseflow_min <- min(veg_points_subset$BASEFLOW_m_AHD)
  
  fresh_max <- max(veg_points_subset$SPRINGFRESH_m_AHD)
  fresh_min <- min(veg_points_subset$SPRINGFRESH_m_AHD)
  
 # max_cov <- max(na.omit(veg_points_sum$COVER))+5
  
  data_base <- data.frame (xmin=baseflow_min, xmax=baseflow_max, ymin=0, ymax=100)
  data_fresh <- data.frame (xmin=fresh_min, xmax=fresh_max, ymin=0, ymax=100)
  
  
  # Plain version
  ggplot(veg_points_sum, aes(x=HEIGHT_AHD,y=COVER)) +
    geom_point() +
    theme_bw() + 
    ylim(0,100) +
    xlab("Elevation (mAHD)") +
    ylab("Cover %") +
    facet_grid(cols=vars(SPECIES)) +
    geom_rect(data=data_base, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill="blue", alpha=0.1, inherit.aes = FALSE) +
    geom_rect(data=data_fresh, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill="red", alpha=0.1, inherit.aes = FALSE) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),  
          axis.line = element_line(colour = "black"),axis.text.x = element_text(angle = 90))
  
  
  
  
  
  
###### Native veg versus exotic
  
  # Select system
  veg_points_subset <- filter(veg_points, SYSTEM=="Campaspe")
  veg_points_subset <- filter(veg_points, SYSTEM=="Yarra")
  veg_points_subset <- filter(veg_points, SYSTEM=="WGippsland")
  veg_points_subset <- filter(veg_points, SYSTEM=="Wimmera")
  veg_points_subset <- filter(veg_points, SYSTEM=="Loddon")
  
  
  veg_points_sum <- group_by(veg_points_subset, TRANSECT,fct_explicit_na(GRAZING), DATE, METRES, HEIGHT_AHD, 
                             fct_explicit_na(ORIGIN), fct_explicit_na(CLASSIFICATION)) %>% 
    summarise( COVER = sum(HITS)/40*100) 
  
  veg_points_sum$MONTH <- month(veg_points_sum$DATE,label=T)
  
  veg_points_sum <- veg_points_sum %>% rename(GRAZING = "fct_explicit_na(GRAZING)")
  veg_points_sum <- veg_points_sum %>% rename(ORIGIN = "fct_explicit_na(ORIGIN)")
  veg_points_sum <- veg_points_sum %>% rename(CLASSIFICATION = "fct_explicit_na(CLASSIFICATION)")
  
  veg_points_sum <- filter(veg_points_sum, ORIGIN=="exotic" | ORIGIN=="native")
  veg_points_sum <- filter(veg_points_sum, 
                                 #CLASSIFICATION=="Aquatic" | 
                                   CLASSIFICATION=="Emergent" | 
                                   CLASSIFICATION=="Fringing_low" |
                                   CLASSIFICATION=="Fringing_high" |
                                   CLASSIFICATION=="Terrestrial" )
  
  veg_points_sum_wide <- spread(veg_points_sum, CLASSIFICATION, COVER)
  veg_points_sum_wide[c(8:11)][is.na(veg_points_sum_wide[c(8:11)])] <- 0
  veg_points_sum <- gather(veg_points_sum_wide, "CLASSIFICATION", "COVER", 8:11) # Use spread and then gather to add zeros
  
  
  veg_points_sum$CLASSIFICATION <- factor(veg_points_sum$CLASSIFICATION, 
                                                levels = c("Emergent","Fringing_low","Fringing_high","Terrestrial"))  # Treatment order
  
  
  veg_points_sum$ORIGIN <- factor(veg_points_sum$ORIGIN, 
                                        levels = c("native","exotic"))
  
  veg_points_sum_wide <- spread(veg_points_sum, ORIGIN, COVER)
  
  veg_points_sum_wide[is.na(veg_points_sum_wide)] <- 0
  
# No colour version of plot
  ggplot(veg_points_sum_wide, aes(x=native,y=exotic)) +
    geom_point() +
    theme_bw() + 
    xlim(0,200) +
    xlab("Native cover %") +
    ylab("Exotic cover %") +
    facet_grid(cols = vars(CLASSIFICATION)) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),  axis.line = element_line(colour = "black"))

# Colour by month  
  ggplot(veg_points_sum_wide, aes(x=native,y=exotic, color=MONTH)) +
    geom_point() +
    theme_bw() + 
    xlim(0,200) +
    xlab("Native cover %") +
    ylab("Exotic cover %") +
    #facet_grid(vars(ORIGIN), vars(CLASSIFICATION)) +
    facet_grid(cols = vars(CLASSIFICATION)) +
   # geom_vline(xintercept = baseflow_level, colour = "blue", lty = 2) +
  #  geom_vline(xintercept = springfresh_level, colour = "red", lty = 2) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),  axis.line = element_line(colour = "black"))
  

  ############### GRAZING
  
  
  
  veg_points_sum <- group_by(veg_points, SYSTEM, TRANSECT,fct_explicit_na(GRAZING), DATE, METRES, HEIGHT_AHD, 
                             fct_explicit_na(ORIGIN), fct_explicit_na(CLASSIFICATION)) %>% 
    summarise( COVER = sum(HITS)/40*100) 
  
  veg_points_sum$MONTH <- month(veg_points_sum$DATE,label=T)
  
  veg_points_sum <- veg_points_sum %>% rename(GRAZING = "fct_explicit_na(GRAZING)")
  veg_points_sum <- veg_points_sum %>% rename(ORIGIN = "fct_explicit_na(ORIGIN)")
  veg_points_sum <- veg_points_sum %>% rename(CLASSIFICATION = "fct_explicit_na(CLASSIFICATION)")
  
  veg_points_sum <- filter(veg_points_sum, ORIGIN=="exotic" | ORIGIN=="native")
  veg_points_sum <- filter(veg_points_sum, 
                           #CLASSIFICATION=="Aquatic" | 
                           CLASSIFICATION=="Emergent" | 
                             CLASSIFICATION=="Fringing_low" |
                             CLASSIFICATION=="Fringing_high" |
                             CLASSIFICATION=="Terrestrial" )
  
  veg_points_sum_wide <- spread(veg_points_sum, CLASSIFICATION, COVER)
  veg_points_sum_wide[c(9:12)][is.na(veg_points_sum_wide[c(9:12)])] <- 0
  veg_points_sum <- gather(veg_points_sum_wide, "CLASSIFICATION", "COVER", 9:12) # Use spread and then gather to add zeros
  
  
  veg_points_sum$CLASSIFICATION <- factor(veg_points_sum$CLASSIFICATION, 
                                          levels = c("Emergent","Fringing_low","Fringing_high","Terrestrial"))  # Treatment order
  
  
  veg_points_sum$ORIGIN <- factor(veg_points_sum$ORIGIN, 
                                  levels = c("native","exotic"))
  
  veg_points_sum <- filter(veg_points_sum, GRAZING=="Y"|GRAZING=="N")
  
 # Grazing  
  ggplot(veg_points_sum, aes(x=ORIGIN,y=COVER+1, fill=GRAZING)) +
    geom_boxplot() +
    theme_bw() + 
    ylim(0,400) +
    scale_fill_manual(values=c("grey", "white")) +
    xlab("Origin") +
    ylab("Cover %") +
    scale_y_log10() +
    #facet_grid(vars(ORIGIN), vars(CLASSIFICATION)) +
    facet_grid(cols = vars(SYSTEM)) +
    # geom_vline(xintercept = baseflow_level, colour = "blue", lty = 2) +
    #  geom_vline(xintercept = springfresh_level, colour = "red", lty = 2) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),  
          axis.line = element_line(colour = "black"),legend.position="top")
  
# 
  
  
  
  ######## General all site veg plot
  ############### 
  
  veg_points_sum <- group_by(veg_points, SYSTEM, TRANSECT,fct_explicit_na(GRAZING), DATE, METRES, HEIGHT_AHD, 
                             fct_explicit_na(ORIGIN), fct_explicit_na(CLASSIFICATION)) %>% 
    summarise( COVER = sum(HITS)/40*100) 
  
  veg_points_sum$MONTH <- month(veg_points_sum$DATE,label=T)
  
  veg_points_sum <- veg_points_sum %>% rename(GRAZING = "fct_explicit_na(GRAZING)")
  veg_points_sum <- veg_points_sum %>% rename(ORIGIN = "fct_explicit_na(ORIGIN)")
  veg_points_sum <- veg_points_sum %>% rename(CLASSIFICATION = "fct_explicit_na(CLASSIFICATION)")
  
  veg_points_sum <- filter(veg_points_sum, ORIGIN=="exotic" | ORIGIN=="native")
  veg_points_sum <- filter(veg_points_sum, 
                           #CLASSIFICATION=="Aquatic" | 
                           CLASSIFICATION=="Emergent" | 
                             CLASSIFICATION=="Fringing_low" |
                             CLASSIFICATION=="Fringing_high" |
                             CLASSIFICATION=="Terrestrial" )
  
  veg_points_sum_wide <- spread(veg_points_sum, CLASSIFICATION, COVER)
  veg_points_sum_wide[c(9:12)][is.na(veg_points_sum_wide[c(9:12)])] <- 0
  veg_points_sum <- gather(veg_points_sum_wide, "CLASSIFICATION", "COVER", 9:12) # Use spread and then gather to add zeros
  
  
  veg_points_sum$CLASSIFICATION <- factor(veg_points_sum$CLASSIFICATION, 
                                          levels = c("Emergent","Fringing_low","Fringing_high","Terrestrial"))  # Treatment order
  
  
  veg_points_sum$ORIGIN <- factor(veg_points_sum$ORIGIN, 
                                  levels = c("native","exotic"))
  
  veg_points_sum <- filter(veg_points_sum, GRAZING=="Y"|GRAZING=="N")
  
  # Grazing  
  ggplot(veg_points_sum, aes(x=CLASSIFICATION,y=COVER+1, fill=ORIGIN)) +
    geom_boxplot() +
    theme_bw() + 
    ylim(0,250) +
    scale_fill_manual(values=c("grey", "white")) +
    xlab("Vegetation type") +
    ylab("Cover %") +
    #scale_y_log10() +
    #facet_grid(vars(ORIGIN), vars(CLASSIFICATION)) +
    facet_grid(cols = vars(SYSTEM)) +
    # geom_vline(xintercept = baseflow_level, colour = "blue", lty = 2) +
    #  geom_vline(xintercept = springfresh_level, colour = "red", lty = 2) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),  
          axis.line = element_line(colour = "black"),legend.position="top",axis.text.x = element_text(angle = 90))
  
  
### RICHNESS  
  veg_points_sum <- group_by(veg_points, SYSTEM, TRANSECT,fct_explicit_na(GRAZING), DATE, METRES, HEIGHT_AHD, 
                             fct_explicit_na(ORIGIN), fct_explicit_na(CLASSIFICATION)) %>% 
    summarise( RICHNESS = length(SPECIES)) 
  
  veg_points_sum$MONTH <- month(veg_points_sum$DATE,label=T)
  
  veg_points_sum <- veg_points_sum %>% rename(GRAZING = "fct_explicit_na(GRAZING)")
  veg_points_sum <- veg_points_sum %>% rename(ORIGIN = "fct_explicit_na(ORIGIN)")
  veg_points_sum <- veg_points_sum %>% rename(CLASSIFICATION = "fct_explicit_na(CLASSIFICATION)")
  
  veg_points_sum <- filter(veg_points_sum, ORIGIN=="exotic" | ORIGIN=="native")
  veg_points_sum <- filter(veg_points_sum, 
                           #CLASSIFICATION=="Aquatic" | 
                           CLASSIFICATION=="Emergent" | 
                             CLASSIFICATION=="Fringing_low" |
                             CLASSIFICATION=="Fringing_high" |
                             CLASSIFICATION=="Terrestrial" )
  
  veg_points_sum_wide <- spread(veg_points_sum, CLASSIFICATION, RICHNESS)
  veg_points_sum_wide[c(9:12)][is.na(veg_points_sum_wide[c(9:12)])] <- 0
  veg_points_sum <- gather(veg_points_sum_wide, "CLASSIFICATION", "RICHNESS", 9:12) # Use spread and then gather to add zeros
  
  
  veg_points_sum$CLASSIFICATION <- factor(veg_points_sum$CLASSIFICATION, 
                                          levels = c("Emergent","Fringing_low","Fringing_high","Terrestrial"))  # Treatment order
  
  
  veg_points_sum$ORIGIN <- factor(veg_points_sum$ORIGIN, 
                                  levels = c("native","exotic"))
  
  veg_points_sum <- filter(veg_points_sum, GRAZING=="Y"|GRAZING=="N")
  
  # Grazing  
  ggplot(veg_points_sum, aes(x=CLASSIFICATION,y=RICHNESS, fill=ORIGIN)) +
    geom_boxplot() +
    theme_bw() + 
    #ylim(0,400) +
    scale_fill_manual(values=c("grey", "white")) +
    xlab("Vegetation type") +
    ylab("Species richness") +
    #scale_y_log10() +
    #facet_grid(vars(ORIGIN), vars(CLASSIFICATION)) +
    facet_grid(cols = vars(SYSTEM)) +
    # geom_vline(xintercept = baseflow_level, colour = "blue", lty = 2) +
    #  geom_vline(xintercept = springfresh_level, colour = "red", lty = 2) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),  
          axis.line = element_line(colour = "black"),legend.position="top",axis.text.x = element_text(angle = 90))
  
  