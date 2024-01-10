#### Checking vegetation survey spreadsheets for errors ####
### Pointing data and recruits data across VEFMAP sites ###


#working directory - for LVs computer
#setwd("C:/Users/lv0e/Dropbox/VEFMAP_MainStuff/PROJECT_Vegetation response to flows paper/Data")
#setwd("C:/Users/cj0a/Dropbox/VEFMAP_MainStuff/PROJECT_Vegetation response to flows paper/Data")
setwd("C:/Users/lv0e/OneDrive - Department of Environment, Land, Water and Planning/R/VEFMAP_VEG_Stage6/VEFMAP_VEG_Stage6/data/raw_data/veg_data")

# library(tidyverse)
# library(dplyr)

#read in data
vegdata <- read.csv("VEFMAPS6_Glenelg_2018_2019_Point.csv")
vegdata <- read.csv("VEFMAPS6_Campaspe_2017_2018_2019_2020_Point.csv")
vegdata <- read.csv("VEFMAPS6_Glenelg_2018_2019_Recruits.csv")
vegdata <- read.csv("VEFMAPS6_Yarra_2018_2019_Point.csv")
vegdata <- read.csv("VEFMAPS6_ThomsonMacalister_2018_2019_Point.csv")
vegdata <- read.csv("VEFMAPS6_Wimmera_2017_2018_2019_Point.csv")
vegdata <- read.csv("VEFMAPS6_Wimmera_2017_2018_2019_Recruits.csv")
vegdata <- read.csv("VEFMAPS6_Yarra_2018_2019_Recruits.csv")
vegdata <- read.csv("VEFMAPS6_ThomsonMacalister_2018_2019_Recruits.csv")
vegdata <- read.csv("VEFMAPS6_Moorabool_2017_2018_Final_Point.csv")
vegdata <- read.csv("VEFMAPS6_Moorabool_2017_2018_Final_Recruits.csv")
vegdata <- read.csv("VEFMAPS6_Loddon_2017_2018_Point.csv")
vegdata <- read.csv("VEFMAPS6_Loddon_2017_2018_Recruits.csv")

speciesmaster <- read.csv("VEFMAP_species_master.csv")

#Column headers, (#delete the ORIGIN ROW (for now) - NO not yet)
names(vegdata)

#Check names of waterways are correct 
waterwaycheck<- 
  select(vegdata,WATERWAY) %>%
  unique() %>%
  arrange(WATERWAY)

#Check names of sites are correct 
sitecheck<- 
  select(vegdata,SITE) %>%
  unique() %>%
  arrange(SITE)

# Site coding for W Gippsland points
#unique(vegdata$SITE)
#[1] Th4a 01 (Cowwarr)  Th4a 03 (Heyfield) Th5 (Riverview)    Hagans Bridge      Bellbird Reserve   Forsyth Road      
#[7] Cowwarr            Heyfield           Riverview          Cowwarr Road       Riverview Road    

#vegdata$SITE <- recode(
  #vegdata$SITE,
  #`Th4a 01 (Cowwarr)` = "Cowwarr",
  #`Th4a 03 (Heyfield)` = "Heyfield",
  #`Th5 (Riverview)` = "Riverview",
  #`Hagans Bridge` = "Hagans",
  #`Bellbird Reserve` = "Bellbird",
  #`Forsyth Road` = "Forsyth",
  #'Cowwarr Road' = "Cowwarr",
  #'Riverview Road' = 'Riverview')

#unique(vegdata$SITE)
#[1] Cowwarr   Heyfield  Riverview Hagans    Bellbird  Forsyth  
#Levels: Bellbird Cowwarr Forsyth Hagans Heyfield Riverview

#write.csv(vegdata,file = file.choose(new = T))

# Site coding for W Gippsland recruits
# unique(vegdata$SITE)
#[1] Th4a 01          Th4a 03          Th5              Hagans Bridge    Bellbird         Forsyth Road    
#[7] Cowwarr          Heyfield         Riverview        Bellbird Reserve Cowwarr Road     Riverview Road

#vegdata$SITE <- recode(vegdata$SITE,
#`Th4a 01` = "Cowwarr",
#`Th4a 03` = "Heyfield",
#`Th5` = "Riverview",
#`Hagans Bridge` = "Hagans",
#`Bellbird Reserve` = "Bellbird",
#`Forsyth Road` = "Forsyth",
#'Cowwarr Road' = "Cowwarr",
#'Riverview Road' = 'Riverview')


# Site coding for Wimmera points
#unique(vegdata$SITE)
#[1] McGuiness Bridge                 Millers Road                     Peuckers Road                   
#[4] Wonwondah Gauge                  Grahams Bridge                   Mt Victory Road                 
#[7] Tatlock Bridge                   Wonwondah Road                   Lake Lonsdale                   
#[10] Roses Gap Road                   Sheepwash Creek                  Tobacco Road                    
#[13] Mt William Tobacco Rd            Rose's Gap                       Rose's Gap (Hall's Gap in book?)
#[16] NE Wanwanda Mackenzie            Graham's Bridge                  Macinnes Burnt creek            
#[19] Miller's Road Burnt Creek        Peuck's Road                     Wanwanda Guage                  
#[22] Tatlock's Bridge                 Mt Victory Road Mackenzie River  Macinnes burnt creek            
#[25] Roses Gap                        McInnes                          Millers                         
#[28] Pueckers 

#vegdata$SITE <- recode(
#vegdata$SITE,
#'McGuiness Bridge' = 'MacInnes',
#'Millers Road' = 'Millers',
#'Peuckers Road'= 'Peuckers',
#'Wonwondah Gauge'='WonwondahGauge',
#'Grahams Bridge'= 'GrahamsBridge',
#'Mt Victory Road' = 'MtVictory',
#'Tatlock Bridge' = 'Laharum',
#'Wonwondah Road' = 'NEWonwondah',
#'Lake Lonsdale' = 'LakeLonsdale',
#'Roses Gap Road'= 'RosesGap',
#'Sheepwash Creek' = 'Sheepwash',
#'Tobacco Road' = 'Tobacco',
#'Mt William Tobacco Rd' = 'Tobacco',
#"Rose's Gap" = 'RosesGap',
#"Rose's Gap (Hall's Gap in book?)" = 'RosesGap',
#"NE Wanwanda Mackenzie" = "NEWonwondah",
#"Graham's Bridge" = "GrahamsBridge",
#'Macinnes Burnt creek' = 'MacInnes',
#"Miller's Road Burnt Creek" = 'Millers',
#"Peuck's Road" =  'Peuckers',
#"Wanwanda Guage" = 'WonwondahGauge',                  
#"Tatlock's Bridge" = 'Laharum',
#"Mt Victory Road Mackenzie River" = 'MtVictory',
#"Macinnes burnt creek" =  "MacInnes",        
#"Roses Gap" = "RosesGap",
#"McInnes" = "MacInnes",
#"Pueckers" = "Peuckers"
#)

#unique(vegdata$SITE)
#[1] MacInnes       Millers        Peuckers       WonwondahGauge GrahamsBridge  MtVictory     
#[7] Laharum        NEWonwondah    LakeLonsdale   RosesGap       Sheepwash      Tobacco 

# Site coding for Wimmera points
#unique(vegdata$SITE)

#[1] McGuiness Bridge                 Millers Road                     Peuckers Road                   
#[4] Wonwondah Gauge                  Grahams Bridge                   Mt Victory Road                 
#[7] Tatlock Bridge                   Wonwondah Road                   Lake Lonsdale                   
#[10] Roses Gap Road                   Sheepwash Creek                  Tobacco Road                    
#[13] Mt William Tobacco Rd            Rose's Gap                       Rose's Gap (Hall's Gap in book?)
#[16] NE Wanwanda Mackenzie            Graham's Bridge                  Macinnes Burnt Creek            
#[19] Miller's Road Burnt Creek        Peucks Road                      Wanwanda Gauge                  
#[22] Tatlock's Bridge                 Mt Victory Road Mackenzie River  Peuck's Road                    
#[25] Wanwanda Guage

#vegdata$SITE <- recode(vegdata$SITE,
#'McGuiness Bridge' = 'MacInnes',
#'Millers Road' = 'Millers',
#'Peuckers Road'= 'Peuckers',
#'Wonwondah Gauge'='WonwondahGauge',
#'Grahams Bridge'= 'GrahamsBridge',
#'Mt Victory Road' = 'MtVictory',
#'Tatlock Bridge' = 'Laharum',
#'Wonwondah Road' = 'NEWonwondah',
#'Lake Lonsdale' = 'LakeLonsdale',
#'Roses Gap Road'= 'RosesGap',
#'Sheepwash Creek' = 'Sheepwash',
#'Tobacco Road' = 'Tobacco',
#'Mt William Tobacco Rd' = 'Tobacco',
#"Rose's Gap" = 'RosesGap',
#"Rose's Gap (Hall's Gap in book?)" = 'RosesGap',
#"NE Wanwanda Mackenzie" = "NEWonwondah",
#"Graham's Bridge" = "GrahamsBridge",
#'Macinnes Burnt Creek' = 'MacInnes',
#"Miller's Road Burnt Creek" = 'Millers',
#"Peucks Road" =  'Peuckers',
#"Wanwanda Gauge" = 'WonwondahGauge',                  
#"Tatlock's Bridge" = 'Laharum',
#"Mt Victory Road Mackenzie River" = 'MtVictory',
#"Peuck's Road" = "Peuckers",
#"Wanwanda Guage" = "WonwondahGauge")

sitecheck2 <- vegdata %>%
  group_by(SYSTEM,WATERWAY,SITE) %>%
  summarise(n()) %>%
  arrange(WATERWAY)

#write.csv(vegdata,file = file.choose(new = T))


#check that the dates and surveys match and make sense and all surveys are present
surveydatecheck<- vegdata %>%
  group_by(DATE,SURVEY) %>%
  summarise(n()) %>%
  arrange(SURVEY)

surveydatecheck2<- vegdata %>%
  group_by(SURVEY) %>%
  summarise(n()) %>%
  arrange(SURVEY)
  
#check that subtransect and metres match. Letters should uniquely match distances.
subtransectcheck<- vegdata %>%
  group_by(SUB_TRANS,METRES) %>%
  summarise(n()) %>%
  arrange(METRES)

subtransectcheck2<- vegdata %>%
  group_by(METRES) %>%
  summarise(n()) %>%
  arrange(METRES)

subtransectcheck3<- vegdata %>%
  group_by(SITE, TRANSECT) %>%
  summarise(max(METRES)) %>%
  arrange(SITE)

#extra check for recruits files - check that subtransects and letters
# also match the distances correctly
subtransectcheck3<- vegdata %>%
  group_by(SUB_TRANS,METRES,METRES.1) %>%
  summarise(n()) %>%
  arrange(METRES)

#check exlcoures
exclosurecheck <-vegdata  %>% 
  group_by(SITE,SURVEY,TRANSECT,EXCLOSURE) %>%
  summarise(n()) %>%
  arrange(!EXCLOSURE)

#check ground layers add to 40
unique(vegdata$HITS) #check for any non-numbers

groundlayercheck<- vegdata %>%
  group_by(SURVEY,SITE,TRANSECT,METRES,ORIGIN) %>%
  summarise(sum(HITS)) %>%
  filter(ORIGIN=="ground")

groundlayercheck2<- vegdata %>%
  group_by(SURVEY,SITE,TRANSECT,METRES,ORIGIN) %>%
  summarise(Sum_hits = sum(HITS)) %>%
  filter(ORIGIN=="ground" & Sum_hits>40)   # For any over 40 (can't be > 40)

groundlayercheck3<- vegdata %>%
  group_by(SURVEY,SITE,TRANSECT,METRES,ORIGIN) %>%
  summarise(Sum_hits = sum(HITS)) %>%
  filter(ORIGIN=="ground" & Sum_hits<36)   # For any <36 (some <36 are correct, but some will be errors)

#list of unique species. check for spelling, spaces etc
speciescheck<- 
  select(vegdata,SPECIES) %>%
  unique() %>%
  arrange(SPECIES)

#check against species master, this gives the original table with the species not in the master 
speciesnotinmaster<-vegdata[!vegdata$SPECIES %in% speciesmaster$SPECIES,]

#unique list of the species not in the master, ignore the ground layers
speciesnotinmaster<-
  select(speciesnotinmaster,SPECIES) %>%
  unique() %>%
  arrange(SPECIES)


# Fix species names: make names consistent (spellings are correct - copied and pasted directly from VicFlora)

vegdata$SPECIES <- recode(
  vegdata$SPECIES,
  `Tree root` = "Tree Root",
  `Aster subulatus` = "Symphyotrichum subulatum",
  `Triglochin procera` = "Cycnogeton procerum",
  `Kikuyu` = "Cenchrus clandestinus",
  `Rytidosperma setacea` = "Rytidosperma setaceum",
  `Pennisetum clandestinum` = "Cenchrus clandestinus",
  'Conyza spp.' = "Erigeron spp.",
  'Conyza bonariensis' = 'Erigeron bonariensis',
  `log` = "Log",
  `litter` = "Litter",
  'tree' = 'Tree',
  'Moss' = 'Moss/Lichen',
  'Hordeum murinum' = 'Hordeum leporinum',
  'Alternanthera denticulata s.s.' = 'Alternanthera denticulata',
  'Eucalyptus camaldulensis var. camaldulensis' ='Eucalyptus camaldulensis subsp. camaldulensis',
  'Anagallis arvensis' = 'Lysimachia arvensis',
  'Melicytus dentatus' = 'Melicytus dentatus s.l.',
  'Triglochin procera' = 'Cycnogeton procerum',
  'Verbena bonariensis' = 'Verbena bonariensis s.l.',
  'Pseudognaphalium luteoalbum' = 'Laphangium luteoalbum'
  )




#if need save file #don't forget to add .csv when creating file name
#can save straight into working directory
#write.csv(vegdata,file = file.choose(new = T))






