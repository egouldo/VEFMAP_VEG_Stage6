# Clean Site Metadata as per issue #24
# Author: Elise Gould
# Date: 29 May 2020

library(tidyverse)

site_metadata <- readr::read_csv("./data/raw_data/site_data/GPS_All.csv", col_types = 
                                 cols(
                                   .default = col_double(),
                                   SYSTEM = col_character(),
                                   WATERWAY = col_character(),
                                   SITE = col_character(),
                                   TRANSECT = col_character(),
                                   METRES = col_character(),
                                   TRANS_SUBTRANS = col_character(),
                                   GPS_NOTE = col_character(),
                                   GPS_DATE = col_character(),
                                   DATAFILE = col_character()
                                 )
)

# Clean

removed_sites <- dplyr::slice(site_metadata, 571:589)

site_metadata_cleaned <- site_metadata %>% 
  dplyr::slice(1:570, 590:nrow(.)) %>% #remove sites that Chris will clean later 
  tidyr::separate(col = TRANS_SUBTRANS, into = c("TRANS_", "METRES_"), sep = "_", remove = FALSE) %>% 
  dplyr::mutate(TRANS_ = str_remove(string = TRANS_, pattern = "\\D"), #remove everything except digits in TRANS_
         TRANSECT = ifelse(!is.na(TRANS_) & is.na(TRANSECT), TRANS_,
                           TRANSECT), #Don't write over existing entries of TRANSECT
         METRES = ifelse(!is.na(METRES_) & is.na(METRES), METRES_, 
                         METRES), #Don't write over existing entries of TRANSECT
         METRES = str_remove(string = METRES, pattern = "M")) %>% 
  dplyr::select(-TRANS_, -METRES_) %>% 
  dplyr::bind_rows(., removed_sites)

  #1545, missing entry because TRANS_SUBTRANS = T6_


# Checks:
#check nrow OK
nrow(site_metadata) == nrow(site_metadata_cleaned) 

#check: everything that has TRANS & SUBTRANS has metres, and everything with SUBTRANS and METRES has TRANS
site_metadata_cleaned %>% 
  dplyr::filter(!is.na(METRES) & !is.na(TRANS_SUBTRANS) & TRANSECT == "") #OK

site_metadata_cleaned %>% 
  dplyr::filter(!is.na(TRANSECT) & !is.na(TRANS_SUBTRANS) & METRES == "") # 1row

site_metadata_cleaned %>% dplyr::slice(1184, 1185) #@TODO From warnings on separate, get Chris to Check these rows. No `_` in 1184

if(nrow(site_metadata) == nrow(site_metadata_cleaned)) readr::write_csv(x = site_metadata_cleaned,path = "./data/raw_data/site_data/GPS_All.csv")
