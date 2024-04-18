# Recode spelling names in Moorabool veg data

library(tidyverse)
library(here)
library(cli)
library(fs)

source("R/utils.R")
source("R/data.R")

# Define Helper Functions

get_named_list_from_df <- function(df, name_col, value_col){
  df %>%
    select(!!name_col, !!value_col) %>%
    deframe()
}

get_spp_fixes <- function(df, .ignore_suggestions = NULL, name_col = "species", value_col = "suggested_replacement"){
  
  out <- df %>% 
    mutate(words = strsplit(!!sym(value_col), "\\W+") %>% 
             lengths) %>% # remove single word suggestions
    filter(words > 1, 
           score > 0.5)
  
  # remove suggestions we marked for ignoring, if exists
   if(is.null(.ignore_suggestions)){
     out <-  filter(out,!(!!sym(value_col) %in% .ignore_suggestions))
   }
      
  out %>% get_named_list_from_df(., name_col, value_col)
  
}


recode_veg_data <- function(df,col_to_recode, recode_table){
  df %>% 
    mutate(!!col_to_recode := recode(!!sym(col_to_recode), !!!recode_table))
} 

get_VEFMAP_stage <- . %>%  str_split("_") %>% flatten_chr() %>% keep(., str_detect(., "VEFMAPS"))


get_veg_file <- function(stage, system, type){
  
  filename <- dir(here::here("data", "raw_data", "veg_data")) %>% 
    keep(., str_detect(., glue("{stage}_")) & str_detect(., system) & str_detect(., glue("_{type}")))
  
  if(length(filename) == 0){
    cli_abort("No matching veg file found for system == `{system}`, stage == `{stage}`, type == `{type}`")
  }
  filename
}

# Create Lookup Objects

.system_list <- c("Campaspe", "Wimmera", "Moorabool", "Loddon", "Yarra", "ThomsonMacalister", "Glenelg") %>%
  set_names(.) #TODO remove once merged into master as appears on diff branch in R/data.R

# suggested fixes we don't want to implement because there are matches in species master
# or there are multiple matches in World Flora Bank and we manually picked the closest match.
.ignore_suggestions <- c("Epilobium billardiereanum f. billardiereanum", #Moorabool
                         "Eucalyptus camaldulensis var. camaldulensis", #Moorabool
                         "Juncus articulatus var. articulatus", #Moorabool
                         "Carex dunnii") #Glenelg, more likely to be: Carex gunniana var. gunniana ?

# Execute Species Recoding

latest_spelling_fixes <-
  dir_info(here::here("outputs", "QA_reports", "suggested_fixes", "spelling_fixes")) %>% 
  select(path, change_time) %>% 
  filter(str_detect(path, ".csv")) %>%
  mutate(system = str_extract(path, paste0(.system_list, collapse= "|")),
         file_type = str_extract(path, paste0(c("Point", "Recruits"), collapse = "|")),
         stage = map_chr(path, get_VEFMAP_stage)) %>% 
  group_by(system, file_type, stage) %>% 
  filter(change_time == last(change_time)) %>% # for each unique QA (system by points by stage), get the latest file
  mutate(spp_fixes_data = map(path, ~readr::read_csv(.x) %>% filter(is.na(genus))), # remove spp that aren't in species master
         spp_fixes_data = map(spp_fixes_data, get_spp_fixes, .ignore_suggestions, "species", "suggested_replacement"),
         veg_data_file = pmap(.l = list(stage, system, file_type), .f = get_veg_file) %>% flatten_chr,
         veg_data = map2(.x = veg_data_file, .y = spp_fixes_data,
                        ~ readr::read_csv(here::here( "data/raw_data/veg_data/", .x),
                                          col_types = readr::cols(.default = 
                                                                    readr::col_character())) %>% 
                          recode_veg_data(., "SPECIES", .y)
         )
           
  )


# Don't forget to save the recoded veg data!

# walk(latest_spelling_fixes, ~write_csv(.x$veg_data, here::here("data", "raw_data", "veg_data", .x$veg_data_file)))




