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
           if_any(matches("score"), ~ .x > 0.5)) # remove low confidence suggestions, if col exists (only present in spelling fixes)
  
  # remove suggestions we marked for ignoring, if exists
   if(!is.null(.ignore_suggestions)){
     out <-  filter(out,!(!!sym(value_col) %in% .ignore_suggestions))
   }
  
  # Remove replacements where there is more than one suggestion for the same species
  # (These will need to be manually fixed)
  out <- out %>% 
    group_by(!!sym(name_col)) %>% 
    filter(n() == 1) %>% 
    ungroup()
      
  out <- out %>% get_named_list_from_df(., name_col, value_col)
  
  out
  
}


recode_veg_data <- function(df,col_to_recode, recode_table){
  out <- df %>% 
    mutate(!!col_to_recode := recode(!!sym(col_to_recode), !!!recode_table))
  
  out
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
# favour replacing with .subsp. as per the species master file
.ignore_suggestions <- c("Epilobium billardiereanum f. billardiereanum", #Moorabool
                         "Eucalyptus camaldulensis var. camaldulensis", #Moorabool
                         "Juncus articulatus var. articulatus", #Moorabool
                         "Carex dunnii") #Glenelg, more likely to be: Carex gunniana var. gunniana ?

# Execute Species Recoding

# extract function from the code that creates object latest_spelling_fixes
# to apply to the latest spelling fixes

apply_veg_data_fixes_from_files <- function(type = c("spelling_fixes", "species_to_recode"), .ignore_suggestions = NULL, write = FALSE) {
  
  if(type == "spelling_fixes"){
    error_col <- "species"
    replacement_col <- "suggested_replacement"
  } else if(type == "species_to_recode"){
    error_col <- "species_veg_data"
    replacement_col <- "species_master"
  }
  
  latest_fixes <-
    dir_info(here::here("outputs", "QA_reports", "suggested_fixes", type)) %>% 
    select(path, change_time) %>% 
    filter(str_detect(path, ".csv")) %>%
    mutate(system = str_extract(path, paste0(.system_list, collapse= "|")),
           file_type = str_extract(path, paste0(c("Point", "Recruits"), collapse = "|")),
           stage = map_chr(path, get_VEFMAP_stage)) %>% 
    group_by(system, file_type, stage) %>% 
    filter(change_time == last(change_time)) %>% # for each unique QA (system by points by stage), get the latest file
    mutate(spp_fixes_data = map(path, ~readr::read_csv(.x)), 
           spp_fixes_data = map(spp_fixes_data, get_spp_fixes, .ignore_suggestions, error_col, replacement_col),
           veg_data_file = pmap(.l = list(stage, system, file_type), .f = get_veg_file) %>% flatten_chr,
           veg_data = map(.x = veg_data_file,
                          ~ readr::read_csv(here::here( "data/raw_data/veg_data/", .x),
                                            col_types = readr::cols(.default = 
                                                                      readr::col_character()))
           ),
           veg_data_recoded = map2(.x = veg_data, .y = spp_fixes_data, 
                                   ~ recode_veg_data(.x, "SPECIES", .y))
           
    )
  
  if(write == TRUE){
    latest_fixes %>% 
      pull(veg_data_recoded, veg_data_file) %>% 
      walk2(.x = ., .y = names(.), ~ write_csv(x = .x, file = here::here("data", "raw_data", "veg_data", .y)))
    
    cli::cli_alert_info("Veg data files have been updated to file")
  }
  
  latest_fixes
}


# Apply spelling fixes to all veg data files where there is a suggested replacements file
# apply_veg_data_fixes_from_files(type = "spelling_fixes", .ignore_suggestions = .ignore_suggestions, write = TRUE)

apply_veg_data_fixes_from_files(type = "species_to_recode", .ignore_suggestions = .ignore_suggestions, write = TRUE)
