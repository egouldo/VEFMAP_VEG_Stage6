# Recode spelling names in Moorabool veg data

library(tidyverse)
library(here)
library(cli)
library(fs)

source("R/utils.R")
source("R/data.R")

get_named_list_from_df <- function(df, name_col, value_col){
  df %>%
    select(!!name_col, !!value_col) %>%
    deframe()
}

get_VEFMAP_stage <- . %>%  str_split("_") %>% flatten_chr() %>% keep(., str_detect(., "VEFMAPS"))

.system_list <- c("Campaspe", "Wimmera", "Moorabool", "Loddon", "Yarra", "ThomsonMacalister", "Glenelg") %>%
  set_names(.) #TODO remove once merged into master as appears on diff branch in R/data.R

get_veg_file <- function(stage, system, type){
  
  filename <- dir(here::here("data", "raw_data", "veg_data")) %>% 
    keep(., str_detect(., glue("{stage}_")) & str_detect(., system) & str_detect(., glue("_{type}")))
  
  if(length(filename) == 0){
    cli_abort("No matching veg file found for system == `{system}`, stage == `{stage}`, type == `{type}`")
  }
  filename
}

debug(get_veg_file)


# Load data
veg_data <- readr::read_csv(here("data/raw_data/veg_data/VEFMAPS7_Moorabool_2022_2023_Point.csv"),
                            col_types = readr::cols(.default = readr::col_character())) # to preserve erroneously entered text that needs to be manually fixed


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
         spp_fixes_data = map(spp_fixes_data, ~get_named_list_from_df(.x, "species", "suggested_replacement")),
         veg_data_file = pmap(.l = list(stage, system, file_type), .f = get_veg_file) %>% flatten_chr)




get_named_list_from_df(latest_spelling_fixes$data[[1]], "species", "suggested_replacement")


.spelling_fixes <- 
  spelling_fixes %>% 
  filter(!is.na(genus)) %>% 
  pull(suggested_replacement, species)

veg_data %>% 
  mutate(SPECIES = recode(SPECIES, !!!.spelling_fixes))

