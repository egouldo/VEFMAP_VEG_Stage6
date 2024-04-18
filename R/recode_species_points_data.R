# ----- Functions for automatically applying species recoding and spelling fixes to veg data -----
## ----- Create Lookup Objects ------ #TODO remove once merged into master as appears on diff branch in R/data.R
library(dplyr)
library(purrr)

.system_list <- c("Campaspe", "Wimmera", "Moorabool", "Loddon", "Yarra", "ThomsonMacalister", "Glenelg") %>%
  set_names(.) 

## ------ Define Helper Functions ------

#' Get a named list from a dataframe
#' 
#' @param df A dataframe
#' @param name_col The name of the column to use as the name of the list
#' @param value_col The name of the column to use as the value of the list
#' @return A named list
#' @export
#' @examples
#' df <- tibble(name = c("a", "b", "c"), value = c(1, 2, 3))
#' get_named_list_from_df(df, "name", "value")
#' #> $a
#' #> [1] 1
#' #>
#' #> $b
#' #> [1] 2
#' #>
#' #> $c
#' #> [1] 3
#' 
#' @importFrom dplyr select
#' @importFrom tibble deframe
#' @importFrom rlang sym
#' @details
#' Designed as a helper function used within the `get_spp_fixes` function
get_named_list_from_df <- function(df, name_col, value_col){
  df %>%
    select(!!name_col, !!value_col) %>%
    deframe()
}

#' Get suggested species fixes
#' 
#' @param df A dataframe
#' @param .ignore_suggestions A named character vector of suggested fixes to ignore
#' @param name_col The name of the column to use as the name of the list
#' @param value_col The name of the column to use as the value of the list
#' @return A named list of suggested fixes
#' @export
#' @importFrom dplyr filter group_by if_any mutate ungroup
#' @importFrom rlang sym
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

#' Recode a column in a dataframe using a recode table
#' 
#' @param df A dataframe
#' @param col_to_recode The name of the column to recode
#' @param recode_table A named list of recoding values
#' @return A dataframe with the column recoded
#' @export
#' @importFrom dplyr mutate
#' @importFrom rlang sym
#' @examples
#' df <- tibble(x = c("a", "b", "c"))
#' recode_table <- list(a = "apple", b = "banana", c = "carrot")
#' recode_veg_data(df, "x", recode_table)
#' #> # A tibble: 3 x 1
#' #>   x
#' #>   <chr>
#' #> 1 apple
#' #> 2 banana
#' #> 3 carrot
#' @importFrom rlang sym
#' @importFrom dplyr mutate
recode_veg_data <- function(df,col_to_recode, recode_table){
  out <- df %>% 
    mutate(!!col_to_recode := recode(!!sym(col_to_recode), !!!recode_table))
  
  out
} 

#' Get the veg data file for a given system, stage, and type
#' 
#' @param stage The stage of the VEFMAP data
#' @param system The system of the VEFMAP data
#' @param type The type of the VEFMAP data
#' @return A character string of the filename
#' @export
#' @details
#' Gets the files from the `data/raw_data/veg_data` directory, keeping only those that match the
#' stage, system, and type. If no matching file is found, an error is thrown.
#' @importFrom here here
#' @importFrom stringr str_detect
#' @import cli
#' @importFrom glue glue
#' @importFrom purrr keep
get_veg_file <- function(stage, system, type){
  
  filename <- dir(here::here("data", "raw_data", "veg_data")) %>% 
    keep(., str_detect(., glue("{stage}_")) & str_detect(., system) & str_detect(., glue("_{type}")))
  
  if(length(filename) == 0){
    cli_abort("No matching veg file found for system == `{system}`, stage == `{stage}`, type == `{type}`")
  }
  filename
}

#' Recode species in veg data files based on suggested replacements
#' 
#' @param df A dataframe of veg data
#' @param replace_spp_from_comments A dataframe of suggested replacements
#' @return A dataframe of veg data with species recoded
#' @export
#' @importFrom dplyr rows_update select rename                                                                                                                ~notes,                             ~suggested_replacement,
recode_from_comment <- function(df, replace_spp_from_comments) {
  out <- df %>% 
    rows_update(replace_spp_from_comments %>% 
                  select(-SPECIES) %>% 
                  rename(SPECIES = suggested_replacement)
                  , by = "NOTES", unmatched = "ignore")
  
  out
}

#' Apply fixes to all veg data files where there is a suggested replacements file
#' 
#' @param type The type of fixes to apply. Either "spelling_fixes", "species_to_recode" or "manual_recode_from_comments"
#' @param .ignore_suggestions A named character vector of suggested fixes to ignore
#' @param write A logical indicating whether to write the updated files to disk
#' @return A tibble of the latest fixes
#' @export
#' @importFrom dplyr group_by filter mutate pull ungroup select last
#' @importFrom purrr map map2 walk2 flatten_chr pmap map_chr keep
#' @importFrom readr read_csv write_csv cols col_character
#' @importFrom stringr str_detect str_extract str_split 
#' @importFrom fs dir_info
#' @importFrom here here
#' @import cli
#' @examples
#' \dontrun{
#' .ignore_suggestions <- c("Epilobium billardiereanum f. billardiereanum", #Moorabool
#' "Eucalyptus camaldulensis var. camaldulensis", #Moorabool
#' "Juncus articulatus var. articulatus", #Moorabool
#' "Carex dunnii") #Glenelg, more likely to be: Carex gunniana var. gunniana 
#' apply_veg_data_fixes_from_files(type = "spelling_fixes", .ignore_suggestions = .ignore_suggestions, write = FALSE)
#' }
apply_veg_data_fixes_from_files <- function(type = c("spelling_fixes", "species_to_recode", "manual_recode_from_comments"), .ignore_suggestions = NULL, write = FALSE) {
  
  if(type == "spelling_fixes"){
    error_col <- "species"
    replacement_col <- "suggested_replacement"
  } else if(type == "species_to_recode"){
    error_col <- "species_veg_data"
    replacement_col <- "species_master"
  }
  
  get_VEFMAP_stage <- . %>%  
    str_split("_") %>% 
    flatten_chr() %>% 
    keep(., str_detect(., "VEFMAPS"))
  
  latest_fixes <-
    dir_info(here::here("outputs", "QA_reports", "suggested_fixes", type)) %>% 
    select(path, change_time) %>% 
    filter(str_detect(path, ".csv")) %>%
    mutate(system = str_extract(path, paste0(.system_list, collapse= "|")),
           file_type = str_extract(path, paste0(c("Point", "Recruits"), collapse = "|")),
           stage = map_chr(path, get_VEFMAP_stage)) %>% 
    group_by(system, file_type, stage) %>% 
    filter(change_time == last(change_time)) %>% # for each unique QA (system by points by stage), get the latest file
    ungroup %>% 
    mutate(spp_fixes_data = map(path, ~readr::read_csv(.x)), 
           veg_data_file = pmap(.l = list(stage, system, file_type), .f = get_veg_file) %>% flatten_chr,
           veg_data = map(.x = veg_data_file,
                          ~ readr::read_csv(here::here( "data/raw_data/veg_data/", .x),
                                            col_types = readr::cols(.default = 
                                                                      readr::col_character()))
           ))
  
  if(type == "spelling_fixes" | type == "species_to_recode"){
    latest_fixes <- latest_fixes %>%
      mutate(
        spp_fixes_l = map(spp_fixes_data, get_spp_fixes, .ignore_suggestions, error_col, replacement_col),
        veg_data_recoded = map2(.x = veg_data, .y = spp_fixes_l, 
                                ~ recode_veg_data(.x, "SPECIES", .y))
      )
  } else if(type == "manual_recode_from_comments"){
    latest_fixes <- latest_fixes %>%
      mutate(veg_data_recoded = map2(.x = veg_data, .y = spp_fixes_data, 
                                     ~ recode_from_comment(.x, .y)))
    
  }
  
  if(write == TRUE){
    latest_fixes %>% 
      pull(veg_data_recoded, veg_data_file) %>% 
      walk2(.x = ., .y = names(.), ~ write_csv(x = .x, file = here::here("data", "raw_data", "veg_data", .y)))
    
    cli::cli_alert_info("Veg data files have been updated to file")
  }
  
  latest_fixes
}
