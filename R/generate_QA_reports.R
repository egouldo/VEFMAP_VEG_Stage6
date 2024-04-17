# This script is used to generate the QA reports for the S7 data
# Load the required libraries
library(dplyr)
library(purrr)
library(here)
library(stringr)
library(cli)
library(gert)
library(quarto)

# source("R/data.R") #TODO need to source once .system_list deleted below
source("R/utils.R")

.system_list <- c("Campaspe", "Wimmera", "Moorabool", "Loddon", "Yarra", "ThomsonMacalister", "Glenelg") %>%
  set_names(.) #TODO remove once merged into master as appears on diff branch in R/data.R

filenameS7 <- dir(here::here("data", "raw_data", "veg_data")) %>% 
  keep(., str_detect(., "VEFMAPS7_") & str_detect(., "_Point"))

if(length(filenameS7) == 0){
  cli::cli_abort("No S7 data found")
  } else{
  
    QA_reports <- tibble(
      system = 
        filenameS7 %>% str_extract(paste0(.system_list, collapse = "|")),
      filename = here::here("data", "raw_data", "veg_data", filenameS7),
      file_sha = filenameS7 %>% 
        paste0("data/raw_data/veg_data/", .) %>%  
        gert::git_stat_files() %>% 
        pluck("head")
    ) %>% mutate(
      file_sha_short = substrRight(file_sha, 8),
      output_file = glue::glue("VEFMAPS7_{system}_QA_Report_{lubridate::today()}_{file_sha_short}.md"),
      execute_params = map2(filename, file_sha, ~ list(input_file = .x, file_sha = .y))
                 )
   
  }


QA_reports |>
  filter(system == "Campaspe") |>
  select(output_file, execute_params) |>
  purrr::pwalk(
    quarto_render_move,
    input = "inst/extdata/_extensions/template_QA_veg_data.qmd",
    output_format = "md",
    output_dir = here::here("outputs", "QA_reports")
  )
