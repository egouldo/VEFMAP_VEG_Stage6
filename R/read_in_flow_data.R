# Read in Streamology Flow data
# Author: Elliot Gould

# library(tidyverse)
# library(readxl)

read_flow_dat <- function(filepath, skip_lines = integer(1L), directory) {
  readxl::read_xlsx(paste0(directory, filepath), sheet = 1,skip = skip_lines,trim_ws = TRUE) %>% 
    select(1:4) %>% 
    readr::write_csv(path = paste0("./data/raw_data/flow_data/", tools::file_path_sans_ext(filepath), ".csv"))
}

#Campaspe
campaspe <- tibble(filename = list.files("./data/Stage 2 data_v4/Campaspe/"),
       skip_lines = c(5,6,20,16,13,6))

map2(.x = campaspe$filename, .y = campaspe$skip_lines, .f = ~read_flow_dat(.x,.y, directory = "./data/Stage 2 data_v4/Campaspe/"))

#Glenelg
glenelg <- tibble(filename = list.files("./data/Stage 2 data_v4/Glenelg/"),
                  skip_lines = c(10,6,6,7))

map2(.x = glenelg$filename, .y = glenelg$skip_lines, .f = ~read_flow_dat(.x,.y, directory = "./data/Stage 2 data_v4/Glenelg/"))

#Loddon

loddon <- tibble(filename = list.files("./data/Stage 2 data_v4/Loddon/"),
                  skip_lines = c(5,12,17,5))

map2(.x = loddon$filename, .y = loddon$skip_lines, .f = ~read_flow_dat(.x,.y, directory = "./data/Stage 2 data_v4/Loddon/"))


#Moorabool
moorabool <- tibble(filename = list.files("./data/Stage 2 data_v4/Moorabool/"),
                 skip_lines = c(12,9))

map2(.x = moorabool$filename, .y = moorabool$skip_lines, .f = ~read_flow_dat(.x,.y, directory = "./data/Stage 2 data_v4/moorabool/"))

#Thomson
thomson <- tibble(filename = list.files("./data/Stage 2 data_v4/Thomson/"),
                    skip_lines = c(12,14,13,11,11,10))
map2(.x = thomson$filename, .y = thomson$skip_lines, .f = ~read_flow_dat(.x,.y, directory = "./data/Stage 2 data_v4/Thomson/"))

#Wimmera

wimmera <- tibble(filename = list.files("./data/Stage 2 data_v4/Wimmera/"),
                  skip_lines = c(6,14,5,11,17))

map2(.x = wimmera$filename, .y = wimmera$skip_lines, .f = ~read_flow_dat(.x,.y, directory = "./data/Stage 2 data_v4/Wimmera/"))

#Yarra

read_flow_dat_alt <- function(filepath, skip_lines = integer(1L), directory) {
  readxl::read_xlsx(paste0(directory, filepath), sheet = 1,skip = skip_lines,trim_ws = TRUE) %>% 
    select(1:3) %>% 
    readr::write_csv(path = paste0("./data/raw_data/flow_data/", tools::file_path_sans_ext(filepath), ".csv"))
}

yarra <- tibble(filename = list.files("./data/Stage 2 data_v4/Yarra/"),
                  skip_lines = c(9,2,9,1))

map2(.x = yarra$filename, .y = yarra$skip_lines, .f = ~read_flow_dat_alt(.x,.y, directory = "./data/Stage 2 data_v4/Yarra/"))


#### Files with missing cols

readxl::read_xlsx("./data/Mount_William_Ck_Roses_Level.xlsx", sheet = 1,skip = 5,trim_ws = TRUE) %>% 
    select(1:3) %>% 
    readr::write_csv(path = paste0("./data/raw_data/flow_data/", tools::file_path_sans_ext("Mount_William_Ck_Roses_Level"), ".csv"))

readxl::read_xlsx("./data/Mount_William_Ck_Tobacco_Level.xlsx", sheet = 1,skip = 5,trim_ws = TRUE) %>% 
  select(1:3) %>% 
  readr::write_csv(path = paste0("./data/raw_data/flow_data/", tools::file_path_sans_ext("Mount_William_Ck_Tobacco_Level"), ".csv"))
