# Generate Report with results of the following checks on species data:

# We can combine with the create GH issue from file action https://github.com/marketplace/actions/create-issue-from-file

# 1. Species names updated from spp.
# 2. Check species list against master species list
# 3. Species names spelled correctly
# 4. Consistent species names
# 5. Check existing comments
# 6. Update Monocots

# Load libraries

library(tidyverse)
library(readxl)
library(stringr)
library(janitor)
library(lubridate)

# Load data

# Load species data
species_master <- read_csv("data/raw_data/veg_data/VEFMAP_species_master.csv") %>% 
  janitor::clean_names()

# Load veg_data

veg_data <- read_csv("data/raw_data/veg_data/VEFMAPS7_Campaspe_2021_2022_2023_Point.csv") %>% 
  janitor::clean_names()

# ------------------------- Checks -------------------------

# Species names Updated from Spp.

# Check for species names that contain "spp." in the veg_data
# These species names need to be updated to the correct species name

has_spp <- 
  veg_data %>% 
  anti_join(species_master) %>% 
  filter(str_detect(species, "spp.")) %>%
  distinct(species, dc_comms, .keep_all = TRUE)

# If there are any species names that contain "spp." in the veg_data,
# check the species_master list to see if there is a matching species name without "spp." in the species column
# if so, update the species name in the veg_data with the species name in the species_master list

suggested_spp_replacements <- 
  has_spp %>%  
  mutate(spp_genus = str_remove(species, " spp.")) %>% 
  left_join(species_master, by = c("spp_genus" = "genus"),suffix = c("_check", "_replacement")) %>% 
  select(-spp_genus)

