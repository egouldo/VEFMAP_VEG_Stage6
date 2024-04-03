# Generate Report with results of the following checks on species data:

# We can combine with the create GH issue from file action https://github.com/marketplace/actions/create-issue-from-file

# - [x] 1. Species names updated from spp.
# - [x] 2. Check species list against master species list
# - [x] 3. Species names spelled correctly
# - [x] 4. Consistent species names (I think the spell check and spp. check should cover this)
# - [ ] 5. Check existing comments
# - [ ] 6. Update Monocots

# Load libraries

library(tidyverse)
library(readxl)
library(stringr)
library(janitor)
library(lubridate)
library(taxize)

# Load data

# Load species data
species_master <- read_csv("data/raw_data/veg_data/VEFMAP_species_master.csv") %>% 
  janitor::clean_names()


ground_spp <- 
  tribble(
    ~species, ~genus, ~family, ~origin, ~lifeform, ~classification, ~instream_veg, ~wpfg, ~group, ~rec_group, ~X11,
    "Log", NA, NA, "exotic", NA, "Ground", NA, NA, NA, "Ground", "Ground",
    "Tree root", NA, NA, "exotic", NA, "Ground", NA, NA, NA, "Ground", "Ground",
    "Tree", NA, NA, "exotic", NA, "Ground", NA, NA, NA, "Ground", "Ground",
    "Tree base", NA, NA, "exotic", NA, "Ground", NA, NA, NA, "Ground", "Ground",
    "Tree (dead)", NA, NA, "exotic", NA, "Ground", NA, NA, NA, "Ground", "Ground",
    "Tree (stump)", NA, NA, "exotic", NA, "Ground", NA, NA, NA, "Ground", "Ground",
    "Rock", NA, NA, "exotic", NA, "Ground", NA, NA, NA, "Ground", "Ground",
    "Sand", NA, NA, "exotic", NA, "Ground", NA, NA, NA, "Ground", "Ground",
    "Poo", NA, NA, "exotic", NA, "Ground", NA, NA, NA, "Ground", "Ground"
  )

# Load veg_data

veg_data <- read_csv("data/raw_data/veg_data/VEFMAPS7_Campaspe_2021_2022_2023_Point.csv") %>% 
  janitor::clean_names()

# ------------------------- Checks -------------------------

# 1. Species names to be updated from Spp.

# Check for species names that contain "spp." in the veg_data
# These species names need to be updated to the correct species name

has_spp <- 
  veg_data %>% 
  anti_join(species_master) %>% 
  filter(str_detect(species, "spp.")) %>%
  distinct(species, dc_comms, .keep_all = TRUE) #TODO consider adding other columns to call distinct on for help with context..
#TODO .keep_all might only keep the first instance?? so could be a problem if there are multiple instances of the same species name

# If there are any species names that contain "spp." in the veg_data,
# check the species_master list to see if there is a matching species name without "spp." in the species column
# if so, update the species name in the veg_data with the species name in the species_master list

suggested_spp_replacements <- 
  has_spp %>%  
  mutate(spp_genus = str_remove(species, " spp.")) %>% 
  left_join(species_master, 
            by = c("spp_genus" = "genus"), 
            suffix = c("_check", "_replacement")) %>% 
  select(-spp_genus)

# 2. Species names spelled correctly

# Check that species not in species_master are spelt correctly

db_title <- "World Flora Online Plant List 2023-12"
db_datasource <- gnr_datasources() %>% filter(title %in% db_title)

taxa_check_result <- 
  veg_data %>% 
  anti_join(species_master) %>% 
  anti_join(ground_spp) %>% 
  anti_join(., {has_spp %>% distinct(species)}) %>% #rm species identified as spp.
  pull(species) %>%
  gnr_resolve(data_source_ids = db_datasource$id, with_canonical_ranks = TRUE) %>% 
  mutate(matched = user_supplied_name == matched_name2,
         suggested_replacement = ifelse(matched == FALSE, matched_name2, NA)) %>% 
  select(-matched_name2) %>% 
  rename(species = user_supplied_name)
  
# Check that suggested replacements are in species_master
# If not, suggest in report that species_master should be updated

# If so, replace the species name in the veg_data with the suggested replacement
taxa_check_result %>% 
  drop_na(suggested_replacement) %>%
  left_join(., species_master, by = c("suggested_replacement" = "species"))

# if nrow not 0, then replace species name in veg_data with suggested replacement

#TODO insert code... when to run in process? After report??
#TODO generate report of auto-fixed species names, these should be flagged and checked by user,
# With opportunity to revert to original name if necessary (this would require updating species master) if reversion
# required, so that the check passes and the species name is not flagged again in the future.

# 3. After fixing spelling, check species list against master species list, report missing entries

spp_not_in_master <- 
  veg_data %>% 
  anti_join(species_master) %>% 
  anti_join(ground_spp) %>% 
  anti_join(., {has_spp %>% distinct(species)}) %>% #rm species identified as spp.
  distinct(species, dc_comms) #rm duplicates but keep dc_comms for context
#TODO keep other columns for some context? Could have two different df's one as short list, the other with all context data.
#TODO mark as flagged for spelling fix, or for user to check and update species master

# 5. Check Comments

# Check that comments are not empty

non_empty_comments <- 
  veg_data %>% 
  filter(dc_comms != "") %>% 
  select(species, dc_comms)
non_empty_comments %>% distinct() %>% print(., n = nrow(.))

#TODO What to do with comments??


# 6. Update Monocots


# 7. Check NAs in HITS

# Investigate NA hits
NA_hits <- 
  veg_data %>% filter(is.na(hits))

# 8. Check Ground Layer Hits

# Ground layer: reduce any that are over 40 (For the sites where sum of ground layer >40, reduce the number of hits of the layer with the most hits)

ground_layer_hits <-
  veg_data %>% 
  left_join(species_master) %>% 
  filter(classification == "Ground") %>% 
  group_by(survey, system, waterway, site, transect, metres) %>% 
  tally(hits,name = "ground_layer_hits")

# Check for sites where sum of ground layer hits > 40

ground_layer_hits_over_40 <- 
  ground_layer_hits %>% 
  filter(ground_layer_hits > 40)

# Identify ground layer species with sum(hits) over 40

layer_type_over_40 <- 
  veg_data %>% 
  left_join(species_master) %>% 
  filter(classification == "Ground") %>% 
  inner_join(ground_layer_hits_over_40, 
             by = c("survey", "system", "waterway", "site", "transect", "metres")) %>% 
  group_by(survey, system, waterway, site, transect, metres, species) %>%
  tally(hits, name = "ground_layer_species_hits")

# Identify the ground layer species with the most hits for each subtransect
max_layer_type_over_40 <- 
  layer_type_over_40 %>% 
  group_by(survey, system, waterway, site, transect, metres) %>% 
  filter(ground_layer_species_hits == max(ground_layer_species_hits))

suggested_hits_reductions <- 
  layer_type_over_40 %>% 
  group_by(survey, system, waterway, site, transect, metres) %>% 
  arrange(desc(ground_layer_species_hits), 
          .by_group = TRUE) %>% 
  mutate(max_val = max(ground_layer_species_hits), 
         total = sum(ground_layer_species_hits), 
         diff = total - max_val, 
         reduced_value = 40 - diff) %>% # subtract hits from the layer with the most hits to reduce total to 40
  ungroup %>%  
  inner_join(max_layer_type_over_40) # remove ground layer species which are non-max hits


# Check for sites where sum of ground layer hits < 36
ground_layer_hits_under_36 <- 
  ground_layer_hits %>% 
  filter(ground_layer_hits < 36)

#TODO check data book to see where error is
#  Ground layer: fix those under 36 (check data book to see where error is)



# ----------------------- Generate Report -----------------------

# Generate report with results of the checks

#TODO generate report with results of the checks
