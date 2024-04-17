library(dplyr)
library(here)

ground_spp <- 
  tribble(
    ~SPECIES, ~GENUS, ~FAMILY, ~ORIGIN, ~LIFEFORM, ~CLASSIFICATION, ~`INSTREAMVEG?`, ~WPFG, ~WPFGSOURCE, ~GROUP, ~REC_GRP,
    "Log", NA, NA, "Ground", NA, "Ground", NA, NA, NA, "Ground", "Ground",
    "Tree Root", NA, NA, "Ground", NA, "Ground", NA, NA, NA, "Ground", "Ground",
    "Tree", NA, NA, "Ground", NA, "Ground", NA, NA, NA, "Ground", "Ground",
    "Tree Base", NA, NA, "Ground", NA, "Ground", NA, NA, NA, "Ground", "Ground",
    "Tree (dead)", NA, NA, "Ground", NA, "Ground", NA, NA, NA, "Ground", "Ground",
    "Tree (stump)", NA, NA, "Ground", NA, "Ground", NA, NA, NA, "Ground", "Ground",
    "Rock", NA, NA, "Ground", NA, "Ground", NA, NA, NA, "Ground", "Ground",
    "Sand", NA, NA, "Ground", NA, "Ground", NA, NA, NA, "Ground", "Ground",
    "Poo", NA, NA, "Ground", NA, "Ground", NA, NA, NA, "Ground", "Ground"
  )

species_master <- 
  read_csv(here::here("data/raw_data/veg_data/VEFMAP_species_master.csv"),
           show_col_types = FALSE)

species_master %>% 
  filter(CLASSIFICATION == "Ground") %>% 
  mutate(ORIGIN = recode(ORIGIN, 
                         `exotic` = "Ground",
                         `native` = "Ground")) %>% 
  bind_rows(., 
            anti_join(species_master, ., by = "SPECIES"), #recode origin for species
            ground_spp # add ground species not included
            ) %>% 
  distinct() %>% 
  write_csv(here::here("data/raw_data/veg_data/VEFMAP_species_master.csv"))
