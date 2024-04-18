source("R/recode_species_points_data.R")

spp_recoded <- apply_veg_data_fixes_from_files("species_to_recode", NULL, write = FALSE)

system = "Moorabool"

spp_recoded %>% 
  pull(spp_fixes_data, system) %>% 
  pluck(system) %>% 
  semi_join(spp_recoded %>% 
              pull(veg_data, system) %>% 
              pluck(system), ., 
            by = c( "SPECIES" = "species_veg_data")) %>% 
  distinct() %>% 
  select(SPECIES, NOTES) %>% 
  distinct() %>% 
  drop_na()


out_file <- spp_recoded %>% 
  filter(system == !!system) %>% 
  pluck("path") %>% 
  fs::path_ext_remove() %>% 
  basename() %>% 
  str_replace(., "species_to_recode", "manual_recode_from_comments") %>% 
  fs::`path_ext<-`(".csv") 

replace_spp_from_comments <-
  tibble::tribble(
    ~SPECIES,                                                                                                                 ~NOTES,                             ~suggested_replacement,
    "Acaena",                  "(Didn't specify, presumably acaena novae-zelandiae based on other records this survey at this site)",                 "Acaena novae-zelandiae",
    # "Acaena",                  NA,                 "Acaena novae-zelandiae",
    "Lomandra",                     "(Didn't specify, presumably lomandra longifolia based on other records this survey at this site)",                    "Lomandra longifolia",
    "Typha",                       "(Didn't specify, presumably Typha domingensis based on other records this survey at this site)",                      "Typha domingensis",
    "Anthoxanthum",                   "(Didn't specify, presumably Anthoxanthum odoratum based on other records this survey at this site)",                  "Anthoxanthum odoratum",
    "Crassula",                        "(Didn't specify, presumably crassula helmsii based on other records this survey at this site)",                       "Crassula helmsii",
    # "Crassula",                        NA,                       "Crassula helmsii",
    "Poa",  "(Didn't specify, presumably Poa labillardierei var. labillardierei based on other records this survey at this site)", "Poa labillardierei var. labillardierei",
    "Hypochaeris",                    "(Didn't specify, presumably hypochaeris radicata based on other records this survey at this site)",                   "Hypochaeris radicata",
    "Cycnogeton",                     "(Didn't specify, presumably Cycnogeton procerum based on other records this survey at this site)",                    "Cycnogeton procerum",
    "Cyperus",                         "(Didn't specify, presumably Cyperus lucidus based on other records this survey at this site)",                        "Cyperus lucidus",
    "Hypochaeris",                    "(Didn't specify, presumably Hypochaeris radicata based on other records this survey at this site)",                   "Hypochaeris radicata",
    "Senecio",                   "(Didn't specify, presumably Senecio runcinifolius based on other records this survey at this site)",                  "Senecio runcinifolius",
    "Oxalis",                        "(Didn't specify, presumably Oxalis perennans based on other records this survey at this site)",                       "Oxalis perennans",
    "Anthoxanthum",                    "(Didn't specify, presumably Anthoxanthum odoratum based on other records this survey at BOSTOCK?)",                  "Anthoxanthum odoratum"
  )


write_csv(replace_spp_from_comments, 
          here::here("outputs", 
                     "QA_reports", 
                     "suggested_fixes", 
                     "manual_recode_from_comments", 
                     out_file))

apply_veg_data_fixes_from_files("manual_recode_from_comments", NULL, write = TRUE) #%>% 
  # pluck("veg_data_recoded", 1) %>% 
  # filter(SPECIES %in% replace_spp_from_comments$SPECIES) 


dat %>% 
  filter(SPECIES %in% replace_spp_from_comments$SPECIES)  %>% 
  print(., n = nrow(.)) %>% 
  filter()

