library(pointblank)
library(testthat)
library(testdat)
library(readr)
library(dplyr)
library(purrr)
library(here)

# Approach: For now, I will build the tests using a single dataset, the Campaspe dataset, 
# and then I will take the approach where all datasets in the raw_data/ are tested.

# ---------- Test Data ----------

# Site Metadata

VEFMAPS6_Site_Metadata <- read_csv("data/raw_data/site_data/VEFMAPS6_Site_Metadata.csv") %>% 
  janitor::clean_names()

# Species Lookup Data

species_master <- readr::read_csv(
  here::here("data", "raw_data", "veg_data", "VEFMAP_species_master.csv"),
  skip = 1,
  col_names = c(
    "species",
    "genus",
    "family",
    "origin",
    "lifeform",
    "classification",
    "instream_veg",
    "wpfg",
    "group",
    "rec_group"
  ),
  col_types =  readr::cols(
    .default = readr::col_character()
  )
) |>
  dplyr::filter(
    !duplicated(species)
  )


# Ground Layer Species Data
ground_species <- 
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
  )



# Point data
load_veg_data <- function(filename) {
  readr::read_csv(
    here::here("data", "raw_data", "veg_data", filename),)
}

filename <- "VEFMAPS7_Campaspe_2021_2022_2023_Point.csv"
veg_data <- load_veg_data(filename) %>% 
  janitor::clean_names()
  
set_testdata(veg_data, quosure = TRUE)



# ---------- Test that veg_data has appropriate R data structure and types ----------

test_that("veg_data is a data frame", {
  expect_is(veg_data, "data.frame")
})

test_that("veg_data has the correct columnn names", {
  colnames_raw <- c( #lowercase expected column names
    "system",
    "waterway", # `waterway` is recoded in the data wrangling process to `waterbody`, but has been entered to date as `waterway` in raw data
    "site",
    "transect",
    "sub_trans",
    "metres",
    "species",
    "origin",
    "hits",
    "height",
    "date",
    "survey"
  )
  
  map(colnames_raw, ~ expect_col_exists(veg_data, all_of(.x)))
  
})


# ---------- Test that veg_data has appropriate values ----------

test_that("veg_data has appropriate site, system, waterway values", {
  # Check that the `system` column has only one unique value
  expect_equal(length(unique(veg_data$system)), 1)
  
  # Check that the `waterway` column has only one unique value
  expect_equal(length(unique(veg_data$waterway)), 1)
  
  # System, waterway, and site values
  
  # Check that the `system` column is in the set of expected values
  expect_col_vals_in_set(veg_data, vars(system),VEFMAPS6_Site_Metadata %>% pluck("system",unique))
  
  # Check that the `waterway` column is in the set of expected values
  veg_data %>% expect_col_vals_in_set(vars(waterway),VEFMAPS6_Site_Metadata %>% pluck("waterway",unique))
  
  # Check that the `site` column is in the set of expected values
  veg_data %>% expect_col_vals_in_set(vars(site), VEFMAPS6_Site_Metadata %>% pluck("site",unique))
})


test_that("veg_data has appropriate species values", {
  # Check that the `species` column is in the set of expected values
  veg_data %>% expect_col_vals_in_set(vars(species), species_master %>% distinct(species))
  
})

# Investigate Species

veg_data %>% anti_join(species_master)


# ---------- Test that veg_data hits are OK ----------

test_that("veg_data hits are positive integers with no missing values", {
  # Check that the `hits` column has no missing values
  expect_col_no_na(veg_data, vars(hits))
  
  # Check that the `hits` column has only numeric values
  expect_col_type(veg_data, vars(hits), "numeric")
  
  # Check that the `hits` column has only positive values
  expect_col_vals_gt(veg_data, vars(hits), 0)
  
  # Check that the `hits` column has only integer values
  expect_col_vals_eq(veg_data, vars(hits), as.integer(hits))
  
})

test_that("veg_data hits are between 36 and 40, inclusive", {
  ground_layer_hits <-
    veg_data %>% 
    left_join(species_master) %>% 
    filter(classification == "Ground") %>% 
    group_by(survey, system, waterway, site, transect, metres) %>% 
    tally(hits,name = "ground_layer_hits")
  
  expect_col_vals_between(ground_layer_hits, vars(ground_layer_hits), 36, 40)
  
})


# ---------- Test that veg_data height are OK ----------

test_that("veg_data height are positive integers with no missing values", {
  # Check that the `height` column has no missing values
  expect_col_no_na(veg_data, vars(height))
  
  # Check that the `height` column has only numeric values
  expect_col_type(veg_data, vars(height), "numeric")
  
  # Check that the `height` column has only positive values
  expect_col_vals_gt(veg_data, vars(height), 0)
  
  # Check that the `height` column has only integer values
  expect_col_vals_eq(veg_data, vars(height), as.integer(height))
  
})
