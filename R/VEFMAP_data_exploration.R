#' ---
#' title: "VEFMAP data exploration"
#' author: William K. Morris
#' date: "`r Sys.Date()`"
#' output:
#'   rmarkdown::html_notebook:
#'     code_folding: hide
#' ---
#'
#'   CJ - 27/2/2019
#'
#' - How does inundation (either a longer term regime or a specific inundation
#'   event) alter the presence and abundance of different species or functional
#'   groups (FGs)? 
#' - Specifically, are species or FGs added or removed (from 0 cover to +ve, or
#'   +ve to 0) at sites in relation to flow regimes or events? Or is the cover 
#'   of species or FGs increased or decreased by flow regimes or events where 
#'   they are present? 
#' - How do these trends vary within rivers and between rivers? (within-river 
#'   variation can be along a gradient of upstream/downstream, or can be 
#'   separated into functional reaches separated by barriers, e.g. 
#'   dam walls or weirs.) 
#' - How does livestock grazing influence trends? 
#'  
#' REGIME = multiple events over a year or multiple years. Some people use 
#'          numbers like 'total days of inundation over the previous 12 months'
#'          or 'days inundation in a season' whatever makes most biological 
#'          sense. 
#'
#' EVENT  = a single rise and fall flow event. Some of these are environmental
#'          flow releases, such as 'freshes' or 'high flows', others are natural 
#'          rainfall flow events, and others are consumptive flows for e.g. 
#'          irrigators. Typically, the winter/spring flows are either natural 
#'          or e-flows, while the summer/autumn flows are e-flows or consumptive 
#'          flows. It may be important to categorise flow components into these
#'          groups over the survey period. 
#'  
#' NOTES:
#' 
#'    - I haven't provided the FGs as a column of data at this stage but I will
#'      do that. There are a few ways that the species can be grouped. 
#'    - We are interested in the places that have no cover of species or FGs 
#'      over the entire time period but they are likely to be places where the 
#'      conditions are unsuitable for those species/FGs. So this would be a
#'      specific question of where are the places that are more or less
#'      suitable. We might need some transect level data for this, e.g. steep
#'      slope, aspect, inside/outside bend etc. We don't want to average across
#'      all transects because we expect some locations to be unsuitable and they
#'      add a lot of noise to the responses of things where they are present. 
#'    - I might need to provide you with a bunch of covariate data, let me know
#'      what you think is needed next.
#'       
#'   CJ - 28/2/2019
#'
#' - Grazing is a site level factor. I will give you a list of all the grazed
#'   sites shortly but none of the Wimmera sites are grazed. The only nuance, 
#'   is for the grazing exclosures, which are transect level, i.e. at a grazed 
#'   site most transects will be grazed but 2 will be ungrazed. There are only 
#'   10 transects in the whole dataset that have exclosures and none of those 
#'   are in the Wimmera. The exclosures are paired though so we could have do 
#'   two levels of analysis: all sites, and a case study subsample of only the
#'   exclosure pairs (10 treatment and 10 control).
#'
#' - Inundation is a sub-transect level factor based on the hydrology data that 
#'   I haven't got yet. It will be continuous elevation data so for all
#'   subtransects we will have data for if and how long they were inundated.
#'
#' - I can get data about slope and aspect etc but I don't have it handy
#' 
#' CJ - 12/3/2019
#'
#' I've added the Campaspe point data to the Dropbox. This is the largest 
#' dataset and the most important one really. 
#'
#' This means there are three more files coming: 
#'   
#' - Campaspe recruits 
#' - Loddon points 
#' - Loddon recruits 
#'
#' I've also provided a metadata file for the grazed sites and grazing exclosure
#' transects on the Campaspe. I can populate this for the other rivers as well. 
#'
#' CJ - 13/3/2019
#'
#' I've added a file to the Dropbox for Campaspe sites. It has the elevations of 
#' all sub-transects at all sites. Actually, there are a couple missing, so it 
#' is NEARLY all the sub-transects. 
#'
#' There is also some flow elevation data for each of the six sites in there, 
#' this is just a subset and it is a single value for each day but we may end 
#' up having a finer timescale than this. 
#' 
#' I've saved the subtransect elevation sheet as a csv but haven't done this 
#' for the flow elevations for each site.

#+ setup, message = FALSE, fig.keep = "none", fig.show = "hide"
# SETUP ----
# knitr::opts_chunk$set(cache = TRUE)
# library(here)
# library(lme4)
# library(lubridate)
# library(readxl)
# library(rstanarm)
# library(tidyverse)

# Function to read in recruit or point data.
read_fun <- function(x) { 
  # Get and fix up the column names.
  nms <- scan(x, character(), nlines = 1, sep = ",") 
  nms <- make.names(nms, TRUE)
  nms <- recode(
    nms, 
    WATERWAY = "RIVER", METRES = "METRES_MIN", RECRUIT.SPECIES = "SPECIES",
    RECRUIT.ORIGIN = "ORIGIN", COUNT = "RECRUIT.COUNT"
  )
  # Read in the data.
  ans <- read_csv(
    x, col_types = cols(SURVEY = col_character(), EXCLOSURE = col_logical()),
    col_names = nms, skip = 1,
    # Specify the many different NA data (might consider fixing these in the raw
    # data).
    na = c("", "-", "#N/A", "NA", "Nil", "q", "unknown", "Unknown")
  )
  # Get the system name from the file name.
  ans$SYSTEM <- str_to_title(strsplit(x, "_")[[1]][2])
  ans <- select(ans, SYSTEM, everything())
  # Convert metres range to two columns.
  ans$METRES.1 <- NULL
  ans$METRES_RANGE <- NULL
  ans <- add_column(ans, METRES_MAX = ans$METRES_MIN + 1, .after = "METRES_MIN")
  ans$DATE <- parse_date_time(ans$DATE, "dmy")
  ans
}

#' # Water Level
#' Read in the water level data.
#+ read_waterlevel, message = FALSE
# WATER LEVEL ----
water_level <- map_dfr(
  # Skip the first two excel sheets.
  tail(excel_sheets(here::here("Campaspe_DELWP_transects_DGPS.xlsx")), -2),
  ~ add_column(
    drop_na(
      read_xlsx(
        here::here("Campaspe_DELWP_transects_DGPS.xlsx"), .x, "A2:D83",
        col_names = c("DATE", "TEMPERATURE", "DEPTH", "WATER_LEVEL"),
      )
    ),
    # Add a column for site derived from the sheet name.
    SITE = .x, .before = 1 
  )
)

#' Fix the spelling of Doaks site.
water_level$SITE <- recode(water_level$SITE, `Doakes` = "Doaks")

#' # Recruits
#' 
#' Read the recruits data.
#+ read_recruits, message = FALSE
# READ RECRUITS ----
vefmap_recruits <- map_dfr(list.files(here::here(), "Recruits.csv"), read_fun)

#' Fix some site names.
vefmap_recruits$SITE <- recode(
  vefmap_recruits$SITE,
  `Sharps Rd` = "Sharps Road", `Sutherland Creek` = "Sutherlands Creek"
)

#' Fix species names: capitalise genera etc.
vefmap_recruits$SPECIES <- map_chr(vefmap_recruits$SPECIES, str_to_sentence)

#' Fix species names: make names consitent.
vefmap_recruits$SPECIES <- recode(
  vefmap_recruits$SPECIES,
  `Unidentified monocot` = "Monocot",
  `Unidentified dicot herb` = "Dicot herb",
  `Unknown dicot herb` = "Dicot herb",
  `Vulpia sp.` = "Vulpia spp."
)

#' Split recruit count into two columns.
vefmap_recruits$RECRUIT.COUNT <- recode(
  vefmap_recruits$RECRUIT.COUNT,
  `0` = "0_0", `1` = "1_1", `2` = '2_2', `5` = "5_5", `7` = "7_7", 
  `Oct-50` = "10_50", `3` = "3_3", `10 to 50` = "10_50", `4` = "4_4",
  `6` = "6_6", `8` = "8_8", `50 to 100` = "50_100", `10` = "10_10",
  `100+` = "100_Inf", `9` = "9_9", `12` = "12_12"
)

vefmap_recruits <- separate(
  vefmap_recruits, RECRUIT.COUNT, c("RECRUITS_MIN", "RECRUITS_MAX")
)

#' Merge with the grazing data.
#+ merge_recruit_grazing, message = FALSE
# ATM this does nothing as only the Campaspe has grazing data and there is no
# recruit data for that system.
vefmap_recruits <- left_join(
  vefmap_recruits, 
  read_csv(
    here::here("SITE_Metadata_2016.17.csv"),  
    col_types = cols(EXCLOSURE = col_logical())
  )
)

#' Set some of the grazing data manually.
vefmap_recruits[vefmap_recruits$SITE == "Bakers Bridge", ]$GRAZED <- TRUE
vefmap_recruits <- replace_na(vefmap_recruits, list(GRAZED = FALSE))

#' Merge with the elevation data
#+ merge_recruit_elevation, message = FALSE
# ATM this does nothing as only the Campaspe has elevation data and there is no
# recruit data for that system.
vefmap_recruits <- left_join(
  vefmap_recruits,
  transmute(
    read_csv(here::here("Campaspe_subtransect_elevations.csv")),
    SITE = Site, TRANSECT = Transect, METRES_MIN = Metres, ELEVATION = Elevation
  )
)

#' Make the transect and subtransect names unique.
vefmap_recruits <- unite(
  vefmap_recruits, "TRANSECT", SITE, TRANSECT, remove = FALSE
)
vefmap_recruits <- unite(
  vefmap_recruits, "SUB_TRANS", TRANSECT, SUB_TRANS, remove = FALSE
)

#' Make sure the column data types are correct.
vefmap_recruits <- type_convert(
  vefmap_recruits, col_types = cols(SURVEY = col_character())
)

#' # Points
#' 
#' Read the points data.
#+ read_points, message = FALSE
# READ POINTS ----
vefmap_points <- map_dfr(list.files(here::here(), "Point.csv"), read_fun)

#' Fix some site names.
vefmap_points$SITE <- recode(
  vefmap_points$SITE,
  `Sharps Rd` = "Sharps Road", `Sutherland Creek` = "Sutherlands Creek"
)

#' Fix species names: capitalise genera etc.
vefmap_points$SPECIES <- map_chr(vefmap_points$SPECIES, str_to_sentence)

#' Fix species names: make names consitent.
vefmap_points$SPECIES <- recode(
  vefmap_points$SPECIES,
  `Exo` = "Exotic",
  `Moss/ lichen` = "Moss/Lichen",
  `Moss/lichen` = "Moss/Lichen",
  `Raphanus raphinastrum` = "Raphanus raphanistrum",
  `Rytidosperma setacea` = "Rytidosperma setaceum",
  `Triglochin striatum` = "Triglochin striata",
  `Unidentified exotic` = "Exotic",
  `Unidentified dicot herb` = "Dicot herb",
  `Unidentified monocot` = "Monocot"
)

#' Remove the transect/surveys that were skipped.
vefmap_points <- vefmap_points[-grep("Skipped", vefmap_points$SPECIES), ]

#' Fix the origin data.
vefmap_points$ORIGIN <- recode(
  vefmap_points$ORIGIN,
  `Exotic Grasstic` = "exotic"
)
vefmap_points$ORIGIN <- recode(
  vefmap_points$SPECIES,
  Exo = "exotic",
  `Moss/Lichen` = "ground",
  `Moraea lewisiae` = "exotic",
  `Rytidosperma setaceum` = "native",
  `Sonchus asper` = "exotic",
  Tree = "ground",
  `Trifolium arvense var. arvense` = "exotic",
  `Vittadinia spp.` = "native",
  .default = vefmap_points$ORIGIN
)

#' Fix the NA hits.
# At the moment the only NA datapoint was entered as "q". Guessing that this is
# data entry mistake and is meant to "1"
vefmap_points <- replace_na(vefmap_points, list(HITS = 1))

#' Make height data consistent.
vefmap_points$HEIGHT <- recode(
  vefmap_points$HEIGHT,
  `<Null>` = "0_1m",
  `1_5`    = "1_5m",
  `1_5M`   = "1_5m"
)

#' Replace height range data with two columns.
vefmap_points <- separate(
  vefmap_points, HEIGHT, c("HEIGHT_MIN", "HEIGHT_MAX"), sep = '_|m',
  extra = 'drop'
)

#' Fix some presumably incorrect date entry. 
vefmap_points[
  vefmap_points$SITE      == "Doaks" & 
  vefmap_points$TRANSECT  == 6 &
  vefmap_points$SUB_TRANS == "E" &
  vefmap_points$SURVEY    == 4, 
]$DATE <- as.POSIXct(date("2017-08-21"))

vefmap_points[
  vefmap_points$SITE        == "Strathallan" & 
    vefmap_points$TRANSECT  == 1 &
    vefmap_points$SUB_TRANS == "W" &
    vefmap_points$SURVEY    == 3,
  ]$DATE <- as.POSIXct(date("2017-05-25"))

#' Replace the NA exlosure data with FALSE.
vefmap_points <- replace_na(vefmap_points, list(EXCLOSURE = FALSE))

#' Merge with the grazing data.
#+ merge_point_grazing, message = FALSE
vefmap_points <- left_join(
  vefmap_points, 
  read_csv(
    here::here("SITE_Metadata_2016.17.csv"), 
    col_types = cols(EXCLOSURE = col_logical())
  )
)

#' Set some of the grazing data manually.
vefmap_points[vefmap_points$SITE == "Bakers Bridge", ]$GRAZED <- TRUE
vefmap_points <- replace_na(vefmap_points, list(GRAZED = FALSE))

#' Merge with the elevation data.
#+ merge_point_elevation, message = FALSE
vefmap_points <- left_join(
  vefmap_points,
  transmute(
    read_csv(here::here("Campaspe_subtransect_elevations.csv")),
    SITE = Site, TRANSECT = Transect, METRES_MIN = Metres, ELEVATION = Elevation
  )
)

#' Make the transect and subtransect names unique.
vefmap_points <- unite(
  vefmap_points, "TRANSECT", SITE, TRANSECT, remove = FALSE
)
vefmap_points <- unite(
  vefmap_points, "SUB_TRANS", TRANSECT, SUB_TRANS, remove = FALSE
)

#' Make sure the column data types are correct.
vefmap_points <- type_convert(
  vefmap_points, col_types = cols(SURVEY = col_character())
)

#' Merge with the recruit data.
#+ merge_points_recruit, message = FALSE
vefmap_points <- full_join(vefmap_points, vefmap_recruits)

#' Merge with the water level data.
#+ merge_points_waterlevel, message = FALSE
vefmap_points <- left_join(
  vefmap_points,
  spread(
    transmute(water_level, SITE, DATE = paste0("LEVEL-", DATE), WATER_LEVEL),
    DATE, -SITE
  )
)

#' Adjust the water level data so that it is relative to the subtransect
#' elevation.
vefmap_points <- mutate_at(
  vefmap_points, vars(starts_with("LEVEL-")), ~. - ELEVATION
)

#' Calculate whether each subtransect was ever inundated before the survey took
#' place.
#  WIP: This is a slow way of doing this. And needs a proper check that it is
#' doing the right thing.
vefmap_points$INUNDATED <- local({
  prfx   <- "LEVEL-"
  # Get the dates of the water level measurements.
  dt     <- colnames(vefmap_points)
  dt     <- grep(prfx, dt, value = TRUE)
  dt     <- gsub(prfx, "", dt)
  dt     <- date(dt)
  # Earliest water level date.
  min_dt <- min(dt)
  # Latest water level date.
  max_dt <- max(dt)
  map_lgl(
    seq(nrow(vefmap_points)),
    function(x) {
      ans <- slice(vefmap_points, x)
      survey_dt <- date(ans$DATE)
      # If the survey was before the earliest water level measurement then cant
      # say if site was recently inundated.
      if (survey_dt < min_dt) return(NA)
      # Which which water level measurements are relevant to the survey date.
      dt_cols <- paste0(prfx, seq(min_dt, survey_dt, 1))
      if (survey_dt > max_dt) dt_cols <- paste0(prfx, seq(min_dt, max_dt, 1))
      # Of the relevent water level survey dates which ones was the water level
      # above the transect.
      ans <- select(ans, one_of(dt_cols))
      ans <- mutate_all(ans, ~ . > 1)
      # If they were all NA return NA
      if (all(is.na(unlist(ans)))) return(NA)
      # If any where TRUE return TRUE 
      ans <- any(unlist(ans), na.rm = TRUE)
      # If all were FALSE and the survey was conducted after the last water
      # level measurment then return NA.
      if (survey_dt > max_dt & !ans) return(NA)
      ans
    }
  )
})

#' Extract data for a species.
#+ pasjab, message = FALSE
PASJUB <- local({
  ans <- select(vefmap_points, SUB_TRANS, SPECIES, SURVEY, HITS)
  ans <- complete(
    ans, SPECIES, nesting(SUB_TRANS, SURVEY), fill = list(HITS = 0)
  )
  ans <- filter(ans, SPECIES == "Paspalidium jubiflorum")
  # ADD filter to take out sites where this species never occurs.
  ans <- select(ans, -SPECIES)
  inundated <- left_join(
    ans, distinct(select(vefmap_points, SUB_TRANS, SURVEY, INUNDATED))
  )
  inundated <- spread(
    select(inundated, -HITS), SURVEY, INUNDATED, sep = "_INUNDATED_"
  )
  grazed <- left_join(
    ans, distinct(select(vefmap_points, SUB_TRANS, SURVEY, GRAZED))
  )
  grazed <- spread(select(grazed, -HITS), SURVEY, GRAZED, sep = "_GRAZED_")
  ans <- left_join(spread(ans, SURVEY, HITS, sep = "_HITS_"), inundated)
  ans <- left_join(ans, grazed)
  left_join(
    ans, 
    distinct(select(vefmap_points, SUB_TRANS, SITE, TRANSECT, RIVER, SYSTEM))
  )
})

#' ## Example model: Maximum likelihood.
mod_PASJUB <- lmer(
  (SURVEY_HITS_2 - SURVEY_HITS_1) ~ SURVEY_GRAZED_2 + (1 | SITE),
  data = PASJUB
)
summary(mod_PASJUB)
coef(mod_PASJUB)

#' ## Example model: Bayesian.
mod_PASJUB_stan <- stan_lmer(
  (SURVEY_HITS_2 - SURVEY_HITS_1) ~ SURVEY_GRAZED_2 + (1 | SITE),
  data = PASJUB
)
summary(mod_PASJUB_stan)

#' ## Plots

#+ plotpoints, warning = FALSE, fig.height = 12, fig.width = 11
# PLOT POINTS ----
vefmap_points %>% 
filter(ORIGIN %in% c("exotic", "native") & METRES_MIN < 4) %>%
group_by(RIVER, SITE, TRANSECT, METRES_MIN, ORIGIN, DATE) %>% 
summarise(HITS = sum(HITS)) %>%
ggplot() +
aes(DATE, HITS, color = ORIGIN) +
geom_smooth(method = "lm", se = FALSE) +
geom_point(size = .6, alpha = .3) +
ylim(0, 120) +
facet_grid(RIVER + SITE ~ METRES_MIN) +
scale_colour_viridis_d(option = "C") +
theme_dark()

#+ plotrecruits, warning = FALSE, fig.height = 12, fig.width = 11
# PLOT RECRUITS ----
vefmap_points %>% 
filter(
  ORIGIN %in% c("exotic", "native") & METRES_MIN < 4 & SYSTEM != "Campaspe"
) %>%
group_by(RIVER, SITE, TRANSECT, METRES_MIN, ORIGIN, DATE) %>% 
summarise(RECRUITS_MIN = sum(RECRUITS_MIN, na.rm = TRUE)) %>%
ggplot() +
aes(DATE, RECRUITS_MIN, color = ORIGIN) +
geom_smooth(method = "lm", se = FALSE) +
geom_point(size = .6, alpha = .3) +
ylim(0, 120) +
facet_grid(RIVER + SITE ~ METRES_MIN) +
scale_colour_viridis_d(option = "C") +
theme_dark()
