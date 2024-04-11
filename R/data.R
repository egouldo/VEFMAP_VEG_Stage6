# lists of target WPFGs and veg types
.wpfg_list <- c(
  "ARp",
  "ATe",
  "ATl",
  "Ate",
  "Atl",
  "Se",
  "Sk",
  "Tda",
  "Tdr"
)
.rec_group_list <- c("Terrestrial", "Riparian", "Aquatic", "Emergent")
.classification_list <- c(
  "Aquatic", 
  "Emergent", 
  "Fringing_low",
  "Fringing_high",
  "Terrestrial"
)
.species_rename <- c(
  "Tree root" = "Tree Root",
  "Aster subulatus" = "Symphyotrichum subulatum",
  "Triglochin procera" = "Cycnogeton procerum",
  "Kikuyu" = "Cenchrus clandestinus",
  "Rytidosperma setacea" = "Rytidosperma setaceum",
  "Pennisetum clandestinum" = "Cenchrus clandestinus",
  "Conyza spp." = "Erigeron spp.",
  "Conyza bonariensis" = "Erigeron bonariensis",
  "log" = "Log",
  "litter" = "Litter",
  "tree" = "Tree",
  "Moss" = "Moss/Lichen",
  "Hordeum murinum" = "Hordeum leporinum",
  "Alternanthera denticulata s.s." = "Alternanthera denticulata",
  "Eucalyptus camaldulensis var. camaldulensis" = "Eucalyptus camaldulensis subsp. camaldulensis",
  "Anagallis arvensis" = "Lysimachia arvensis",
  "Melicytus dentatus" = "Melicytus dentatus s.l.",
  "Triglochin procera" = "Cycnogeton procerum",
  "Verbena bonariensis" = "Verbena bonariensis s.l.",
  "Pseudognaphalium luteoalbum" = "Laphangium luteoalbum"
)
.system_list <- c("Campaspe", "Wimmera", "Moorabool", "Loddon", "Yarra", "ThomsonMacalister", "Glenelg") %>% 
  set_names(.)
# lookup to list waterways in each system
.waterway_lu <- list(
  Campaspe = c("Campaspe_R"),
  Glenelg = c("Glenelg_R", "Wannon"),
  Loddon = c("Loddon_R", "Tullaroop", "Twelve_Mile"),
  Moorabool = c("Moorabool_R"),
  ThomsonMacalister = c("Macalister_R", "Thomson_R"),
  Wimmera = c("Burnt_Ck", "MacKenzie_R", "Mount_William_Ck"),
  Yarra = c("Yarra_R")
)

# function to load site coordinates and metadata, with cleaned version 
#   saved to data/compiled_data/gps-compiled.qs
load_coordinates <- function(recompile = FALSE) {
  
  # check if data exist
  exists <- grepl("gps-compiled", dir(here::here("data", "compiled_data")))
  if (any(exists) & !recompile) {
    
    # load saved version if it exists and recompilation isn't required
    filename <- dir(here::here("data", "compiled_data"))[exists]
    filename <- sort(filename, decreasing = TRUE)[1]
    out <- qs::qread(here::here("data", "compiled_data", filename))
    
  } else {
    
    # load data, subsetted to pilot data set (Campaspe)
    input <- readr::read_csv(
      here::here("data", "raw_data", "site_data", "GPS_All_corrected.csv"),
      skip = 1,
      col_names = c(
        "system",
        "waterbody",
        "site",
        "lon",
        "lat",
        "height_ahd",
        "transect",
        "metres",
        "transect_subtransect",
        "gps_note",
        "gps_date",
        "datafile",
        "filt_pos",
        "gps_second",
        "gnss_height",
        "vert_prec",
        "horz_prec",
        "std_dev",
        "point_id",
        "old_gps_height"
      ),
      col_types =  readr::cols(
        .default = readr::col_double(),
        system = readr::col_character(),
        waterbody = readr::col_character(),
        site = readr::col_character(),
        transect = readr::col_character(),
        metres = readr::col_character(),
        transect_subtransect = readr::col_character(),
        gps_note = readr::col_character(),
        gps_date = readr::col_character(),
        datafile = readr::col_character()
      )
    )
    
    # filter out levels with multiple sub levels
    
    input <- input |> filter(!grepl("_W", transect_subtransect))
    
    # tidy up fields, splitting up transect/subtransect field if needed    
    out <- input |> 
      dplyr::mutate(
        transect_subtransect = gsub("TDOWN_", "TDOWN-", transect_subtransect)
      ) |>
      tidyr::separate(
        col = transect_subtransect, 
        into = c("transect_split", "metres_split"),
        sep = "_", 
        remove = FALSE
      ) |>
      dplyr::mutate(
        transect_split = stringr::str_remove(
          string = transect_split, pattern = "\\D"
        ),
        transect = ifelse(
          !is.na(transect_split) & is.na(transect), transect_split, transect
        ),
        metres = ifelse(
          !is.na(metres_split) & is.na(metres), metres_split, metres
        ),
        metres = stringr::str_remove(string = metres, pattern = "M")
      ) |> 
      dplyr::select(-transect_split, -metres_split)
    
    # check: everything that has transect and subtransect has metres
    metres_filled <- out |> 
      dplyr::filter(is.na(metres) & !is.na(transect) & !is.na(transect_subtransect))
    if (nrow(metres_filled) > 0) {
      warning(paste0(
        "transect_subtransect has not been split correctly; ",
        "check transect and metres for ",
        metres_filled |> 
          dplyr::mutate(
            wb_site = paste0(
              "\n",
              paste(waterbody, site, sep = ": "),
              " (", transect, ")"
            )
          ) |>
          dplyr::pull(wb_site) |> 
          unique() |> 
          paste(collapse = "")
      ))
    }
    
    # repeat to check that transect field has been filled
    transect_filled <- out |>
      dplyr::filter(is.na(transect) & !is.na(transect_subtransect))
    if (nrow(transect_filled) > 0) {
      warning(paste0(
        "transect_subtransect has not been split correctly; ",
        "check transect and metres for ",
        metres_filled |> 
          dplyr::mutate(
            wb_site = paste0(
              "\n",
              paste(waterbody, site, sep = ": "),
              " (", transect, ")"
            )
          ) |>
          dplyr::pull(wb_site) |> 
          unique() |> 
          paste(collapse = "")
      ))
    }
    
    # save compiled version to file
    qs::qsave(
      out, 
      file = here::here("data", "compiled_data", "gps-compiled.qs")
    )
    
  }
  
  # return
  out
  
}

# function to load site metadata, with cleaned version saved to
#   data/compiled-data/metadata-compiled.qs
load_metadata <- function(recompile = FALSE) {
  
  # check if data exist
  exists <- grepl(
    "metadata-compiled", dir(here::here("data", "compiled_data"))
  )
  if (any(exists) & !recompile) {
    
    # load saved version if it exists and recompilation isn't required
    filename <- dir(here::here("data", "compiled_data"))[exists]
    filename <- sort(filename, decreasing = TRUE)[1]
    out <- qs::qread(here::here("data", "compiled_data", filename))
    
  } else {
    
    # load data, subsetted to pilot data set (Campaspe)
    input <- readr::read_csv(
      here::here("data", "raw_data", "site_data", "VEFMAPS6_Site_Metadata.csv"),
      skip = 1,
      col_names = c(
        "system",
        "waterbody",
        "site",
        "reach",
        "transect",
        "grazing",
        "sheep_cattle",
        "exclosure",
        "exclosure_con",
        "exc_install", 
        "soil_moisture",
        "flow_logger",
        "springfresh_m_ahd",
        "baseflow_m_ahd",
        "ahd_correlation_level",
        "ahd_old",
        "spfresh_diff",
        "bsflow_diff",
        "ivtflow",
        "ivtflow_diff"
      ),
      col_types = readr::cols(
        .default = readr::col_double(),
        system = readr::col_character(),
        waterbody = readr::col_character(),
        site = readr::col_character(),
        reach = readr::col_character(),
        transect = readr::col_character(),
        grazing = readr::col_character(),
        sheep_cattle = readr::col_character(),
        exclosure = readr::col_character(),
        exclosure_con = readr::col_character(),
        soil_moisture = readr::col_character(),
        flow_logger = readr::col_character()
      )
    )
    
    # add the thresholds and sub-transect AHD measurements and
    #    calculate flow zones
    coords <- load_coordinates(recompile = recompile)
    out <- coords |> 
      mutate(coords_available = 1, metres = as.numeric(metres)) |>
      left_join(
        input |> mutate(thresholds_available = 1),
        by = c("system", "waterbody", "site", "transect")
      ) |>
      mutate(
        zone = ifelse(
          height_ahd < baseflow_m_ahd, "below_baseflow",
          ifelse(height_ahd < springfresh_m_ahd, "baseflow_to_springfresh",
                 "above_springfresh")
        )
      )
    
    # return a subset of fields for simplicity
    out <- out |>
      select(
        system, waterbody, site, lon, lat, height_ahd, transect, metres,
        grazing, sheep_cattle, springfresh_m_ahd, baseflow_m_ahd, 
        ahd_correlation_level, spfresh_diff, bsflow_diff, ivtflow,
        ivtflow_diff, zone
      )
    
    # and filter out rows without transect info
    out <- out |>
      filter(!is.na(transect) & !is.na(metres))
    
    # save compiled version to file
    qs::qsave(
      out, file = here::here("data", "compiled_data", "metadata-compiled.qs")
    )
    
  }
  
  # return
  out
  
}

# function to load flow data for a given site, with cleaned version 
#   saved to data/compiled_data/flow-compiled-[SYSTEM].qs
load_flow <- function(system, recompile = FALSE, pilot = TRUE) {
  
  # stop if not loading pilot data
  if (pilot & system != "Campaspe") 
    stop("Pilot analysis must focus on Campaspe system only", call. = FALSE)
  
  # check system is OK
  sys_list <- c(
    "Campaspe", 
    "Glenelg",
    "Loddon",
    "Moorabool",
    "ThomsonMacalister",
    "Wimmera",
    "Yarra"
  )
  if (!system %in% sys_list) {
    stop(
      "system must be one of ",
      paste(sys_list, collapse = ", "), 
      call. = FALSE
    )
  }
  
  # list relevant sites for the system
  site_list <- .waterway_lu[[system]]
  
  # check if data exist
  exists <- grepl(
    paste0("flow-compiled-", system),
    dir(here::here("data", "compiled_data"))
  )
  if (any(exists) & !recompile) {
    
    # load saved version if it exists and recompilation isn't required
    filename <- dir(here::here("data", "compiled_data"))[exists]
    filename <- sort(filename, decreasing = TRUE)[1]
    out <- qs::qread(here::here("data", "compiled_data", filename))
    
  } else {
    
    # work out all required files for a specific system
    file_list <- dir(here::here("data", "raw_data", "flow_data"))
    exists <- grepl(paste(site_list, collapse = "|"), file_list)
    filenames <- file_list[exists]
    
    # load these into a list
    out <- lapply(
      filenames,
      \(x, ...) readr::read_csv(here::here("data", "raw_data", "flow_data", x), ...),
      skip = 1,
      col_names = c(
        "system",
        "waterbody",
        "site",
        "date_time",
        "water_level_m",
        "qc",
        "water_level_m_ahd"
      ),
      col_types =  readr::cols(
        .default = readr::col_character(),
        water_level_m = readr::col_double(),
        qc = readr::col_double(),
        water_level_m_ahd = readr::col_double()
      )
    )
    
    # flatten into a single table
    out <- bind_rows(out)
    
    # remove sites with low data for now
    out <- out |> dplyr::filter(!(system == "Wimmera" & site == "Laharum" | site == "MtVictory")) |>  dplyr::filter(!(system == "Loddon" & site == "Mullins"))

      
  
    # clean up dates
    out <- out |>
   dplyr::mutate(
        date_time = case_when(system == "Yarra" & site == "Millgrove" ~ sapply(strsplit(date_time, ".000"), \(x) x[1]), #remove weird date formatting
        system == "Yarra" & site == "Warrandyte" ~ sapply(strsplit(date_time, ".000"), \(x) x[1]),
        .default = date_time)) |>
      dplyr::mutate(
      date_formatted= case_when(system == "Wimmera" & site == "Peuckers" | site == "RosesGap" | site == "Tobacco" | site == "MtVictory" | site == "Laharum" ~
       lubridate::floor_date(
        lubridate::parse_date_time(
          date_time,
          orders = c("dmy_HM")
        ),
        unit = days()
      ),system == "Loddon" & site == "Mullins"  ~
        lubridate::floor_date(
          lubridate::parse_date_time(
            date_time,
            orders = c("dmy_HM")
          ),
          unit = days()
        ),.default =
       lubridate::floor_date(
          lubridate::parse_date_time(
            date_time,
            orders = c("ymd_HMS")
          ),
          unit = days()
        )
      ))
  
    
    
    # and collapse to daily averages
    out <- out |>
      dplyr::group_by(system, waterbody, site, date_formatted) |>
      dplyr::summarise(
        water_level_m = median(water_level_m, na.rm=T),
      #  qc = ifelse(all(qc %in% "NA"), "NA", extract_mode(qc)), # come back to this if needed
        water_level_m_ahd = median(water_level_m_ahd, na.rm=T)
      )
    
    # save compiled version to file
    savename <- paste0("flow-compiled-", system, ".qs")
    qs::qsave(
      out, file = here::here("data", "compiled_data", savename)
    )
    
  }
  
  # return
  out
  
}

# function to calculate some flow metrics based on flow data and baseflow and
#   spring fresh thresholds
calculate_metrics <- function(x, thresholds) {
  rlang::check_required(x)
  rlang::check_required(thresholds)
  # calculate specific thresholds for each site
  vals <- thresholds |> 
    group_by(system, waterbody, site) |>
    summarise(
      springfresh_m_ahd = median(springfresh_m_ahd),
      baseflow_m_ahd = median(baseflow_m_ahd)
    )
  
  # use these to calculate metrics for each site
  site_list <- unique(x$site)
  out <- vector("list", length = length(site_list))
  for (i in seq_along(site_list)) {
    
    # subset flow and thresholds to a single site
    xsub <- x |> filter(site == site_list[i])
    vals_site <- vals |> 
      filter(
        waterbody == unique(xsub$waterbody),
        site == site_list[i]
      )
    
    # calculate days above baseflows and days above springfresh level
    #   (default is Oct-Sep of the first survey in a year, which is 
    #    at lag = 1. Longer lags mean earlier years)
    out_tm0 <- calculate_metrics_internal(
      x = xsub, thresholds = vals_site, site = site_list[i], lag = 1
    )
    out_tm1 <- calculate_metrics_internal(
      x = xsub, thresholds = vals_site, site = site_list[i], lag = 2
    )
    out_tm2 <- calculate_metrics_internal(
      x = xsub, thresholds = vals_site, site = site_list[i], lag = 3
    )
    out_before_spring <- out_tm0 |>
      left_join(
        out_tm1,
        by = c("system", "waterbody", "site", "survey_year"),
        suffix = c("", "_tm1")
      ) |>
      left_join(
        out_tm2,
        by = c("system", "waterbody", "site", "survey_year"),
        suffix = c("", "_tm2")
      ) |>
      rowwise() |>
      mutate(
        days_above_springfresh = mean(c(
          days_above_springfresh,
          days_above_springfresh_tm1,
          days_above_springfresh_tm2
        )),
        days_above_baseflow = mean(c(
          days_above_baseflow,
          days_above_baseflow_tm1,
          days_above_baseflow_tm2
        ))
      ) |>
      select(
        system, waterbody, site, survey_year,
        days_above_springfresh, days_above_baseflow
      )|>
      mutate(period = "before_spring")

    # repeat for the after spring survey period
    #   (different lags because we drop 12 months in the season IDs)
    out_tm0 <- calculate_metrics_internal(
      x = xsub, 
      thresholds = vals_site,
      site = site_list[i], 
      season = 2:13, 
      lag = 0
    )
    out_tm1 <- calculate_metrics_internal(
      x = xsub, 
      thresholds = vals_site,
      site = site_list[i], 
      season = 2:13, 
      lag = 1
    )
    out_tm2 <- calculate_metrics_internal(
      x = xsub, 
      thresholds = vals_site,
      site = site_list[i], 
      season = 2:13, 
      lag = 2
    )
    out_after_spring <- out_tm0 |>
      left_join(
        out_tm1,
        by = c("system", "waterbody", "site", "survey_year"),
        suffix = c("", "_tm1")
      ) |>
      left_join(
        out_tm2,
        by = c("system", "waterbody", "site", "survey_year"),
        suffix = c("", "_tm2")
      ) |>
      rowwise() |>
      mutate(
        days_above_springfresh = mean(c(
          days_above_springfresh,
          days_above_springfresh_tm1,
          days_above_springfresh_tm2
        )),
        days_above_baseflow = mean(c(
          days_above_baseflow,
          days_above_baseflow_tm1,
          days_above_baseflow_tm2
        ))
      ) |>
      select(
        system, waterbody, site, survey_year,
        days_above_springfresh, days_above_baseflow
      )|>
      mutate(period = "after_spring")
    
    # repeat for the after summer survey period
    out_tm0 <- calculate_metrics_internal(
      x = xsub, 
      thresholds = vals_site,
      site = site_list[i], 
      season = 5:16, 
      lag = 0
    )
    out_tm1 <- calculate_metrics_internal(
      x = xsub, 
      thresholds = vals_site,
      site = site_list[i], 
      season = 5:16, 
      lag = 1
    )
    out_tm2 <- calculate_metrics_internal(
      x = xsub, 
      thresholds = vals_site,
      site = site_list[i], 
      season = 5:16, 
      lag = 2
    )
    out_after_summer <- out_tm0 |>
      left_join(
        out_tm1,
        by = c("system", "waterbody", "site", "survey_year"),
        suffix = c("", "_tm1")
      ) |>
      left_join(
        out_tm2,
        by = c("system", "waterbody", "site", "survey_year"),
        suffix = c("", "_tm2")
      ) |>
      rowwise() |>
      mutate(
        days_above_springfresh = mean(c(
          days_above_springfresh,
          days_above_springfresh_tm1,
          days_above_springfresh_tm2
        )),
        days_above_baseflow = mean(c(
          days_above_baseflow,
          days_above_baseflow_tm1,
          days_above_baseflow_tm2
        ))
      ) |>
      select(
        system, waterbody, site, survey_year,
        days_above_springfresh, days_above_baseflow
      ) |>
      mutate(period = "after_summer")
    
    # bind these together
    out[[i]] <- bind_rows(out_before_spring, out_after_spring, out_after_summer)
    
  }
  
  # bind everything together and return
  bind_rows(out)
  
}

#' @title Calculate flow metrics for a specific lag and period (internal)
#' @description This function calculates the number of days above a threshold
#'  for a given period and lag.
#'  
#'  The function is used internally by \code{\link{calculate_metrics}}.
#'
#'  @details
#'  The function calculates the number of days above a threshold for a given
#'  period and lag. The period is defined by the season argument, which is a
#'  vector of month numbers. The lag is the number of years to go back from
#'  the current year. The function calculates the number of days above the
#'  threshold for each year in the period and lag, and then averages these
#'  values. #TODO egouldo to check this...
#'  @importFrom pointblank expect_col_exists
#'  @importFrom rlang check_required
#'  @importFrom assertthat assert_that
#'  @importFrom dplyr tibble
#'  @importFrom dplyr pull
#'  @importFrom aes.hydro calculate
#'  @importFrom aes.hydro days_above
#'  @importFrom aes.hydro survey
#'  @param x A data frame with columns system, waterbody, water_level_m_ahd,
#'  and date_formatted.
#'  @param thresholds A data frame with columns system, waterbody, site,
#'  springfresh_m_ahd, and baseflow_m_ahd.
#'  @param site The site name.
#'  @param season A vector of month numbers defining the period.
#'  @param lag The number of years to go back from the current year.
#'  @param cli cli_abort
#'  @return A tibble with columns system, waterbody, site, survey_year,
#'  days_above_springfresh, and days_above_baseflow.
calculate_metrics_internal <- function(
    x, thresholds, site, season = 10:21, lag = 0
) {
  # Check & Validate Arguments
  rlang::check_required(x)
  rlang::check_required(thresholds)
  rlang::check_required(season)
  assert_that(is.data.frame(x))
  assert_that(rlang::is_bare_numeric(season))
  assert_that(rlang::is_bare_numeric(lag))
  assert_that(length(lag) == 1)
  if(!test_col_exists(x, 
                    columns = c("system", 
                                "waterbody", 
                                "water_level_m_ahd", 
                                "date_formatted"))){
    cli_abort("x must have columns {.var system}, {.var waterbody}, {.var water_level_m_ahd}, and {.var date_formatted}")
  }
   
  # calculate and return directly
  tibble(
    system = unique(x$system),
    waterbody = unique(x$waterbody),
    site = site,
    survey_year = calculate(
      value = x$water_level_m_ahd,
      date = x$date_formatted,
      resolution = survey(season = season, lag = lag),
      fun = days_above,na.rm=T,
      threshold = thresholds |> pull(springfresh_m_ahd)
    )$date,
    days_above_springfresh = calculate(
      value = x$water_level_m_ahd,
      date = x$date_formatted,
      resolution = survey(season = season, lag = lag),
      fun = days_above, na.rm=T,
      threshold = thresholds |> pull(springfresh_m_ahd)
    )$metric,
    days_above_baseflow = calculate(
      value = x$water_level_m_ahd,
      date = x$date_formatted,
      resolution = survey(season = season, lag = lag),
      fun = days_above,na.rm=T,
      threshold = thresholds |> pull(baseflow_m_ahd)
    )$metric
  )
  
}

# function to load point data for a single system, with cleaned version
#   saved to data/compiled-data/points-compiled-[SYSTEM].qs
load_points <- function(system, recompile = FALSE, pilot = TRUE, s6s7 = FALSE) {
  
  # stop if not loading pilot data
  if (pilot & system != "Campaspe") 
    stop("Pilot analysis must focus on Campaspe system only", call. = FALSE)
  
  # check system is OK
  sys_list <- c(
    "Campaspe", 
    "Glenelg",
    "Loddon",
    "Moorabool",
    "ThomsonMacalister",
    "Wimmera",
    "Yarra"
  )
  if (!system %in% sys_list) {
    stop(
      "system must be one of ",
      paste(sys_list, collapse = ", "), 
      call. = FALSE
    )
  }
  
  # check if data exist
  exists <- grepl(
    paste0("points-compiled-", system),
    dir(here::here("data", "compiled_data"))
  )
  if (any(exists) & !recompile) {
    
    # load saved version if it exists and recompilation isn't required
    filename <- dir(here::here("data", "compiled_data"))[exists]
    filename <- sort(filename, decreasing = TRUE)[1]
    out <- qs::qread(here::here("data", "compiled_data", filename))
    
  } else {
  #  if(system == "ThompsonMacalister"){ # change system designation in the ThomsonMacalister dataset
      
      
  #  }
    
    if (s6s7 == TRUE){
    # load data for a specific site in S6
    existsS6 <- grepl(
      paste0("VEFMAPS6_", system, ".*", "_Point"),
      dir(here::here("data", "raw_data", "veg_data"))
    )
   
    filenameS6 <- dir(here::here("data", "raw_data", "veg_data"))[existsS6]
    filenameS6 <- sort(filenameS6, decreasing = TRUE)[1]
    outS6 <- readr::read_csv(
      here::here("data", "raw_data", "veg_data", filenameS6),
      skip = 1,
      col_names = c(
        "system",
        "waterbody",
        "site",
        "transect",
        "subtransect",
        "metres",
        "species",
        "origin",
        "hits",
        "height",
        "date",
        "survey"
      ),
      col_types =  readr::cols(
        .default = readr::col_character(),
        metres = readr::col_double(),
        hits = readr::col_double()
      )
    )
    
    
    # load data for a specific site in S7
    existsS7 <- grepl(
      paste0("VEFMAPS7_", system, ".*", "_Point"),
      dir(here::here("data", "raw_data", "veg_data"))
    )
    
    if(any(existsS7)){
      filenameS7 <- dir(here::here("data", "raw_data", "veg_data"))[existsS7]
      filenameS7 <- sort(filenameS7, decreasing = TRUE)[1]
      outS7 <- readr::read_csv(
        here::here("data", "raw_data", "veg_data", filenameS7),
        skip = 1,
        col_names = c(
          "system",
          "waterbody",
          "site",
          "transect",
          "metres",
          "species",
          "hits",
          "height",
          "date",
          "survey",
          "comments"
        ),
        col_types =  readr::cols(
          .default = readr::col_character(),
          metres = readr::col_double(),
          hits = readr::col_double()
        )
      )
      #select columns we want as some of the files have multiple 'comments' columns
      outS7 <- outS7 |> select(c("system",
                                 "waterbody",
                                 "site",
                                 "transect",
                                 "metres",
                                 "species",
                                 "hits",
                                 "height",
                                 "date",
                                 "survey"))
      
      # merge the S6 and S7 datasets - note this will need modification if more data is brought in
      
      out <- dplyr::bind_rows(outS6, outS7)
    } else {
      cli::cli_warn(glue::glue("No S7 data available for `{system}` despite being requested. Using S6 data only."))
      out <- outS6
    }
    
    
    } else {
      exists <- grepl(
        paste0("VEFMAPS6_", system, ".*", "_Point"),
        dir(here::here("data", "raw_data", "veg_data"))
      )
      
      filename <- dir(here::here("data", "raw_data", "veg_data"))[exists]
      filename <- sort(filename, decreasing = TRUE)[1]
      out <- readr::read_csv(
        here::here("data", "raw_data", "veg_data", filename),
        skip = 1,
        col_names = c(
          "system",
          "waterbody",
          "site",
          "transect",
          "subtransect",
          "metres",
          "species",
          "origin",
          "hits",
          "height",
          "date",
          "survey"
        ),
        col_types =  readr::cols(
          .default = readr::col_character(),
          metres = readr::col_double(),
          hits = readr::col_double()
        )
      )
    }
    # fix up date
    out <- out |>
      dplyr::mutate(date = readr::parse_date(date, format = "%d/%m/%Y"))
    
    # fix missing system data in Yarra dataset
    if (system == "Yarra"){
      out$system <- "Yarra"
    }
    
    # and fix up some species names
    out <- out |>
      dplyr::mutate(
        species = ifelse(
          species %in% names(.species_rename), .species_rename[species], species
        )
      )
    
    # and add species info
    species <- readr::read_csv(
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
        "wpfg_source",
        "group",
        "rec_group"
      ),
      col_types =  readr::cols(
        .default = readr::col_character()
      )
    )
    
    # ditch repeats for exotic/native litter/bare ground
    species <- species |>
      dplyr::filter(
        !(species %in% c("Bare", "Litter", "Nil", "Water") & origin == "exotic"),
        !duplicated(species)
      )
    
    # merge with output
    out <- out |>
      dplyr::select(-origin) |>
      dplyr::left_join(species, by = "species")
    
    # filter to target years for pilot study
    if (pilot)
      out <- out |> dplyr::filter(lubridate::year(date) <= 2019)
    
    # correct one date that is incorrectly assigned the wrong survey
    out <- out |>
      mutate(
        survey = ifelse(survey == "4b", "4", survey),
        survey = as.numeric(survey),
        survey = ifelse(date == "2017-01-30", 2, survey)
      )
    
    # classify period (before/after flow events)
    out <- out |>
      mutate(
        period = "before_spring",
        period = ifelse(survey %in% c(1) & system == "Campaspe", "post_flood", period), # this accounts for the post flood period only relevant to the Campaspe system
        period = ifelse(survey %in% c(2, 5, 8, 11, 12, 14, "2b"), "after_spring", period),
        period = ifelse(survey %in% c(2, 4, 5, 7) & system == "Glenelg", "after_spring", period), # 
        period = ifelse(survey %in% c(4) & system == "Yarra", "after_spring", period), # 
        period = ifelse(survey %in% c(3, 6, 9, 13, 15, "3b"), "after_summer", period),
        period = ifelse(survey %in% c(6) & system == "Glenelg", "before_spring", period), #
        period = ifelse(survey %in% c(2, 4, 5, 7) & system == "Glenelg", "after_spring", period), #
        period = ifelse(survey %in% c(16), "post_flood", period),
        survey_year = ifelse(system %in% c("Campaspe"), 2017, 2018),
        survey_year = ifelse(system %in% c("WGippsland"), 2019, survey_year),
        survey_year = ifelse(system %in% c("Yarra"), 2019, survey_year),
        survey_year = ifelse(survey %in% c(4)& system == "Yarra", 2020, survey_year),
        survey_year = ifelse(survey %in% c(1:3)& system == "Glenelg", 2019, survey_year),
        survey_year = ifelse(survey %in% c(4:6)& system == "Campaspe", 2018, survey_year),
        survey_year = ifelse(survey %in% c(4:6)& system == "Wimmera", 2019, survey_year),
        survey_year = ifelse(survey %in% c(4:6)& system == "Moorabool", 2023, survey_year),
        survey_year = ifelse(survey %in% c(4:5)& system == "Glenelg", 2020, survey_year),
        survey_year = ifelse(survey %in% c(6:7)& system == "Glenelg", 2023, survey_year),
        survey_year = ifelse(survey %in% c(7:9)& system == "Campaspe", 2019, survey_year),
        survey_year = ifelse(survey %in% c(10:11)& system == "Campaspe", 2020, survey_year),
        survey_year = ifelse(survey %in% c(12:13)& system == "Campaspe", 2021, survey_year),
        survey_year = ifelse(survey %in% c(14:15)& system == "Campaspe", 2022, survey_year),
        survey_year = ifelse(survey %in% c(16)& system == "Campaspe", 2023, survey_year)
        
      )
    
    # filter out skipped surveys
    out <- out |>
      filter(!is.na(survey))
    
    # save compiled version to file
    savename <- paste0("points-compiled-", system, ".qs")
    qs::qsave(
      out, file = here::here("data", "compiled_data", savename)
    )
    
  }
  
  # return
  out
  
}

# function to load cover data for a single system
load_cover <- function(system, recompile = FALSE, pilot = TRUE, ar = FALSE, s6s7 = FALSE) {
  
  # load pointing data
  out <- load_points(system = system, recompile = recompile, pilot = pilot, s6s7 = s6s7)

  # calculate cover and fill zeros
  out <- out |>
    dplyr::select(
      waterbody,
      site, 
      transect,
      metres,
      hits,
      date,
      survey,
      survey_year,
      period,
      origin,
      rec_group,
      wpfg,
      species
    ) |>
    dplyr::group_by(
      waterbody, 
      site,
      transect,
      metres, 
      date, 
      survey,
      survey_year,
      period,
      origin,
      rec_group,
      wpfg,
      species
    ) |>
    dplyr::summarise(hits = sum(hits), npoint = 40) |>
    dplyr::ungroup() |>
    dplyr::mutate(hits = ifelse(hits > npoint, npoint, hits)) |>
    tidyr::complete(
      tidyr::nesting(
        waterbody, site, transect, metres, date, survey, survey_year, period
      ), 
      tidyr::nesting(origin, rec_group, wpfg, species),
      fill = list(hits = 0, npoint = 40)
    )     
  
  # collapse over repeat surveys within a survey period
  out <- out |> 
    dplyr::group_by(
      waterbody, 
      site,
      transect, 
      metres, 
      survey, 
      survey_year, 
      period, 
      origin,
      rec_group,
      wpfg,
      species
    ) |>
    dplyr::summarise(
      hits = sum(hits),
      npoint = unique(npoint)
    ) |>
    dplyr::ungroup()
  
  # if autoregressive, reformat to include previous and current surveys
  #   side-by-side
  if (ar) {
    
    # add an indicator for previous survey
    out_prev <- out |>
      dplyr::mutate(survey = survey + 1) |>
      dplyr::select(
        waterbody, site, transect, metres, survey, origin, rec_group, wpfg, species, hits, npoint
      ) |>
      dplyr::rename(hits_tm1 = hits, npoint_tm1 = npoint)
    
    # pull out cover column and merge
    out <- out |>
      dplyr::left_join(
        out_prev,
        by = c("waterbody", "site", "transect", "metres", "survey", "origin", "species", "rec_group", "wpfg")
      )
    
    # remove rows missing previous survey (only focus on recorded pairs
    #   of surveys)
    out <- out |> dplyr::filter(!is.na(hits_tm1))
    
  }
  
  # reload species info to perform post-expansion filtering
  species <- readr::read_csv(
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
      "wpfg_source",
      "group",
      "rec_group"
    ),
    col_types =  readr::cols(
      .default = readr::col_character()
    )
  )
  
  # ditch repeats for exotic/native litter/bare ground
  species <- species |>
    dplyr::filter(
      !(species %in% c("Bare", "Litter", "Nil", "Water") & origin == "exotic"),
      !duplicated(species)
    )
  
  # filter out non-target species
  #.  (do this after creating the full survey table to avoid
  #.   dropping some survey locations)
  out <- out |>
    left_join(species |> select(-origin, -wpfg, -rec_group), by = "species") |>
    dplyr::filter(
      classification %in% .classification_list,
      #wpfg %in% .wpfg_list
    )
  
  # return
  out
  
}

# function to load richness data for a single system
load_richness <- function(system, recompile = FALSE, pilot = TRUE, s6s7 = FALSE) {
  
  # load pointing data
  out <- load_points(system = system, recompile = recompile, pilot = pilot, s6s7 = s6s7)
  
  # filter out non-target species
  out <- out |>
    dplyr::filter(
      classification %in% .classification_list,
      wpfg %in% .wpfg_list,
      !is.na(hits)
    )
  
  # calculate cover and fill zeros
  out <- out |>
    dplyr::select(
      waterbody,
      site, 
      transect,
      metres,
      hits,
      date,
      survey,
      survey_year,
      period,
      origin,
      rec_group,
      wpfg,
      species
    ) |>
    dplyr::group_by(
      waterbody, 
      site,
      transect,
      metres, 
      date, 
      survey,
      survey_year,
      period,
      origin,
      rec_group,
      wpfg
    ) |>
    dplyr::filter(hits > 0) |>
    dplyr::summarise(richness = length(unique(species))) |>
    dplyr::ungroup() |>
    tidyr::complete(
      tidyr::nesting(
        waterbody, site, transect, metres, date, survey, survey_year, period
      ), 
      tidyr::nesting(origin, rec_group, wpfg),
      fill = list(richness = 0)
    )
  
  # collapse over repeat surveys within a survey period
  out <- out |> 
    dplyr::group_by(
      waterbody, 
      site,
      transect, 
      metres, 
      survey, 
      survey_year, 
      period, 
      origin,
      wpfg,
      rec_group
    ) |>
    dplyr::summarise(richness = sum(richness)) |>
    dplyr::ungroup()
  
  # return
  out
  
}
