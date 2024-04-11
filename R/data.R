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
  "moss" = "Moss/Lichen",
  "Hordeum murinum" = "Hordeum leporinum",
  "Alternanthera denticulata s.s." = "Alternanthera denticulata",
  "Eucalyptus camaldulensis var. camaldulensis" = "Eucalyptus camaldulensis subsp. camaldulensis",
  "Anagallis arvensis" = "Lysimachia arvensis",
  "Melicytus dentatus" = "Melicytus dentatus s.l.",
  "Triglochin procera" = "Cycnogeton procerum",
  "Verbena bonariensis" = "Verbena bonariensis s.l.",
  "Pseudognaphalium luteoalbum" = "Laphangium luteoalbum"
)

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
      here::here("data", "raw_data", "site_data", "GPS_All.csv"),
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
    
    # clean up dates
    out <- out |>
      dplyr::mutate(
        date_formatted = lubridate::floor_date(
          lubridate::parse_date_time(
            date_time,
            orders = c("ymd_HMS")
          ),
          unit = days()
        )
      )
    
    # and collapse to daily averages
    out <- out |>
      dplyr::group_by(system, waterbody, site, date_formatted) |>
      dplyr::summarise(
        water_level_m = median(water_level_m),
        qc = extract_mode(qc),
        water_level_m_ahd = median(water_level_m_ahd)
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

# internal function to calculate metrics for a specific lag and period
calculate_metrics_internal <- function(
    x, thresholds, site, season = 10:21, lag = 0
) {
  
  # calculate and return directly
  tibble(
    system = unique(x$system),
    waterbody = unique(x$waterbody),
    site = site,
    survey_year = calculate(
      value = x$water_level_m_ahd,
      date = x$date_formatted,
      resolution = survey(season = season, lag = lag),
      fun = days_above,
      threshold = thresholds |> pull(springfresh_m_ahd)
    )$date,
    days_above_springfresh = calculate(
      value = x$water_level_m_ahd,
      date = x$date_formatted,
      resolution = survey(season = season, lag = lag),
      fun = days_above,
      threshold = thresholds |> pull(springfresh_m_ahd)
    )$metric,
    days_above_baseflow = calculate(
      value = x$water_level_m_ahd,
      date = x$date_formatted,
      resolution = survey(season = season, lag = lag),
      fun = days_above,
      threshold = thresholds |> pull(baseflow_m_ahd)
    )$metric
  )
  
}

# function to load point data for a single system, with cleaned version
#   saved to data/compiled-data/points-compiled-[SYSTEM].qs
load_points <- function(system, recompile = FALSE, pilot = TRUE) {
  
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
    
    # load data for a specific site
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
    
    # fix up date
    out <- out |>
      dplyr::mutate(date = readr::parse_date(date, format = "%d/%m/%Y"))
    
    # and fix up some species names
    out <- out |>
      dplyr::mutate(
        species = ifelse(
          species %in% .species_rename, .species_rename[species], species
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
        !(species %in% c("Bare", "Litter", "Nil", "Water") & origin == "exotic"), #TODO what about instances where species %in% c("Water", "Bare") & origin == "native" in master species file?
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
        period = ifelse(survey %in% c(2, 5, 8, 11), "after_spring", period),
        period = ifelse(survey %in% c(3, 6, 9), "after_summer", period),
        survey_year = 2017,
        survey_year = ifelse(survey %in% c(4:6), 2018, survey_year),
        survey_year = ifelse(survey %in% c(7:9), 2019, survey_year),
        survey_year = ifelse(survey %in% c(10:11), 2020, survey_year)
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
load_cover <- function(system, recompile = FALSE, pilot = TRUE, ar = FALSE) {
  
  # load pointing data
  out <- load_points(system = system, recompile = recompile, pilot = pilot)

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
      tidyr::nesting(origin, wpfg, species),
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
        waterbody, site, transect, metres, survey, origin, wpfg, species, hits, npoint
      ) |>
      dplyr::rename(hits_tm1 = hits, npoint_tm1 = npoint)
    
    # pull out cover column and merge
    out <- out |>
      dplyr::left_join(
        out_prev,
        by = c("waterbody", "site", "transect", "metres", "survey", "origin", "species", "wpfg")
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
    left_join(species |> select(-origin, -wpfg), by = "species") |>
    dplyr::filter(
      classification %in% .classification_list,
      wpfg %in% .wpfg_list
    )
  
  # return
  out
  
}

# function to load richness data for a single system
load_richness <- function(system, recompile = FALSE, pilot = TRUE) {
  
  # load pointing data
  out <- load_points(system = system, recompile = recompile, pilot = pilot)
  
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
      wpfg
    ) |>
    dplyr::filter(hits > 0) |>
    dplyr::summarise(richness = length(unique(species))) |>
    dplyr::ungroup() |>
    tidyr::complete(
      tidyr::nesting(
        waterbody, site, transect, metres, date, survey, survey_year, period
      ), 
      tidyr::nesting(origin, wpfg),
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
      wpfg
    ) |>
    dplyr::summarise(richness = sum(richness)) |>
    dplyr::ungroup()
  
  # return
  out
  
}
