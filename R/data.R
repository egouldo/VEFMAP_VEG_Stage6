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
      dplyr::filter(!(species %in% c("Bare", "Litter", "Nil", "Water") & origin == "exotic")) |>
      dplyr::filter(!duplicated(species))
    
    # merge with output
    out <- out |>
      dplyr::select(-origin) |>
      dplyr::left_join(species, by = "species")

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
load_cover <- function(system, recompile = FALSE, pilot = TRUE) {
  
  # load pointing data
  out <- load_points(system = system, recompile = recompile, pilot = pilot)
  
  # filter out non-target species
  out <- out |>
    filter(
      classification %in% .classification_list,
      wpfg %in% .wpfg_list,
      !is.na(hits)
    )
  
  # calculate cover and fill zeros
  out <- out |>
    select(
      waterbody,
      site, 
      transect,
      metres,
      hits,
      date,
      origin,
      wpfg
    ) |>
    group_by(
      waterbody, 
      site,
      transect,
      metres, 
      date, 
      origin,
      wpfg
    ) |>
    summarise(hits = sum(hits), npoint = 40) |>
    ungroup() |>
    mutate(hits = ifelse(hits > npoint, npoint, hits)) |>
    complete(
      nesting(waterbody, site, transect, metres, date), 
      nesting(origin, wpfg),
      fill = list(hits = 0, npoint = 40)
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
    filter(
      classification %in% .classification_list,
      wpfg %in% .wpfg_list,
      !is.na(hits)
    )
  
  # calculate cover and fill zeros
  out <- out |>
    select(
      waterbody,
      site, 
      transect,
      metres,
      hits,
      date,
      origin,
      wpfg,
      species
    ) |>
    group_by(
      waterbody, 
      site,
      transect,
      metres, 
      date, 
      origin,
      wpfg
    ) |>
    filter(hits > 0) |>
    summarise(richness = length(unique(species))) |>
    ungroup() |>
    complete(
      nesting(waterbody, site, transect, metres, date), 
      nesting(origin, wpfg),
      fill = list(richness = 0)
    )       
  
  # return
  out
  
}
