# function to load site coordinates and metadata, with cleaned version 
#   saved to data/compiled_data/gps-compiled.qs
load_coordinates <- function(recompile = FALSE) {
  
  # check if data exist
  exists <- grepl("gps-compiled", here::here("data", "compiled_data"))
  if (any(exists) & !recompile) {
    
    # load saved version if it exists and recompilation isn't required
    filename <- dir(here::here("data", "compiled_data"))[exists]
    filename <- sort(filename, decreasing = TRUE)[1]
    out <- qread(here::here("data", "compiled_data", filename))
    
  } else {
    
    # load data, subsetted to pilot data set (Campaspe)
    in <- readr::read_csv(
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
      col_types =  cols(
        .default = col_double(),
        system = col_character(),
        waterbody = col_character(),
        site = col_character(),
        transect = col_character(),
        metres = col_character(),
        transect_subtransect = col_character(),
        gps_note = col_character(),
        gps_date = col_character(),
        datafile = col_character()
      )
    )
    
    # tidy up fields, splitting up transect/subtransect field if needed    
    out <- in |> 
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
        # remove everything except digits in transect_split
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
          mutate(
            wb_site = paste0(
              "\n",
              paste(waterbody, site, sep = ": "),
              " (", transect, ")"
            )
          ) |>
          pull(wb_site) |> unique() |> paste(collapse = "")
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
          mutate(
            wb_site = paste0(
              "\n",
              paste(waterbody, site, sep = ": "),
              " (", transect, ")"
            )
          ) |>
          pull(wb_site) |> unique() |> paste(collapse = "")
      ))
    }
    
    # save compiled version to file
    qsave(
      out, 
      file = here::here("data", "compiled_data", "gps-compiled.qs")
    )
    
  }
  
  # return
  site_metadata_cleaned
  
}

# function to load site metadata, with cleaned version saved to
#   data/compiled-data/metadata-compiled.qs
load_metadata <- function(recompile = FALSE) {
  
  # check if data exist
  exists <- grepl("metadata-compiled", here::here("data", "compiled_data"))
  if (any(exists) & !recompile) {
    
    # load saved version if it exists and recompilation isn't required
    filename <- dir(here::here("data", "compiled_data"))[exists]
    filename <- sort(filename, decreasing = TRUE)[1]
    site_metadata_cleaned <- qread(here::here("data", "compiled_data", filename))
    
  } else {
    
    # load data, subsetted to pilot data set (Campaspe)
    out <- readr::read_csv(
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
      col_types =  cols(
        .default = col_double(),
        system = col_character(),
        waterbody = col_character(),
        site = col_character(),
        reach = col_character(),
        transect = col_character(),
        grazing = col_character(),
        sheep_cattle = col_character(),
        exclosure = col_character(),
        exclosure_con = col_character(),
        soil_moisture = col_character(),
        flow_logger = col_character()
      )
    )
    
    # add reach info from AAEDB
    ## TODO
    
    # save compiled version to file
    qsave(
      out, file = here::here("data", "compiled_data", "metadata-compiled.qs")
    )
    
  }
  
  # return
  out
  
}
