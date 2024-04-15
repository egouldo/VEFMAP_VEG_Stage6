#' Calculate the mode of a vector (most common value)
#' 
#' This function calculates the mode of a vector, i.e. the most common value.
#' 
#' @param x A vector of values
#' @return The mode of the vector
#' @export
extract_mode <- function(x) {
  
  # work out frequency of all values
  freq <- table(x)
  
  # return the name of the value with the highest frequency
  names(freq)[order(freq, decreasing = TRUE)][1]
  
}

#' Calculate a constant based on the variable `pr_cover`
#' 
#' This function calculates a constant value to add to the response variable.
#' @details 
#' The constant is calculated as the minimum value of `pr_cover` where `pr_cover > 0` plus 0.5, multiplied by 0.5.
#' The constant is to be added to the response variable to avoid log(0) errors in the lognormal models.
#' 
#' @param df A data frame containing the response variable
#' @return A constant value to add to the response variable
#' @export
#' @importFrom purrr pluck
#' @importFrom dplyr filter
#' @examples
#' calc_constant(veg_cover_ar)
calc_constant <- function(df) {
  constant <- df %>% 
    filter(pr_cover > 0) %>% 
    pluck("pr_cover", 
          min) %>%
    sum(0.5)
  
  constant * 0.5
}

#' Prepare data for modelling
#' 
#' This function prepares the data for modelling by adding site information, flow data and metrics data into the veg data set.
#' 
#' @details
#' If `cover_ar_sum` is `TRUE`, the function aggregates cover over all species within each wpfg.
#' If `outcome` is "cover", the function calculates the proportion of cover and adds a constant for avoiding log(0) errors in preparation for lognormal models. 
#' If `outcome` is "richness", this step if skipped.
#' 
#' For all combinations of `outcome` and `cover_ar_sum`, the function adds the following columns:
#' - `days_above_baseflow_std`: the standardised number of days above baseflow
#' - `days_above_springfresh_std`: the standardised number of days above springfresh
#' - `days_above_baseflow_std_sq`: the square of the standardised number of days above baseflow
#' - `days_above_springfresh_std_sq`: the square of the standardised number of days above springfresh
#' 
#' When `outcome` is "cover", the function also adds the following columns:
#' - `pr_cover`: the proportion of cover
#' - `log_hits_tm1`: the log of the number of hits in the previous time period
#' - `pr_cover_tf`: the proportion of cover with a constant added
#' - `log_pr_cover_tm1_tf`: the log of the proportion of cover in the previous time period with a constant added
#' 
#' @param df A data frame containing the vegetation data
#' @param site_info A data frame containing site information
#' @param metrics A data frame containing metrics data
#' @param cover_ar_sum A logical value indicating whether to sum cover over all species within each wpfg
#' @param outcome A character vector indicating the outcome variable to be used in the model
#' @return A data frame containing the prepared data
#' @export
#' @importFrom dplyr group_by summarise filter left_join mutate
#' @importFrom tidy unite
#' @importFrom pointblank col_vals_equal
#' @examples
#' prepare_modelling_data(veg_cover_ar, site_info, metrics, cover_ar_sum = FALSE, outcome = "cover")
prepare_modelling_data <- function(df, site_info = site_info, metrics = metrics, cover_ar_sum = FALSE, outcome = c("cover", "richness")) {
  if(cover_ar_sum) {
    df <- df |>
      group_by(
        waterbody, site, transect, metres, survey, survey_year,
        period, origin, wpfg
      ) |>
      summarise( #sum cover over all species within each wpfg
        hits = sum(hits),
        npoint = unique(npoint),  # each point can have multiple overlapping veg records,
        #   so it potentially makes more sense to treat it as
        #   40 points with possibility of > 100% cover.
        hits_tm1 = sum(hits_tm1),
        npoint_tm1 = unique(npoint_tm1)
      ) %>% 
      pointblank::col_vals_equal(npoint, 40)  
  }
  
  df <- df %>% # add site info, flow and metrics data into the veg data set, removing plots with missing values
    left_join(site_info, by = c("waterbody", "site", "transect", "metres")) |>
    filter(!is.na(zone)) |>
    left_join(
      metrics,
      by = c("system", "waterbody", "site", "survey_year", "period")
    ) |>
    filter(!is.na(days_above_springfresh)) %>%  #TODO temporary due to incomplete flow data
    mutate(
      days_above_baseflow_std = scale(days_above_baseflow) %>% as.numeric,
      days_above_springfresh_std = scale(days_above_springfresh) %>% as.numeric,
      days_above_baseflow_std_sq = days_above_baseflow_std ^ 2,
      days_above_springfresh_std_sq = days_above_springfresh_std ^ 2
    ) %>% 
    unite(wpfg_ori, wpfg, origin, sep = "_", remove = FALSE)
  
  if(outcome == "cover") {
    df <- df %>% 
      mutate(
        pr_cover = hits/npoint,
        log_hits_tm1 = log(hits_tm1 + 1),
        #log_pr_cover_tm1 = log((hits_tm1/npoint) + 1)
      )
    
    constant_ar_sum <- calc_constant(df) 
    
    df <- df %>%  
      mutate(., # add constant for avoiding log(0) errors in prep for lognormal models
             pr_cover_tf = pr_cover + constant_ar_sum,
             log_pr_cover_tm1_tf = log((hits_tm1/npoint_tm1) + constant_ar_sum)
      )
  }
  
  df
  
}

#' Custom Theme for modifying effects plots
#' 
#' @param rm_axis.text.x logical, remove x axis text
#' @param rm_axis.ticks.x logical, remove x axis ticks
#' @return ggplot2 theme
#' @importFrom ggplot2 theme_bw element_blank element_line theme theme_replace
theme_effects <- function(rm_axis.text.x = FALSE, rm_axis.ticks.x = FALSE){
  t <- theme_bw() +
    theme(panel.spacing.x = unit(0, "mm"), 
          panel.border = element_blank(), 
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), 
          axis.line = element_line(colour = "black"), 
          legend.position = "right")
  t <- t %+replace%
    if(rm_axis.text.x){
      theme(axis.text.x = element_blank())
    } else if(rm_axis.ticks.x){
      theme(axis.ticks.x = element_blank())
    } else{
      t
    }
  
  t
  
}