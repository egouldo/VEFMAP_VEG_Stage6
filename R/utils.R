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
theme_effects <- function(rm_axis.text.x = FALSE, rm_axis.ticks.x = FALSE, rotate_axis.text.x = FALSE){
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
    }  else if(rotate_axis.text.x){
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
    } else{
      t
    }
  
  t
  
} #TODO ensure logic allows both removing axis text and ticks

#' Plot effects by variables
#' 
#' This function plots the effects of two variables on the response variable, coloured by a third variable and faceted by a fourth variable.
#' 
#' @param model A model object
#' @param var1 A bare name of length 1 indicating the first variable
#' @param var2 A bare name of length 1 indicating the second variable
#' @param colour_var A bare name of length 1 indicating the variable to colour by
#' @param facet_var A bare name of length 1 indicating the variable to facet by
#' @return A ggplot object
plot_effects_by_vars <- function(model, var1, var2, colour_var, facet_var){ #TODO rename function and update calls to it in script
  
  
  .zone_lvls <- c( "below_baseflow", 
                   "baseflow_to_springfresh", 
                   "above_springfresh")
  
  .period_lvls <- c( "before_spring", 
                     "after_spring", 
                     "after_summer")
  
  if(rlang::expr_text(substitute(var1)) == "zone"){
    var1_levels <- .zone_lvls
  } else if(rlang::expr_text(substitute(var1)) == "period"){  
    var1_levels  <- .period_lvls
  } else if(rlang::expr_text(substitute(var1)) == "wpfg"){
    var1_levels <- .wpfg_list
  } else{
    cli_abort("{.var var1} must be either {.var zone}, {.var period} or {.var wpfg}, not: {var1}")
  }
  
  if(rlang::expr_text(substitute(var2)) == "zone"){
    var2_levels <- .zone_lvls
  } else if(rlang::expr_text(substitute(var2)) == "period"){  
    var2_levels  <- .period_lvls
  } else if(rlang::expr_text(substitute(var2)) == "wpfg"){
    var2_levels <- .wpfg_list
  } else{
    cli_abort("{.var var1} must be either {.var zone}, {.var period} or {.var wpfg}, not: {var1}")
  }
  
  plot_data <- 
    model$frame %>%
    as_tibble() %>% 
    unite(wpfg_ori, wpfg, origin, sep = "_", remove = FALSE) %>%
    filter(!wpfg_ori %in% c("Atl_native", "Ate_native", "Tda_unknown")) %>% 
    mutate({{var1}} := factor({{var1}}, levels = var1_levels, ordered = TRUE),
           {{var2}} := factor({{var2}}, levels = var2_levels, ordered = TRUE) #TODO what if wpfg
    )
  
  
  
  effects_data <- as.data.frame(Effect(c(rlang::expr_text(substitute(var1)), 
                                         rlang::expr_text(substitute(var2))), 
                                       model, 
                                       xlevels = 20)) %>% 
    rename(hits = fit) %>% 
    mutate({{var1}} := factor({{var1}}, levels = var1_levels, ordered = TRUE),
           {{var2}} := factor({{var2}}, levels = var2_levels, ordered = TRUE)
    )
  
  plot_out <- effects_data %>% 
    ggplot(aes({{var2}}, hits, colour = {{colour_var}})) +
    geom_point(size = 5) +
    geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.3,  size= 1)+
    geom_sina(data= plot_data , alpha = 0.05) +
    coord_cartesian(ylim = c(0, 20))+
    labs(x = rlang::expr_text(substitute(facet_var)) %>% R.utils::capitalize(), y = "Hits") + 
    theme_bw() + 
    facet_grid(cols = vars({{facet_var}}), switch="x", scales = "free") # coord_cartesian(ylim = c(0.5, 1))
  
  plot_out
  
}

# ----------------- Performance checks -----------------


#' Tabulate performance check results
#' 
#' Generate a tibble of performance check results generated with the `performance::` package
#' @param x object of class `check_overdisp` or `check_overdisp` generated with the `performance::` package
#' @return A tibble of performance check results
#' @export
#' @importFrom tibble as_tibble_row
#' @importFrom tibble set_names
tabulate_performance_checks <- function(x){ 
  tibble::as_tibble_row(set_names(c(x), names(x)))
}

#' Check for overdispersion and tabulate results
#' 
#' Checks for overdispersion on a model object using [performance::check_overdispersion] and tabulates the results using [tabulate_performance_checks()]
#' 
#' @param x A model object supported by the `performance::` package
#' @return A tibble of performance check results
#' @details
#' [tabulate_performance_checks()] takes the output of [performance::check_overdispersion] and converts it to a tibble,
#' while [extract_over_dispersion()] performs the check using [performance::check_overdispersion] *and* tabulates the results using [tabulate_performance_checks()].
#' @export
#' @importFrom performance check_overdispersion
#' @importFrom purrr compose
extract_over_dispersion <- compose(check_overdispersion, tabulate_performance_checks, .dir = "forward")

#' Check for zero inflation and tabulate results
#' 
#' Checks for zero inflation on a model object using [performance::check_zeroinflation] and tabulates the results using [tabulate_performance_checks()]
#' 
#' @param x A model object supported by the `performance::` package
#' @return A tibble of performance check results
#' @details
#' [tabulate_performance_checks()] takes the output of [performance::check_zeroinflation] and converts it to a tibble,
#' while [extract_zeroinflation()] performs the check using [performance::check_zeroinflation] *and* tabulates the results using [tabulate_performance_checks()].
#' @export
#' @importFrom performance check_zeroinflation
#' @importFrom purrr compose
extract_zeroinflation <- compose(check_zeroinflation, tabulate_performance_checks, .dir = "forward")


#' Diagnose overdispersion
#' 
#' Diagnoses overdispersion on results of [extract_over_dispersion()]
#' @details
#' May be performed on results of [tabulate_performance_checks()] if the results are generated from [performance::check_overdispersion()].
#' Function uses the logic within the `performance::` package to diagnose overdispersion based on the p-value and dispersion ratio.
#' 
#' @param x A tibble of performance check results
#' @return A character vector indicating the diagnosis
#' @export
diagnose_overdispersion <- function(x) {
  stopifnot(is.data.frame(x))
  if (x$p_value > 0.05) {
    result <- "No overdispersion detected"
  } else if (x$dispersion_ratio > 1) {
    result <- "Overdispersion detected"
  } else {
    result <- "Underdispersion detected"
  }
  result
}

#' Diagnose zero inflation
#' 
#' Diagnoses zero inflation on results of [extract_zeroinflation()]
#' @details
#' May be performed on results of [tabulate_performance_checks()] if the results are generated from [performance::check_zeroinflation()].
#' Function uses the logic within the `performance::` package to diagnose zero inflation based on the ratio of zeros to the total number of observations.
#' Note, may need to change method for the richness model.
#' 
#' @param x A tibble of performance check results
#' @return A character vector indicating the diagnosis
#' @export
diagnose_zeroinflation <- function(x) {
  stopifnot(is.data.frame(x))
  
  lower <- 1 - x$tolerance
  upper <- 1 + x$tolerance
  
  if (x$ratio < lower) {
    result <- "Underfitting zeros"
  } else if (x$ratio > upper) {
    result <- "Overfitting zeros"
  } else {
    result <- "No zero inflation detected"
  }
  result
}

#' extract R^2
#'
#' @param model_object 
#'
#' @return A numeric vector of length one equal to model \code{R^2}
#' @export
#' @importFrom stats fitted
#' @importFrom purrr pluck
#' @importFrom stats cor
#' @examples
extract_r2 <- function(model_object){
  cor(fitted(model_object), 
      pluck(model_object, "frame", "hits"))^2
}
