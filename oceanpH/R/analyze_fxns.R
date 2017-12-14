# ------------------------------------------------------------------------------
# Purpose: This script contains the functions that are required to get the
# monthly amplitude, mean, and sd
#
# Created by: Dorheim, Kalyn
# Created on: October 4
# Modified:   xxx
#
# Notes:
# ------------------------------------------------------------------------------
# single.sum_stats()
# ------------------------------------------------------------------------------
#' Calculate the summary stats for a 30 year period for a singl cmip model run
#'
#' \code{single.sum_stats} for a data frame of a single cmip model, ensemble,
#' experiment, variable and so on calculate the monthly amplitude, range, and
#' sd. Because the data sets have not been detrending yet, use the first 30 years
#' for historical experiments and the last 30 years from the future experiments
#' to calculate these values.
#'
#' @param df monthly data frame
#' @importFrom dplyr %>%
#' @return a data frame with all of the meta information
#' @keywords internal

single.sum_stats <- function(df){

  # Extract the meta information for a single cmip observation.
  info <- cmip.meta(df)

  # Use the experiment to determine how to filer the years.
  if(grepl("rcp", unique(info$experiment))){
    # If its the future experiment then use the last 30 years.
    end_year   <- max(df$year)
    start_year <- end_year - 30

  } else if (info$experiment == "historical") {
    # If looking at the historical experiment then use the first 30 years.
    start_year <- min(df$year)
    end_year   <- start_year + 30

  } else {
    stop("Issue with the experiment")
  } # end of start and end year if statement

  # Subset the appropriate 30 years from the input data frame then
  # calculate the monthly mean, amplitude (range), and standard deviation.
  df %>%
    dplyr::filter(year >= start_year, year <= end_year) %>%
    dplyr::mutate(start_year = min(year), end_year = max(year)) %>%
    dplyr::group_by(month, month_name, start_year, end_year) %>%
    dplyr::summarise(min = min(value), max = max(value),
                     range = max(value) - min(value),
                     mean = mean(value),
                     sd = sd(value),
                     sum = sum(value)) %>%
    dplyr::ungroup() %>%
    tidyr::gather(value_type, value, mean, range, sd ,min, max, sum)

}


# ------------------------------------------------------------------------------
# get.summary_stats()
# ------------------------------------------------------------------------------
#' Get the monthly amplitude
#'
#' \code{get.summary_stats} calculate the monthly amplitude, range, and sd for all
#' for each model/basin/experiment/variable combination in the basin mean data frame.
#'
#' @param df monthly data frame of weighted cmip averages
#' @param file the location where to save the list function output default is in the data file.
#' @importFrom dplyr %>%
#' @return a nested list containing the a dataframe containing the value returned by single.amplitude

get.summary_stats <- function(df){

  df %>%
    group_by(method, units, ensemble, experiment, variable, basin, model) %>%
    do(dataframe = single.sum_stats(.))

}

# ------------------------------------------------------------------------------
# get.amplitude()
# ------------------------------------------------------------------------------
#' Get annual seasonal amplitude
#'
#' \code{get.amplitude} seasonal amplitude for each model / ensemble / experiment / basin year
#' this function only calculates the inter annual seasonal amplitude for years with 12 month
#' observations to prevent underestimation of the seasonal amplitude
#'
#' @param df monthly data frame of weighted cmip averages
#' @param dir the directory locatin where to write the amplitude out to
#' @import dplyr
#' @return a data frame of the annual amplitude, max and min values

get.amplitude <- function(df, dir){

  # 12 month observation screening
  df %>%
    dplyr::select(ensemble, experiment, model, variable, year, month) %>%
    group_by(ensemble, experiment, model, variable, year) %>%
    # Count the distinct number of months in each model / variable / year and subset
    # for model / variable / years with less than 12 observations
    summarise(count = n_distinct(month)) %>%
    filter(count < 11) %>%
    mutate(remove = TRUE) %>%
    ungroup ->
    incomplete_years

  # Save the incomplete years
  save(incomplete_years, file = paste0(dir, "/incomplete_years.rda"))

  # Remove the incomplete years from the data to be processed
  df %>%
    left_join(incomplete_years, by = c("ensemble", "experiment", "model", "variable", "year")) %>%
    filter(is.na(remove)) %>%
    select(-count, -remove)->
    to_process

  to_process %>%
    group_by(method, units, ensemble, experiment, variable, basin, model, year) %>%
    summarise(amplitude = max(value) - min(value), min = min(value), max = max(value)) %>%
    ungroup()


}




# ------------------------------------------------------------------------------
# get.K_S
# ------------------------------------------------------------------------------
#' Get kurtosis and skewness
#'
#' \code{get.K_S} Kurtosis and skewness of model / ensemble / experiment / basin year amplitudes
#'
#' @param df annual seasonal amplitude
#' @importFrom dplyr %>%
#' @return a list of delta and raw kurtosis and skewness data frames

get.K_S <- function(data){

  # For each ensemble / experiment / variable / basin / model distribution of annual
  # seasonal amplitude measure the kurtosis and skewness.
  data %>%
    dplyr::group_by(ensemble, experiment, variable, units, basin, model) %>%
    dplyr::summarise(K = e1071::kurtosis(amplitude), S = e1071::skewness(amplitude)) %>%
    dplyr::ungroup() %>%
    # Format the data frame so that it is in the long format.
    tidyr::gather(stat_variable, value, K, S) ->
    raw

  # Find delta K and S for difference between the future and historical period K and S.
  raw %>%
    tidyr::spread(experiment, value) %>%
    na.omit %>%
    dplyr::mutate(delta = rcp85 - historical) %>%
    dplyr::mutate(stat_variable = paste0("delta  ", stat_variable)) %>%
    dplyr::select(ensemble, variable, units, basin, model, stat_variable, delta) ->
    delta

  # Format outputs into lists and return
  out = list(K_S = raw, delta_K_S = delta)
  return(out)

} # end of get.K_S function




# ------------------------------------------------------------------------------
# get.timing_info
# ------------------------------------------------------------------------------
#' Determine when each seasonal min and max occurs
#'
#' \code{get.timing_info} Determine when each annual min and max occurs for each
#' model / basin/ varible combination
#'
#' @param data a dataframe of monthly bains means
#' @importFrom dplyr %>%
#' @return a data frame of annual max and mins
#' @export

get.timing_info <- function(input_data){

  # The internal funciton that that determines the max and min for each year
  internal.max_min <- function(data){

    max_month <- filter(data, value == max(value))
    max_month$value_type <- "max"

    min_month <- filter(data, value == min(value))
    min_month$value_type <- "min"

    bind_rows(min_month, max_month)

  }

  # Use the interal function to dermine the max and min of each year for
  # every ensemble / experiment / model / variable combination.
  input_data %>%
    group_by(basin, ensemble, experiment, variable, year, model, units) %>%
    do(internal.max_min(.)) %>%
    ungroup

}


# ----
# End

