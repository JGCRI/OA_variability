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
  if(grepl("rcp", info$experiment)){
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
#'
#' @param df monthly data frame of weighted cmip averages
#' @importFrom dplyr %>%
#' @return a data frame of the annual amplitude, max and min values

get.amplitude <- function(df){

  df %>%
    group_by(method, units, ensemble, experiment, variable, basin, model, year) %>%
    summarise(amplitude = max(value) - min(value), min = min(value), max = max(value))


}




# ----
# End

