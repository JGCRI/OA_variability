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
# single.amplitude()
# ------------------------------------------------------------------------------
#' Calculate the monthly amplitude, mean, and sd for a single cmip realization
#'
#' \code{single.amplitude} for a data frame of a single cmip model, ensemble,
#' experiment, variable and so on calculate the monthly amplitude, range, and
#' sd. Because the data sets have not been detrending yet, use the first 30 years
#' for historical experiments and the last 30 years from the future experiments
#' to calculate these values.
#'
#' @param df monthly data frame
#' @importFrom dplyr %>%
#' @return a data frame with all of the meta information
#' @keywords internal

single.amplitude <- function(df){

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

} # end of the single.amplitude function


# ------------------------------------------------------------------------------
# get.amplitude_values()
# ------------------------------------------------------------------------------
#' Get the monthly amplitude
#'
#' \code{get.amplitude_values} calculate the monthly amplitude, range, and sd for all
#' for each model/basin/experiment/variable combination in the basin mean data frame.
#'
#' @param df monthly data frame of weighted cmip averages
#' @param file the location where to save the list function output default is in the data file.
#' @importFrom dplyr %>%
#' @return a nested list containing the a dataframe containing the value returned by single.amplitude

get.amplitude_values <- function(df, file = "data/basin_amplitude.RData"){

  df %>%
    group_by(method, units, ensemble, experiment, variable, basin, model) %>%
    do(dataframe = single.amplitude(.))

} # end of the get.amplitude_values function


# ------------------------------------------------------------------------------
# vis.amplitude()
# ------------------------------------------------------------------------------
#' Line plots for a monthly amplitude data frame
#'
#' \code{vis.amplitude} for a single monthly amplitude data frame create a monthly
#' line plot
#'
#' @param input a single cmip observation amplitude data frame
#' @import ggplot2
#' @importFrom dplyr %>%
#' @return a list of figures of all of the monthly value types
vis.amplitude <- function(input){
  to_plot <- tidyr::unnest(input)
  info    <- cmip.meta(to_plot)

  type_list <- info$value_type

  info %>%
    dplyr::select(-value_type) %>%
    dplyr::distinct() ->
    info

  if(nrow(info) > 1) {
    stop("too many cmip observations")
  }

  out = list()

  # Use a for loop to generate a plot for each of the
  # value types.
  for(i in 1:length(type_list)){

    type <- type_list[i]

    to_plot %>%
      dplyr::filter(value_type == type) %>%
      dplyr::mutate(value = as.numeric(value)) %>%
      ggplot(aes(x = month, y = value, color = model)) +
      geom_line(size = 1.2) +
      theme(text = element_text(size = 13)) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      theme(legend.position = "none") +
      scale_x_discrete(limits = month_name_df$month, labels = month_name_df$month_name) +
      labs(y = paste0(type, " ", info$variable, " ", info$units),
           x = "Month",
           title = paste0("Monthly ", type, "\n",
                          info$basin, " ", info$model, " ", info$experiment, " ", info$ensemble)) ->
      out[[paste0("Monthly_",type)]]

  } # end of the value type for loop

  return(out)

} # end of the vis.amplitude function

# ----
# End

