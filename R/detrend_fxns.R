# ------------------------------------------------------------------------------
# Purpose: This script contains the functions related to de-trending time series
# data for further analysis.
#
# Created by: Dorheim, Kalyn
# Created on: October 10 2017
# Modified:   xxx
#
# Notes: Started with linear de-trending but will need to add polynomial methods...
# ------------------------------------------------------------------------------
# internal.detrend_linear
# ------------------------------------------------------------------------------
#' For a single input detrend a time series based on the slope
#'
#' \code{internal.detrend_linear} for a single model observation detrend the time series
#' by the mean from a linear fit.
#'
#' @param df data frame to detrend
#' @param return_type return the slope or the data
#' @importFrom dplyr %>%
#' @return a data frame containing the slope or detrended data
#' @keywords internal

internal.detrend_linear <- function(df, return_type){

  # Find slope using a simple linear regression.
  fit  <- lm(df$value ~ df$time)
  pred <- sapply(df$time, FUN = function(time){fit$coefficients[1] + time * fit$coefficients[2]})
  pred <- as.vector(pred)

  df$value <- df$value - pred

  if(grepl(pattern = "[s|s]lope", x = return_type)){
    return(pred)
  } else {
    return(df)
  }

} # end of the internal detrend function

# ------------------------------------------------------------------------------
# detrend.linear
# ------------------------------------------------------------------------------
#' Linear regression detrending
#'
#' \code{detrend} for a single model observation detrend the time series
#' by the mean from a linear fit.
#'
#' @param df data frame containing the different observations to detrend.
#' @importFrom dplyr %>%
#' @return a data frame containing a list of all the detrended observations and slopes

detrend.linear <- function(df){

  # Check the columns
  check.column(df, "df for detrend function", required_columns = c("value", "model", "experiment", "ensemble", "basin",
                                                                   "variable", "time"))

  # Detrend the data
  df %>%
    group_by(ensemble, experiment, model, variable, basin) %>%
    do(value = internal.detrend_linear(., return_type = "data")) %>%
    tidyr::unnest() ->
    data

  # Save the slope
  df %>%
    group_by(ensemble, experiment, model, variable, basin) %>%
    do(value = internal.detrend_linear(., return_type = "slope")) %>%
    tidyr::unnest() ->
    slope

  # Return the output
  return(list(slope = slope, data = data))

} # end of detrend

# ----
# End



