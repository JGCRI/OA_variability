# ------------------------------------------------------------------------------
# Purpose: This script contains the driver function for the OA variability
# project
#
# Created by: Dorheim, Kalyn
# Created on: October 11 2017
# Modified:   October 19 2017
#
# Notes:
# ------------------------------------------------------------------------------
# driver()
# ------------------------------------------------------------------------------
#' driver
#'
#' \code{driver} main processing function, uses the basin_mean.rda object in the data folder (may change)
#' then detrends, calculates summary statistics, amplitude, and Kurtosis/Skewness data objects.
#'
#' @param df annual seasonal amplitude
#' @importFrom dplyr %>%
#' @return a list of delta and raw kurtosis and skewness data frames

driver <- function(){

  # Find the path to the basin mean
  path <- list.files(path = "data", "basin_mean.rda", full.names = TRUE)
  data <- get(load(path))

  # Detrend the basin means by ensemble / experiment / variable / model / basin and save.
  message("detrending basin mean")
  detrened_data <- detrend(data)
  save(detrened_data, file = "data/detrended_data.rda")

  # Get 30 year summary statistics for each / experiment / variable / model / basin and save.
  message("calculating 30 year summary statistics")
  summary_stats <- tidyr::unnest(get.summary_stats(detrened_data))
  save(summary_stats, file = "data/summary_stats.rda")

  # Get seasonal amplitude values for each / experiment / variable / model / basin and save.
  message("finding annual seasonal amplitude")
  amplitude <- get.amplitude(detrened_data)
  save(amplitude, file = "data/amplitude.rda")

  # Get the kurtosis and skewness stats of the amplitude distribution
  message("calcualting distribution K and s")
  Kurtosis_Skewness <- get.K_S(amplitude)
  save(Kurtosis_Skewness, file = "data/Kurtosis_Skewness.rda")

  message("all done. outputs are saved in data dir.")

}



