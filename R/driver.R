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
#' @param dir the data dir
#' @importFrom dplyr %>%
#' @return a list of delta and raw kurtosis and skewness data frames

driver <- function(dir = "data/cmip"){

  # Find the path to the basin mean
  path <- list.files(path = dir, "basin_mean.rda", full.names = TRUE)
  data <- get(load(path))

  # Detrend the basin means by ensemble / experiment / variable / model / basin
  message("detrending basin mean")
  detrened_data <- detrend(data)


  # 30 year summary statistics for each / experiment / variable / model / basin
  message("calculating 30 year summary statistics")
  summary_stats <- tidyr::unnest(get.summary_stats(detrened_data))


  # Seasonal amplitude values for each / experiment / variable / model / basin
  message("finding annual seasonal amplitude")
  amplitude <- get.amplitude(detrened_data, dir = dir)

  # Get the kurtosis and skewness stats of the amplitude distribution
  message("calcualting distribution K and s")
  Kurtosis_Skewness <- get.K_S(amplitude)


  # Save
  save(detrened_data, file = paste0(dir, "/detrended_data.rda"))
  save(summary_stats, file = paste0(dir, "/summary_stats.rda"))
  save(amplitude, file = paste0(dir, "/amplitude.rda"))
  save(Kurtosis_Skewness, file = paste0(dir, "/Kurtosis_Skewness.rda"))

  message("all done. outputs are saved in data dir.")

}



