# ------------------------------------------------------------------------------
# Purpose: This script detrends basin means and gets amplitude values for the
# data object containing all observations not just the CESM1-BGC.
#
# Created by: Dorheim, Kalyn
# Created on: October 11 2017
# Modified:   October 19 2017
#
# Notes:
# ------------------------------------------------------------------------------
# Set up the environment
library(dplyr); devtools::load_all()


# Find the path to the basin mean .rda file created by scripts from the raw-data subdir
path <- list.files(path = "data", pattern = "ALL_trended_basin_mean", full.names = TRUE)
ALL_trended_basin_mean <- get(load(path))

# Detrend the basin means by ensemble / experiment / variable / model / basin and save.
ALL_detrened_basin_mean <- detrend(ALL_trended_basin_mean)
devtools::use_data(ALL_detrened_basin_mean, overwrite = TRUE)

# Get 30 year summary statistics for each / experiment / variable / model / basin and save.
ALL_summary_stats <- get.summary_stats(ALL_trended_basin_mean)
devtools::use_data(ALL_summary_stats, overwrite = TRUE)

# get seasonal amplitude values for each / experiment / variable / model / basin and save.
ALL_amplitude <- get.amplitude(ALL_trended_basin_mean)
devtools::use_data(ALL_amplitude, overwrite = TRUE)

# ----
# End
