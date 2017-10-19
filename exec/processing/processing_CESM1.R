# ------------------------------------------------------------------------------
# Purpose: This script does detrends basin means and gets amplitude values for
# the CESM1 data objects.
#
# Created by: Dorheim, Kalyn
# Created on: October 11 2017
# Modified:   xxx
#
# Notes:
# ------------------------------------------------------------------------------
# Set up the environment
library(dplyr); devtools::load_all()

# Find the path to the basin mean .rda file created by scripts from the raw-data subdir
PATH_basin_mean <- list.files(path = "data", pattern = "CESM1_trended_basin_mean", full.names = TRUE)
trended_basin_mean <- get(load(PATH_basin_mean))

# Detrend the basin means by ensemble / experiment / variable / model / basin and save.
CESM1_detrened_basin_mean <- detrend(trended_basin_mean)
devtools::use_data(CESM1_detrened_basin_mean, overwrite = TRUE)

# Get 30 year amplitude values for each / experiment / variable / model / basin and save.
CESM1_amplitude_values <- get.amplitude_values(CESM1_detrened_basin_mean)
devtools::use_data(CESM1_amplitude_values, overwrite = TRUE)

# ----
# End
