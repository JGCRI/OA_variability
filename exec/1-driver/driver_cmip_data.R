# Script Info --- ------------------------------------------------------------------------------
# Purpose: This script acts as the driver for processing the cmip data sets. Note this script
# requires post cdo processing or that the data processed on pic (saved in raw-data/cmip) has
# been formated, the materials do do that can be found in the raw-data directory.
#
# Created by: Dorheim, Kalyn
# Created on: Nov 16 2017
# Modified:   xxx
#
# Notes: I am unsure if I want to go with driver functions or driver scripts??? what would
# be easiest? Also this takes forever when we include the max and min timing scripts... idk
# what we should do about that
#
# Set Up Environment  -------------------------------------------------------------------------

# Load the required libraries
library(dplyr)
library(tidyr)
devtools::load_all()

# Define the input and output directories for this script
INPUT_DIR  <- "data/cmip"
OUTPUT_DIR <- "data/cmip"


# Load the cmip_observations data to start with, or the raw data
data_path <- list.files(INPUT_DIR, "basin_mean.rda", full.names = TRUE)
raw_data  <- get(load(data_path))


# Detrend  -----------------------------------------------------------------------------------

# Detrend the raw data set to remove long term changes to focus on intra-annual variability.
detrended_data <- detrend(raw_data)

# Save
save(detrended_data, file = paste0(OUTPUT_DIR, "/detrended_data.rda"))


# Amplitude ----------------------------------------------------------------------------------

# Calculate the annual/seasonal amplitude. This process will discard observations that do not
# meet requirements and will save those observations in a .rda object at the specified location.
amplitude <- get.amplitude(detrended_data, OUTPUT_DIR)

# Save
save(amplitude, file = paste0(OUTPUT_DIR, "/amplitude.rda"))


# Max and Min Timing ---------------------------------------------------------------------------

# Determine when the annaul max and min occur.
max_min_timing <- get.timing_info(detrended_data)

# Save
save(max_min_timing, file = paste0(OUTPUT_DIR, "/seasonal_max_min.rda"))


# Monthly Stats ------------------------------------------------------------------------------

# 30 year summary statistics for each / experiment / variable / model / basin
summary_stats <- tidyr::unnest(get.summary_stats(detrended_data))

# Save
save(summary_stats, file = paste0(OUTPUT_DIR, "/summary_stats.rda"))


# K and S ------------------------------------------------------------------------------------

# Get the K and S values for the amplitude distribution.
KS_data <- get.K_S(amplitude)

# Save
save(KS_data, file = paste0(OUTPUT_DIR, "/Kurtosis_Skewness.rda"))


# End -----
message("Script complete")
