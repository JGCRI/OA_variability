# Purpose: This script acts as the driver for processing the cmip data sets. Note this script
# requires post cdo processing or that the data processed on pic (saved in raw-data/cmip) has
# been formated for use in the pacakge. Input for this function comes from the basin means stored
# in the inst/exdata directory and saves the outpus in the output dir.
#
# Created by: Dorheim, Kalyn
# Created on: Nov 16 2017
# Modified:   Jan 4 2018
#
# Notes: I think that the monthly average function may have some problems.... also I am not running
# the min and max timing function any more becasue we decided that analysis was unintersting.
#
# Set Up Environment  -------------------------------------------------------------------------

# Load the required libraries
library(dplyr)
library(tidyr)
devtools::load_all()

# Save a copy of the script name
script_name <- find_scriptName()

# Define the input and output directories for this script
BASE <- getwd()
INPUT_DIR  <- file.path(BASE, "inst", "extdata", "cmip", "AGUoceans_rcp85")
OUTPUT_DIR <- file.path(BASE, "output", "cmip", "AGUoceans_rcp85")

# If the output direcotry does not exists then create it
if(!file.exists(OUTPUT_DIR)){dir.create(path = OUTPUT_DIR, recursive = TRUE)}


# "Driver Like Behavior"  ----------------------------------------------------------------------
#
# Load the cmip_observations data to start with, or the raw data
data_path <- list.files(INPUT_DIR, "basin_mean.rda", full.names = TRUE)
raw_data  <- get(load(data_path))


# Detrend
#
# Detrend the raw data set to remove long term changes to focus on intra-annual variability.
detrended_data <- detrend(raw_data)


# Amplitude
#
# Calculate the annual/seasonal amplitude. This process will discard observations that do not
# meet requirements and will save those observations in a .rda object at the specified location.
amplitude <- get.amplitude(detrended_data, OUTPUT_DIR)


# Monthly Stats
#
# 30 year summary statistics for each / experiment / variable / model / basin
summary_stats <- tidyr::unnest(get.summary_stats(detrended_data))


# K and S
#
# Get the K and S values for the amplitude distribution.
KS_data <- get.K_S(amplitude)


# Save -----------------------------------------------------------------------------------
# Add script name as an attribute to output objects.
attributes(raw_data)$script_name <- attributes(KS_data)$script_name <-
  attributes(detrended_data)$script_name <- attributes(amplitude)$script_name <-
  attributes(summary_stats)$script_name <- attributes(KS_data)$script_name <- script_name

# Save data frames
save(raw_data, file = file.path(OUTPUT_DIR, "basin_mean.rda"))
save(detrended_data, file = file.path(OUTPUT_DIR, "detrended_data.rda"))
save(amplitude, file = file.path(OUTPUT_DIR, "amplitude.rda"))
save(summary_stats, file = file.path(OUTPUT_DIR, "summary_stats.rda"))
save(KS_data, file = file.path(OUTPUT_DIR, "Kurtosis_Skewness.rda"))


# Copy over output info
output_info_dir <- file.path(OUTPUT_DIR, "info"); dir.create(output_info_dir, F)

processd_info <- list.files(INPUT_DIR, "processed_observations_kable", full.names = T)
file.copy(processd_info, file.path(output_info_dir, "processed_observations_kable.rda"), overwrite = T)

removed_obs <- list.files(INPUT_DIR, "removed_observations", full.names = T)
file.copy(removed_obs, file.path(output_info_dir, "removed_observations.rda"), overwrite = T)


message("Script complete. Output saved at ", OUTPUT_DIR)

# End ----
