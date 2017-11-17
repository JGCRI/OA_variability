# Script Info --- ------------------------------------------------------------------------------
# Purpose: This script acts as the driver for the observations & cmip comparison data set. This
# scipt reuiqres a combinations of data from pic/obs data that must be processed. These materials
# can be found in raw-data/observations and can be sources from exec/0.5-pic_post_processing
#
# Created by: Dorheim, Kalyn
# Created on: Nov 15 2017
# Modified:   xxx
#
# Notes: I am unsure if I want to go with driver functions or driver scripts??? what would
# be easiest?
#
# Set Up Environment  -------------------------------------------------------------------------

# Load the required libraries
library(dplyr)
library(tidyr)
devtools::load_all()

# Define the input and output directories for this script
INPUT_DIR  <- "data/observations"
OUTPUT_DIR <- "data/observations"


# Load the cmip_observations data to start with, or the raw data
data_path <- list.files(INPUT_DIR, "comparison_mean.rda", full.names = TRUE)
raw_data  <- get(load(data_path))


# Detrend  -----------------------------------------------------------------------------------
# Detrend the raw data set to remove long term changes to focus on intra-annual variability.
detrended_data <- detrend(raw_data)

# Save
save(detrended_data, file = paste0(OUTPUT_DIR, "/obs_cmip_detrended.rda"))


# Amplitude ----------------------------------------------------------------------------------
# Calculate the annual/seasonal amplitude. This process will discard observations that do not
# meet requirements and will save those observations in a .rda object at the specified location.
amplitude <- get.amplitude(detrended_data, OUTPUT_DIR)

# Save
save(amplitude, file = paste0(OUTPUT_DIR, "/obs_cmip_amplitude.rda"))


# Monthly Stats ------------------------------------------------------------------------------
# Get the monthly mean and sd, observations will need to be removed for cmip models in order to
# be comparable with the obs data.

# Subset the detrended data for the observations data set, this data set will be joined with
# the detrended_data set to discard the cmip observations without corresponding observational
# data.
detrended_data %>%
  filter(ensemble == "obs") %>%
  select(year, month, variable, basin) %>%
  mutate(KEEP = TRUE) ->
  intermediate_obs_df

# Keep only the observations that have actual observational data.
detrended_data %>%
  left_join(intermediate_obs_df,  by = c("variable", "basin", "year", "month")) %>%
  filter(KEEP == TRUE) %>%
  select(-KEEP) ->
  use_for_monthly

# Get monthly mean and sd for each model / basin / variable combination.
use_for_monthly %>%
  group_by(model, basin, variable, month, month_name, units, experiment, ensemble) %>%
  summarise(mean = mean(value), sd = sd(value)) %>%
  ungroup() ->
  monthly_data

# Save
save(monthly_data, file = paste0(OUTPUT_DIR, "/obs_cmip_comparisons_monthly.rda"))


# K and S ------------------------------------------------------------------------------
# Get the K and S values for the amplitude distribution. Note, we will not be able to
# calculate the delta K and S because of there no experiments for the different
# observations data sets.

#  I am nervous that we will not have enough data in order to be confident our distribution.
# Also will not be able to get the delta K and S values b
KS_data <- get.K_S(amplitude)

# Save
save(KS_data, file = paste0(OUTPUT_DIR, "/cmip_obs_KSdata.rda"))


# End ----
message("End of the script.")

