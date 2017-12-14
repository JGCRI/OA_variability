# ------------------------------------------------------------------------------
# Purpose: What's the difference between the amplitude values before and after
# detrending the time series.
#
# Created by: Dorheim, Kalyn
# Created on: October 11 2017
# Modified:   xxx
#
# Notes:
# ------------------------------------------------------------------------------
# Environment and Data Sets
# ------------------------------------------------------------------------------
library(dplyr)
library(ggplot2)
devtools::load_all()


# Import data sets for comparison
#
# Load the raw data
load_this <- list.files("data", "basin_mean", full.names = TRUE)
raw_df    <- get(load(load_this))


# Load the detrended data
load_this  <- list.files("data", "detrended_data", full.names = TRUE)
detrend_df <- get(load(load_this))



# ------------------------------------------------------------------------------
# Get Amplitudes
# ------------------------------------------------------------------------------
amp_raw     <- get.amplitude(raw_df) %>%  rename(raw_amp = amplitude) %>% select(-min, -max)
amp_detrend <- get.amplitude(detrend_df) %>%  rename(detrend_amp = amplitude) %>% select(-min, -max)


# ------------------------------------------------------------------------------
# Compare
# ------------------------------------------------------------------------------
left_join(amp_raw, amp_detrend,
          by = c("method", "units", "ensemble", "experiment", "variable", "basin", "model",  "year")) %>%
  mutate(dif = ((detrend_amp - raw_amp)/raw_amp) * 100) ->
  ampltidue_df


ampltidue_df %>%
   filter(dif > 10) %>%  dim

# It seems to make some what of a difference but idk if it is significant it seems like there are only a handful of the total
# that have a percent difference greater than 1 and a few that are greater than 10.
