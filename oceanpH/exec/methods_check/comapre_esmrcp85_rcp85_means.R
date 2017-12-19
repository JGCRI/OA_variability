# Purpose: check to see if the output from the esm experiment is different or not. There was some concern that I had
# processed the wrong cmip experiment model data. The intended purpose of this script it to qunatify the differences between
# the raw basin means.
#
# Created on: December 14
# Created by: Kalyn Dorheim


# Environement ----------------------------------------------------------------------------------------------------------
library(dplyr)

esm_path <- list.files(file.path("output", "cmip", "esmrcp85"), "basin_mean.rda", full.names = T)
rcp_path <- list.files(file.path("inst", "extdata", "cmip", "rcp85"), "basin_mean.rda", full.names = T)

esm_data <- get(load(esm_path))
rcp_data <- get(load(rcp_path))


# Qunatify Differences --------------------------------------------------------------------------------------------------

# Start by matching the data frames by year / model / units basin / time / variable / method / date / month / month_name
# and ensemble. Do not match by experiement.
esm_data %>%
  rename(esm_value = value) %>%
  left_join(rcp_data %>% rename(rcp_value = value), by = c("year", "model", "units", "basin", "time", "variable",
                             "method", "date", "month", "month_name", "ensemble")) %>%
  na.omit() ->
  matched_data_frames


# Mean absolute difference
matched_data_frames %>%
  mutate(value = abs(esm_value - rcp_value)) %>%
  summarise(mean = mean(value)) ->
  avg_abs_difference; avg_abs_difference

# Mean Percent Difference - since we are looking at difference variables with difference magnitudes I think it may be better
# to look at an average relative difference.
matched_data_frames %>%
  mutate(value = 100 * ((esm_value - rcp_value) / rcp_value)) %>%
  group_by(variable) %>%
  summarise(mean = mean(value)) ->
  avg_percent_difference; avg_percent_difference

# Seems like a small percent difference to me. Nothing noticable different from one another.
# Idk what to draw from as a conclusion.





