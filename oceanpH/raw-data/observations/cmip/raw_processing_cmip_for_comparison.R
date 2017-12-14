# ------------------------------------------------------------------------------
# Purpose: The purpose of this script is to format the cmip / pic output for
# comparison with the observation data sets
#
# Created by: Dorheim, Kalyn
# Created on: Nov 10 2017
#
# Notes:
# ------------------------------------------------------------------------------
# Environment -- may be defined in the sourcing script
# ------------------------------------------------------------------------------
# library(dplyr)
# library(tidyr)


# Define the location where to save the products/outputs from the script. In this
# case the cmip observations will be concatenated witht the obs data set for the
# cmip-obs_comparison data set

# OUTPUT_DIR <- "data/formated" most likely defiend in the sourcein script

# ------------------------------------------------------------------------------
# Load and concatenate the csv files created on pic.
# ------------------------------------------------------------------------------
# Find all of the csv files within the raw-data/observations/cmip directory
csv_list <- tibble::tibble(file = list.files("raw-data/observations/cmip", pattern = ".csv", full.names = TRUE))

csv_list %>%
  filter(grepl("raw-data", file)) %>%
  filter(grepl("cmip", file)) %>%
  filter(grepl("L1", file)) ->
  to_process_df

# Open all of the csv files in the to process list.
data <- data.frame()
for(i in 1:length(to_process_df$file)){
  data <- rbind(data, read.csv(to_process_df$file[i], stringsAsFactors = FALSE))
} # end of the import for loop


# ------------------------------------------------------------------------------
# Convert Units
# ------------------------------------------------------------------------------
# Convert temperature data from K to C and assign ph blank units (for graphs).
data %>%
  dplyr::mutate(value = ifelse(variable == "tos", value - 273.17, value)) %>%
  dplyr::mutate(units = ifelse(variable == "tos", "C", units)) %>%
  dplyr::mutate(units = ifelse(variable == "ph", "", units)) ->
  data_units


# ------------------------------------------------------------------------------
# Filter Out Models
# ------------------------------------------------------------------------------
# Import the "to models to remove" from the csv and remove these models from the
# data frame. The models to be removed were identified in the exploratory analysis.
remove    <- list.files("raw-data/assumptions", pattern = "models_to_remove.csv", recursive = TRUE, full.names = TRUE)
to_remove <- read.csv(remove, stringsAsFactors = FALSE)

# Filter for "bad" models and dumby data.
data_units %>%
  filter(!model %in% to_remove$model) %>%
  filter(value != -999) %>%
  filter(year <= 2100) ->
  filtered_data

# The observations that were removed during the filtering process.
data_units %>%
  filter(!model %in% to_remove$model) %>%
  filter(value != -999) %>%
  select(model) %>%
  mutate(reason = "code -999 (could not process netcdf)") %>%
  distinct ->
  removed_data

# Save the removed data just in case you are curious.
save(removed_data, file = paste0(OUTPUT_DIR, "/removed_data.rda"))

# ------------------------------------------------------------------------------
# Format data frame for graphing (add factor levels).
# ------------------------------------------------------------------------------
# Month Names Data frames
month_name_df <- data.frame(month = 1:12,
                            month_name = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                                           "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))

data_month <- dplyr::left_join(filtered_data, month_name_df, by = "month")

# Add factor level information for the month names, this will plot the month names in
# the calender order.
data_month$month_name <- factor(data_month$month_name, levels = month_name_df$month_name, ordered = TRUE)

basin_mean <- data_month
# ------------------------------------------------------------------------------
# Save as RData object.
# ------------------------------------------------------------------------------
save(basin_mean, file = paste0(OUTPUT_DIR, "/cmip_obs_basin_mean.rda"))



# ----
# End





