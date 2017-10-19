# ------------------------------------------------------------------------------
# Purpose: The purpose of this script is to convert the output from pic into an
# RData objects for speed in visualizing and de-trending. This script must be
# sourced from the top level of the package directory of the developmental branch
# due to the way that devtools::save works.
#
# Created by: Dorheim, Kalyn
# Created on: October 3
#
# Notes:
# ------------------------------------------------------------------------------
# Set up the environment.
# ------------------------------------------------------------------------------
library(dplyr); library(tidyr); library(ggplot2)

# ------------------------------------------------------------------------------
# Load and concatenate the csv files created on pic.
# ------------------------------------------------------------------------------
# Find all of the csv files within the inst/extdata directory
path <- getwd()
tibble::tibble(file = list.files(path = path, pattern = ".csv", full.names = TRUE, recursive = TRUE)) %>%
  filter(grepl("raw-data", file)) %>%
  filter(grepl("L1", file), grepl("CESM", file)) ->
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
  mutate(value = ifelse(variable == "tos", value - 273.17, value)) %>%
  mutate(units = ifelse(variable == "tos", "C", units)) %>%
  mutate(units = ifelse(variable == "ph", "", units)) ->
  data_units

# ------------------------------------------------------------------------------
# Filter Out Observations
# ------------------------------------------------------------------------------
# Import the "to models to remove" from the csv and remove these models from the
# data frame. The models to be removed were identified in the exploratory analysis.

remove    <- list.files(path, pattern = "models_to_remove.csv", recursive = TRUE, full.names = TRUE)
to_remove <- read.csv(remove, stringsAsFactors = FALSE)

# Remove the models identified as "bad" in the exploratory stage, data with the value -999 (dumby data)
# and observations for years greater than 20100.
data_units %>%
  filter(!model %in% to_remove$model) %>%
  filter(year < 2100) %>%
  filter(value != -999) ->
  filtered_data

data_units %>%
  filter(model %in% to_remove$model) %>%
  filter(value == -999) ->
  removed_CESM1

# Save the removed data just in case you are curious.
devtools::use_data(removed_CESM1, overwrite = TRUE)


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

# Import the defined_basins.csv file
csv_list            <- tibble::tibble(file = list.files(path, pattern = ".csv", recursive = TRUE, full.names = TRUE))
defined_basins_path <- dplyr::filter(csv_list, grepl(pattern = "raw-data/assumptions/defined_basins.csv", x = file))
defined_basins      <- read.csv(defined_basins_path$file, stringsAsFactors = FALSE)

# My basins order
basin_order <- c("Global", "North Hemi", "South Hemi", "Atlantic", "NH Atlantic" , "SH Atlantic",
                 "Pacific", "NH Pacific", "SH Pacific", "Indian", "Arctic", "Southern Ocean")

data_month$basin <- factor(data_month $basin, levels = basin_order, ordered = TRUE)

CESM1_trended_basin_mean <- data_month


# ------------------------------------------------------------------------------
# Save as RData object.
# ------------------------------------------------------------------------------

devtools::use_data(CESM1_trended_basin_mean, defined_basins, overwrite = TRUE)

# ----
# End





