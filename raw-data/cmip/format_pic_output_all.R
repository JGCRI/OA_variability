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

# Define the location where to save the products/outputs from the script
OUT_DIR <- "data/cmip"

# Basins

# As of 12/1 we decided to narrow down the number of basins we are looking at.
# the basins to keep list will determine which basins should be included in the
# the upstream analysis.

# If set to ALL then all of the basins will be processed
keep_basins <- c("Arctic", "NH Atlantic",  "SH Atlantic",
                 "NH Pacific", "Southern Ocean", "SH Pacific")

# ------------------------------------------------------------------------------
# Load and concatenate the csv files created on pic.
# ------------------------------------------------------------------------------
# Find all of the csv files within the inst/extdata directory
csv_list <- tibble::tibble(file = list.files("raw-data/cmip", pattern = ".csv", full.names = TRUE))

csv_list %>%
  filter(grepl("raw-data", file)) %>%
  filter(grepl("cmip", file)) %>%
  filter(grepl("L1", file)) %>%
  filter(!grepl("CESM1", file)) ->
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
  dplyr::filter(basin %in% keep_basins) %>%
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
save(removed_data, file = paste0(OUT_DIR, "/removed_data.rda"))

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
defined_basins_path <- list.files("raw-data/assumptions", "defined_basins", full.names = TRUE)
defined_basins <- read.csv(defined_basins_path, stringsAsFactors = FALSE)

# My basins order
basin_order <- c("Global", "North Hemi", "South Hemi", "Atlantic", "NH Atlantic" , "SH Atlantic",
                 "Pacific", "NH Pacific", "SH Pacific", "Indian", "Arctic", "Southern Ocean")

data_month$basin <- factor(data_month $basin, levels = basin_order, ordered = TRUE)


# ------------------------------------------------------------------------------
# Format date and time
# ------------------------------------------------------------------------------
data_month %>%
  mutate(date = paste0(substr(time, 5, 6), "-01-", substr(time, 1,4))) %>%
  mutate(date = as.Date(date, "%m-%d-%Y")) ->
  data


# ------------------------------------------------------------------------------
# Save as RData object.
# ------------------------------------------------------------------------------
save(data, file = paste0(OUT_DIR, "/basin_mean.rda"))
save(defined_basins, file = paste0(OUT_DIR, "/defined_basins.rda"))


# ----
# End





