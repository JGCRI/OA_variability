# ------------------------------------------------------------------------------
# Purpose: The purpose of this script is to convert the output from pic into an
# RData objects for speed in visualizing and detrending. This script must be
# soruced from the top level of the pacakge direcotry of the devlopmental branch
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
# Determine the base name path to search
path <- paste0(getwd(), "/OA_variability/raw-data/")

# Find all of the csv files within the inst/extdata direcotry
csv_list <- tibble::tibble(file = list.files(path = path, pattern = ".csv", full.names = TRUE, recursive = FALSE))

# Select the files to import into R NOTE! this section is subject to change....
csv_list  %>%
  dplyr::filter(grepl("L1", file)) ->
  to_process_df

# Open all of the csv files in the to process list.
data <- data.frame()
for(i in 1:length(to_process_df$file)){
  data <- rbind(data, read.csv(to_process_df$file[i], stringsAsFactors = FALSE))
} # end of the import for loop

# ------------------------------------------------------------------------------
# Remove the unwanted models.
# ------------------------------------------------------------------------------
# Import the "to models to remove" from the csv and remove these models from the
# data frame. The models to be removed were identified in the exploratory analysis.
path <- paste0(getwd(), "/OA_variability/raw-data/assumptions/")

# Find all of the csv files within the inst/extdata direcotry
csv_list <- tibble::tibble(file = list.files(path = path, pattern = ".csv", full.names = TRUE, recursive = TRUE))

to_remove_path <- dplyr::filter(csv_list, grepl("models_to_remove", file))
to_remove      <- read.csv(to_remove_path$file, stringsAsFactors = FALSE)

data <- dplyr::filter(data, !model %in% to_remove$model)


# ------------------------------------------------------------------------------
# Format data frame for graphing (add factor levels).
# ------------------------------------------------------------------------------
# Month Names Data frames
month_name_df <- data.frame(month = 1:12,
                            month_name = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                                           "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))

data <- dplyr::left_join(data, month_name_df, by = "month")

# Add factor level information for the month names, this will plot the month names in
# the calander order.
data$month_name <- factor(data$month_name, levels = month_name_df$month_name, ordered = TRUE)
basin_mean <- data

# Import the defined_basins.csv file
defined_basins_path <- dplyr::filter(csv_list, grepl("defined_basins.csv", file))
defined_basins <- read.csv(defined_basins_path$file, stringsAsFactors = FALSE)

# Add factor level information for the basins, this will help with the plotting.
# This statement is set up to order the basins by what ever order they were
# entered in the defined_basins.csv. Can mannually change the facotr order here
# if so desired.
data$basin <- factor(data$basin, levels = defined_basins$basin, ordered = TRUE)

# ------------------------------------------------------------------------------
# Save as RData object.
# ------------------------------------------------------------------------------

devtools::use_data(basin_mean, defined_basins, pkg = "OA_variability", internal = FALSE, overwrite = TRUE)


# ----
# End





