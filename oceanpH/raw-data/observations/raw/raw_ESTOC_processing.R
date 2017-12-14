# ------------------------------------------------------------------------------
# Purpose: The purpose of this script is to process the ESTOC observations
# data set in preparation for the observational data analysis.
#
# This script will be sourced by another script so some of the environment will
# actually be defined in another script.
#
# Created by: Dorheim, Kalyn
# Created on: November 9
#
# Notes: All csv files were downloaded from http://www.fixo3.eu/observatory-data-ESTOC/
# Lon -15.5, Lat 29.17 found at http://earthvo.fixo3.eu/
# ------------------------------------------------------------------------------
# Environment
# ------------------------------------------------------------------------------
# # Load the required libraries
# library(dplyr)
# library(tidyr)
# INPUT_DIR <- "raw-data/observations/raw"
# OUTPUT_DIR <- needs to be defined here or in the script that calls this one

# Save the paths to the observations to csv files to process
# spco2
spco2_path <- list.files(INPUT_DIR, "earthvo_data_spco2", full.names = TRUE)

# sst
sst_path <- list.files(INPUT_DIR, "earthvo_data_sst", full.names = TRUE)

# First create the month name data frame
month_df <- tibble(month_name = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
                   month_num = 1:12,
                   month_chr = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12"))

# ------------------------------------------------------------------------------
# Format spco2 observations
# ------------------------------------------------------------------------------
# Read in the spco2 data and data column names
data_unformatted <- readr::read_csv(spco2_path, col_names = FALSE)
names(data_unformatted) <- c("Date", "Depth", "value", "source", "variable", "units", "variable name")


# Subset the observations and convert units to the Pa for consistency with the cmip data.
#
# Conversion factor to get from mico atm to Pa
con_factor <- 1e-6 * 101325

# Convert
data_unformatted %>%
  filter(units == "uatm") %>%
  mutate(value = value * con_factor) %>%
  mutate(units = "Pa") %>%
  mutate(variable = "spco2") ->
  data_Pa

# Now average the spco2 over the depth for surface values.
data_Pa %>%
  filter(Depth <= 1.0) %>%
  group_by(Date, source, variable, units, `variable name`) %>%
  summarise(value = mean(value)) %>%
  ungroup ->
  data_Pa_noyear

# Now parse out year / month / date from the Date column
data_Pa_noyear %>%
  # Parse out year and save as an integer
  mutate(year = substr(Date, 1, 4)) %>%
  mutate(year = as.integer(year)) %>%
  # Parse out month information and save as an integer
  mutate(month_num = substr(Date, 6, 7)) %>%
  mutate(month_num = as.integer(month_num)) %>%
  # Add the month name column to the data frame.
  left_join(month_df, by = "month_num") %>%
  # Final data frame clean up.
  select(-Date, -`variable name`) ->
  final_data_spco2


# ------------------------------------------------------------------------------
# Format sst observations
# ------------------------------------------------------------------------------
# Read in the sst data and data column names
data_unformatted <- readr::read_csv(sst_path, col_names = FALSE)
names(data_unformatted) <- c("Date", "Depth", "value", "source", "variable", "units", "variable name")

# Convert
data_unformatted %>%
  filter(Depth <= 1.0) %>%
  group_by(Date, source, variable, units, `variable name`) %>%
  summarise(value = mean(value)) %>%
  ungroup ->
  data_surface

# Now parse out year / month / date from the Date column
data_surface %>%
  # Parse out year and save as an integer
  mutate(year = substr(Date, 1, 4)) %>%
  mutate(year = as.integer(year)) %>%
  # Parse out month information and save as an integer
  mutate(month_num = substr(Date, 6, 7)) %>%
  mutate(month_num = as.integer(month_num)) %>%
  # Add the month name column to the data frame.
  left_join(month_df, by = "month_num") %>%
  # Final data frame clean up.
  select(-Date, -`variable name`) %>%
  mutate(units = "C", variable = "sst") ->
  final_data_sst

# ------------------------------------------------------------------------------
# Final Formating
# ------------------------------------------------------------------------------
# Concatenate into a single data frame, complete final formating, and save.
data <- bind_rows(final_data_spco2, final_data_sst)



# Format to match the data frames of the cmip outputs. This will allow us to use
# existing functions.
data %>%
  rename(model = source) %>%
  mutate(basin = model, month = month_num, ensemble = "obs", experiment = "obs") %>%
  mutate(time = paste0(year, month_chr)) %>%
  mutate(time = as.integer(time)) %>%
  select(ensemble, experiment, time, model, year, month, month_name, value, variable, units, basin) %>%
  mutate(variable = if_else(units == "C", "tos", variable)) ->
  final_formated_data

# Add month order information to the month name column.
final_formated_data$month_name <- factor(final_formated_data$month_name, levels = month_df$month_name, ordered = TRUE)


# ------------------------------------------------------------------------------
# Save
# ------------------------------------------------------------------------------
save(final_formated_data, file = paste0(OUTPUT_DIR, "/ESTOC_formated.rda"))
message("done with raw ESTOC processing")
# ----
# End

