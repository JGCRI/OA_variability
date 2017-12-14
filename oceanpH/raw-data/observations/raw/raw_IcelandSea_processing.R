# ------------------------------------------------------------------------------
# Purpose: The purpose of this script is to process the Iceland Sea observations
# data set in preparation of the analysis
#
# Created by: Dorheim, Kalyn
# Created on: November 9
#
# Notes: Data was downloaded from https://www.nodc.noaa.gov/ocads/oceans/Moorings/Iceland_Sea.html
# Iceland Sea (North Atlantic, Iceland Sea, 68N° 12.66°W 64.3°N,28°W )
# Meta data located at https://www.nodc.noaa.gov/ocads/data/0100063.xml
# ------------------------------------------------------------------------------
# Environment -- commented out environment is set up in the sourcing script
# ------------------------------------------------------------------------------
# # Load required libs
# library(dplyr)
# library(tidyr)
#
# INPUT_DIR <- required may be defined in the sourcing script
# OUTPUT_DIR <- required may be defined in the sourcing script

# Find the data path
# This CSV file was down loaded from https://www.nodc.noaa.gov/ocads/oceans/Moorings/Iceland_Sea.html
data_path <- list.files(INPUT_DIR, "IcelandSea.exc.csv", full.names = TRUE)

# Load the csv file, there is a lot of meta data & header information at the beginning of the
# csv file so skip these rows. Information about the column is in spread out into two rows.
# Save the first row of column names and then remove the second row containing NAs/units
readr::read_csv(data_path, skip = 136) %>%
  na.omit ->
  unformatted_data
# This step will generate a lot of warnings due to the way that the original csv is
# structured.

# The month name data frame.
month_df <- tibble(month_name = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
                   month_num = 1:12,
                   month_chr = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12"))


# ------------------------------------------------------------------------------
# Format Data
# ------------------------------------------------------------------------------
# Now subset the data for the surface levels observations.
unformatted_data %>%
  filter(CTDPRS <= 1.0) %>%
  select(DATE, PCO2) %>%
  # Parse out year information from the date.
  # Make sure the year data is stored as a number not a character.
  mutate(year = substr(DATE, 1, 4)) %>%
  mutate(year = as.integer(year)) %>%
  # Parse out month information from the date column
  mutate(month_num = substr(DATE, 5, 6)) %>%
  mutate(month_num = as.integer(month_num)) %>%
  select(-DATE) ->
  data_year


# Format the data set into long format and then average to find the
# average surface value.
data_year %>%
  rename(spco2 = PCO2) %>%
  gather(variable, value, spco2) %>%
  group_by(year, month_num, variable) %>%
  # Make sure the value column is stored as a number and not
  # a character.
  mutate(value = as.numeric(value)) %>%
  summarise(value = mean(value)) %>%
  # Ungroup
  ungroup ->
  data_month_num

# Add the month name column to data frame by month number.
data_month_num %>%
  left_join(month_df,  by = "month_num") ->
  data


# Units
#
# Add units information based on https://www.nodc.noaa.gov/ocads/data/0100063.xml
data_units <- mutate(data, units = if_else(variable == "spco2", "uatm", "NA"))

# Convert units for spcco2 from uatm to Pa, first define the conversion factor.
spco2_con_factor <- 1e-6 * 101325

data_units %>%
  filter(! value %in% c(-99, -999)) %>%
  mutate(value = if_else(variable == "spco2", value * spco2_con_factor, value)) %>%
  mutate(units = if_else(variable == "spco2", "Pa", units)) ->
  data_converted


# ------------------------------------------------------------------------------
# Final Formating
# ------------------------------------------------------------------------------
# Format the data frame so that it is compatible with the cmip functions.
data_converted %>%
  mutate(model = "IcelandSea", basin = "IcelandSea", ensemble = "obs", experiment = "obs") %>%
  mutate(time = paste0(year, month_chr)) %>%
  mutate(time = as.integer(time)) %>%
  select(time, value, variable, units, basin, model, ensemble, experiment, year, month = month_num, month_name) ->
  final_formated_data

final_formated_data$month_name <- factor(final_formated_data$month_name, month_df$month_name, ordered = TRUE)

# ------------------------------------------------------------------------------
# Save
# ------------------------------------------------------------------------------
save(final_formated_data, file = paste0(OUTPUT_DIR, "/IcelandSea_formated.rda"))

message("When sourced this script will generate a lot of warnings \n theses warnings come from line 24 and are expected")
message("done with IcelandSea raw processing\n\n\n")
# ----
# End


