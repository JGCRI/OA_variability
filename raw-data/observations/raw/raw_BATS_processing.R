# ------------------------------------------------------------------------------
# Purpose: The purpose of this script is to process the BATS observations
# data set in preparation for the observational data analysis.
#
# Created by: Dorheim, Kalyn
# Created on: November 9
#
# Notes:  .dat files are imported from http:// addresses by this script the read
# me file can be found at http://batsftp.bios.edu/BATS/pco2/pco2_readme.txt
# ------------------------------------------------------------------------------
# Environment -- some of the environment is set up in the sourcing script
# ------------------------------------------------------------------------------
# Load libraries
# library(dplyr)
# library(tidyr)

# OUTPUT_DIR <- this is required an is most likely defined in the sourcing script

# Import parial pressure data frames
bind_rows(readr::read_table("http://batsftp.bios.edu/BATS/pco2/1994_pco2.dat", col_names = FALSE),
          readr::read_table("http://batsftp.bios.edu/BATS/pco2/1995_pco2.dat", col_names = FALSE),
          readr::read_table("http://batsftp.bios.edu/BATS/pco2/1996_pco2.dat", col_names = FALSE)) ->
  unformatted_data


# ------------------------------------------------------------------------------
# Format Data
# ------------------------------------------------------------------------------
# Column names and units are found at http://batsftp.bios.edu/BATS/pco2/pco2_readme.txt
col_names <- c("year", "day", "Lat", "Long", "Ship Heading", "Ship speed", "Relative Wind Direction", "Relative wind speed",
               "No data", "Air temperature", "Relative humidity", "no data", "Incoming Short Wave Radiation",
               "No data", "Temperature at seawater intake", "Salinity at seawater", "Rainfall",
               "Temperature at pCO2 equilibrator", "Atmospheric pressure at pCO2 system", "Seawater pCO2",
               "Atmospheric pCO2")

units_list <- c( "YY", "NA", "NA", "NA", "deg", "m/s", "deg", "m/s", "NA", "deg C", "perc", "NA", "W/m2",
            "NA", "deg C", "NA", "mm", "deg C", "mb", "uatm", "ppm")

units_df <- tibble::tibble(variable = col_names, units = units_list)

# The month name data frame.
month_df <- tibble(month_name = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
                   month_num = 1:12,
                   month_chr = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12"))


# Add the column names
names(unformatted_data) <- col_names

# Remove the columns that do not contain data
to_remove <- which(grepl("[N|n]o data", names(unformatted_data)))

# Drop the columns
unformatted_data[, -to_remove] %>%
  # Format years
  mutate(year = if_else(year > 70, paste0("19", year), paste0("20", year))) %>%
  mutate(year = as.integer(year)) %>%
  # Convert decimal date to date and time using lubridate then parse out month number,
  # make sure to save it as an integer.
  mutate(day = as.numeric(day)) %>%
  mutate(date = lubridate::date_decimal(day)) %>%
  mutate(month_num = substr(date, 6, 7)) %>%
  mutate(month_num = as.integer(month_num)) %>%
  # Add the month name column using the month name / number data frame.
  left_join(month_df, by = "month_num") ->
  data_year

# Subset for data of interest
data_year %>%
  select(year, month_num, month_name, month_chr, `Seawater pCO2`) %>%
  # Remove the no data observations, -9.99 and -999 were defined as no data in
  # the read me file (http://batsftp.bios.edu/BATS/pco2/pco2_readme.txt)
  mutate(`Seawater pCO2` = as.numeric(`Seawater pCO2`)) %>%
  filter(!`Seawater pCO2` %in% c(-9.99, -999)) %>%
  # Format as long data
  gather(variable, value, `Seawater pCO2`) ->
  data_long

# Find monthly average
data_long %>%
  group_by(year, month_num, month_name, month_chr, variable) %>%
  summarise(value = mean(value)) %>%
  ungroup ->
  data_monthly

# Add units to the monthly data set
data_units <- left_join(data_monthly, units_df, by = "variable")

# Convert uatm to the correct Pa for comparison with cmip data.
# Start by defining the conversion factor
con_factor <- 1e-6 * 101325

# Use the conversion factor to convert uatm -> Pa and then make the according
# changes to the data frame (unit name and variable name)
data_units %>%
  mutate(value = if_else(units == "uatm", value * con_factor, value)) %>%
  mutate(units = if_else(units == "uatm", "Pa", units)) %>%
  mutate(variable = ifelse(units == "Pa", "spco2", variable)) ->
  data


# ------------------------------------------------------------------------------
# Final Formatting
# ------------------------------------------------------------------------------
# Format the final output data frame so that it is compatible with the cmip
# processing functions.
data %>%
  mutate(ensemble = "obs", experiment = "obs", units, model = "BATS", basin = "BATS") %>%
  mutate(time = paste0(year, month_chr)) %>%
  mutate(time = as.integer(time)) %>%
  select(ensemble, experiment, basin, model, time, year, month = month_num, month_name, value, variable, units) ->
  final_formated_data

# Add month name factor information
final_formated_data$month_name <- factor(final_formated_data$month_name, levels = month_df$month_name, ordered = TRUE)


# ------------------------------------------------------------------------------
# Save
# ------------------------------------------------------------------------------
save(final_formated_data, file = paste0(OUTPUT_DIR, "/BATS_formated.rda"))
message("done with raw BATS processing")
# ----
# End
