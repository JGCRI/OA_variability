# ------------------------------------------------------------------------------
# Purpose: This script formats the raw HOTS data set in preparation for the
# OA variability analysis.
#
# Created by: Dorheim, Kalyn
# Created on: November 3 2017
# Modified:   xxx
#
# Notes: HOTS data was obtained from http://hahana.soest.hawaii.edu/hot/products/HOT_surface_CO2.txt
# and the read me file with units information can be found at http://hahana.soest.hawaii.edu/hot/products/HOT_surface_CO2_readme.pdf
# ------------------------------------------------------------------------------
# Environment
# ------------------------------------------------------------------------------
# # Load required libraries
# library(dplyr)
# library(ggplot2)
# library(tidyr)
# devtools::load_all()
#
# INPUT_DIR <- required
# OUTPUT_DIR <- required


# ------------------------------------------------------------------------------
# Data frames
# ------------------------------------------------------------------------------
# Import the HOT observations csv file
# The data set was downloaded from http://hahana.soest.hawaii.edu/hot/products/HOT_surface_CO2.txt
path <- list.files(INPUT_DIR, "HOT_surface_CO2", full.names = TRUE)
unformatted_df <- read.table(path,sep = "\t", skip = 8, header = TRUE)


# Create the month name data frame. This will be used to format the observational data
# frame into an object that is compatible the functions from this project. Because of the
# way that HOTS stores date information the month number in this month data frame must
# contain 2 digits and therefore must be a saved as characters not integers.
month_df <- tibble(month_name = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
                   month_num = 1:12,
                   month_chr = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12"))


# ------------------------------------------------------------------------------
# Format HOTS Data
# ------------------------------------------------------------------------------
unformatted_df %>%
  # First remove the NAs at the end of the data frame.
  na.omit %>%
  # Parse out month name, year and date from data frame. Will need to concatenate
  # all of the the time related columns into a time column for the detrending.
  mutate(month_name = substr(date, 4, 6)) %>%
  mutate(year = substr(date, 8, 9)) %>%
  # Parse out year information and add the first two digits
  mutate(year = if_else( year >= 88, paste0("19",year), paste0("20",year))) %>%
  mutate(day = substr(date, 9, 10)) %>%
  left_join(month_df, by = "month_name") %>%
  # Combine year, month, and day to create the time column used in detrending.
  mutate(time = paste0(year,month_chr)) %>%
  # Mutate the time column to integer form so that use in detrending.
  mutate(time = as.integer(time))->
  time_formated_df

# Now that time is formated properly select the variables to save.
time_formated_df %>%
  select(time, year, month_name, tos = temp, ph = pHcalc_insitu, co3 = carbonate_insitu, spco2 = pCO2calc_insitu) %>%
  mutate(source = "HOTS") %>%
  # Add month number
  left_join(month_df, by = "month_name") %>%
  # Make sure that year and month are integers and not characters
  mutate(month = as.integer(month_num)) %>%
  mutate(year = as.integer(year)) %>%
  # Format into a tibble
  as_tibble %>%
  gather(variable, value, tos, ph, co3, spco2) %>%
  mutate(value = as.numeric(value)) %>%
  # Remove the NA values, indicated that -999 is the code for NA in the description file.
  filter(value != -999) ->
  HOTS_data

# Add the units information based on the readme file located at
# http://hahana.soest.hawaii.edu/hot/products/HOT_surface_CO2_readme.pdf
HOTS_data %>%
  mutate(units = if_else(variable == "tos", "C", "NA")) %>%
  mutate(units = if_else(variable == "ph", "", units)) %>%
  mutate(units = if_else(variable == "co3", " µmol / kg", units)) %>%
  mutate(units = if_else(variable == "spco2", " µatm", units))  ->
  HOTS_data_units


# Convert the units
#
# Start by defining all of the conversion factors
spco2_con_factor <- 1e-6 * 101325
co3_con_factor   <- 1e-6 / 0.00042

HOTS_data_units %>%
  # Convert spco2 to Pa
  mutate(value = if_else(variable == "spco2", value * spco2_con_factor, value)) %>%
  mutate(units = if_else(variable == "spco2", "Pa", units)) %>%
  # Convert co3 to mol/m-3
  mutate(value = if_else(variable == "co3", value * co3_con_factor, value)) %>%
  mutate(units = if_else(variable == "co3", "mol m-3", units)) ->
  data_converted


# ------------------------------------------------------------------------------
# Final Formating
# ------------------------------------------------------------------------------
# Add columns to the data frame in order to make it compatible with the cmip
# function.
data_converted %>%
  mutate(model = "HOTS", basin = "HOTS", experiment = "obs", ensemble = "obs") %>%
  select(ensemble, experiment, model, basin, time, year, month = month_num, month_name, value, units, variable) ->
  final_formated_data

# Add month order as a factor
final_formated_data$month_name <- factor(final_formated_data$month_name, levels = month_df$month_name, ordered = TRUE)

save(final_formated_data, file = paste0(OUTPUT_DIR, "/HOTS_formated.rda"))
message("Done with raw HOTS processing")
# ----
# End



