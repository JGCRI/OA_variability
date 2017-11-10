# ------------------------------------------------------------------------------
# Purpose: The purpose of this script is to process the Irminger Sea observations
# data set in preparation of the analysis
#
# Created by: Dorheim, Kalyn
# Created on: November 9
#
# Notes: Data was downloaded from https://www.nodc.noaa.gov/archive/arc0093/0149098/1.1/data/0-data/
# Location of sampling can be found in https://www.earth-syst-sci-data.net/2/99/2010/essd-2-99-2010.pdf
# Coverage: 64◦ N–68◦ N; 28◦ W–12◦ W
# ------------------------------------------------------------------------------
# Environment --- required portions defined in sourcing script are commented out
# ------------------------------------------------------------------------------
# # Load required libs
# library(dplyr)
# library(tidyr)
#
# INPUT_DIR <- required may be defined in the sourcing script
# OUPUT_DIR <- required may be defined in the sourcing script

# Define path to csv file
# Note the CSV file was acquired from https://www.nodc.noaa.gov/archive/arc0093/0149098/1.1/data/0-data/
data_path <- list.files(INPUT_DIR, "IrmingerSea.exc_V2", full.names = TRUE)

# Import data from csv
# Note the header column is in two rows... Save the first row of header information and then
# remove the second header row that contains units and NA
readr::read_csv(data_path, skip = 151) %>%
  na.omit ->
  unformatted_data

# The month name data frame.
month_df <- tibble(month_name = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
                   month_num = 1:12,
                   month_chr = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12"))

# ------------------------------------------------------------------------------
# Lat and Lon Info
# ------------------------------------------------------------------------------
# Create a data frame of unique coordinates
unformatted_data %>%
  select(LATITUDE, LONGITUDE) %>%
  distinct ->
  coord_df

# Create a data frame of max and min lat and lon values
lat_1 <- min(coord_df$LATITUDE)
lat_2 <- max(coord_df$LATITUDE)
lon_1 <- min(coord_df$LONGITUDE)
lon_2 <- max(coord_df$LONGITUDE)

coordinates <- tibble(lat_1, lat_2, lon_1, lon_2)


# ------------------------------------------------------------------------------
# Format data
# ------------------------------------------------------------------------------
unformatted_data %>%
  # Subset to select the surface observations aka those with a pressure less than 1.
  filter(CTDPRS <= 1.0) %>%
  select(DATE, PCO2, TCARBN)  %>%
  # Parse out the year information
  mutate(year = substr(DATE, 1, 4)) %>%
  mutate(year = as.integer(year)) %>%
  # Parse out the month number
  mutate(month_num = substr(DATE, 5, 6)) %>%
  mutate(month_num = as.integer(month_num)) %>%
  select(-DATE) ->
  unformatted_year

# Format the data into long format and then average month / year / variable for
# surface values.
unformatted_year %>%
  rename(spco2 = PCO2, totC = TCARBN) %>%
  gather(variable, value, spco2, totC) %>%
  # Add units information - variable units can be found in the header of the csv file
  mutate(units = if_else(variable == "spco2", "uatm", "NA")) %>%
  # Average across depths to get a singular monthly surface value.
  group_by(year, month_num, variable, units) %>%
  mutate(value = as.numeric(value)) %>%
  summarise(value = mean(value)) %>%
  filter(variable == "spco2") %>%
  filter(!value %in% c(-999, -99)) %>%
  ungroup ->
  surface_data_year

# Add month name column by joining with the month_df
data_month <- left_join(surface_data_year, month_df, by = "month_num")


# ------------------------------------------------------------------------------
# Final Formating
# ------------------------------------------------------------------------------
# Format the data so that is ti compatible with the cmip functions.
data_month %>%
  # Add time column
  mutate(time = paste0(year, month_chr)) %>%
  mutate(time = as.integer(time)) %>%
  # Add the cmip stuff
  mutate(ensemble = "obs", experiment = "obs", basin = "IrmingerSea", model = "IrmingerSea") ->
  data_cmip

# Convert the spco2 from uatm to Pa for comparison with with the cmip data
# First define the conversion factor.
spco2_con_factor <- 1e-6 * 101325
data_cmip %>%
  # Multiply by the conversion factor and change the units.
  mutate(value = if_else(units == "uatm", value * spco2_con_factor, value)) %>%
  mutate(units = if_else(units == "uatm", "Pa", units)) %>%
  select(time, year, month = month_num, month_name, value, variable, units, model, ensemble, experiment, basin) ->
  data_final_formated


# Order the month names
data_final_formated$month_name <- factor(data_final_formated$month_name, levels = month_df$month_name, ordered = TRUE)


# ------------------------------------------------------------------------------
# Save
# ------------------------------------------------------------------------------
save(data_final_formated, file = paste0(OUTPUT_DIR, "/IrmingerSea_formated.rda"))
message("done with processing raw IrmingerSea")
# ----
# End

