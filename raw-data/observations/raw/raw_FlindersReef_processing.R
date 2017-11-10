# ------------------------------------------------------------------------------
# Purpose: This script downloads and formats NOAA paleo ocean observations from
# Flinders Reef.
#
# Created by: Dorheim, Kalyn
# Created on: November 3 2017
# Modified:   xxx
#
# Trying to decide if I should modify the data frame so that it is compatible
# with the functions used in the driver or if I should change the functions....
# ------------------------------------------------------------------------------
# Environment -- some of the required environment may be defined in sourcing script
# ------------------------------------------------------------------------------
# library(dplyr)
#
# OUTPUT_DIR <- required may be defined in the sourcing script

# Get data frame from source
readr::read_table("ftp://ftp.ncdc.noaa.gov/pub/data/paleo/coral/east_pacific/palmyra2011.txt", skip = 131, col_names = TRUE) %>%
  # Remove the NAs from the tails of the data frame
  na.omit ->
  data_unformatted

# ------------------------------------------------------------------------------
# Format Data Frame
# ------------------------------------------------------------------------------
# One of the characters used in the column name causes error so you must rename the columns.
names(data_unformatted) <- c("date", "d180", "Sr/Ca", "SST", "d180 per sw")

# Start adding year and month data to the data frame.
data_unformatted %>%
  # Parse out the year number from the date column, save as an integer.
  mutate(year = substr(date, 1, 4)) %>%
  mutate(year = as.integer(year)) %>%
  # Parse out the percent of the year aka the month from the date column.
  mutate(percent_year = substr(date, 6, 7)) %>%
  mutate(percent_year = as.numeric(percent_year)) ->
  data_percent_year


# Now we need a way to convert the percent of year to normal month names.
# Save the list of the percent of years from the data frame and arrange in
# ascending order.
data_percent_year %>%
  select(percent_year) %>%
  distinct %>%
  arrange(percent_year) ->
  percent_month_order

# Add the percent of year column to the month data frame to create month name mapping file.
# The month name data frame.
month_df <- tibble(month_name = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
                   month_num = 1:12,
                   month_chr = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12"))
month_df$percent_year <- percent_month_order$percent_year

# Now match the month name with the percent.
data_percent_year %>%
  inner_join(month_df, by = "percent_year") %>%
  # Remove the percent of the year nonsense
  select(-percent_year, -date) %>%
  # make into long format
  gather(variable, value, -year, -month_name, -month_num, -month_chr) %>%
  filter(variable == "SST") ->
  data

# ------------------------------------------------------------------------------
# Final Formating
# ------------------------------------------------------------------------------
# Format the data so that is comparable with the cmip functions
data %>%
  mutate(ensemble = "obs", experiment = "obs", basin = "FlindersReef", model = "FlindersReef") %>%
  mutate(time = paste0(year, month_chr)) %>%
  mutate(time = as.integer(time)) %>%
  mutate(variable = if_else(variable == "SST", "tos", variable)) %>%
  mutate(units = if_else(variable == "tos", "C", "NA")) %>%
  select(time, year, month = month_num, month_name, value, variable, units, basin, model, experiment, ensemble) ->
  final_formated_data

# ------------------------------------------------------------------------------
# Save Data Frame
# ------------------------------------------------------------------------------
save(final_formated_data, file = paste0(OUTPUT_DIR, "/Flinders_Reef.rda"))
message("done with processing the Flinders_Reef raw data")

# -----
# End

