# ------------------------------------------------------------------------------
# Purpose: This script formats the raw HOTS output for OAvariability analysis.
#
# Created by: Dorheim, Kalyn
# Created on: Novemeber 3 2017
# Modified:   xxx
#
# Trying to decide if I should modify the data frame so that it is compatible
# with the functions used in the driver or if I should change the functions....
# ------------------------------------------------------------------------------
# Environment
# ------------------------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(tidyr)
devtools::load_all()


# ------------------------------------------------------------------------------
# Data frames
# ------------------------------------------------------------------------------
# Import the HOT observations csv file
path <- list.files("raw-data/observations", "HOT_surface_CO2", full.names = TRUE)
unformated_df <- read.table(path,sep = "\t", skip = 8, header = TRUE)


# Create the month name data frame. This will be used to format the observational data
# frame into an object that is compatible the functions from this project. Because of the
# way that HOTS stores date information the month number in this month data frame must
# contain 2 digits and therefore must be a saved as characters not integers.
month_name_df <- data.frame(month = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11","12"),
                            month_name = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                                           "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))


# ------------------------------------------------------------------------------
# Format the data frame
# ------------------------------------------------------------------------------
unformated_df %>%
  # First remove the NAs at the end of the data frame.
  na.omit %>%
  # Parse out month name, year and date from data frame. Will need to concatenate
  # all of the the time related columns into a time column for the detrending.
  mutate(month_name = substr(date, 4, 6)) %>%
  mutate(year = substr(date, 8, 9)) %>%
  # Parse out year information and add the first two digits
  mutate(year = if_else( year >= 88, paste0("19",year), paste0("20",year))) %>%
  mutate(day = substr(date, 9, 10)) %>%
  left_join(month_name_df, by = "month_name") %>%
  # Combine year, month, and day to create the time column used in detrending.
  mutate(time = paste0(year,month,day)) %>%
  # Mutate the time column to integer form so that use in detrending.
  mutate(time = as.integer(time))->
  time_formated_df

# Now that time is formated properly selec the variables to save.
time_formated_df %>%
  select(time, year, month_name, tos = temp, ph = pHcalc_insitu, co3 = carbonate_insitu, spco2 = pCO2calc_insitu) %>%
  mutate(source = "HOTS") %>%
  # Add month number
  left_join(month_name_df, by = "month_name") %>%
  # Make sure that year and month are integers and not characters
  mutate(month = as.integer(month)) %>%
  mutate(year = as.integer(year)) %>%
  # Format into a tibble
  as_tibble %>%
  gather(variable, value, tos, ph, co3, spco2) %>%
  mutate(value = as.numeric(value)) %>%
  # Remove the NA values, indicated that -999 is the code for NA in the description file.
  filter(value != -999) ->
  HOTS_data

# Add month order as a factor
HOTS_data$month_name <- factor(HOTS_data$month_name, levels = month_name_df$month_name, ordered = TRUE)

save(HOTS_data, file = "data/observations/HOTS.rda")

# ----
# End for now
# Things I need to decide, if I want to add columns to the data set to make it compatible with the other
# functions or not...




