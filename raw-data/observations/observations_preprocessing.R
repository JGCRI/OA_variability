# ------------------------------------------------------------------------------
# Purpose: The purpose of this script is to format the observation data for the
# OA variability project
#
# Created by: Dorheim, Kalyn
# Created on: November 10
#
# Notes: Will need to change the INPUT_DIR inorder to process the cmip comparison
# in the CMIP observation basins
# ------------------------------------------------------------------------------
# Environment
# ------------------------------------------------------------------------------
# Load the required packages
library(dplyr)
library(tidyr)
devtools::load_all()

# Define the input and output directories
INPUT_DIR  <- "raw-data/observations/raw"
OUTPUT_DIR <- "raw-data/observations/formatted"


# ------------------------------------------------------------------------------
# Format Obs Data 1
# ------------------------------------------------------------------------------
# Because each of the observational data sets we are working with have different
# formats, sources, and units each data set it was easier to write unique raw
# processing/formating scripts for each data set. These scripts can be found in
# raw-data/observations/raw.
#
# This section sources all of these scripts and saves data frames that have been
# formated to be compatible with all of functions in the R/ of this package.


# Start by locating all of the raw processing scripts
processing_scripts <- list.files("raw-data/observations/raw", ".R", full.names = TRUE)

# Use lapply to source all of the raw processing scripts. Note using lapply to source multiple
# scripts will return an empty list.
# will return an empty list.
lapply(processing_scripts, source)


# ------------------------------------------------------------------------------
# Format Obs Data 2
# ------------------------------------------------------------------------------
# Concatenate the observation data sets together and save in the data dir. The
# .rda object should be able to be processed using the driver(), make sure you
# don't accidently pick up the cmip.rda when you are doing this step or the removed
# observations in this step.

obs_rda <- list.files("raw-data/observations/formatted", ".rda", full.names = TRUE)
obs_rda <- obs_rda[which(!grepl("cmip", obs_rda) == TRUE)]
obs_rda <- obs_rda[which(!grepl("removed_data", obs_rda) == TRUE)]

# Create an empty data frame to save the obs data in
out <- data.frame()
for(i in 1:length(obs_rda)){

  get(load(obs_rda[i])) %>%
    bind_rows(out) ->
    out

}

# Add factor order to month names
# The month name data frame.
month_df <- tibble(month_name = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
                   month_num = 1:12,
                   month_chr = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12"))


out$month_name <- factor(out$month_name, levels = month_df$month_name, ordered = TRUE)

# Check to make sure there are no NAs, if there are NAs throw an error
problem <- any(is.na(out))

# Add final column for cmip function compatibility
out$method <- "obs"


# Create the date column for time series plots
out %>%
  mutate(date = paste0(substr(time, 5, 6), "-01-", substr(time, 1,4))) %>%
  mutate(date = as.Date(date, "%m-%d-%Y")) ->
  out


if(problem){stop("NAs identified in the concatenated data frame")}


# ------------------------------------------------------------------------------
# CMIP observation basins
# ------------------------------------------------------------------------------
# Find the R sciprt that processes the cmip csv output for compaison with the
# the observation data
path       <- list.files("raw-data/observations/cmip", ".R", full.names = TRUE)
INPUT_DIR  <- "raw-data/observations/cmip"
OUTPUT_DIR <- "raw-data/observations/formatted"

# Source the cmip processing code
source(path)

# Load the cmip output
path <- list.files("raw-data/observations/formatted", "cmip_obs_basin_mean.rda", full.names = TRUE)
cmip_data <- get(load(path))

# Since we are only interested in the cmip data that we can compare with the observation data sets
# we will need to subset the cmip basin mean data frame.
#
# First start by creating a data frame from the observations data frame that contains information
# to filter the cmip data by. (This DF will be joined with the cmip_data and then used to subset for
# observations of interest)
out %>%
  select(model, basin, variable, year) %>%
  group_by(model, basin, variable) %>%
  summarise(min_year = min(year), max_year = max(year)) %>%
  ungroup %>%
  select(basin, variable, min_year, max_year) ->
  subset_by

# Subset the df for the basin / variable / time observations that are also in the
# observations data sets.
cmip_data %>%
  left_join(subset_by, by = c("variable", "basin")) %>%
  # First remove the NAs
  na.omit %>%
  # Subset by the years within the observation time sereris
  mutate(KEEP = if_else(min_year <= year & year <= max_year , TRUE, FALSE)) %>%
  filter(KEEP == TRUE) %>%
  select(time, value, variable, units, basin, model, ensemble, experiment, method,
         year, month, month_name) ->
  filtered_cmip_df


# ------------------------------------------------------------------------------
# Concatenate with cmip comparison data
# ------------------------------------------------------------------------------
# Combine the obs and cmip data
out <- bind_rows(filtered_cmip_df, out)

# Add plotting indicaotrs, I think right not the id column will contain the
# cmip models, CESM1-BGC, and then what ever observations it is.

# Save a list of the id names to use when defining the id levels.
observational_data <- filter(out, ensemble == "obs")
observational_list <- unique(observational_data$model)

id_names <- c("cmip models", "CESM1-BGC", observational_list)

out %>%
  mutate(id = if_else(ensemble == "obs", model, id_names[1])) %>%
  mutate(id = if_else(grepl("CESM1", model), id_names[2], id)) %>%
  # Make sure that time is saved as an integer otherwise plotting becomes difficult
  mutate(time = substr(time, 1, 6)) %>%
  mutate(time = as.numeric(time))->
  out_id

out_id$id <- factor(out_id$id, levels = id_names, ordered = TRUE)



# ------------------------------------------------------------------------------
# Save
# ------------------------------------------------------------------------------
save(out_id, file = "data/observations/obs_cmip_comparison_mean.rda")


# ----
# End
message("done with observations pre-processing script")


