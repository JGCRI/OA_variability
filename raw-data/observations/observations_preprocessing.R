# ------------------------------------------------------------------------------
# Purpose: The purpose of this script is to format the observation data for the
# OA variability project
#
# Created by: Dorheim, Kalyn
# Created on: November 10
#
# Notes:
# ------------------------------------------------------------------------------
# Environment
# ------------------------------------------------------------------------------
# Load the required packages
library(dplyr)
library(tidyr)
devtools::load_all()

# Define the input and output directories
INPUT_DIR  <- "raw-data/observations/raw"
OUTPUT_DIR <- "raw-data/observations/formated"


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
# .rda object should be able to be processed using the driver()

obs_rda <- list.files("raw-data/observations/formated", ".rda", full.names = TRUE)

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

if(problem){stop("NAs identified in the concatenated data frame")}


# ------------------------------------------------------------------------------
# Save
# ------------------------------------------------------------------------------
save(out, file = "data/observations/observation_mean.rda")

# ----
# End
message("done with observations pre-processing script")


