# ------------------------------------------------------------------------------
# Purpose: The purpose of this script is to format the observation data for the
# OA variability project
#
# Created by: Dorheim, Kalyn
# Created on: November 10
#
# Notes: Will need to change the INPUT_DIR inorder to process the cmip comparison
# in the Concatenate with cmip comparison data section
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

if(problem){stop("NAs identified in the concatenated data frame")}

# ------------------------------------------------------------------------------
# Concatenate with cmip comparison data
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

# Combine the obs and cmip data
out <- bind_rows(cmip_data, out)


# ------------------------------------------------------------------------------
# Save
# ------------------------------------------------------------------------------
save(out, file = "data/observations/obs_cmip_comparison_mean.rda")


# ----
# End
message("done with observations pre-processing script")


