# ------------------------------------------------------------------------------
# Purpose: This script useses the oceanpH package functions to get the weighted
# mean for ph.
#
# Created by: Dorheim, Kalyn
# Created on: September 18, 2017
# Modified:   xxx
#
# Notes: This script will need to be executed on pic with an sbatch and will
# take about an hour to run.

# Environment ------------------------------------------------------------------------------

# Define the base name directory, should be the location of the package.
BASE_NAME <- "/pic/projects/GCAM/Dorheim/OA_variability/oceanpH"

# Define the output directory.
OUTPUT_DIR <- file.path(BASE_NAME, "raw-data", "cmip", "rcp85")

# Call the pacakge

source(file.path(BASE_NAME,"exec", "0-pic", "build_package.R"))

# Import the defined basins
basins <- read.csv2(file.path(BASE_NAME,"exec", "0-pic", "defined_basins.csv"), sep = ",")

# ------------------------------------------------------------------------------
# PH
# ------------------------------------------------------------------------------
# Find the netcdfs to process
cmip.find_me(path = "/pic/projects/GCAM/CMIP5-CHartin",
             variable = "ph", domain = "Omon", experiment = c("rcp85", "historical"), ensemble = "r1i1p1") %>%
  cmip.file_info %>%
  mutate(variable = "ph") ->
  df


# Use cdo operator to process the netcdfs, format and save
output <- cdo.sellonlat(cdo_operator = "fldmean", data_input = df, defined_basins = basins)

final_output <- mutate(output, year = substr(time, 1, 4), month = substr(time, 5, 6))

write.csv(final_output, paste0(OUTPUT_DIR,"L1_basin_fldmean_ph.csv"), row.names = FALSE)


# ----
# End
message("Complete no errors")


