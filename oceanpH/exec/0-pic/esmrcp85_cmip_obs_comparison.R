# ------------------------------------------------------------------------------
# Purpose: This script useses the oceanpH package functions to get the weighted
# mean for tos in the regions that are comparable to the observation data.
#
# Created by: Dorheim, Kalyn
# Created on: September 18, 2017
# Modified:   December 15, 2017
#
# Notes: This script will need to be executed on pic with an sbatch and will
# take about an hour to run.
# ------------------------------------------------------------------------------
# Environment
# ------------------------------------------------------------------------------
# Define the base name directory, should be the location of the package.
BASE_NAME <- "/pic/projects/GCAM/Dorheim/OA_variability"

# Define the output directory.
OUTPUT_DIR <- paste0(BASE_NAME, "/raw-data/observations/cmip/esmrcp85")

# Call the pacakge
source(file.path(BASE_NAME,"exec", "0-pic", "build_package.R"))

# Import the defined basins
basins <- read.csv2(paste0(BASE_NAME,"/raw-data/assumptions/observation_boundaries.csv"), sep = ",")

# ------------------------------------------------------------------------------
# co3
# ------------------------------------------------------------------------------
# Find the netcdfs to process
cmip.find_me(path = "/pic/projects/GCAM/CMIP5-CHartin",
             variable = "co3", domain = "Omon", experiment = c("esmrcp85", "esmHistorical"), ensemble = "r1i1p1") %>%
  cmip.file_info %>%
  mutate(variable = "co3") ->
  df


# Use cdo operator to process the netcdfs, format and save
output <- cdo.sellonlat(cdo_operator = "fldmean", data_input = df, defined_basins = basins)

final_output <- mutate(output, year = substr(time, 1, 4), month = substr(time, 5, 6))

write.csv(final_output, paste0(OUTPUT_DIR,"L1_basin_fldmean_co3.csv"), row.names = FALSE)


# ------------------------------------------------------------------------------
# pH
# ------------------------------------------------------------------------------
# Find the netcdfs to process
cmip.find_me(path = "/pic/projects/GCAM/CMIP5-CHartin",
             variable = "ph", domain = "Omon", experiment = c("esmrcp85", "esmHistorical"), ensemble = "r1i1p1") %>%
  cmip.file_info %>%
  mutate(variable = "ph") ->
  df


# Use cdo operator to process the netcdfs, format and save
output <- cdo.sellonlat(cdo_operator = "fldmean", data_input = df, defined_basins = basins)

final_output <- mutate(output, year = substr(time, 1, 4), month = substr(time, 5, 6))

write.csv(final_output, paste0(OUTPUT_DIR,"L1_basin_fldmean_ph.csv"), row.names = FALSE)


# ------------------------------------------------------------------------------
# spco2
# ------------------------------------------------------------------------------
# Find the netcdfs to process
cmip.find_me(path = "/pic/projects/GCAM/CMIP5-CHartin",
             variable = "spco2", domain = "Omon", experiment = c("esmrcp85", "esmHistorical"), ensemble = "r1i1p1") %>%
  cmip.file_info %>%
  mutate(variable = "spco2") ->
  df


# Use cdo operator to process the netcdfs, format and save
output <- cdo.sellonlat(cdo_operator = "fldmean", data_input = df, defined_basins = basins)

final_output <- mutate(output, year = substr(time, 1, 4), month = substr(time, 5, 6))

write.csv(final_output, paste0(OUTPUT_DIR,"L1_basin_fldmean_spco2.csv"), row.names = FALSE)



# ------------------------------------------------------------------------------
# TOS
# ------------------------------------------------------------------------------
# Find the netcdfs to process
cmip.find_me(path = "/pic/projects/GCAM/CMIP5-CHartin",
             variable = "tos", domain = "Omon", experiment = c("esmrcp85", "esmHistorical"), ensemble = "r1i1p1") %>%
  cmip.file_info %>%
  mutate(variable = "tos") ->
  df


# Use cdo operator to process the netcdfs, format and save
output <- cdo.sellonlat(cdo_operator = "fldmean", data_input = df, defined_basins = basins)

final_output <- mutate(output, year = substr(time, 1, 4), month = substr(time, 5, 6))

write.csv(final_output, paste0(OUTPUT_DIR,"L1_basin_fldmean_tos.csv"), row.names = FALSE)


# ----
# End
message("Complete no errors")


