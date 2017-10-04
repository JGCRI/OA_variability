# ------------------------------------------------------------------------------
# Purpose: This script useses the oceanpH package functions to get the weighted
# mean pH, co3, tos, and pCO2 for different ocean basins.
#
# Created by: Dorheim, Kalyn
# Created on: September 18, 2017
# Modified:   xxx
#
# Notes: This script will not currently run due to some problem with the co3
# netcdfs.
# ------------------------------------------------------------------------------
# 0. Decisions
# ------------------------------------------------------------------------------
# Define the base name directory, should be the location of the package.
BASE_NAME <- "/pic/projects/GCAM/Dorheim/OA_variability"

# The CMIP5 file attributes to serach for are defiend in section 2 where the
# find_me() is used.

# Script name
script_name <- "L1_basin_mean_all.R"

# Define the output directory.
OUTPUT_DIR <- paste0(BASE_NAME, "/exec/processing/L1/output/")

# ------------------------------------------------------------------------------
# 1. Environment Set Up
# ------------------------------------------------------------------------------
# Call the pacakge
source(paste0(BASE_NAME,"/exec/processing/call_package.R"))

# Import the defined basins
basins <- read.csv2(paste0(BASE_NAME,"/exec/processing/L0/defined_basins.csv"), sep = ",")


# ------------------------------------------------------------------------------
# 2. pH
# ------------------------------------------------------------------------------
# Find the cmip5 netcdfs for pH, CO3, ocean surface temperature, and pCO2. Because
# of issues with the pH netcdf file naming system pH will need to be serach for on
# its own.
#
# pH netcdfs
cmip.find_me(path = "/pic/projects/GCAM/CMIP5-CHartin",
             variable = "ph", domain = "Omon", experiment = c("rcp85", "historical"), ensemble = "r1i1p1") %>%
  cmip.file_info %>%
  # Because some of the file names have letters infront of the ph (idk why) replace the
  # variable column with the correct variable name so extracting the varriable does not
  # call an error.
  dplyr::mutate(variable = "ph") ->
  ph_df

output <- cdo.sellonlat(cdo_operator = "fldmean",
                        data_input = ph_df, defined_basins = basins, intermediate_output = OUTPUT_DIR)

# Separate time into year and month.
output %>%
  mutate(year = substr(time, 1, 4), month = substr(time, 5, 6)) %>%
  # Subset for years less than 2100 as Corinne requested.
  filter(year <= 2100) ->
  final_output

write.csv(final_output, paste0(OUTPUT_DIR,"L1_basin_fldmean_ph.csv"), row.names = FALSE)


# ------------------------------------------------------------------------------
# 3. tos
# ------------------------------------------------------------------------------
# tos netcdfs
cmip.find_me(path = "/pic/projects/GCAM/CMIP5-CHartin",
             variable = "tos", domain = "Omon", experiment = c("rcp85", "historical"), ensemble = "r1i1p1") %>%
  cmip.file_info  %>%
  mutate(variable = "tos") ->
  tos_df

output <- cdo.sellonlat(cdo_operator = "fldmean",
                        data_input = tos_df, defined_basins = basins, intermediate_output = OUTPUT_DIR)

# Separate time into year and month.
output %>%
  mutate(year = substr(time, 1, 4), month = substr(time, 5, 6)) %>%
  # Subset for years less than 2100 as Corinne requested.
  filter(year <= 2100) ->
  final_output

write.csv(final_output, paste0(OUTPUT_DIR,"L1_basin_fldmean_tos.csv"), row.names = FALSE)


# ------------------------------------------------------------------------------
# 4. spco2
# ------------------------------------------------------------------------------
# spco2 netcdfs
cmip.find_me(path = "/pic/projects/GCAM/CMIP5-CHartin",
             variable = "spco2", domain = "Omon", experiment = c("rcp85", "historical"), ensemble = "r1i1p1") %>%
  cmip.file_info %>%
  mutate(variable = "spco2") ->
  spco2_df

output <- cdo.sellonlat(cdo_operator = "fldmean",
                        data_input = spco2, defined_basins = basins, intermediate_output = OUTPUT_DIR)

# Separate time into year and month.
output %>%
  mutate(year = substr(time, 1, 4), month = substr(time, 5, 6)) %>%
  # Subset for years less than 2100 as Corinne requested.
  filter(year <= 2100) ->
  final_output

write.csv(final_output, paste0(OUTPUT_DIR,"L1_basin_fldmean_spco2.csv"), row.names = FALSE)


# ------------------------------------------------------------------------------
# 5. co3
# ------------------------------------------------------------------------------
# co3 netcdfs
cmip.find_me(path = "/pic/projects/GCAM/CMIP5-CHartin",
             variable = "co3", domain = "Omon", experiment = c("rcp85", "historical"), ensemble = "r1i1p1") %>%
  cmip.file_info  %>%
  mutate(variable = "co3") ->
  co3_df

output <- cdo.sellonlat(cdo_operator = "fldmean",
                        data_input = co3_df, defined_basins = basins, intermediate_output = OUTPUT_DIR)

# Separate time into year and month.
output %>%
  mutate(year = substr(time, 1, 4), month = substr(time, 5, 6)) %>%
  # Subset for years less than 2100 as Corinne requested.
  filter(year <= 2100) ->
  final_output

write.csv(final_output, paste0(OUTPUT_DIR,"L1_basin_fldmean_co3.csv"), row.names = FALSE)


# Final messages
messge("Output from ", script_name, " is saved in at ", paste0(OUTPUT_DIR, results.csv))
messge("End of ", script_name)
# ----
# End









