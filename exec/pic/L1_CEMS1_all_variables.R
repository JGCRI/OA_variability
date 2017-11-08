# ------------------------------------------------------------------------------
# Purpose: This script useses the oceanpH package functions to get the weighted
# mean pH, co3, tos, and pCO2 for different ocean basins. for CESM1 only.
#
# Created by: Dorheim, Kalyn
# Created on: September 18, 2017
# Modified:   October 19, 2017
#
# Notes:
# ------------------------------------------------------------------------------
# Environment Set Up
# ------------------------------------------------------------------------------
# Define the base name directory, should be the location of the package.
BASE_NAME <- "/pic/projects/GCAM/Dorheim/OA_variability"

# Define the output directory.
OUTPUT_DIR <- paste0(BASE_NAME, "/raw-data/")

# Call the pacakge
source(paste0(BASE_NAME,"/exec/processing/call_package.R"))

# Import the defined basins
basins <- read.csv2(paste0(BASE_NAME,"/exec/processing/L0/defined_basins.csv"), sep = ",")


# ------------------------------------------------------------------------------
# Find CEMS1 netcds
# ------------------------------------------------------------------------------
# ph netcdfs sometimes have weird variable names in the file so find on own.
cmip.find_me(path = "/pic/projects/GCAM/CMIP5-CHartin", variable = "ph",
             domain = "Omon", experiment = c("rcp85", "historical"), ensemble = "r1i1p1") %>%
  cmip.file_info %>%
  mutate(variable = "ph") %>%
  filter(grepl(x = model, "CESM1")) ->
  ph_df

# Find the tos, spco2 and co3 files.
cmip.find_me(path = "/pic/projects/GCAM/CMIP5-CHartin", variable = c("tos", "spco2", "co3"),
             domain = "Omon", experiment = c("rcp85", "historical"), ensemble = "r1i1p1") %>%
  cmip.file_info %>%
  filter(grepl(x = model, "CESM1")) ->
  tos_spco2_co3_df

# Combine all of the netcdfs to process into a single dataframe.
to_process <- bind_rows(ph_df, tos_spco2_co3_df)

output <- cdo.sellonlat(cdo_operator = "fldmean", data_input = to_process, defined_basins = basins)

# Separate time into year and month.
output %>%
  mutate(year = substr(time, 1, 4), month = substr(time, 5, 6)) ->
  final_output

write.csv(final_output, paste0(OUTPUT_DIR,"L1_CEMS1_all_variables.csv"), row.names = FALSE)
# ----
# End









