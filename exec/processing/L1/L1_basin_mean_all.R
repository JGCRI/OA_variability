# ------------------------------------------------------------------------------
# Purpose: This script useses the oceanpH package functions to get the weighted
# mean pH, co3, tos, and pCO2 for different ocean basins.
#
# Created by: Dorheim, Kalyn
# Created on: September 18, 2017
# Modified:   xxx
#
# Notes: This script will need to be executed on pic with a batch query and will
# most likely take several hours to run.
# ------------------------------------------------------------------------------
# 0. Decisions
# ------------------------------------------------------------------------------
# Define the base name directory, should be the location of the package.
BASE_NAME <- "/pic/projects/GCAM/Dorheim/OA_variability"

# The CMIP5 file attributes to serach for are defiend in section 2 where the
# find_me() is used.

# Define the name of the csv file to save.
results.csv <- "L1_basin_mean_all.csv"

# Script name
script_name <- "L1_basin_mean_all.R"

# Define the output directory.
OUTPUT_DIR <- paste0(BASE_NAME, "/exec/output/L1/")

# ------------------------------------------------------------------------------
# 1. Environment Set Up
# ------------------------------------------------------------------------------
# Call the pacakge
source(paste0(BASE_NAME,"/exec/processing/call_package.R"))

# Import the defined basins
basins <- read.csv2(paste0(BASE_NAME,"/exec/processing/L0/defined_basins.csv"), sep = ",")

# ------------------------------------------------------------------------------
# 2. Get Ocean Cella Area Netcdfs
# ------------------------------------------------------------------------------
# Find all of the cell area for the oceans. These netcdfs will be used as area
# weights in the cdo that calculates the basin statistics.
cmip.find_me(path = "/pic/projects/GCAM/CMIP5-CHartin/oceanfrac", variable = "areacello", domain = "fx") %>%
  # Get the cmip 5 file informaiton.
  cmip.file_info ->
  area_df

# ------------------------------------------------------------------------------
# 3. Get Data Netcdfs
# ------------------------------------------------------------------------------
# Find the cmip5 netcdfs for pH, CO3, ocean surface temperature, and pCO2. Because
# of issues with the pH netcdf file naming system pH will need to be serach for on
# its own.
#
# pH netcdfs
cmip.find_me(path = "/pic/projects/GCAM/CMIP5-CLynch/PH_extra",
             variable = "ph", domain = "Omon", experiment = c("rcp85", "historical"), ensemble = "r1i1p1") %>%
  cmip.file_info %>%
  # Because some of the file names have letters infront of the ph (idk why) replace the
  # variable column with the correct variable name so extracting the varriable does not
  # call an error.
  dplyr::mutate(variable = "ph") ->
  ph_df


# CO3 , tos, and pCO2 netcdfs
cmip.find_me(path = "/pic/projects/GCAM/CMIP5-CHartin",
             variable = c("co3", "tos", "spco2"), domain = "Omon", experiment = c("rcp85", "historical"), ensemble = "r1i1p1") %>%
  cmip.file_info ->
  nonph_df

# Concatenate the dataframe containing data netcdf information.
data_df <- bind_rows(ph_df,  nonph_df)

# Get the area data frames 
area_df <- cmip.find_me(path = "/pic/projects/GCAM/CMIP5-CHartin", variable = "areacello", domain = "fx") %>% cmip.file_info

# ------------------------------------------------------------------------------
# 4. Execute CDO
# ------------------------------------------------------------------------------
output <- cdo.sellonlat(cdo_operator = "-fldmean", area_input = area_df,
                        data_input = data_df, defined_basins = basins, intermediate_output = OUTPUT_DIR)

# Separate time into year and month.
output %>%
  mutate(year = substr(time, 1, 4), month = substr(time, 5, 6)) %>%
# Subset for years less than 2100 as Corinne requested.
  filter(year <= 2100) ->
  final_output

# ------------------------------------------------------------------------------
# 5. Save Output
# ------------------------------------------------------------------------------
# Save the processed cdo output as a csv.
write.csv(final_output, paste0(OUTPUT_DIR,results.csv), row.names = FALSE)


# Final messages
messge("Output from ", script_name, " is saved in at ", paste0(OUTPUT_DIR,results.csv))
messge("End of ", script_name)
# ----
# End









