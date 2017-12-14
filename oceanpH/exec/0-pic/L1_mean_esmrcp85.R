# Purpose: This script useses the oceanpH package functions to get the weighted
# mean for tas, CO2, fgco2, ph, tos, ans spCO2 from the emission driven cmip runs. After a dicussion with
# Steve I became concerened that we were looking at the wrong CMIP experiment, I need to
# do more reading on the different experiments but he suggested that I look at the emissions
# driven instead of the radiative forcing driven ones...
#
# Created by: Dorheim, Kalyn
# Created on: December 13
# Modified:   xxx
#
# Notes: This script will need to be executed on pic with an sbatch and will
# take about an hour to run.

# Environment ------------------------------------------------------------------------------

# Define the base name directory, should be the location of the package.
BASE_NAME <- "/pic/projects/GCAM/Dorheim/OA_variability"

# Define the output directory.
OUTPUT_DIR <- paste0(BASE_NAME, "/raw-data/esmrcp85")

# Call the pacakge

source(paste0(BASE_NAME,"/exec/0-pic/call_package.R"))

# Import the defined basins
basins <- read.csv2(paste0(BASE_NAME,"/exec/0-pic/defined_basins.csv"), sep = ",")


# tas ------------------------------------------------------------------------------
# Find the netcdfs to process
cmip.find_me(path = "/pic/projects/GCAM/CMIP5-CHartin",
             variable = "tas", domain = "Amon", experiment = c("esmrcp85", "historical"), ensemble = "r1i1p1") %>%
  cmip.file_info %>%
  mutate(variable = "tas") ->
  df


# Use cdo operator to process the netcdfs, format and save
output <- cdo.sellonlat(cdo_operator = "fldmean", data_input = df, defined_basins = basins)

final_output <- mutate(output, year = substr(time, 1, 4), month = substr(time, 5, 6))


write.csv(final_output,file.path(OUTPUT_DIR,"L1_basin_fldmean_tas.csv"), row.names = FALSE)


# co2 ------------------------------------------------------------------------------
# Find the netcdfs to process
cmip.find_me(path = "/pic/projects/GCAM/CMIP5-CHartin",
             variable = "co2", domain = "Amon", experiment = c("esmrcp85", "historical"), ensemble = "r1i1p1") %>%
  cmip.file_info %>%
  mutate(variable = "co2") ->
  df


# Use cdo operator to process the netcdfs, format and save
output <- cdo.sellonlat(cdo_operator = "fldmean", data_input = df, defined_basins = basins)

final_output <- mutate(output, year = substr(time, 1, 4), month = substr(time, 5, 6))


write.csv(final_output,file.path(OUTPUT_DIR,"L1_basin_fldmean_co2.csv"), row.names = FALSE)

# fgCO2 ------------------------------------------------------------------------------
# Find the netcdfs to process
cmip.find_me(path = "/pic/projects/GCAM/CMIP5-CHartin",
             variable = "fgco2", domain = "Omon", experiment = c("esmrcp85", "historical"), ensemble = "r1i1p1") %>%
  cmip.file_info %>%
  mutate(variable = "fgco2") ->
  df


# Use cdo operator to process the netcdfs, format and save
output <- cdo.sellonlat(cdo_operator = "fldmean", data_input = df, defined_basins = basins)

final_output <- mutate(output, year = substr(time, 1, 4), month = substr(time, 5, 6))


write.csv(final_output,file.path(OUTPUT_DIR,"L1_basin_fldmean_fgco2.csv"), row.names = FALSE)

# ph ------------------------------------------------------------------------------
# Find the netcdfs to process
cmip.find_me(path = "/pic/projects/GCAM/CMIP5-CHartin",
             variable = "ph", domain = "Omon", experiment = c("esmrcp85", "historical"), ensemble = "r1i1p1") %>%
  cmip.file_info %>%
  mutate(variable = "ph") ->
  df


# Use cdo operator to process the netcdfs, format and save
output <- cdo.sellonlat(cdo_operator = "fldmean", data_input = df, defined_basins = basins)

final_output <- mutate(output, year = substr(time, 1, 4), month = substr(time, 5, 6))


write.csv(final_output,file.path(OUTPUT_DIR,"L1_basin_fldmean_ph.csv"), row.names = FALSE)

# tos ------------------------------------------------------------------------------
# Find the netcdfs to process
cmip.find_me(path = "/pic/projects/GCAM/CMIP5-CHartin",
             variable = "tos", domain = "Omon", experiment = c("esmrcp85", "historical"), ensemble = "r1i1p1") %>%
  cmip.file_info %>%
  mutate(variable = "tos") ->
  df


# Use cdo operator to process the netcdfs, format and save
output <- cdo.sellonlat(cdo_operator = "fldmean", data_input = df, defined_basins = basins)

final_output <- mutate(output, year = substr(time, 1, 4), month = substr(time, 5, 6))


write.csv(final_output,file.path(OUTPUT_DIR,"L1_basin_fldmean_tos.csv"), row.names = FALSE)

# co3 ------------------------------------------------------------------------------
# Find the netcdfs to process
cmip.find_me(path = "/pic/projects/GCAM/CMIP5-CHartin",
             variable = "co3", domain = "Omon", experiment = c("esmrcp85", "historical"), ensemble = "r1i1p1") %>%
  cmip.file_info %>%
  mutate(variable = "co3") ->
  df


# Use cdo operator to process the netcdfs, format and save
output <- cdo.sellonlat(cdo_operator = "fldmean", data_input = df, defined_basins = basins)

final_output <- mutate(output, year = substr(time, 1, 4), month = substr(time, 5, 6))


write.csv(final_output,file.path(OUTPUT_DIR,"L1_basin_fldmean_co3.csv"), row.names = FALSE)




# End -----
