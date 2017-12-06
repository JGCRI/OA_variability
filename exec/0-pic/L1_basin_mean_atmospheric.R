# Purpose: This script useses the oceanpH package functions to get the weighted
# mean for tas and atm CO2
#
# Created by: Dorheim, Kalyn
# Created on: September 18, 2017
# Modified:   xxx
#
# Notes: This script will need to be executed on pic with an sbatch and will
# take about an hour to run.

# Environment ------------------------------------------------------------------------------

# Define the base name directory, should be the location of the package.
BASE_NAME <- "/pic/projects/GCAM/Dorheim/OA_variability"

# Define the output directory.
OUTPUT_DIR <- paste0(BASE_NAME, "/raw-data/")

# Call the pacakge

source(paste0(BASE_NAME,"/exec/0-pic/call_package.R"))

# Import the defined basins
basins <- read.csv2(paste0(BASE_NAME,"/exec/0-pic/defined_basins.csv"), sep = ",")


# tas ------------------------------------------------------------------------------
# Find the netcdfs to process
cmip.find_me(path = "/pic/projects/GCAM/CMIP5-CHartin",
             variable = "tas", domain = "Amon", experiment = c("rcp85", "historical"), ensemble = "r1i1p1") %>%
  cmip.file_info %>%
  mutate(variable = "tas") ->
  df


# Use cdo operator to process the netcdfs, format and save
output <- cdo.sellonlat(cdo_operator = "fldmean", data_input = df, defined_basins = basins)

final_output <- mutate(output, year = substr(time, 1, 4), month = substr(time, 5, 6))


write.csv(final_output, paste0(OUTPUT_DIR,"L1_basin_fldmean_tas.csv"), row.names = FALSE)



# End -----
