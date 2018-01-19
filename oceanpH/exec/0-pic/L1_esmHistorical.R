# Purpose: This script useses the oceanpH package functions to get the weighted basin mean for
# fgco2
#
# Created by: Dorheim, Kalyn
# Created on: December 13
# Modified:   xxx
#
# Notes: This script will need to be executed on pic with an sbatch and will
# take about an hour to run.

# Environment ------------------------------------------------------------------------------

# Define the base name directory, should be the location of the package.
BASE_NAME <- "/pic/projects/GCAM/Dorheim/OA_variability/oceanpH"

# Define the output directory.
OUTPUT_DIR <- file.path(BASE_NAME, "raw-data", "cmip", "esmrcp85")

# Call the pacakge

source(file.path(BASE_NAME,"exec", "0-pic", "build_package.R"))

# Import the defined basins
basins <- read.csv2(file.path(BASE_NAME,"exec", "0-pic", "defined_basins.csv"), sep = ",")

# fgCO2 ------------------------------------------------------------------------------
# Find the netcdfs to process
cmip.find_me(path = "/pic/projects/GCAM/CMIP5-CHartin",
             variable = "fgco2", domain = "Omon", experiment = c("esmrcp85", "esmHistorical"), ensemble = "r1i1p1") %>%
  cmip.file_info ->
  df

# Use cdo operator to process the netcdfs, format and save
output <- cdo.sellonlat(cdo_operator = "fldmean", data_input = df, defined_basins = basins)

final_output <- mutate(output, year = substr(time, 1, 4), month = substr(time, 5, 6))

write.csv(final_output,file.path(OUTPUT_DIR,"L1_basin_fldmean_fgco2.csv"), row.names = FALSE)

# End ---

