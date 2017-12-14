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
BASE_NAME <- "/pic/projects/GCAM/Dorheim/OA_variability/oceanpH"

# Define the output directory.
OUTPUT_DIR <- file.path(BASE_NAME, "raw-data", "cmip", "esmrcp85")

# Call the pacakge

source(file.path(BASE_NAME,"exec", "0-pic", "build_package.R"))

# Import the defined basins
basins <- read.csv2(file.path(BASE_NAME,"exec", "0-pic", "defined_basins.csv"), sep = ",")

# tas ------------------------------------------------------------------------------
# Find the netcdfs to process
cmip.find_me(path = "/pic/projects/GCAM/CMIP5-CHartin",
             variable = "tas", domain = "Amon", experiment = c("esmrcp85", "esmHistorical"), ensemble = "r1i1p1") %>%
  cmip.file_info ->
  df

# Use cdo operator to process the netcdfs, format and save
output <- cdo.sellonlat(cdo_operator = "fldmean", data_input = df, defined_basins = basins)

final_output <- mutate(output, year = substr(time, 1, 4), month = substr(time, 5, 6))


write.csv(final_output,file.path(OUTPUT_DIR,"L1_basin_fldmean_tas.csv"), row.names = FALSE)


# End -----
