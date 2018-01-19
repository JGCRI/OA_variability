# Purpose: This script uses the oceanpH package functions to process the esmrcp_spco2 CMIP5
# netcdfs for the OA variability analysis.
#
# Created by: Dorheim, Kalyn
# Created on: Jan 19
# Modified:   xxx
#
# Notes: This script will need to be executed on pic with an sbatch and will
# take about an hour to run.

# Environment ------------------------------------------------------------------------------
# This script requires functions from the oceanpH package which is still under development,
# loading the package from the library may change in the future after package development
# is complete. For right now I load the library by sourcing a script that uses devtools
# build and load.

# Define script directories.
BASE_NAME  <- "/pic/projects/GCAM/Dorheim/OA_variability/oceanpH"
OUTPUT_DIR <- file.path(BASE_NAME, "raw-data", "cmip", "esmrcp85_AGU")

# Call the package
source(file.path(BASE_NAME,"exec", "0-pic", "build_package.R"))

# Import the AGU basins.
AGU_basins <- read.csv2(file.path(BASE_NAME,"exec", "0-pic", "AGUoceans_defined_basins.csv"), sep = ",")


# pH and spco2 ------------------------------------------------------------------------------
# Process the pH and spco2 CMIP5 netcdfs for the esmHisotircal and esmrcp85 experiments. Now the
# esmrcp85 spco2 netcdfs are stored else where on pic and will have to be processed separately.

# Find the files to process
path_esm  <- "/pic/projects/GCAM/CMIP5-CHartin"
variables <- c("spco2", "ph")
experiments <- c("esmrcp85", "esmHistorical")

cmip.find_me(path = path_esm, variable = variables, domain = "Omon", experiment = experiments, ensemble = "r1i1p1") %>%
  cmip.file_info %>%
  mutate(variable = ifelse(grepl("ph", variable), "ph", variable))->
  files_to_process

# Process the netcdfs with the cdo operator.
output <- cdo.sellonlat(cdo_operator = "fldmean", data_input = files_to_process, defined_basins = AGU_basins)

# Format final output and save
final_output <- mutate(output, year = substr(time, 1, 4), month = substr(time, 5, 6))
write.csv(final_output,file.path(OUTPUT_DIR,"basinmean_esmRCP_esmHist_spco2_ph.csv"), row.names = FALSE)


# esmrcp85 spco2 ------------------------------------------------------------------------------
# Process the spco2 CMIP5 netcdfs for the esmrcp85 experiments. N

# Find the files to process
path_esmrcp_spco2 <- "/pic/projects/GCAM/Dorheim/WGET/ncdf"

cmip.find_me(path = path_esmrcp_spco2, variable = "spco2", domain = "Omon", experiment = "esmrcp85", ensemble = "r1i1p1") %>%
  cmip.file_info ->
  files_to_process

# Process the netcdfs with the cdo operator.
output <- cdo.sellonlat(cdo_operator = "fldmean", data_input = files_to_process, defined_basins = AGU_basins)

# Format final output and save
final_output <- mutate(output, year = substr(time, 1, 4), month = substr(time, 5, 6))
write.csv(final_output,file.path(OUTPUT_DIR,"basinmean_rcp85_spco2.csv"), row.names = FALSE)

# End -----

