# Purpose: This script uses the oceanpH package functions to process the sea surface temperature and
# the dissolved in organic carbon cmip.ncs for the AGU analysis.
#
# Created on: Jan 24
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
OUTPUT_DIR <- file.path(BASE_NAME, "raw-data", "cmip", "AGUoceans_rcp85")

# Call the package
source(file.path(BASE_NAME,"exec", "0.A.pic", "build_package.R"))

# Import the AGU basins.
AGU_basins <- read.csv2(file.path(BASE_NAME,"exec", "0.A.pic", "AGUoceans_defined_basins.csv"), sep = ",")


# tos cmip.nc ----------------------------------------------------------------------------------
# Process the pH and spco2 CMIP5 netcdfs for the rcp85 and the historical experiments. There will
# be a lot of models with tos data so limit to the models we want to used in the OA AGU analysis.

# Find the files to process
path_esm  <- "/pic/projects/GCAM/CMIP5-CHartin"
variables <- "tos"
experiments <- c("rcp85", "historical")
models <- c("bcc-csm1-1", "bcc-csm1-1-m", "BNU-ESM", "CanESM2", "CESM1-BGC", "CMCC-CESM",
            "GFDL-ESM2G", "GFDL-ESM2M", "GISS-E2-H-CC", "GISS-E2-R-CC", "HadGEM2-CC", "HadGEM2-ES",
            "inmcm4", "IPSL-CM5A-MR", "IPSL-CM5B-LR", "MIROC-ESM", "MIROC-ESM-CHEM", "MPI-ESM-LR",
            "MPI-ESM-MR", "MRI-ESM1", "NorESM1-ME", "IPSL-CM5A-LR")

cmip.find_me(path = path_esm, variable = variables, domain = "Omon", experiment = experiments,
             model = models, ensemble = "r1i1p1") %>%
  cmip.file_info ->
  files_to_process

# Process the netcdfs with the cdo operator.
output <- cdo.sellonlat(cdo_operator = "fldmean", data_input = files_to_process, defined_basins = AGU_basins)

# Format final output and save
final_output <- mutate(output, year = substr(time, 1, 4), month = substr(time, 5, 6))
write.csv(final_output,file.path(OUTPUT_DIR,"basinmean_rcp_tos_AGUbasins.csv"), row.names = FALSE)



# dissic cmip.nc ----------------------------------------------------------------------------------
# Process the pH and spco2 CMIP5 netcdfs for the rcp85 and the historical experiments. There will
# not be as many dissc cmip.nc files so do not limit the number of models processed.

# Find the files to process
path_esm  <- "/pic/projects/GCAM/CMIP5-CHartin"
variables <- "dissic"
experiments <- c("rcp85", "historical")


cmip.find_me(path = path_esm, variable = variables, domain = "Omon", experiment = experiments,
             ensemble = "r1i1p1") %>%
  cmip.file_info ->
  files_to_process

# Process the netcdfs with the cdo operator.
output <- cdo.sellonlat(cdo_operator = "fldmean", data_input = files_to_process, defined_basins = AGU_basins)

# Format final output and save
final_output <- mutate(output, year = substr(time, 1, 4), month = substr(time, 5, 6))
write.csv(final_output,file.path(OUTPUT_DIR,"basinmean_rcp_dissic_AGUbasins.csv"), row.names = FALSE)


# End -----

