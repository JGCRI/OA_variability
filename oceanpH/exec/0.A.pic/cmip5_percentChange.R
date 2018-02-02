# Purpose: This script creates the percent change NetCDFS is the first attemp to run on pic

# Environment
# This script requires functions from the oceanpH package which is still under development,
# loading the package from the library may change in the future after package development
# is complete. For right now I load the library by sourcing a script that uses devtools
# build and load.

# Define script directories.
BASE_NAME  <- "/pic/projects/GCAM/Dorheim/OA_variability/oceanpH"
OUTPUT_DIR <- file.path(BASE_NAME, "raw-data", "cmip", "percentChange_NetCDFs")

# Call the package
source(file.path(BASE_NAME,"exec", "0.A.pic", "build_package.R"))

# These years need to change to match the AGU data AGH panicking that the stats are all based off of thw wrong years...
# what is going on...
experiment_years <- tibble::tibble(experiment = c("historical", "rcp85"),
                                   start_year = c(1861, 2070),
                                   end_year = c(1891, 2100))

cmip.find_me(path = "/pic/projects/GCAM/CMIP5-CHartin", domain = "Omon",
             variable = c("ph"), experiment = c("historical", "rcp85"), ensemble = "r1i1p1") %>%
  cmip.file_info ->
  to_process

output <- batch_percentChange(to_process, "/share/apps/netcdf/4.3.2/gcc/4.4.7/bin/cdo", experiment_years, "/people/dorh012", OUTPUT_DIR)

save(to_process, file = file.path(OUTPUT_DIR, "to_process.rda"))
save(output, file = file.path(OUTPUT_DIR, "safety_output.rda"))
