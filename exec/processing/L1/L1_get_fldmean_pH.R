# ------------------------------------------------------------------------------
# Purpose: Get the the weight monthly mean pH for the rcp85 and histroical
# CMIP5 experiments. Write two csv files, one of all the cmip5 files identified
# on pic and then the second is all the cmip5 files actually processed.
#
# Created by: Dorheim, Kalyn
# Created on: Augut 22 2017
# Modified:   xxx
#
# Notes: takes about 10 min to run on a node on pic
# ------------------------------------------------------------------------------
# 0. Decisions
# ------------------------------------------------------------------------------
# Define the base name directory, should be the location of the package.
BASE_NAME <- "/pic/projects/GCAM/Dorheim/oceanpH"

# The CMIP5 file attributes to serach for are defiend in section 2 where the
# find_me() is used.

# Define the name of the csv file to save.
files_csv   <- "cmip5_identified_fldmean_monthly_pH.csv"
results_csv <- "fldmean_monthly_pH.csv"

# Define the output directory.
OUTPUT_DIR <- paste0(BASE_NAME, "/exec/output/L1/")

# ------------------------------------------------------------------------------
# 1. Envrionement Set Up
# ------------------------------------------------------------------------------
# Call the pacakge
source(paste0(BASE_NAME,"/exec/code/call_package.R"))

# ------------------------------------------------------------------------------
# 2. Functional Code
# ------------------------------------------------------------------------------
# Search for the CMIP5 netcdfs and extract CMIP5 scenario infromation. Then
# concatenate netcdf files and get teh weighted average for pH.

cmip.find_me(path = "/pic/projects/GCAM/CMIP5-CLynch/PH_extra",
                variable = "ph", domain = "Omon", experiment = c("rcp85", "historical"), ensemble = "r1i1p1") %>%
  cmip.file_info %>%
  # Because some of the file names have letters infront of the ph (idk why) replace the
  # variable column with the correct variable name so extracting the varriable does not
  # call an error.
  dplyr::mutate(variable = "ph") ->
  list

list %>%
  dplyr::group_by(variable, model, experiment, ensemble) %>%
  dplyr::do(results = cdo.processor(., cdo_command = "fldmean")) ->
  out

# Format output into a single data frame.
out <- dplyr::bind_rows(out$results)

# Save the resutls and the files identified as csv files
write.csv(list, paste0(OUTPUT_DIR, files_csv), row.names = FALSE)
write.csv(out, paste0(OUTPUT_DIR, results_csv), row.names = FALSE)

# ----
# End


