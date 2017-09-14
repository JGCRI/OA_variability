# ------------------------------------------------------------------------------
# Purpose: Get the the weight monthly mean pH for the rcp85 and histroical
# CMIP5 experiments. For pH, CO3, and surface temp.
# For each variable, write 3 csv files, one of the cmip5 files identified,
# one containing the depth status, and lastly one with results.
#
# Created by: Dorheim, Kalyn
# Created on: August 22 2017
# Modified:   August 31
#
# Notes: takes about 10 min to run on a node on pic, in the future I can probably
# comment out the check for depth, I have alreadt checked for depth.
# ------------------------------------------------------------------------------
# 0. Environment Set up
# ------------------------------------------------------------------------------
# Define the base name directory, should be the location of the package.
BASE_NAME <- "/pic/projects/GCAM/Dorheim/oceanpH"

# Call the pacakge
source(paste0(BASE_NAME,"/exec/processing/exploratory/call_package.R"))

# The CMIP5 file attributes to serach for are defiend in the subsequnet
# sections wherever the cmip.find_me() is used.

# ------------------------------------------------------------------------------
# 1. pH
# ------------------------------------------------------------------------------
# 1. Envrionment
# Define the name of the csv file to save.
files_csv   <- "cmip5_identified_fldmean_monthly_pH.csv"
results_csv <- "fldmean_monthly_pH.csv"
depth_csv   <- "checked_for_depth.csv"

# Define the output directory.
OUTPUT_DIR <- paste0(BASE_NAME, "/exec/output/L1/")

# 2. Functional Code
# Find the cmip 5 files
cmip_list <- cmip.find_me(path = "/pic/projects/GCAM/CMIP5-CLynch/PH_extra", variable = "ph",
                          domain = "Omon", experiment = c("rcp85", "historical"), ensemble = "r1i1p1")

# Check the cmip 5 files to see if there is any dpeth information.
depth <- cmip.check_depth(cmip_list)

# Use the list of cmip5 files to get the cmip5 file information.
cmip_list %>%
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
write.csv(depth, paste0(OUTPUT_DIR, depth_csv), row.names = FALSE)
write.csv(list, paste0(OUTPUT_DIR, files_csv), row.names = FALSE)
write.csv(out, paste0(OUTPUT_DIR, results_csv), row.names = FALSE)

message("pH analysis is complete")

# ------------------------------------------------------------------------------
# 2. CO3
# ------------------------------------------------------------------------------
# 1. Envrionment
# Define the name of the csv file to save.
files_csv   <- "cmip5_identified_fldmean_monthly_co3.csv"
results_csv <- "fldmean_monthly_co3.csv"
depth_csv   <- "checked_for_depth_co3.csv"

# 2. Functional Code
# Find the cmip 5 files
cmip_list <- cmip.find_me(path = "/pic/projects/GCAM/CMIP5-CHartin", variable = "co3",
                          domain = "Omon", experiment = c("rcp85", "historical"), ensemble = "r1i1p1")

# Check the cmip 5 files to see if there is any dpeth information.
depth <- cmip.check_depth(cmip_list)

# Use the list of cmip5 files to get the cmip5 file information.
cmip_list %>%
  cmip.file_info ->
  list

list %>%
  dplyr::group_by(variable, model, experiment, ensemble) %>%
  dplyr::do(results = cdo.processor(., cdo_command = "fldmean")) ->
  out

# Format output into a single data frame.
out <- dplyr::bind_rows(out$results)

# Save the resutls and the files identified as csv files
write.csv(depth, paste0(OUTPUT_DIR, depth_csv), row.names = FALSE)
write.csv(list, paste0(OUTPUT_DIR, files_csv), row.names = FALSE)
write.csv(out, paste0(OUTPUT_DIR, results_csv), row.names = FALSE)

message("co3 analysis is complete")


# ------------------------------------------------------------------------------
# 3. TOS
# ------------------------------------------------------------------------------
# 1. Envrionment
# Define the name of the csv file to save.
files_csv   <- "cmip5_identified_fldmean_monthly_tos.csv"
results_csv <- "fldmean_monthly_tos.csv"
depth_csv   <- "checked_for_depth_tos.csv"

# 2. Functional Code
# Find the cmip 5 files
cmip_list <- cmip.find_me(path = "/pic/projects/GCAM/CMIP5-CHartin", variable = "tos",
                          domain = "Omon", experiment = c("rcp85", "historical"), ensemble = "r1i1p1")

# Check the cmip 5 files to see if there is any dpeth information.
depth <- cmip.check_depth(cmip_list)

# Use the list of cmip5 files to get the cmip5 file information.
cmip_list %>%
  cmip.file_info ->
  list

list %>%
  dplyr::group_by(variable, model, experiment, ensemble) %>%
  dplyr::do(results = cdo.processor(., cdo_command = "fldmean")) ->
  out

# Format output into a single data frame.
out <- dplyr::bind_rows(out$results)

# Save the resutls and the files identified as csv files
write.csv(depth, paste0(OUTPUT_DIR, depth_csv), row.names = FALSE)
write.csv(list, paste0(OUTPUT_DIR, files_csv), row.names = FALSE)
write.csv(out, paste0(OUTPUT_DIR, results_csv), row.names = FALSE)

message("tos analysis is complete")

# ---
# End
message("End")

