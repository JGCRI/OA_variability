# ------------------------------------------------------------------------------
# Purpose: This script useses the oceanpH package functions to get the weighted
# mean for ph.
#
# Created by: Dorheim, Kalyn
# Created on: September 18, 2017
# Modified:   xxx
#
# Notes: This script will need to be executed on pic with a batch query and will
# take about an hour to run.
# ------------------------------------------------------------------------------
# Environment
# ------------------------------------------------------------------------------
# Define the base name directory, should be the location of the package.
BASE_NAME <- "/pic/projects/GCAM/Dorheim/OA_variability"

# Define the output directory.
OUTPUT_DIR <- paste0(BASE_NAME, "/inst/extdata/raw-data/")

# Call the pacakge
source(paste0(BASE_NAME,"/exec/processing/call_package.R"))

# Import the defined basins
basins <- read.csv2(paste0(BASE_NAME,"inst/extdata/assumptions/defined_basins.csv"), sep = ",")

# ------------------------------------------------------------------------------
# ph
# ------------------------------------------------------------------------------
# ph netcdfs
cmip.find_me(path = "/pic/projects/GCAM/CMIP5-CHartin",
             variable = "ph", domain = "Omon", experiment = c("rcp85", "historical"), ensemble = "r1i1p1") %>%
  cmip.file_info %>%
  mutate(variable = "ph") ->
  df

output <- cdo.sellonlat(cdo_operator = "fldmean",
                        data_input = df, defined_basins = basins, intermediate_output = OUTPUT_DIR)

# Separate time into year and month.
output %>%
  mutate(year = substr(time, 1, 4), month = substr(time, 5, 6)) %>%
  # Subset for years less than 2100 as Corinne requested.
  filter(year <= 2100) ->
  final_output

write.csv(final_output, paste0(OUTPUT_DIR,"L1_basin_fldmean_ph.csv"), row.names = FALSE)

message("complete")
