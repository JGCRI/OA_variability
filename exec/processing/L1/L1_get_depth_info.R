# ------------------------------------------------------------------------------
# Purpose: This script searches for the CMIP5 netcdf files, checks to see if
# the files contains any information about ocean depth. File information and
# dpeth status is collected into a single data frame then exported as a .csv
# Output from this script will be written to the results folder as
# ------------------------------------------------------------------------------
# 0. Decisions to make.
# ------------------------------------------------------------------------------
# Define the directory, it should the directory of the pacakage.
DIR <- "/pic/projects/GCAM/Dorheim/oceanpH/"

# Define the direcotry to serach for the CMIP5 files.
CMIP5_DIR <- "/pic/projects/GCAM/CMIP5-CLynch/PH_extra"

# Select the CMIP5 files to search for
VARIABLE   <- "ph"
DOMAIN     <- "Omon"
EXPERIMENT <- c("rcp85", "historical")
ENSEMBLE   <- "r1i1p1"

# Define the output file name (will be saved in DIR/results/)
file_name <- "CMIP5_depth.csv"

# ------------------------------------------------------------------------------
# 1. Set up the environment
# ------------------------------------------------------------------------------
# Load libraries
library(dplyr)

# Source the script that builds and loads the package.
source(paste0(DIR,"exec/code/call_package.R"))

# ------------------------------------------------------------------------------
# 2. Functional Code
# ------------------------------------------------------------------------------
cmip.find_me(path = paste0(CMIP5_DIR), variable = VARIABLE,
        domain = DOMAIN, experiment = EXPERIMENT, ensemble = ENSEMBLE) %>%
  cmip.check_depth %>%
  write.csv(paste0(DIR,"exec/output/L1/",file_name), row.names = FALSE)
