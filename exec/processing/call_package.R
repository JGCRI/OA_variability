# ------------------------------------------------------------------------------
# Purpose: this script contains the code that must be sourced to use the
# oceanpH package on pic.
# ------------------------------------------------------------------------------
# 0. Decisions to make.
# ------------------------------------------------------------------------------
# The location of the package folder on pic
PCKG_DIR <- BASE_NAME

# ------------------------------------------------------------------------------
# 1. Set up the environment.
library(devtools)
library(roxygen2)
library(dplyr)

# Set the working directory to the package directory.
PCKG_DIR <- "/pic/projects/GCAM/Dorheim/OA_variability"
setwd(paste0(PCKG_DIR))

# Build and load the package.
devtools::build()
devtools::load_all()

# -----
# End
