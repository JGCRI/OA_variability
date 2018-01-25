# Purpose: this script contains the code that must be sourced to use the
# oceanpH package on pic.
#

# Start by defining the package directroy on pic
setwd("pic/projects/GCAM/Dorheim/OA_variability/oceanpH")

# Load the required libs
library(devtools)
library(roxygen2)
library(dplyr)


# Build and load the package.
devtools::build()
devtools::load_all()

# End ---
