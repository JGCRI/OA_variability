# Purpose: This script creates the figures and kables from the driver output for the cmip esmrcp85
# experiments.


# Environment ------------------------------------------------------------------------------------
library(dplyr)
library(tidyr)
library(ggplot2)
devtools::load_all()

# Directories
BASE <- getwd()
INPUT_DIR  <- file.path(BASE, "output", "cmip", "esmrcp85")
OUTPUT_DIR <- file.path(BASE, "figs", "cmip", "esmrcp85")


# Processed Models ------------------------------------------------------------------------------------
models_to_remove_path <- list.files(file.path(BASE, "raw-data", "assumptions"), "models_to_remove.csv", full.names = T)
models_removed_path   <- list.files(file.path(BASE, "inst", "extdata", "cmip", "esmrcp85"), "removed_data.rda", full.names = T)
raw_data_path <- list.files(file.path(BASE, "inst", "extdata", "cmip", "esmrcp85"), "basin_mean.rda", full.names = T)

# There is some problem going on here
# x <- generate.processed_models_kables(models_to_remove_path , models_removed_path ,raw_data_path , OTUPUT_DIR)


# Models Indiviudally ---------------------------------------------------------------------------------

generate.figs_models_individually(INPUT_DIR, OUTPUT_DIR, "individual_models.rda")

# Multiple Models -------------------------------------------------------------------------------------

generate.figs_multiple_models(INPUT_DIR, OUTPUT_DIR, "comparing_multiple_models.rda")


# Multiple Models -------------------------------------------------------------------------------------

amplitude_path <- list.files(INPUT_DIR, "amplitude.rda", full.names = T)
generate.robustness_index(amplitude_path, OUTPUT_DIR, "sameness_robustness.rda")

# End ----
