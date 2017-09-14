# ------------------------------------------------------------------------------
# Purpose:  This script generates the models to remove from data analysis based
# on observations from the exploratory analysis. The first criteria being that
# there are some models that have exreemely unrealistic values or pH (we are mainly
# refering to MRI-ESM1 which has ocean pH of 6) and we only tos models that have
# co3 and pH data.
#
# Created by: Dorheim, Kalyn
# Created on: September 13 2017
# Modified:   xxx
#
# Notes:
# ------------------------------------------------------------------------------
# 0. Decisions
# ------------------------------------------------------------------------------
# Define the minimum pH values that are realisic. the pH_min will be used
# to identify models with unrealistic pH values.
pH_min <- 6

# This script will write a csv containg the names of the models to be removed
# from futher analysis. Name the output csv file here.
out_file <- "models_to_remove.csv"

# ------------------------------------------------------------------------------
# 1. Environment Set Up
# ------------------------------------------------------------------------------
# Define the base name directory, should be the same for the entire project
BASE_NAME <- getwd()

# Define the script name
script_name <- "screening_models.R"

# Required Libraries
devtools::load_all(BASE_NAME)
library(dplyr)
library(tidyr)
library(ggplot2)

# Define the data and output directories
DATA_DIR <- paste0(BASE_NAME, "/exec/processing/exploratory/output/")
OUT_DIR  <- paste0(BASE_NAME, "/exec/processing/L0/")

# The list to save output in
out = list()


# ------------------------------------------------------------------------------
# 2. Import and Format Inputs
# ------------------------------------------------------------------------------
read.csv(paste0(DATA_DIR, "fldmean_monthly_tos.csv")) %>%
  bind_rows(read.csv(paste0(DATA_DIR, "fldmean_monthly_co3.csv"))) %>%
  bind_rows(read.csv(paste0(DATA_DIR, "fldmean_monthly_pH.csv")) %>% mutate(units = NA)) ->
  data

# ------------------------------------------------------------------------------
# 3. Identify Models To Remove
# ------------------------------------------------------------------------------
# Make a vector of the models with unrealistic pH.
data %>%
  filter(variable == "ph", value < pH_min) %>%
  select(model) %>%
  unique ->
  bad_pH

# Now determine which models have either pH or CO3 data.
# These are the models we consider to have complete data.
data %>%
  group_by(ensemble, experiment, model, variable) %>%
  summarise(value = mean(value)) %>%
  spread(variable, value) %>%
  filter((!is.na(ph)|!is.na(co3))) %>%
  ungroup %>%
  select(model) %>%
  unique ->
  models_with_all

# Determine which models do not contian either co3
# or pH data. These are the models with incomplete
# data.
data %>%
  filter(!model %in% bad_pH$model) %>%
  filter(!model %in% model_to_keep$model) %>%
  select(model) %>%
  unique ->
  models_missing_data


# ------------------------------------------------------------------------------
# 4. Models to Screen For
# ------------------------------------------------------------------------------
bad_pH$reason              <- "bad ph"
models_missing_data$reason <- "missing co3 &/or ph data"

bind_rows(bad_pH, models_missing_data) %>%
  write.csv(file = paste0(OUT_DIR,out_file), row.names = FALSE)

message("Output from ", script_name, " is saved in a csv called", out_file)
# ----
# End
# messge("End of ", script_name)
