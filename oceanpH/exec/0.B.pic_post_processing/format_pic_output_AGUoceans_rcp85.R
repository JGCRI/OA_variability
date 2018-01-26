# Purpose: This is the level 0.5 post pic processing script for the AGU oceans poster analysis / figures
#
# Created by: Dorheim, Kalyn
# Created on: Jan 4
# Modified:   Jan 26
#
# Notes: could functionalize left_join(bad_ph_values, by = "model") %>% filter(is.na(reason)) %>%
# select(names(data_experiments_completeT_completeV)) ???
#
# Setup Environment ------------------------------------------------------------------------------

# Load libs
library(dplyr)
library(ggplot2)
library(tidyr)
devtools::load_all()

# Define the script name
script_name <- find_scriptName()

# Visual checks a logic statement to control the if statements that will make figures as sanity checks.
vis_check <- F


# Define the directories
BASE <- getwd()
INPUT_DIR  <- file.path(BASE, "raw-data", "cmip", "AGUoceans_rcp85")
OUTPUT_DIR <- file.path(BASE, "inst", "extdata", "cmip", "AGUoceans_rcp85")

# Import the basin means data csv from pic and concatenate into a single tibble.
csv_paths  <- list.files(INPUT_DIR, ".csv", full.names = T)
data_paths <- csv_paths[!grepl("defined", csv_paths)]

lapply(data_paths, function(x){readr::read_csv(x)}) %>%
  lapply(., function(x){mutate(x, units = as.character(units))}) %>%
  bind_rows ->
  pic_tibble


# Data Quality: Ensure Completeness ----------------------------------------------------------------

# Keep track of the model / experiment / variable that contains -999 values.
pic_tibble %>%
  distinct %>%
  filter(value == -999) %>%
  select(model, variable, experiment) %>%
  distinct %>%
  mutate(reason = "found code -999") ->
  removed_observations


# Discard dublicate entries and anything that contains the -9999 code, also only keep obervations
# for less than 2100, says CH.
pic_tibble %>%
  distinct %>%
  filter(value != -999) %>%
  filter(year <= 2100) ->
  pic_real_data


# Contains both experiments
#
# Check to make sure that all of the model / variable combinations contain data from both the
# historical and future scenarios.

# Determine which model / variable has an exmperiment without units.
pic_real_data %>%
  select(model, variable, experiment, units) %>%
  distinct %>%
  spread(experiment, units) %>%
  filter_all(any_vars(is.na(.))) %>%
  gather(experiment, units, -model, -variable) %>%
  mutate(reason = if_else(is.na(units), paste0("missing ", experiment), "NA")) %>%
  filter(reason != "NA") %>%
  select(model, variable, reason) ->
  missing_experiment

# Add the missing experiments info to the removed obersvations tibble
removed_observations <- bind_rows(removed_observations, missing_experiment)

# Use the missing epxeriments data frame to add remove model / variables from the
# basin data frame, only discard the observations that do not have a reason to be
# removed.
pic_real_data %>%
  left_join(missing_experiment, by = c("variable", "model")) %>%
  filter(is.na(reason)) %>%
  select(names(pic_real_data)) ->
  data_experiments


# Now that we have ensured that the variables contain the requiered expreiments check to make
# sure that the the time series is not missing more than the allowed numnber of years.

missing_years_allowed <- 3

data_experiments %>%
  select(model, variable, experiment, year) %>%
  distinct %>%
  group_by(model, variable) %>%
  do(year_dif = diff(.$year)) %>%
  unnest %>%
  filter(year_dif >= missing_years_allowed) %>%
  mutate(reason = paste0("missing ", year_dif, " in time series")) %>%
  select(model, variable, reason) ->
  missing_too_many_years

# Add the models / variables that are missing too many years to the removed observations tibble
removed_observations <- bind_rows(removed_observations, missing_too_many_years)


# Discard the models / variables that are missing too many years from future processing, keep
# only the observations with no reason to be discarded.
data_experiments %>%
  left_join(missing_too_many_years, by = c("variable", "model")) %>%
  filter(is.na(reason)) %>%
  select(names(data_experiments)) ->
  data_experiments_completeT


# Variable consistency
#
# Check to make sure that the models are consistent across the required variables.

required_variables <- c("ph")

# Determine which models / variables are missing units.
data_experiments_completeT %>%
  select("model", "units", "variable") %>%
  distinct %>%
  filter(variable %in% required_variables) %>%
  spread(variable, units) %>%
  gather(variable, units, -model) %>%
  mutate(reason = if_else(is.na(units), paste0("missing ", variable), "NA")) %>%
  filter(reason != "NA") %>%
  select(model, variable, reason) ->
  missing_variable

# Add the models that are missing missing variables to the removed_observations tibble
removed_observations <- bind_rows(removed_observations, missing_variable)

# Discared the observations that are missing one of the required variables aka they have
# no reason to be removed.
data_experiments_completeT %>%
  left_join(removed_observations, by = c("variable", "model", "experiment")) %>%
  filter(is.na(reason)) %>%
  select(names(data_experiments_completeT)) ->
  data_experiments_completeT_rcompleteV

# Now restrict the variables that may have extra models to the models in the required
# variables.

data_experiments_completeT_rcompleteV %>%
  filter(variable %in% required_variables) %>%
  select(model) %>%
  unique %>%
  pull ->
  models_required_variable

# The variables to restict if there are extra models
vari_restrict_extra_models <- c("tos", "spco2")

data_experiments_completeT_rcompleteV %>%
  filter(variable %in% vari_restrict_extra_models) %>%
  select(model, variable) %>%
  distinct %>%
  filter(!model %in% models_required_variable) %>%
  mutate(reason = paste0("model is missing ", paste0(required_variables, collapse = " &/or "))) ->
  missing_req_vari

# Add the models from the variable that must be resticted and are missing the required variables
# to the remove from observations list.
removed_observations <- bind_rows(removed_observations, missing_req_vari)

# Discard the models from the resticted variable does not contain the required variables tibble.
data_experiments_completeT_rcompleteV %>%
  left_join(missing_req_vari, by = c("variable", "model")) %>%
  filter(is.na(reason)) %>%
  select(names(data_experiments_completeT_rcompleteV)) ->
  data_experiments_completeT_completeV


# Now that the data meets the completeness requiremets for experimesnt, timeseries, and variables
# make sure that non of the models have bad pH values.
ph_threshold <- 6

# Find the models with the bad ph values
data_experiments_completeT_completeV %>%
  filter(variable == "ph" & value < ph_threshold) %>%
  select(model) %>%
  mutate(reason = paste0("ph less than ", ph_threshold)) ->
  bad_ph_values

# Add the models from the variable that must be resticted and are missing the required variables
# to the remove from observations list.
removed_observations <- bind_rows(removed_observations, bad_ph_values)

# Discard any models with a bad pH
data_experiments_completeT_completeV %>%
  left_join(bad_ph_values, by = "model") %>%
  filter(is.na(reason)) %>%
  select(names(data_experiments_completeT_completeV)) ->
  data_filtered


# Format Data Frame ----------------------------------------------------------------

# Now that the data has been completly filtered and contains complete model / experiment / variable
# coverage format the data frame.

# Convert Units
data_units <- mutate(data_filtered, units = ifelse(variable == "ph", "", units))


# Now that we have ensured the quality of the data frame and that all of the observations
# a consistent we need to finish formating the dataframe so that it has the correct time/date
# columns and factors for plotting.

# Add a date column
data_units %>%
  # Arbitrarily set the date of each month equal to the first of the month
  mutate(date = paste0(year,month,"01")) %>%
  mutate(date = as.integer(date)) ->
  data_units_date

# Add month names and factors
data_units_date %>%
  rename(month_num = month) %>%
  left_join(oceanpH:::MONTH_NAME, by = c("month_num" = "month_ch")) ->
  data_units_date_month


# Add factor level to the month name
data_units_date_month$month_name <- factor(data_units_date_month$month_name , levels = oceanpH:::MONTH_NAME$month_name, ordered = TRUE)


# Rename basins and add basin factor levels for plotting
data_units_date_month %>%
  mutate(basin = if_else(basin == "NH Atlantic", "N Atlantic", basin)) %>%
  mutate(basin = if_else(basin == "SH Atlantic", "S Atlantic", basin)) %>%
  mutate(basin = if_else(basin == "NH Pacific", "N Pacific", basin)) %>%
  mutate(basin = if_else(basin == "SH Pacific", "S Pacific", basin)) ->
  data_units_date_month_basins

data_units_date_month_basins$basin <- factor(data_units_date_month_basins$basin, levels = c("Global", "N Atlantic", "S Atlantic", "N Pacific", "S Pacific"), ordered = T)


# Translate pH to protons --------------------------------------------------------------------

data_units_date_month_basins %>%
  filter(variable == "ph") %>%
  mutate(value = -log10(value), variable = "H ion", units = "[H+]") ->
  proton_tibble

data_units_date_month_basins %>%
  bind_rows(proton_tibble) ->
  basin_mean


# Visual Check --------------------------------------------------------------------
if(vis_check){

  # Okay validation figures! Plot the time series to make sure that they look good and then
  basin_mean %>%
    ggplot(aes(x = year, y = value, color = model)) +
    geom_line() +
    facet_wrap("variable", scales = "free") ->
    fig

  attributes(fig)$script_name <- fig
  save(fig, file = file.path(OUTPUT_DIR, "Fig_basin_mean_check.rda"))

}


# Make a summary table of what models / variables we do have

basin_mean %>%
  select(model, variable, experiment) %>%
  distinct %>%
  arrange(model, variable, experiment) %>%
  group_by(model, variable) %>%
  summarise(experiments = paste0(unique(experiment), collapse = "  ")) %>%
  ungroup %>%
  spread(variable, experiments) %>%
  mutate_all(any_vars(if_else(is.na(.), "", .))) %>%
  knitr::kable(.) ->
  processed_observations_kable



# Save  --------------------------------------------------------------------
attributes(basin_mean)$script_name <- script_name
save(basin_mean, file = file.path(OUTPUT_DIR, "basin_mean.rda"))
message("Saved at ", file.path(OUTPUT_DIR, "basin_mean.rda"))

attributes(removed_observations)$script_name <- script_name
save(removed_observations, file = file.path(OUTPUT_DIR, "removed_observations.rda"))
message("Saved at ", file.path(OUTPUT_DIR, "removed_observations.rda"))


attributes(processed_observations_kable)$script_name <- script_name
save(processed_observations_kable, file = file.path(OUTPUT_DIR, "processed_observations_kable.rda"))
message("Saved at ", file.path(OUTPUT_DIR, "processed_observations_kable.rda"))


# End ----

