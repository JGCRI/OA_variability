# Purpose: This is the level 0.5 post pic processing script for the AGU oceans poster analysis / figures
#
# Created by: Dorheim, Kalyn
# Created on: Jan 4
# Modified:   Jan 24
#
# Notes: okay this level of code is not felxible.... basically every time I change pic output
# i ahve to do an overhall of this level of code :(
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
vis_check <- T


# Define the directories
BASE <- getwd()
INPUT_DIR  <- file.path(BASE, "raw-data", "cmip", "AGUoceans_rcp85")
OUTPUT_DIR <- file.path(BASE, "inst", "extdata", "cmip", "AGUoceans_rcp85")

# Import the basin means data csv from pic
csv_paths  <- list.files(INPUT_DIR, ".csv", full.names = T)
data_paths <- csv_paths[!grepl("defined", csv_paths)]


lapply(data_paths, function(x){readr::read_csv(x)}) %>%
  # Because pH netcdfs have units = 1 mutate the units to be the same data
  # type before concatenating the list.
  lapply(., function(x){mutate(x, units = as.character(units))}) %>%
  bind_rows ->
  pic_tibble


# Data Quality: Remove Models ----------------------------------------------------------------

# Start by check to see which model is missing which variable.
pic_tibble %>%
  select(model, variable, units) %>%
  distinct %>%
  spread(variable, units) ->
  pic_variables

# Current required data
req_variables <- c("ph", "spco2", "model")

# If a model is missing a required variable then  it will have the value NA.
pic_variables %>%
  select(req_variables) %>%
  filter_all(any_vars(is.na(.))) %>%
  # Manipulate the data to determine which variable is missing or not.
  tidyr::gather(variable, units, -model) %>%
  mutate(missing = ifelse(is.na(units), variable, "NA")) %>%
  select(model, missing) %>%
  filter(missing != "NA") %>%
  mutate(reason = paste0("missing ", missing)) %>%
  select(model, reason) ->
  models_removed

# SUbset the pic output for the models that contain all of the required variables,
# the non required variables (tos and dissic) do not need to be subjected to this
# filtering process.
pic_tibble %>%
  filter(!(model %in% models_removed$model & variable %in% c(req_variables))) ->
  pic_complete_variables


# Complete time series
#
# Check to make sure that the observations passed on do not have missing data or -9999 (also missing)
# or are missing a historical or future run, so that models with complete time series
# are passed through to be analyzed are complete time series.

# Check for models with values = missing code
pic_complete_variables %>%
  filter(value == -9999, value == -999) %>%
  select(model) %>%
  mutate(reason = "time series contains -9999 values") %>%
  bind_rows(models_removed) ->
  models_removed

pic_complete_variables %>%
  filter(value != -999, value != -9999) ->
  pic_complete_vari_no999

# Define the passable number of missing years, sometimes the historical and
# future rcp don't match up so it's okay for this number not to be 1.
passable_missing_years <- 2

pic_complete_vari_no999 %>%
  # Find the max year difference for each model / variable.
  select(model, year, variable) %>%
  distinct %>%
  group_by(model, variable) %>%
  mutate(year_difference = c(1, diff(year))) %>%
  summarise(max_year_difference = max(year_difference)) %>%
  ungroup %>%
  # Subset to determine which models have more missing years
  # than allowable.
  filter(max_year_difference > passable_missing_years) %>%
  # Add information to the models removed data frame.
  select(model, variable) %>%
  mutate(reason = paste0("Missing more than ", passable_missing_years, " in time series")) ->
  missing_years

missing_years %>%
  bind_rows(models_removed) ->
  models_removed

# Remove the models / variables that are missing the years, the model and variable
# combinations that have no reason (NA) to be removed.
pic_complete_vari_no999 %>%
  left_join(missing_years, by = c("model", "variable")) %>%
  filter(is.na(reason)) %>%
  select(names(pic_complete_vari_no999)) ->
  pic_complete_vari_no999_allYRs


# Now figure out which models and variables are missing a historic or future experiment.
pic_complete_vari_no999_allYRs %>%
  select(model, variable, experiment, units) %>%
  distinct %>%
  tidyr::spread(experiment, units) %>%
  filter_all(any_vars(is.na(.))) %>%
  tidyr::gather(experiment, units, -model, -variable) %>%
  mutate(missing = if_else(is.na(units), experiment, "NA")) %>%
  filter(missing != "NA") %>%
  mutate(reason = paste0("is missing ", missing)) %>%
  select(model, variable, reason) ->
  missing_experiment

models_removed %>%
  bind_rows(missing_experiment) ->
  models_removed


# Remove the model / variable that are missing an experiment or have no
# reason to be removed.
pic_complete_vari_no999_allYRs %>%
  left_join(missing_experiment, by = c("model", "variable")) %>%
  filter(is.na(reason)) %>%
  select(names(pic_complete_vari_no999_allYRs)) ->
  pic_complete_vari_no999_allYRs_exp


# Remove bad data
#
# When we first did our analysis we saw that that some models had unrealistic pH values remove these models now.
ph_cut_off <- 6

pic_complete_vari_no999_allYRs_exp %>%
  filter(variable == "ph" & value < ph_cut_off) %>%
  mutate(reason = paste0("ph value less than ", ph_cut_off)) %>%
  distinct %>%
  select(model, reason, variable) ->
  bad_ph

bad_ph %>%
  bind_rows(models_removed) ->
  models_removed

# Remove the observations with the bad pH
pic_complete_vari_no999_allYRs_exp %>%
  left_join(bad_ph, by = c("model", "variable")) %>%
  filter(is.na(reason)) %>%
  select(names(pic_complete_vari_no999_allYRs_exp)) ->
  pic_complete_vari_no999_allYRs_exp_pH


# Subset for the years
#
# Now subset the data frame so that the all of the time series start at the same time.
max_year <- 2100 # CH says so

# Determine the min year by figuring out the startdate all of the models / variables have
# in common.
pic_complete_vari_no999_allYRs_exp_pH %>%
  # Figure out the start year for each model / variable time series
  select(model, variable, year) %>%
  group_by(model, variable) %>%
  summarise(start_year = min(year)) %>%
  ungroup %>%
  # Determine the most common start year (max start year)
  filter(start_year == max(start_year)) %>%
  select(start_year) %>%
  distinct %>%
  pull(start_year) ->
  min_year

pic_complete_vari_no999_allYRs_exp_pH %>%
  filter(year >= min_year & year <= max_year) ->
  complete_basin_mean_tibble

# Format Data Frame ----------------------------------------------------------------

# Convert Units
complete_basin_mean_tibble <- mutate(complete_basin_mean_tibble, units = ifelse(variable == "ph", "", units))


# Now that we have ensured the quality of the data frame and that all of the observations
# a consistent we need to finish formating the dataframe so that it has the correct time/date
# columns and factors for plotting.

# Add a date column
complete_basin_mean_tibble %>%
  # Arbitrarily set the date of each month equal to the first of the month
  mutate(date = paste0(year,month,"01")) %>%
  mutate(date = as.integer(date)) ->
  complete_basin_mean_date

# Add month names and factors
complete_basin_mean_date %>%
  rename(month_num = month) %>%
  left_join(oceanpH:::MONTH_NAME, by = c("month_num" = "month_ch")) ->
  basin_mean_month_name


# Add factor level to the month name
basin_mean_month_name$month_name <- factor(basin_mean_month_name$month_name , levels = oceanpH:::MONTH_NAME$month_name, ordered = TRUE)


# Rename basins and add basin factor levels for plotting
basin_mean_month_name %>%
  mutate(basin = if_else(basin == "NH Atlantic", "N Atlantic", basin)) %>%
  mutate(basin = if_else(basin == "SH Atlantic", "S Atlantic", basin)) %>%
  mutate(basin = if_else(basin == "NH Pacific", "N Pacific", basin)) %>%
  mutate(basin = if_else(basin == "SH Pacific", "S Pacific", basin)) ->
  basin_mean

basin_mean$basin <- factor(basin_mean$basin, levels = c("Global", "N Atlantic", "S Atlantic", "N Pacific", "S Pacific"), ordered = T)


# Translate pH to protons --------------------------------------------------------------------

basin_mean %>%
  filter(variable == "ph") %>%
  mutate(value = -log10(value), variable = "H ion", units = "[H+]") ->
  proton_tibble

basin_mean %>%
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

# Save  --------------------------------------------------------------------
attributes(basin_mean)$script_name <- script_name
save(basin_mean, file = file.path(OUTPUT_DIR, "basin_mean.rda"))
message("Saved at ", file.path(OUTPUT_DIR, "basin_mean.rda"))

attributes(models_removed)$script_name <- script_name
save(models_removed, file = file.path(OUTPUT_DIR, "models_removed.rda"))
message("Saved at ", file.path(OUTPUT_DIR, "models_removed.rda"))

# End ----

