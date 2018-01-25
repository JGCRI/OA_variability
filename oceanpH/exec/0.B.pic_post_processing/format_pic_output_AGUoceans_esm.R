# Purpose: This is the level 0.5 post pic processing script for the AGU oceans poster analysis / figures
# for the esm experiments
#
# Created by: Dorheim, Kalyn
# Created on: Jan 24
# Modified:   xxx
#
# Notes: May want to turn into a function or something one day
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
INPUT_DIR  <- file.path(BASE, "raw-data", "cmip", "AGUoceans_esm")
OUTPUT_DIR <- file.path(BASE, "inst", "extdata", "cmip", "AGUoceans_esm")

# Find basin mean paths
csv_paths  <- list.files(INPUT_DIR, ".csv", full.names = T)
data_paths <- csv_paths[which(grepl("defined", csv_paths) == F)]

# Read in csv files
# Because pH netcdfs have units = 1 mutate the units to be the same data
# type before concatenating the list.
lapply(data_paths, function(x){readr::read_csv(x)}) %>%
  lapply(., function(x){mutate(x, units = as.character(units))}) %>%
  bind_rows ->
  pic_tibble


# Import the post pic processd rcp85 AGU data frame, this will contain the models we want to include in this
# analysis.
AGUrcp_path <- list.files(file.path(BASE, "inst/extdata", "cmip", "AGUoceans_rcp85"), "basin_mean.rda", full.names = T)
AGUrcp_data <- get(load(AGUrcp_path))


# Convert Units ---------------------------------------------------------------------------
pic_tibble_units <- mutate(pic_tibble, units = ifelse(variable == "ph", "", units))

# Quality Control: Remove Models ----------------------------------------------------------------

# Consisitent with rcp
#
# Keep only the models that are included in the rcp analysis. Discard the models that
# are not in the rcp analysis and which ones are missing from the esm data.
expected_models <- unique(AGUrcp_data$model)

# What models are in the rcp data frame but not in the esm data frame
missing_models <- tibble::tibble(model = expected_models[which(!expected_models %in% pic_tibble_units$model == TRUE)])
missing_models$reason <- "missing esm data"

# Discard the models not in the rcp data
models_removed <- tibble::tibble(model = unique(pic_tibble_units$model)[which(!expected_models %in% pic_tibble_units$model == TRUE)])
models_removed$reason <- "no rcp data"

pic_tibble_models <- filter(pic_tibble_units, model %in% expected_models)


# Ensure complete variable count for each model
#
# start by determining if / which variables are missing for each model. There is probably
# a better way of doing this, idk.

expected_variables_list <- unique(pic_tibble_models$variable)

# Figure out which models do not meet the variable requirement.
pic_tibble_models %>%
  select(model, variable) %>%
  distinct %>%
  group_by(model) %>%
  summarise(count = n()) %>%
  ungroup %>%
  filter(count < 2) ->
  insufficent_variable_count

# Determine which variable is missing and add to the missing model list.
pic_tibble_models %>%
  filter(model %in% insufficent_variable_count$model) %>%
  select(model, variable) %>%
  distinct %>%
  mutate(missing = expected_variables_list[expected_variables_list != variable]) %>%
  mutate(reason = paste0("missing ", missing)) %>%
  select(model, reason) %>%
  bind_rows(missing_models) ->
  missing_models


# Subset the tibble of pic output with the correct number of units so that it only
# includes model NOT in the insufficient_variable_count tibble.
pic_tibble_units_variables <- filter(pic_tibble_models, !model %in% insufficent_variable_count$model)


# Complete time series
#
# Check to make sure that the observations passed on do not have missing data or -9999 (also missing)
# or are missing a historical or future run, so that models with complete time series
# are passed through to be analyzed are complete time series.

# Check for models with values = missing code
pic_tibble_units_variables %>%
  filter(value == -9999, value == -999) %>%
  select(model) %>%
  mutate(reason = "time series contains -9999 values") %>%
  bind_rows(models_removed) ->
  models_removed


# Define the passable number of missing years, sometimes the historical and
# future rcp don't match up so it's okay for this number not to be 1.
passable_missing_years <- 2

pic_tibble_units_variables %>%
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
  select(model) %>%
  mutate(reason = paste0("Missing more than ", passable_missing_years, " in time series")) %>%
  bind_rows(models_removed) ->
  models_removed

# Now figure out which models do not have historical and future experiments.
pic_tibble_units_variables %>%
  # Count the number of experiments for all of the unique model / variable combinations.
  select(model, variable, experiment) %>%
  distinct %>%
  group_by(model, variable) %>% arrange(model, variable, experiment) %>%  filter(variable == "spco2")
  summarise(experiment_count = n()) %>%
  ungroup %>%
  # SUbset to find the models that are missing an experiment.
  filter(experiment_count < 2) %>%
  # Add additional information and then add to the models_removed data frame.
  mutate(reason = paste0("Missing an experiment for ", variable)) %>%
  select(model, reason) %>%
  bind_rows(models_removed) ->
  models_removed


# Subset the tibble of pic output with the correct number of units and complete variable set so that it
# now only also contains models observations with complete time series. So that it contains complete data set.
pic_tibble_complete_tseries <- filter(pic_tibble_units_variables, !model %in% models_removed$model)


# Quality Control: Remove Bad Data ----------------------------------------------------------------

# When we first did our analysis we saw that that some models had unrealistic pH values remove these models now.
ph_cut_off <- 6

pic_tibble_complete_tseries %>%
  filter(variable == "ph" & value < ph_cut_off) %>%
  mutate(reason = paste0("ph value less than ", ph_cut_off)) %>%
  distinct %>%
  select(model, reason) %>%
  bind_rows(models_removed) ->
  models_removed

pic_tibble_filtered_data <- filter(pic_tibble_complete_tseries, !model %in% models_removed$model)

# Now subset the data frame so that the all of the time series start at the same time.
max_year <- 2100 # CH says so
min_year <- min(AGUrcp_data$year)

pic_tibble_filtered_data %>%
  filter(year >= min_year & year <= max_year) ->
  complete_basin_mean_tibble


# Format Data Frame ----------------------------------------------------------------

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

basin_mean$basin <- factor(basin_mean$basin, levels = c("N Atlantic", "S Atlantic", "N Pacific", "S Pacific", "Global"), ordered = T)


# Translate pH to protons --------------------------------------------------------------------

basin_mean %>%
  filter(variable == "ph") %>%
  mutate(value = -log10(value), variable = "proton", units = "[H+]") ->
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

