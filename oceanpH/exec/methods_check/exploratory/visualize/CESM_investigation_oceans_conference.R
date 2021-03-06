# ------------------------------------------------------------------------------
# Purpose: CESM monthly analysis for some Ocean's conference. CH requested the
# analysis/figres slack Sept 1.
#
# Created by: Dorheim, Kalyn
# Created on: Sep 1 2107
# Modified:   xxx
#
# Notes: can you take CESM and calculate the seasonal amplitude in ph, co3 and tos?
# Take the difference between the maximum and minimum monthly values for the historical
# and rcp85 and you can plot them month vs amplitude.  average over the first 30 years
# in the historical and the last 30 years of rcp8.5
# or i guess you could call it monthly amplitude
# 1. take the monthly average over the first 30 years,
# 2. calulate the difference between min and max for each month,
# 3. take the monthly average over the last 30 years (2070-2100),
# 4. calculate the difference between min and max for each month
# ------------------------------------------------------------------------------
# 0. Environment Set up
# ------------------------------------------------------------------------------
# # Define the base name directory, should be the same for the entire project
# BASE_NAME <- getwd()
#
# # Required Libraries
# devtools::load_all(BASE_NAME)
# library(dplyr)
# library(tidyr)
# library(ggplot2)

# Define the data and output directories
DATA_DIR <- paste0(BASE_NAME, "/exec/processing/exploratory/output/")
OUT_DIR  <- DATA_DIR

# The list to save output in
out = list()

# Define the base name for the output files.
out_fname <- "CESM_investigation.csv"

# Script name
script_name <- "CESM_investigation_oceans_conference.R"

# ------------------------------------------------------------------------------
# 1. Import and Format Inputs
#
# ------------------------------------------------------------------------------
# Import the tos, pH, and CO3 data for the CESM model only.
read.csv(paste0(DATA_DIR,"fldmean_monthly_tos.csv")) %>%
  bind_rows(read.csv(paste0(DATA_DIR,"fldmean_monthly_pH.csv")) %>% mutate(units = NA)) %>%
  bind_rows(read.csv(paste0(DATA_DIR, "fldmean_monthly_co3.csv"))) %>%
  filter(model == "CESM1-BGC") ->
  df

# Check that the data frame only includes one model, this may be over kill but whateves
if(df$model %>% unique %>% length != 1) {
  stop("Multiple models idenfied in the CESM_df")
  stop("Error occured in section1")
}

# Add the month name
df %>%
  add_month_name ->
  df

# Save a back up of the orignal df with monthly names
og_df_months <- df

# ------------------------------------------------------------------------------
# 2. Historical First 30 Years
#
# ------------------------------------------------------------------------------
# A. Get monthly average and range
# ------------------------------------------------------------------------------
# 1. take the monthly average over the first 30 years for the historical figures

# First add the year number and then select the first 30 years in
# each group.
df %>%
  group_by(ensemble, variable, units, experiment, model) %>%
  mutate(year_number = year - (min(year)-1)) %>%
  filter(year_number >= 1, year_number <= 30, experiment == "historical") %>%
  ungroup ->
  historical_first30

# Now find the monthly average for the first 30 years for each
# experiment, variable an model group.
historical_first30 %>%
  group_by(ensemble, variable, units, experiment, model, month) %>%
  summarise(mean = mean(value)) %>%
  ungroup ->
  historical_first30_mean


# 2. calulate the difference between min and max for each month
#
# Now find the range for each month during the first 30 years
historical_first30 %>%
  group_by(ensemble, variable, units, experiment, model, month) %>%
  summarise(range = max(value) - min(value)) %>%
  ungroup ->
  historical_first30_range

# ------------------------------------------------------------------------------
# B. Figures
# ------------------------------------------------------------------------------
# Mean Figures
historical_first30_mean %>%
  mutate(id = model) %>%
  add_month_name %>%
  rename(time = month, value = mean) %>%
  group_by(variable) %>%
  dplyr::do(fig = vis.compare_id(.))  %>%
  rename(name = variable) %>%
  vis.add_fig_name ->
  mean_figs

# Make mean figs prettish
# For loop to modify the figures by adding a title
n <- length(names(mean_figs))

for(i in 1:n){
  # The figure name
  fig_name <- names(mean_figs)[i]

  # Add the month names as the x axis table and then add a title
  mean_figs[[fig_name]][["fig"]] +
    scale_x_discrete(limits = og_df_months$month, labels = og_df_months$month_names) +
    labs(title = "Monthly Mean from the 1st 30 Years") ->
    mean_figs[[fig_name]][["fig"]]

} # end of add title and month names for loop

# Range Figures
historical_first30_range %>%
  mutate(id = model) %>%
  add_month_name %>%
  rename(time = month, value = range) %>%
  group_by(variable) %>%
  dplyr::do(fig = vis.compare_id(.)) %>%
  rename(name = variable) %>%
  vis.add_fig_name ->
  range_figs

# For loop to modify the figures by adding a title
n <- length(names(range_figs))

for(i in 1:n){
  # The figure name
  fig_name <- names(range_figs)[i]

  # Add the month names as the x axis table and then add a title
  range_figs[[fig_name]][["fig"]] +
    scale_x_discrete(limits = og_df_months$month, labels = og_df_months$month_names) +
    labs(title = "Monthly Amplitude from the 1st 30 Years") ->
    range_figs[[fig_name]][["fig"]]

} # end of add title and month names for loop

# ------------------------------------------------------------------------------
# C. Save
# ------------------------------------------------------------------------------
# Save the historical data frames and fiugres in a list called histroical_output
histroical_output = list()

# Combine the range and mean historical values and save in the historical output list.
historical_first30_range %>%
  left_join(historical_first30_mean,
            by = c("ensemble", "variable", "units", "experiment", "model" , "month")) ->
  histroical_output[["data"]]

histroical_output[["figs"]] <- list(range = range_figs, mean = mean_figs)


# Make a table of amplitude mean and sd for all of the variables
var_list <- names(histroical_output$figs$range)

for(i in 1:length(var_list)){

  histroical_output[["figs"]][["range"]][[paste0(var_list[i])]][["data"]] %>%
    group_by(ensemble, experiment, model) %>%
    summarise(mean = mean(value), sd = sd(value)) %>%
    knitr::kable(., format = "markdown") ->
    histroical_output[["tab"]][["range"]][[paste0(var_list[i])]]
} # end of for loop



# messge("Output from ", script_name, " is saved in a list called xxx.")
# messge("Output from ", script_name, " is saved in a csv file... (add details about the where the file is saved).")
# ------------------------------------------------------------------------------
# 3. rcp 8.5 Last 30 Years
#
# ------------------------------------------------------------------------------
# A. Get monthly average and range
# ------------------------------------------------------------------------------
# 1. take the monthly average over the last 30 years for the rcp 8.5 figures

# First add the year number and then select the first 30 years in
# each group.
df %>%
  group_by(ensemble, variable, units, experiment, model) %>%
  # Number the years realtive to the max year to filter for the lat 30 years
  mutate(year_number = (max(year)+1) - year) %>%
  filter(year_number >= 1, year_number <= 30, experiment == "rcp85") %>%
  ungroup ->
  rcp85_last30

# Now find the monthly average for the last 30 years for each
# experiment, variable an model group.
rcp85_last30 %>%
  group_by(ensemble, variable, units, experiment, model, month) %>%
  summarise(mean = mean(value)) %>%
  ungroup ->
  rcp85_last30_mean


# 2. calulate the difference between min and max for each month
#
# Now find the range for each month during the last 30 years
rcp85_last30 %>%
  group_by(ensemble, variable, units, experiment, model, month) %>%
  summarise(range = max(value) - min(value)) %>%
  ungroup ->
  rcp85_last30_range

# ------------------------------------------------------------------------------
# B. Figures
# ------------------------------------------------------------------------------
# Empty the lits for the new figures
remove(mean_figs); remove(range_figs)

# Mean Figures
rcp85_last30_mean %>%
  mutate(id = model) %>%
  add_month_name %>%
  rename(time = month, value = mean) %>%
  group_by(variable) %>%
  dplyr::do(fig = vis.compare_id(.))  %>%
  rename(name = variable) %>%
  vis.add_fig_name ->
  mean_figs

# Make mean figs prettish
# For loop to modify the figures by adding a title
n <- length(names(mean_figs))

for(i in 1:n){
  # The figure name
  fig_name <- names(mean_figs)[i]

  # Add the month names as the x axis table and then add a title
  mean_figs[[fig_name]][["fig"]] +
    scale_x_discrete(limits = og_df_months$month, labels = og_df_months$month_names) +
    labs(title = "Monthly Mean from the last 30 Years") ->
    mean_figs[[fig_name]][["fig"]]

} # end of add title and month names for loop

# Range Figures
rcp85_last30_range %>%
  mutate(id = model) %>%
  add_month_name %>%
  rename(time = month, value = range) %>%
  group_by(variable) %>%
  dplyr::do(fig = vis.compare_id(.)) %>%
  rename(name = variable) %>%
  vis.add_fig_name ->
  range_figs

# For loop to modify the figures by adding a title
n <- length(names(range_figs))

for(i in 1:n){
  # The figure name
  fig_name <- names(range_figs)[i]

  # Add the month names as the x axis table and then add a title
  range_figs[[fig_name]][["fig"]] +
    scale_x_discrete(limits = og_df_months$month, labels = og_df_months$month_names) +
    labs(title = "Monthly Amplitude from the last 30 Years") ->
    range_figs[[fig_name]][["fig"]]

} # end of add title and month names for loop

# ------------------------------------------------------------------------------
# C. Save
# ------------------------------------------------------------------------------
# Save the historical data frames and fiugres in a list called rcp85_output
rcp85_output = list()

# Combine the range and mean historical values and save in the historical output list.
rcp85_last30_mean %>%
  left_join(rcp85_last30_range,
            by = c("ensemble", "variable", "units", "experiment", "model" , "month")) ->
  rcp85_output[["data"]]

rcp85_output[["figs"]] <- list(range = range_figs, mean = mean_figs)


# For each variable calculate the amplitudes mean and sd, save in a table and
# export to the list.
var_list <- names(rcp85_output$figs$range)

for(i in 1:length(var_list)){

  rcp85_output[["figs"]][["range"]][[paste0(var_list[i])]][["data"]] %>%
    group_by(ensemble, experiment, model) %>%
    summarise(mean = mean(value), sd = sd(value)) %>%
    knitr::kable(., format = "markdown") ->
    rcp85_output[["tab"]][["range"]][[paste0(var_list[i])]]

} # end of for loop



# messge("Output from ", script_name, " is saved in a list called xxx.")
# messge("Output from ", script_name, " is saved in a csv file... (add details about the where the file is saved).")

# ------------------------------------------------------------------------------
# 4. Format output
#
# ------------------------------------------------------------------------------
out[["historical"]] <- histroical_output
out[["rcp85"]] <- rcp85_output
# ----
# End
# messge("End of ", script_name)
