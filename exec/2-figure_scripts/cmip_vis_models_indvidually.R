# visualize_individual_modles.R ------------------------------------------------------------------
# Purpose: This script creates figures for each model indvidually
#
# Created by: Dorheim, Kalyn
# Created on: October 31 2017
# Modified:   xxx
#
# Notes: Saving as rda objects and pdfs can take a while, check set up in the Environment section
# to ensure that script will run.
#
# Environment ----------------------------------------------------------------------------------

# Load the reruied libraries
devtools::load_all()
library(ggplot2)
library(dplyr)

# Define the script DIRs
INPUT_DIR <- "data/cmip"
OUTPUT_DIR <- "figs"

# pint_pdfs is a logic statement used to turn on or off the function that prints all of the
# figures generated in this script in pdfs for each model. Printing the pdfs is time intensive,
# to improve run time by setting to FALSE.
print_pdfs <- TRUE

# Load Data Sets  ------------------------------------------------------------------------------

# Start by finding all of the data path
raw_path <- list.files(INPUT_DIR, "basin_mean.rda", full.names = TRUE)
detrended_path <- list.files(INPUT_DIR, "detrended_data.rda", full.names = TRUE)
summary_path   <- list.files(INPUT_DIR, "summary_stats.rda", full.names = TRUE)
amplitude_path <- list.files(INPUT_DIR, "amplitude.rda", full.names = TRUE)
kurotsis_path  <- list.files(INPUT_DIR, "Kurtosis", full.names = TRUE)
max_min_path   <- list.files(INPUT_DIR, "max_min", full.names = TRUE)

# Now load data sets
raw_data <- get(load(raw_path))
detrended_data <- get(load(detrended_path))
summary_data   <- get(load(summary_path))
amplitude_data <- get(load(amplitude_path))
max_min_data <- get(load(max_min_path))

# Since KS values are saved as dataframes in a list you will need to extract the
# data frames from the list.
KS_list  <- get(load(kurotsis_path))
KS_data  <- KS_list$K_S
delta_KS <- KS_list$delta_K_S


# Time series ---------------------------------------------------------------------------------

# The data before trending
raw_data %>%
  group_by(model) %>%
  do(raw_tseries = plot.time_series(., "before detrending")) ->
  raw_tseries

# The data after detrending
detrended_data %>%
  group_by(model) %>%
  do(detrended_tseries = plot.time_series(., "after detrending")) ->
  detrended_tseries

# Amplitude time series
amplitude_data %>%
  # Rename the data columns to meet plot.time_series function requirements
  rename(value = amplitude, date = year) %>%
  group_by(model) %>%
  do(amplitude_tseries = plot.time_series(., "interanual seasonal amplitude")) ->
  amplitude_tseries


# Save all of the time series figures in and object called FIGURES. All of the figures
# created by this script will be added to this object. At the end of this sript the
# figures stored in the FIGURES object will be parsed out by model and saved as model.rda
# objects.
raw_tseries %>%
  inner_join(detrended_tseries, by = "model") %>%
  inner_join(amplitude_tseries, by = "model") ->
  FIGURES


# Monthly Mean ----------------------------------------------------------------------------------

# Use the summary data to get the monthly mean line plots for each model.
summary_data %>%
  group_by(model) %>%
  do(monthly_mean = vis.monthly_mean(.)) %>%
  # Add to the FIGURES objects
  left_join(FIGURES, by = "model") ->
  FIGURES


# Amplitude Distribution -------------------------------------------------------------------------

# Plot the distribution of seasonal amplitudes by basin / experiment for each model.
amplitude_data %>%
  group_by(model) %>%
  do(amplitude_density = vis.amplitude_density(.)) %>%
  # Add to the FIGURES data frame
  left_join(FIGURES, by = "model") ->
  FIGURES


# K and S Distributions ----------------------------------------------------------------------

# Group by model and make the K/S historical vs future scatter plots.
KS_data %>%
  group_by(model) %>%
  do(KS_scatter = vis.KS_scatter_plots(.)) %>%
  # Add to the figures data frame.
  left_join(FIGURES, by = "model") ->
  FIGURES

# Group by model and make the delta K & S scatter plots.
delta_KS %>%
  group_by(model) %>%
  do(delatKS_scatter = vis.delta_KS(.)) %>%
  # Add to the FIGURES data frame
  left_join(FIGURES, by = "model") ->
  FIGURES



# Annual Extreme Heat Maps ----------------------------------------------------------------

# The only percent we should see in these is 100, could turn the the legend off and chagne
# the label but I think that is a trival change to the figure and not really that important
# for the overall analysis.

# Start by calcualting the percent of max or min ocurring in each month by
# year / basin / variable. Count the number of observations in each month
# and divide by number of observations in each year.
max_min_data %>%
  group_by(model, year, basin, variable, month_name, value_type) %>%
  summarise(count = n()) %>%
  ungroup %>%
  group_by(model, year, basin, variable, value_type) %>%
  mutate(percent = 100 * count / sum(count)) %>%
  ungroup ->
  percent_data

# Since the percent_data only contains observations for the months of annual
# extemes the figures will not show a full 12 month annual cycle. In order to
# include all of the months in the figures, create at data frame for year 2101
# and add to the data frame to plot.
percent_data %>%
  select(model, basin, variable, value_type) %>%
  gcamdata::repeat_add_columns(tibble(month_name = MONTH_NAME$month_name)) %>%
  mutate(year = 2101, percent = NA) ->
  fake_2101

# Combine the percent_data and the fake 2101 observations in to a single data
# frame to plot.
fake_2101 %>%
  bind_rows(percent_data) ->
  percent_data_2101

# Order the months for plotting.
percent_data_2101$month_name <- factor(percent_data_2101$month_name, levels = rev(MONTH_NAME$month_name), ordered = TRUE)

# Make figures
percent_data_2101 %>%
  group_by(model) %>%
  do(extreme_fig = vis.annual_extremes(.)) %>%
  left_join(FIGURES, by = "model") ->
  FIGURES


# Save --------------------------------------------------------------------------------------

# Define the function for saving the figures
save_rda <- function(data, path = "figs/cmip/individual_models/"){

  model_name <- unique(data$model)

   data %>%
     select(-model) %>%
     purrr::flatten(.) ->
     intermediate

   out = list(raw_tseries = intermediate$raw_tseries, detrended_tseries = intermediate$detrended_tseries,
              amplitude_tseries = intermediate$amplitude_tseries, monthly_mean = intermediate$monthly_mean,
              amplitude_density = intermediate$amplitude_density, deltaKS_scatter = data$delatKS_scatter,
              KS_scatter = intermediate$KS_scatter,
              extreme_heat_map = intermediate$extreme_fig)

   save(out, file = paste0(path, model_name, ".rda"))

   message("saved ", model_name)

   return(out)

}

# Save each model as a separate rda object
FIGURES %>%
  group_by(model) %>%
  do(fig = save_rda(.)) ->
  formated_figs

# Function for savin each model's figures as a pdfs
save_pdfs <- function(data, path = "figs/cmip/pdfs/"){

  model_name <- unique(data$model)

  to_print <- purrr::flatten(purrr::flatten(data$fig))

  pdf(paste0(path, model_name, ".pdf"))

  for(i in 1:length(to_print)){
    print(to_print[i])
  }

  dev.off()

  message("printed plots for ", model_name)
}


# If print_pdfs (defined in the Environment section of this script) then print all of the figures
# off in pdfs for each model.

if(print_pdfs){

  # Save each model as a separate pdf file
  formated_figs %>%
    group_by(model) %>%
    do(save_pdfs(.))
}


# End ------
message("End of script")
