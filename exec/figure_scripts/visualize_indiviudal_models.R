# ------------------------------------------------------------------------------
# Purpose: This script creates figures for each model indvidually
#
# Created by: Dorheim, Kalyn
# Created on: October 31 2017
# Modified:   xxx
#
# Notes: Saving as rda objects and pdfs can take a while...
# ------------------------------------------------------------------------------
# Environment
# ------------------------------------------------------------------------------
devtools::load_all()
library(ggplot2)
library(dplyr)

# ------------------------------------------------------------------------------
# Load Data Sets
# ------------------------------------------------------------------------------
# Start by finding all of the data path
raw_path <- list.files("data/cmip", "basin_mean.rda", full.names = TRUE)
detrended_path <- list.files("data/cmip", "detrended_data.rda", full.names = TRUE)
summary_path   <- list.files("data/cmip", "summary_stats.rda", full.names = TRUE)
amplitude_path <- list.files("data/cmip", "amplitude.rda", full.names = TRUE)
kurotsis_path <- list.files("data/cmip", "Kurtosis", full.names = TRUE)

# Now load data set
raw_data <- get(load(raw_path))
detrended_data <- get(load(detrended_path))
summary_data <- get(load(summary_path))
amplitude_data <- get(load(amplitude_path))
KS_list <- get(load(kurotsis_path))
KS_data <- KS_list$K_S
delta_KS <- KS_list$delta_K_S

# ------------------------------------------------------------------------------
# Time series
# ------------------------------------------------------------------------------
# The data before trending
raw_data %>%
  group_by(model) %>%
  do(fig = plot.time_series(., "before detrending")) ->
  raw_tseries

# The data after detrending
detrended_data %>%
  group_by(model) %>%
  do(fig = plot.time_series(., "after detrending")) ->
  detrended_tseries

# Amplitude time series
amplitude_data %>%
  # Rename the data columns to meet plot.time_series function requirements
  rename(value = amplitude, date = year) %>%
  group_by(model) %>%
  do(fig = plot.time_series(., "interanual seasonal amplitude")) ->
  amplitude_tseries


# Add to FIGURES object.
raw_tseries %>%
  rename(raw_tseries = fig) %>%
  inner_join(detrended_tseries %>% rename(detrended_tseries = fig), by = "model") %>%
  inner_join(amplitude_tseries %>%  rename(amplitude_tseries = fig), by = "model") ->
  FIGURES


# ------------------------------------------------------------------------------
# Monthly mean
# ------------------------------------------------------------------------------
# Use the summary data to get the monthly mean line plots for each model.
summary_data %>%
  group_by(model) %>%
  do(fig = vis.monthly_mean(.)) ->
  monthly_mean_figures

# Add to FIGURES object.
FIGURES %>%
  inner_join(monthly_mean_figures %>% rename(monthly_mean = fig), by = "model") ->
  FIGURES

# ------------------------------------------------------------------------------
# Amplitude distribution
# ------------------------------------------------------------------------------
# Plot the distribution of seasonal amplitudes by basin / experiment for each model.
amplitude_data %>%
  group_by(model) %>%
  do(fig = vis.amplitude_density(.)) ->
  amp_density_figures


# Add to FIGURES object.
FIGURES  %>%
  inner_join(amp_density_figures %>% rename(amplitude_density = fig), by = "model") ->
  FIGURES



# ------------------------------------------------------------------------------
# Save
# ------------------------------------------------------------------------------
# Fucntion for saving each model's fiugres a rda objects.
save_rda <- function(data, path = "figs/cmip/individual_models/"){

  model_name <- unique(data$model)

   data %>%
     select(-model) %>%
     purrr::flatten(.) ->
     intermediate

   out = list(raw_tseries = intermediate$raw_tseries, detrended_tseries = intermediate$detrended_tseries,
              amplitude_tseries = intermediate$amplitude_tseries, monthly_mean = intermediate$monthly_mean,
              amplitude_density = intermediate$amplitude_density)

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

# Save each model as a separate pdf file
formated_figs %>%
  group_by(model) %>%
  do(save_pdfs(.))

# ----
# End
