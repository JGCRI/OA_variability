# Visually Compare Obs and CMIP Data ----------------------------------------------------------
# Purpose: Just exploring the observational data
#
# Created by: Dorheim, Kalyn
# Created on: Nov 15 2017
# Modified:   xxx
#
# Notes: this script contains several functions that may be canidates to become pacakge functions.
# the print to pdf option does not work yet.
#
# Environment ---------------------------------------------------------------------------------

# Load the libraries
devtools::load_all()
library(dplyr)
library(ggplot2)

# Define the scirpt dir
INPUT_DIR  <- "data/observations"
OUTPUT_DIR <- "figs/observations"

# Print as pdfs - TRUE or FALSE will print the fiugres as pdfs, not working yet.
print_pdfs <- FALSE


# Load data  ----------------------------------------------------------------------------------

# Define the data paths
data_path <- list.files(INPUT_DIR, "mean.rda", full.names = TRUE)
detrend_path <- list.files(INPUT_DIR, "detrended", full.names = TRUE)
amp_path <- list.files(INPUT_DIR, "amplitude", full.names = TRUE)
KS_path  <- list.files(INPUT_DIR, "KS", full.names = TRUE)
monthly_path <- list.files(INPUT_DIR, "monthly", full.names = TRUE)


# Load the data sets
data <- get(load(data_path))
detrend_df   <- get(load(detrend_path))
amplitude_df <- get(load(amp_path))
monthly_df   <- get(load(monthly_path))

# I do not think that we have enough observations to look at the K and S distrbution

# Script Functions ------------------------------------------------------------------------------

# These functions are not pacakge functions but may become canidates

# This is the function that plots the cmip models / observations / cesm comparison
# figures
vis.tseries_comparison <- function(data, subtitle = ""){

  internal_subtitle <- subtitle

  internal.func <- function(data, subtitle = subtitle){

    data %>%
      mutate(date = paste0(month, "-1-", substr(time,1,4))) %>%
      mutate(date = as.Date(date, "%m-%d-%Y")) %>%
      mutate(time = date) ->
      data

    all_models  <- filter(data, id == "cmip models")
    ex_model    <- filter(data, grepl("CESM", id))
    observation <- filter(data, id != "cmip models", ! grepl("CESM", id))

    vari <- unique(observation$variable)
    uni  <- unique(observation$units)
    basin <- unique(observation$basin)


    all_models %>%
      # First plot all of the cmip models
      ggplot(aes(x = time,y = value)) +
      geom_point(size = 7, color = "grey", alpha = 0.5, na.rm = TRUE) +
      # Plot the example model
      geom_point(data = ex_model, aes(x = time, y = value), color = "orange", size = 7, na.rm = TRUE, alpha = 0.5) +
      # Plot the pbservational data set
      geom_point(data = observation, aes(x = time, y = value), color = "blue", size = 7, na.rm = TRUE, alpha = 0.5) +
      # Remove the background for clarity
      theme(panel.background = element_blank()) +
      labs(title = paste0("CMIP Observation Comparison\n", basin),
           subtitle = subtitle,
           y = paste0(vari, "  ", uni),
           caption = "blue = observational data set \norange = CESM1-BGC\ngrey = other cmip models \n\n\n") +
      scale_x_date(date_labels ="%b%Y") +
      oceanpH::MY_SETTINGS
  }

  data %>%
    group_by(basin, variable) %>%
    do(fig = internal.func(., internal_subtitle))

}


# This functions adds identificaiton information to the cmip observation data frame
add.obs_cmip_id <- function(data){

  # Save a list of the id names to use when defining the id levels.
  observational_data <- filter(data, ensemble == "obs")
  observational_list <- unique(observational_data$model)

  id_names <- c("cmip models", "CESM1-BGC", observational_list)

  data %>%
    mutate(id = if_else(ensemble == "obs", model, id_names[1])) %>%
    mutate(id = if_else(grepl("CESM1", model), id_names[2], id))

}


# Compare the monthly means for the detrended cmip vs model data - these are not the most useful
# plots unless there is sufficent data
vis.obs_monthly_mean <- function(data){
  basin <- unique(data$basin)
  vari  <- unique(data$variable)

  obs <- filter(data, experiment == "obs") %>% add.obs_cmip_id
  mod <- filter(data, experiment != "obs") %>% add.obs_cmip_id

  ggplot(data = mod, aes(month, mean, color = id)) +
    geom_point(size = 3) +
    geom_ribbon(data = obs, aes(x = month, ymin = mean - sd, ymax = mean + sd, color = id, linetype = NA),
                alpha = 0.25) +
    geom_point(data = obs, aes(month, mean, color = id), size = 3.5) +
    labs(title = paste0("Mean Monthly ", vari, "\n", basin)) +
    scale_x_discrete(limits = obs$month, labels = obs$month_name) +
    oceanpH::MY_SETTINGS
}


# Time Series ---------------------------------------------------------------------------------

# Raw time series
raw <- vis.tseries_comparison(data, "Before Detrending") %>%  rename(raw_t = fig)


# Detrended time series
detrend_fig <- vis.tseries_comparison(detrend_df, "After Detrending") %>%  rename(detr_t = fig)


# Amplitude time series
  amplitude_df %>%
    # In order to use the vis.tseries_comparison function the amplitude data frame will need
    # to be reformated and a dummy time column (%Y%m%d) will need to be created.
    add.obs_cmip_id %>%
    mutate(time = paste0(year, "01"), month = "01") %>%
    rename(value = amplitude) %>%
    vis.tseries_comparison(subtitle = "Amplitude") %>%
    # Rename the figure column in preparation of the joining the all of the time series figures
    # together.
    rename(tseries_amplitude = fig) ->
    amplitude_fig


# Combine all of the time series figures data frames into a single object by bains / model combination
# to create a giant time series data frame.
raw %>%
  left_join(detrend_fig, by = c("basin", "variable")) %>%
  left_join(amplitude_fig, by = c("basin", "variable")) ->
  time_figs_df


# Amplitude Distibution Figures ---------------------------------------------------------------

# Because the get.amplitude function will not calculate the seasonal amplitude for
# years less than need to remove those years from the amplitude distribution from the cmip
# data points because it may not be possible to compare the distributions of the cmip vs obs.
amplitude_df %>%
  filter(ensemble == "obs") %>%
  select(variable, basin, year) %>%
  mutate(KEEP = TRUE) ->
  obs_amp_df


# Join the abs_amp_df and the amplitude data frame together and discard the mis matching values.
amplitude_df %>%
  full_join(obs_amp_df,  by = c("variable", "basin", "year")) %>%
  filter(KEEP) %>%
  add.obs_cmip_id %>%
  mutate(experiment = id) %>%
  group_by(basin, variable) %>%
  do(distribution = vis.amplitude_density(.)) ->
  out

# Add the amplitude distribution figures to the figures data fame
time_figs_df %>%
  left_join(out, by = c("basin", "variable")) ->
  FIGURES_df


# Mean Monthly Line Plots --------------------------------------------------------------------

# Montly mean line graphs, are only useful when there is a large sample size for the observational
# data points.
monthly_df %>%
  group_by(basin, variable) %>%
  do(monthly_figs = vis.obs_monthly_mean(.)) %>%
  # Add to the figures data frame
  left_join(FIGURES_df,  by = c("basin", "variable")) ->
  FIGURES_df


# Save -----------------------------------------------------------------------------------------
# going to have to do the by row thing that I did with the models indivudally

# Define the function for saving the figures
save_rda <- function(input_data, path = OUTPUT_DIR){

  basin_name <- unique(input_data$basin)

  # Create an empty list where to save the figures
  out = list()

  # Row for loop or the variable for loop since we know that each row = a unique variable
  for(i in 1:nrow(input_data)){

    data <- input_data[i,]

    vari <- unique(data$variable)

    out[[paste0(vari)]] <- list(monthly_mean = data$monthly_figs[[1]], raw_tseries = data$raw_t[[1]],
                                detrended_tsereis = data$detr_t[[1]], amp_tseries = data$tseries_amplitude[[1]],
                                amp_distribution = data$distribution[[1]])
  }

  save(out, file = paste0(path, "/", basin_name, ".rda"))

  return(out)

}

# Save each basin (obs comparison) as an indiviual output
FIGURES_df %>%
  group_by(basin) %>%
  do(fig = save_rda(.)) ->
  formated_figs

# Function for savin each model's figures as a pdfs
save_pdfs <- function(data, path = OUTPUT_DIR){

 basin_name <- unique(data$basin)

  to_print <- purrr::flatten(purrr::flatten(data$fig))

  pdf(paste0(path, basin_name, ".pdf"))

  for(i in 1:length(to_print)){

    if(!is.null(to_print[i])){ print(to_print[i]) }

  }

  dev.off()

  message("printed plots for ", basin_name)
}


# If print_pdfs (defined in the Environment section of this script) then print all of the figures
# off in pdfs for each model.

if(print_pdfs){

  # Save each model as a separate pdf file
  formated_figs %>%
    group_by(basin) %>%
    do(save_pdfs(.))
}
