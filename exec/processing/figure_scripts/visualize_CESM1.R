# ------------------------------------------------------------------------------
# Purpose: This script looks at all of the CESM1 data objects.
#
# Created by: Dorheim, Kalyn
# Created on: October 11 2017
# Modified:   xxx
#
# Notes:
# ------------------------------------------------------------------------------
# Environment
# ------------------------------------------------------------------------------
library(dplyr); devtools::load_all()

# Find the path to the basin mean .rda file created by scripts from the raw-data subdir
data_paths <- list.files(path = "data", pattern = "CESM1_", full.names = TRUE)


# Load the data frames to plot and make sure the only model included is CESM1-BGC

# Get the raw, trended or undtrended data from
raw <- data_paths[which(grepl(pattern = "CESM1_trended_", x = data_paths) == TRUE)]
  get(load(raw)) %>%
    filter(grepl(x = model, pattern = "CESM1-BGC")) ->
    raw_data

# Get the detrended .rda object for the CESM1 model
detrended <- data_paths[which(grepl(pattern = "detrened_", x = data_paths) == TRUE)]
  get(load(detrended)) %>%
    filter(grepl(x = model, pattern = "CESM1-BGC")) ->
    detrended_data

# Get the smplitude .rda object for the CESM1 model
amplitude <- data_paths[which(grepl(pattern = "amplitude", x = data_paths) == TRUE)]
  get(load(amplitude)) %>%
    filter(grepl(x = model, pattern = "CESM1-BGC")) %>%
    tidyr::unnest() ->
    amplitude_data

# ------------------------------------------------------------------------------
# Raw Time Series Plots (Sanity Check)
# ------------------------------------------------------------------------------
# Most likely will not use we want to check that the pH is what we expected.
raw <- plot.time_series(raw_data)

# ------------------------------------------------------------------------------
# Detrended Time Series Plots
# ------------------------------------------------------------------------------
detrended <- plot.time_series(detrened_data)

# Combine the time series plots into a list
time_series = list(detrended = detrended, raw = raw)

# ------------------------------------------------------------------------------
# Plotting Functions Used in this script only
# ------------------------------------------------------------------------------
# I think this will be usefull will probably move it to the vis_fxns script
amp.year_labels <- function(data){

  data %>%
    dplyr::select(experiment, start_year, end_year) %>%
    dplyr::distinct() %>%
    dplyr::mutate(label = paste0(experiment, " : ", start_year, " - ", end_year)) %>%
    dplyr::select(label)

} #amp.year_labels


# ------------------------------------------------------------------------------
# CESM mean monthly line plots
# ------------------------------------------------------------------------------
data <- tidyr::spread(amplitude_data, value_type, value)
mean_monthly = list()

var_list <- unique(data$variable)
for(i in 1:length(var_list)){

  to_plot   <- filter(data, variable == var_list[i])
  yr_labels <- amp.year_labels(to_plot)
  variable  <- unique(to_plot$variable)

  to_plot %>%
    ggplot(aes(x = month, y = mean, color = experiment)) +
    geom_line(size = 1.2) +
    geom_ribbon(aes(ymin = mean - 2*sd, ymax = mean + 2*sd, x = month, group = experiment, linetype=NA), alpha = 0.25) +
    facet_wrap(facets = "basin", ncol = 3, scale = "free") +
    theme(text = element_text(size = 13)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title = "CESM1-BGC mean monthly with 2 sigma band",
         y = paste0("Mean Detrended ", variable),
         caption = paste0(yr_labels[1,], "\n", yr_labels[2,])) +
    scale_x_discrete(limits = to_plot$month, labels = to_plot$month_name) ->
    mean_monthly[[paste0(variable)]]

} # end of plotting for loop



# ------------------------------------------------------------------------------
# CESM monthly range line plots
# ------------------------------------------------------------------------------
data <- tidyr::spread(amplitude_data, value_type, value)
monthly_range = list()

var_list <- unique(data$variable)
for(i in 1:length(var_list)){

  to_plot   <- filter(data, variable == var_list[i])
  yr_labels <- amp.year_labels(to_plot)
  variable  <- unique(to_plot$variable)

  to_plot %>%
    ggplot(aes(x = month, y = range, color = experiment)) +
    geom_line(size = 1.2) +
    facet_wrap(facets = "basin", ncol = 3, scale = "free") +
    theme(text = element_text(size = 13)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_x_discrete(limits = to_plot$month, labels = to_plot$month_name) +
    labs(title = "CESM1-BGC monthly range",
         y = paste0("Detrended range ", variable),
         caption = paste0(yr_labels[1,], "\n", yr_labels[2,])) ->
    monthly_range[[paste0(variable)]]

} # end of plotting for loop


# ------------------------------------------------------------------------------
# CESM monthly sd line plots
# ------------------------------------------------------------------------------
data <- tidyr::spread(amplitude_data, value_type, value)
monthly_sd = list()

var_list <- unique(data$variable)
for(i in 1:length(var_list)){

  to_plot   <- filter(data, variable == var_list[i])
  yr_labels <- amp.year_labels(to_plot)
  variable  <- unique(to_plot$variable)

  to_plot %>%
    ggplot(aes(x = month, y = sd, color = experiment)) +
    geom_line(size = 1.2) +
    facet_wrap(facets = "basin", ncol = 3, scale = "free") +
    theme(text = element_text(size = 13)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_x_discrete(limits = to_plot$month, labels = to_plot$month_name) +
    labs(title = "CESM1-BGC monthly standard deviation",
         y = paste0("Detrended standard deviation for ", variable),
         caption = paste0(yr_labels[1,], "\n", yr_labels[2,])) ->
    monthly_sd[[paste0(variable)]]

} # end of plotting for loop

# ------------------------------------------------------------------------------
# Save CESM Plots
# ------------------------------------------------------------------------------
FIGS.CESM1 = list(time_series = time_series, mean_monthly = mean_monthly,
                  monthly_range = monthly_range, monthly_sd = monthly_sd )
save(FIGS.CESM1, file = "data/figs/FIGS.CESM1.rda")
# ----
# End
