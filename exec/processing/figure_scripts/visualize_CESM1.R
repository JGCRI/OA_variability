# ------------------------------------------------------------------------------
# Purpose: This script looks at the observations for the complete data set.
#
# Created by: Dorheim, Kalyn
# Created on: October 11 2017
# Modified:   xxx
#
# Notes:
# ------------------------------------------------------------------------------
# Environment
# ------------------------------------------------------------------------------
library(dplyr); devtools::load_all(); library(ggplot2)

# ------------------------------------------------------------------------------
# Script Functions
# ------------------------------------------------------------------------------
# I think this will be usefull will probably move it to the vis_fxns script
stats.year_labels <- function(data){

  data %>%
    dplyr::select(experiment, start_year, end_year) %>%
    dplyr::distinct() %>%
    dplyr::mutate(label = paste0(experiment, " : ", start_year, " - ", end_year)) %>%
    dplyr::select(label)

}


# ------------------------------------------------------------------------------
# Load Data Sets
# ------------------------------------------------------------------------------
# Find the path to the basin mean .rda file created by scripts from the raw-data subdir
data_paths <- list.files(path = "data", pattern = "CESM1_", full.names = TRUE)


# Load the data frames to plot and make sure the only model included is CESM1-BGC
#
# Get the raw, trended or undtrended data from
raw <- data_paths[which(grepl(pattern = "CESM1_trended_", x = data_paths) == TRUE)]
  get(load(raw)) %>%
    filter(grepl(x = model, pattern = "CESM1-BGC")) ->
    raw_data

# Get the detrended .rda object for the CESM1 model
detrended <- data_paths[which(grepl(pattern = "CESM1_detrened", x = data_paths) == TRUE)]
  get(load(detrended)) %>%
    filter(grepl(x = model, pattern = "CESM1-BGC")) ->
    detrened_data

# Get the summary stats .rda object for the CESM1 model
stats <- data_paths[which(grepl(pattern = "summary_stats", x = data_paths) == TRUE)]
  get(load(stats)) %>%
    filter(grepl(x = model, pattern = "CESM1-BGC")) %>%
    tidyr::unnest() ->
    stats_data

# Get the amplitude .rda object for the CESM1 model
amp_path <- data_paths[which(grepl(pattern = "amplitude", x = data_paths) == TRUE)]
  get(load(amp_path)) %>%
    filter(grepl(x = model, pattern = "CESM1-BGC")) ->
    ampltidue_data



# ------------------------------------------------------------------------------
# Figures Code
# ------------------------------------------------------------------------------
# Time Series Plots
# ------------------------------------------------------------------------------
# Raw and detrended data
raw <- plot.time_series(raw_data)
detrended <- plot.time_series(detrened_data)


# ------------------------------------------------------------------------------
# Monthly Means
# ------------------------------------------------------------------------------
data <- tidyr::spread(stats_data, value_type, value)
mean_monthly = list()

var_list <- unique(data$variable)
for(i in 1:length(var_list)){

  to_plot   <- filter(data, variable == var_list[i])
  yr_labels <- stats.year_labels(to_plot)
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
# CESM Amplitude Time Series
# ------------------------------------------------------------------------------
amplitude = list()
var_list  <- unique(ampltidue_data$variable)


for(i in 1:length(var_list)){

  to_plot   <- dplyr::filter(ampltidue_data, variable == var_list[i])
  variable  <- unique(to_plot$variable)

  to_plot %>%
    ggplot(aes(x = year, y = amplitude, color = experiment)) +
    geom_line(size = 1.2) +
    facet_wrap(facets = "basin", ncol = 3, scales = "free" ) +
    theme(text = element_text(size = 13)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(x = "Year",
         y = "Seasonal Amplitude",
         title = paste0("CESM1-BGC ", variable, " Amplitude Time Series")) ->
    amplitude[[paste0(variable)]]

} # end of plotting for loop


# ------------------------------------------------------------------------------
# CESM Amplitude Distribution
# ------------------------------------------------------------------------------
amplitude_distribution = list()
var_list  <- unique(ampltidue_data$variable)


for(i in 1:length(var_list)){

  to_plot   <- dplyr::filter(ampltidue_data, variable == var_list[i])
  variable  <- unique(to_plot$variable)

  to_plot %>%
    ggplot(aes(x = amplitude, fill = experiment)) +
   # geom_histogram(bins = 40, alpha=.5, position="identity") +
    geom_density(alpha=.5) +
   # stat_density() +
    facet_wrap(facets = "basin", ncol = 3, scales = "free") +
    theme(text = element_text(size = 13)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(x = "Seasonal Amplitude",
         y = "Density",
         title = paste0("CESM1-BGC ", variable, " Amplitude Distrubution Plot")) ->
    amplitude_distribution[[paste0(variable)]]

} # end of plotting for loop


# ------------------------------------------------------------------------------
# Save CESM Plots
# ------------------------------------------------------------------------------
# Combine the time series plots into a list
time_series = list(detrended = detrended, raw = raw, amplitude = amplitude)

# Save all of the figures in a single list
FIGS.CESM1 = list(time_series = time_series, mean_monthly = mean_monthly,
                  distribution = amplitude_distribution)
save(FIGS.CESM1, file = "data/figs/FIGS.CESM1.rda")
# ----
# End
