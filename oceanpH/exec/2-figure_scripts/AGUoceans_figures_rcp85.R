# Purpose: building the code to do the AGUoceans figures see https://github.com/JGCRI/OA_variability/issues/17 for
# more details.
#
# Created by: Dorheim, Kalyn
# Created on: Jan 8 2018
# Modified:   xxx
#
# Notes: Figures made by this script have not been finalized yet. there are several notes to search for
# fractionalize the make table 1? also write code for a second table that is more consolidated. Smallest CMIP percent change
# Also what about the t test results? where does that fit in?
#
# CH wants the global arrangemetn for figure 3 also wnats to include H+ in the anlaysis, we are
# removing figures 1 and 3 from the project, need to decide what I want to do with the code. idk if I want to
# delet it or make it non functinoal any more... things I need to think about and decide.
#
# Setup Environment ------------------------------------------------------------------------------
# Libraries
library(ggplot2)
library(dplyr)
devtools::load_all()

# Determine script / module name.
script_name <- find_scriptName()

# Save for pngs for CH
save_CH <- F

# Define Directories -- these may need to change
BASE <- getwd()
INPUT_DIR  <- file.path(BASE, "output", "cmip", "AGUoceans_rcp85")
OUTPUT_DIR <- file.path(BASE, "output", "cmip", "AGUoceans_figs_rcp85"); dir.create(OUTPUT_DIR, F)


# Specific script visual settings
AGUoceans_SETTINGS <- ggplot2::theme(text = ggplot2::element_text(size = 16)) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
  ggplot2::theme_bw() +
  ggplot2::theme(panel.grid.major =  ggplot2::element_blank(),
                 panel.grid.minor =  ggplot2::element_blank(),
                 axis.line =  ggplot2::element_line(colour = "black"))


# Functions ----------------------------------------------------------------------------------------------

# make_figure1: makes the first figure for GitHub issue 17 (see there for more details). This function
# will plot a time series of input data with cmip 5 models individually as grey lines and then with the
# median value as a dark black line. Optional inputs include line size and captions.
make_figure1 <- function(data, line_size = 1, caption = ""){

  # Check for the required columns
  req_columns <- c("date", "model", "value", "variable", "basin")
  oceanpH::check.column(data, "", req_columns)

  # Save variable info for label
  vari <- unique(data$variable)

  # Transform the data into date format.
  data <- mutate(data, date = lubridate::ymd(date))

  # Find the median value for the time series.
  data %>%
    group_by(date) %>%
    summarise(value = median(value)) %>%
    ungroup() %>%
    mutate(value_label = "CMIP5 Median", model = "Multi Model Median") ->
    data_median

  # Make the fist layer of the plot with all of the cmip models individually.
  data %>%
    mutate(value_label = "CMIP5 Models") %>%
    ggplot(aes(date, value, group = model)) +
    geom_line(aes(color = "CMIP5 Models"), size = line_size)  +
    AGUoceans_SETTINGS ->
    cmip_models_layer

  # Add the time series median information to the plot.
  cmip_models_layer +
    geom_line(data = data_median, aes(date, value, color = "Multi Model Median"), size = line_size) ->
    cmip_models_median

  # Manipulate the color scheme and legend.
  cmip_models_median +
    # Manually set the desired color characteristics and labels.
    scale_color_manual(name = "model",
                       values = c("grey", "black"),
                       breaks=c("CMIP5 Models", "Multi Model Median"),
                       labels=c("CMIP5 Models", "Multi Model Median")) +
    # Change the number of years shown on the axis.
    scale_x_date(date_breaks = '20 years') +
    # Final figure edits
    theme(legend.title = element_blank(),
          legend.position = "bottom") +
    facet_wrap("basin", ncol = 1) +
    labs(title = vari,
         caption = caption)

}


# make_figure2: makes the second figure for GitHub issue 17 (see there for more details). This function
# make a box plot of the change in K and S for each model by basin / variable. It will also include
# points for the K and S from the multi model median amplitude distribution.
#
# NOTE: Trying to decide if we want to use the mean or the median. I when we make that decision
# then we will see major changes in this specific function.
make_figure2 <- function(cmip_data, median_data, mean_data, caption = ""){

  # Start by making the box plot layer to illustrate the distribution
  # of individual model amplitude change in K and S from the amplitude historical and
  # future periods.
  cmip_data %>%
    ggplot(aes(basin, delta, fill = stat_variable)) +
    # Because we are going to add a layer of the median amplitude change in K and S
    # make sure the outliers are different from the points added in the next layer.
    geom_boxplot(outlier.shape = 1) +
    # Standard visual preferences.
    AGUoceans_SETTINGS ->
    boxplot_layer

  # Add a layer of the median amplitude delta K and S values and facet by variable
  boxplot_layer +
    # Add a point for the multi model mean or median distribution delta K and S
    geom_point(data = mean_data, aes(basin, delta, shape = stat_variable,
                                     group = factor(stat_variable, c("delta  K", "delta  S")),
                                     color = "Multi Model Mean"),
               position = position_dodge(width = 0.75), size = 3) +
    geom_point(data = median_data, aes(basin, delta, shape = stat_variable,
                                     group = factor(stat_variable, c("delta  K", "delta  S")),
                                     color = "Multi Model Median"),
               position = position_dodge(width = 0.75), size = 3) ->
     boxplot_median

  # Now specify what color and legend options, facet by variable, and add labels.
  boxplot_median +
    scale_color_manual(name = "delete this label",
                       values = c("black", "blue"),
                       breaks=c("Multi Model Mean", "Multi Model Median"),
                       labels=c("Multi Model Mean", "Multi Model Median")) +
    facet_wrap("variable") +
    theme(legend.title = element_blank(),
          legend.position = "bottom") +
    labs(x = "Basin",
         y = "Change (Future - Historical)",
         title = "Change in Amplitude Distribution\nKurtosis and Skewness")

}


# make_figure3: makes the third figure for the GitHub issue 17 (see there for more details). This function
# makes a 3 panel faceted figure for each basin / variable of annual min, annual max, and amplitude time
# series.
make_figure3 <- function(data, line_size = 1.5, caption = ""){

  # Save variable info for label
  variable_name <- unique(data$variable)
  basin_name  <- unique(data$basin)

  # Change the data frame format from wide to long.
  data  %>%
    rename(Amplitude = amplitude, `Annual Minimum` = min, `Annual Maximum` = max) %>%
    tidyr::gather(value_type, value, Amplitude, `Annual Minimum`, `Annual Maximum`) ->
    long_data

  # Add factor information for the order time series plots will appear in the
  # faceted figure.
  long_data$value_type <- factor(long_data$value_type, levels = c("Annual Minimum", "Annual Maximum", "Amplitude"), ordered = T)


  # Find the median value for the time series.
  long_data %>%
    # Because we are faceting by the value type so that amplitude, min, and max all appear as separate time
    # series group by year and value_type in order to determine the median value type time series.
    group_by(year, value_type) %>%
    summarise(dev = sd(value), mean = mean(value), value = mean(value)) %>%
    ungroup() %>%
    mutate(value_label = "CMIP5 Median", model = "Multi Model Mean") ->
    data_mean


  # Now start making the figure.
  # Make the fist layer of the plot with all of the cmip models individually.
  long_data %>%
    mutate(value_label = "CMIP5 Models") %>%
    ggplot(aes(year, value, group = model)) +
    geom_line(aes(color = "CMIP5 Models"), size = line_size - 0.5)  +
    # Use the standardized script aesthetic settings.
    AGUoceans_SETTINGS ->
    cmip_models_layer

  # Add the time series median and the sd +/- mean ribbon to the plot.
  cmip_models_layer +
    geom_line(data = data_mean, aes(year, mean, color = "Multi Model Mean"), size = line_size) +
    geom_ribbon(data = data_mean, aes(ymin = mean - dev, ymax = mean + dev, x = year, fill = "Multi Model Mean +/- sd"), alpha = 0.3) ->
    cmip_models_mean

  # Manipulate the color scheme and legend.
  cmip_models_mean +
    scale_color_manual(name = "model",
                       values = c("grey", "black"),
                       breaks=c("CMIP5 Models", "Multi Model Mean"),
                       labels=c("CMIP5 Models", "Multi Model Mean")) +
    theme(legend.title = element_blank(),
          legend.position = "bottom") +
    facet_wrap("value_type", scales = "free", ncol = 1) +
    labs(y = variable_name,
         title = basin_name)

}

# make_figure4: is a function that makes the CMIP vs multi model mean annual cycle.
make_figure4 <- function(data, line_size = 1){

  # Save the basin name to use in the plot title
  basin_name <- unique(data$basin)

  # Make the CMIP annual cycle plot first.
  data %>%
    mutate(variable = paste0(variable, "  ", units)) %>%
    # Use Interaction to group by both model and experiment so that each
    # CMIP model and experiment is an individual line.
    ggplot(aes(month, value, group = interaction(model, experiment))) +
    geom_line(aes(linetype = experiment, color = model), size = line_size) +
    facet_grid(variable ~ basin, switch = "y", scales = "free") +
    # Aesthetics
    AGUoceans_SETTINGS +
    ggplot2::theme(text = ggplot2::element_text(size = 24)) +
    # Labels and legends
    scale_x_discrete(limits = data$month, labels = data$month_name) +
    labs(y = NULL, x = NULL) +
    guides(color = "none") +
    theme(legend.title = element_blank(), legend.position = "none") ->
    CMIP_panel


  # Make the multi model mean annual cycle

  # Start by creating a data frame of the multi model mean annual cycle and related stats.
  # This data frame will be used to make the ribbon on the multi model mean plot.
  data %>%
    group_by(experiment, variable, basin, month, month_name, units) %>%
    summarise(dev = sd(value), mean = mean(value)) %>%
    ungroup %>%
    mutate(model = "Multi Model Mean") ->
    mean_annual_cycle

  # Plot the multi model mean line graph with a variability cloud.
  mean_annual_cycle %>%
    mutate(variable = paste0(variable, "  ", units)) %>%
    ggplot(aes(month, mean, group = experiment)) +
    geom_line(aes(linetype = experiment, color = experiment), size = line_size) +
    geom_ribbon(aes(ymin = mean - dev, ymax = mean + dev, x = month, fill = experiment), alpha = 0.3) +
    facet_grid(variable ~ basin, switch = "y", scales = "free") +
    # Aesthetics
    AGUoceans_SETTINGS +
    ggplot2::theme(text = ggplot2::element_text(size = 24)) +
    # Labels and legends
    scale_x_discrete(limits = mean_annual_cycle$month, labels = mean_annual_cycle$month_name) +
    labs(y = NULL, x = NULL) +
    # In order to combine the line and the ribbon plot together manually reset the
    # legend.
    scale_color_manual(values = c("grey", "blue"),
                       breaks = c("historical", "rcp85"),
                       labels = c("historical", "rcp85")) +
    scale_fill_manual(values = c("grey", "blue"),
                      breaks = c("historical", "rcp85"),
                      labels = c("historical", "rcp85")) +
    theme(legend.title = element_blank(),
          legend.position = "none") ->
    mean_panel

  # Combine the plots for now, the figures output from this function will be
  # stored in a multi panel figure using gridExtra
  list(CMIP = CMIP_panel, mean = mean_panel)
}

# make_figure5: is a function that uses the long format of a percent change data frame and
# to create a boxplot that illustrates the spread of the individual model percent change and
# compares it with the percent change in the multi model mean. THis is meant to be an alternative
# to the percent change tables.
make_figure5 <- function(percent_change_long, subtitle = NULL){

  cmip_percent_change <- filter(percent_change_long, model != "Multi-Model Mean")
  multi_model_percent_change <- filter(percent_change_long, model == "Multi-Model Mean")


  if(nrow(cmip_percent_change) <= 1){stop("No CMIP data found")}
  if(nrow(multi_model_percent_change) <= 1){stop("No multi model mean found")}


  # Create the cmip percent change boxplot layer.
  cmip_percent_change %>%
    ggplot(aes(basin, value, fill = stat_type)) +
    geom_boxplot(outlier.shape = 1) ->
    boxplot_layer

  # Add the multi model mean percent change points on top.
  boxplot_layer +
    # Use position to manually set the points on top of the correct stat type boxplot.
    geom_point(data = multi_model_percent_change, aes(basin, value, shape = stat_type, group = stat_type),
               size = 3, position = position_dodge(width = 0.75)) +
    facet_wrap("variable", scales = "free") ->
    boxplot_points

  # Adjust figure aesthetics.
  boxplot_points +
    AGUoceans_SETTINGS +
    labs(y = "% Change",
         x = NULL,
         title = "Seasonal Stat Percent Change",
         caption = "empty points are the outliers\nthe filled in points represent the multi-model mean",
         subtitle = subtitle) +
    theme(legend.title = element_blank())

}


# format_output_helper: is a function that unfortunately uses a for loop, I really need
# to better figure out how to use purr, anyways the function uses a tibble, a column
# name string, and then a list. The function saves object from the tibble stored in
# the column that matches the column_name string and stores them in the output list.
format_output_helper <- function(tibble, column_name, output){

  # Check to make sure that the column name is in the tibble.
  if(!column_name %in% names(tibble)){stop(column_name, "  is not found.")}

  # Parse out basin / variable / figure information into a flat list.
  for(row in 1:nrow(tibble)){

    # Save the basin and variable name.
    basin <- tibble[["basin"]][[row]]
    vari  <- tibble[["variable"]][[row]]

    # Stor the
    fig   <- tibble[[paste0(column_name)]][[row]]

    output[[paste0(basin)]][[paste0(vari)]][[paste0(column_name)]] <- fig

  }

  # Return output.
  return(output)

}

# png_output: a function that can be used to save the output as individual png files
# as per CH's request.
png_output <- function(tibble, column_name, output_dir){

  # Check to make sure that the column name is in the tibble.
  if(!column_name %in% names(tibble)){stop(column_name, "  is not found.")}

  # Parse out basin / variable / figure information into a flat list.
  for(row in 1:nrow(tibble)){

    # Save the basin and variable name.
    basin <- tibble[["basin"]][[row]]
    vari  <- tibble[["variable"]][[row]]

    # Store the figure
    fig   <- tibble[[paste0(column_name)]][[row]]

    # Determine name
    basin <- gsub(" ", "", basin)
    png_name <- file.path(output_dir, paste0(column_name, "_", basin, "_", vari, ".png"))

    if(column_name == "fig_28"){
      ggsave(filename = png_name, plot = fig)
    } else {
      ggsave(filename = png_name, plot = fig, width = 11)

    }


  }

}

# Figure 1 ------------------------------------------------------------------------------
# Detreneded time series line plots -- I think that these figures may be tossed.

# Import the detrended data
detrened_path <- list.files(INPUT_DIR, "detrended_data.rda", full.names = T)
detrened_data <- get(load(detrened_path))

# # Make figure one for each variable / basin.
# detrened_data %>%
#   group_by(variable, basin) %>%
#   do(fig_1 = make_figure1(.)) ->
#   figures


# Figure 2 ------------------------------------------------------------------------------
# The change in Kurtosis and Skewness box plots. The individual model delta K and S
# values have been calculated as part of the "driver" but we want to add the change
# in K and S for the multi model median or mean ?? distribution. This delta K and S
# value will have to be calculated now. --- we are trying to decide if we want to go
# with mean or median, when we make that decision then this section will change.

# Load the delta K and S tibble now.
KS_path <- list.files(INPUT_DIR, "Kurtosis_Skewness.rda", full.names = T)
KS_list <- (get(load(KS_path)))
cmip_delta_KS <- KS_list$delta_K_S

# Load the amplitude data and use to get the multi model mean delta K and S
amplitude_path <- list.files(INPUT_DIR, "amplitude.rda", full.names = T)
amplitude_data <- get(load(amplitude_path))

# Get the multi model annual mean amplitude for ensemble / experiment / variable and basin.
amplitude_data %>%
  group_by(ensemble, experiment, variable, units, basin, year) %>%
  summarise(amplitude = median(amplitude)) %>%
  ungroup %>%
  # Add a model to describe the value, also to make the function that
  # will calculate K and S and the deltas happy.
  mutate(model = "Multi Model Median") %>%
  get.K_S(.) ->
  median_model_KS_list


amplitude_data %>%
  group_by(ensemble, experiment, variable, units, basin, year) %>%
  summarise(amplitude = mean(amplitude)) %>%
  ungroup %>%
  # Add a model to describe the value, also to make the function that
  # will calculate K and S and the deltas happy.
  mutate(model = "Multi Model Median") %>%
  get.K_S(.) ->
  mean_model_KS_list

median_model_KS <- median_model_KS_list$delta_K_S
mean_model_KS <- mean_model_KS_list$delta_K_S


# Make the figure.
fig_2 <- make_figure2(cmip_data = cmip_delta_KS, median_data = median_model_KS, mean_data = mean_model_KS)


# Figure 3 ------------------------------------------------------------------------------
# Make figure 3 using the amplitude_data tibble, amplitude, min, and max time series for
# every basin / variable combination. Plot the CMIP5 models individually and also a
# multi model mean +/- uncertainty.

amplitude_data %>%
  group_by(basin, variable) %>%
  do(fig_3 = make_figure3(.)) ->
  figure3


# Figure 4 ------------------------------------------------------------------------------
# Make the figures similar to Zhao and Zeng of the mean annual cycle from the 30 year
# periods. NOTE --- I do not know how to create the common legend yet. But there is an
# example on https://cran.r-project.org/web/packages/egg/vignettes/Ecosystem.html?utm_content=buffercef33&utm_medium=social&utm_source=twitter.com&utm_campaign=buffer

# Import the summary stat data frame that contains monthly mean values from the
# 30 year period.
path <- list.files(file.path(INPUT_DIR), "summary_stats", full.names = T)
summary_data <- get(load(path))

# Use the MEAN VALUES to make the individual panels.
summary_data %>%
  filter(value_type == "mean") %>%
  group_by(basin) %>%
  do(fig_4 = make_figure4(.)) ->
  figure_4_no_names

# Set names for figure 4
figure_4 <- setNames(object = figure_4_no_names$fig_4, nm = figure_4_no_names$basin)

ordered_list_fig4 <- list(figure_4$`N Pacific`$CMIP, figure_4$`N Pacific`$mean,
                     figure_4$`N Atlantic`$CMIP, figure_4$`N Atlantic`$mean,
                     figure_4$`S Pacific`$CMIP, figure_4$`S Pacific`$mean,
                     figure_4$`S Atlantic`$CMIP, figure_4$`S Atlantic`$mean)




# Table 1 ------------------------------------------------------------------------------
# This is the first table from https://github.com/JGCRI/OA_variability/issues/19
# it is the precent change in the mean seasonal amp, annual min, or annual max.

# Start by finding the cut off years that will match the summary data frame to determine
# the years to use in percent change calculation.
summary_data %>%
  select(experiment, start_year, end_year) %>%
  distinct ->
  thirty_yr_info


# Save each experiment's start and stop year info separately.
historical_period <- filter(thirty_yr_info, grepl("[H|h]istorical", experiment))
future_period <- filter(thirty_yr_info, grepl("rcp", experiment))

# Subset the amplitude data set so that it only contains amplitude for the years within each experiments
# time span.
amplitude_data %>%
  filter(c(experiment == historical_period$experiment & year >= historical_period$start_year &year <= historical_period$end_year) |
         c(experiment == future_period$experiment & year >= future_period$start_year &year <= future_period$end_year)) ->
  amplitude_data_30yrs


# Now determine each model's mean seasonal statistic. Latter on we will use this data frame
# in to get the percent change for each model.
amplitude_data_30yrs %>%
  select(experiment, variable, basin, model, year, amplitude, min, max) %>%
  group_by(experiment, variable, basin, model) %>%
  summarise(mean_amplitude = mean(amplitude), mean_min = mean(min), mean_max = mean(max)) %>%
  ungroup ->
  mean_all_stats


# Get the Multi Model Mean average seasonal statistic to use in the table, the multi model
# values will be added to the mean_all_stats data frame.
amplitude_data_30yrs %>%
  select(experiment, variable, basin, year, amplitude, min, max) %>%
  group_by(experiment, variable, basin) %>%
  summarise(mean_amplitude = mean(amplitude), mean_min = mean(min), mean_max = mean(max)) %>%
  ungroup %>%
  mutate(model = "Multi-Model Mean") ->
  multi_model_mean_df

# Combine the mean CMIP models and multi model mean data frames into a single data frame.
mean_all_stats %>%
  bind_rows(multi_model_mean_df) ->
  amplitude_data_30yrs_complete


# Now calculate the percent change in the average seasonal statistic for every
# variable / basin / model.
amplitude_data_30yrs_complete %>%
  tidyr::gather(stat_type, value, mean_amplitude, mean_min, mean_max) %>%
  tidyr::spread(experiment, value) %>%
  mutate(dif = rcp85 - historical) %>%
 # mutate(percent_dif = 100 * dif / abs(historical)) %>%
  mutate(percent_dif = 100 * dif / abs(historical)) %>%
  select(variable, basin, model, stat_type, percent_dif) %>%
  tidyr::spread(stat_type, percent_dif) ->
  percent_change_df

# Arrange the percent change data frame and store in a kable.
percent_change_df %>%
  arrange(model) %>%
  group_by(variable) %>%
  do(kable = knitr::kable(., digits = 1, format = "latex", caption = "Percent Change in Mean") %>%
       kableExtra::kable_styling("striped", full_width = T)) ->
  table_1_no_names

table_1 <- setNames(object = table_1_no_names$kable, nm = table_1_no_names$variable)


# Table 2 ------------------------------------------------------------------------------
# This is the second table or consolidated version of table 1.

# Use the percent_change_df that was calculated above. Save the multi model mean
multi_model_mean <- filter(percent_change_df, model == "Multi-Model Mean")

# Now determine the magnitude of each percent change.
percent_change_df %>%
  tidyr::gather(stat_type, value, mean_amplitude, mean_max, mean_min) %>%
  mutate(abs_value = abs(value)) ->
  abs_stat_df

# Use the absolute value of the stat to determine which value is the largest or smallest
# percent change for each basin / variable / seasonal stat.
abs_stat_df %>%
  group_by(basin, variable, stat_type) %>%
  filter(abs_value == min(abs_value)) %>%
  select(-abs_value, -model) %>%
  tidyr::spread(stat_type, value) %>%
  mutate(model = "Smallest observed CMIP5") ->
  smallest_percent_change

# Do the same for the largest percent change.
abs_stat_df %>%
  group_by(basin, variable, stat_type) %>%
  filter(abs_value == max(abs_value)) %>%
  select(-abs_value, -model) %>%
  tidyr::spread(stat_type, value) %>%
  mutate(model = "Largest observed CMIP5") ->
  largest_percent_change


# Combine all of the percent change information data frames together
summary_percent_change <- bind_rows(multi_model_mean, smallest_percent_change, largest_percent_change)

summary_percent_change$model <- factor(summary_percent_change$model,
                                       levels = c("Smallest observed CMIP5", "Largest observed CMIP5", "Multi-Model Mean"),
                                       ordered = T)

summary_percent_change %>%
  arrange(basin, model) %>%
  group_by(variable) %>%
  do(kable = knitr::kable(., digits = 1, format = "latex", caption = "Consolidated Percent Change Table") %>%
       kableExtra::kable_styling(full_width = T) %>%
       kableExtra::collapse_rows(columns = 1:2)) ->
  table_2_no_names

table_2 <- setNames(object = table_2_no_names$kable, nm = table_2_no_names$variable)


# Figure 5  ----------------------------------------------------------------------------

# The tables are limited in the amount of information they can communicate. Use a boxplot to plot the
# change in seasonal stat for all of the CMIP models superimposed with points for the multi model mean
# percent change. The hope is that this figure can capture the spread in magnitude and direction of the percent
# change.

# Create two data frames of the multi model mean and cmip percent change in the long format.
percent_change_long <- tidyr::gather(data = percent_change_df, stat_type, value, mean_amplitude, mean_max, mean_min)

figure_5 <- make_figure5(percent_change_long)



# Format Output ------------------------------------------------------------------------
# Create an empty list to store the output int
output = list()

# Save in a flat list
#output <- format_output_helper(figures, "fig_1", output)
#output <- format_output_helper(figures, "fig_3", output)
output[["fig_2"]] <- fig_2
output[["fig_4"]] <- ordered_list_fig4
output[["table_1"]] <- table_1
output[["table_2"]] <- table_2
output[["fig_5"]] <- figure_5


# Add attributes and save as an rda file
attributes(output)$script_name <- script_name
output_name <- paste0(gsub(".R", "", script_name), ".rda")
save(output, file = file.path(OUTPUT_DIR, output_name))


# Save as PNG
if(save_CH){

  # Define CH's png output directory
  CH_output_dir <- file.path(OUTPUT_DIR, "CH_pngs"); dir.create(CH_output_dir, F)

  # Save all of the fig 1
  png_output(figures, "fig_1", CH_output_dir)

  # Save the close up of fig 1 and the figure 2. Do not use the png_output function
  # because these are already flat lists.
  ggsave(filename = file.path(CH_output_dir, "fig_1_close_up.png"), plot = fig1_close_up_example)
  ggsave(filename = file.path(CH_output_dir, "fig_2.png"), plot = fig_2)

  # Save all of the figure 3
  png_output(figures, "fig_3", CH_output_dir)

  # Save all of the figure 4

  for(grid in 1:length(figure_4)){

    grid_name   <- paste0("fig_4_", names(figure_4)[grid],".png")
    output_name <- file.path(CH_output_dir, grid_name)

    png(filename = output_name, width = 960)
    gridExtra::grid.arrange(figure_4[[grid]])
    dev.off()

  }

}

# End ----
