# Purpose: building the code to do the AGUoceans figures see https://github.com/JGCRI/OA_variability/issues/17 for
# more details. Also makes the figure requested in https://github.com/JGCRI/OA_variability/issues/21
#
# Created by: Dorheim, Kalyn
# Created on: Jan 8 2018
# Modified:   Jan 24
#
# Notes: need to add the robustness figures for amplitude back in...
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
AGUoceans_SETTINGS <- ggplot2::theme(text = ggplot2::element_text(size = 12)) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
  ggplot2::theme_bw() +
  ggplot2::theme(panel.grid.major =  ggplot2::element_blank(),
                 panel.grid.minor =  ggplot2::element_blank(),
                 axis.line =  ggplot2::element_line(colour = "black"))


# Functions ----------------------------------------------------------------------------------------------

# plot_seasonal_cycle: is a function that makes the CMIP and multi model mean annual cycle. This function
# returns a list that can be visualized using the gridExtra::arrange.grid function, a better enhancement
# would be to just print it but that is extra right now.
plot_seasonal_cycle <- function(data, line_size = 1){

  # Save the basin name to use in the plot title
  basin_name <- unique(data$basin)

  # Make the CMIP annual cycle plot first.
  data %>%
    # Use Interaction to group by both model and experiment so that each
    # CMIP model and experiment is an individual line.
    ggplot(aes(month, value, group = interaction(model, experiment))) +
    geom_line(aes(linetype = experiment, color = model), size = line_size) +
    facet_grid(variable ~ basin, switch = "y", scales = "free") +
    # Aesthetics
    AGUoceans_SETTINGS +
    ggplot2::theme(text = ggplot2::element_text(size = 12)) +
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
    ggplot(aes(month, mean, group = experiment)) +
    geom_line(aes(linetype = experiment, color = experiment), size = line_size) +
    geom_ribbon(aes(ymin = mean - dev, ymax = mean + dev, x = month, fill = experiment), alpha = 0.4) +
    facet_grid(variable ~ basin, switch = "y", scales = "free") +
    # Aesthetics
    AGUoceans_SETTINGS +
    ggplot2::theme(text = ggplot2::element_text(size = 12)) +
    # Labels and legends
    scale_x_discrete(limits = mean_annual_cycle$month, labels = mean_annual_cycle$month_name) +
    labs(y = NULL, x = NULL) +
    # In order to combine the line and the ribbon plot together manually reset the
    # legend.
    scale_color_manual(values = c("black", "blue"),
                       breaks = c("historical", "rcp85"),
                       labels = c("historical", "rcp85")) +
    scale_fill_manual(values = c("black", "blue"),
                      breaks = c("historical", "rcp85"),
                      labels = c("historical", "rcp85")) +
    theme(legend.title = element_blank(),
          legend.position = "none") ->
    mean_panel

  # Combine the plots for now, the figures output from this function will be
  # stored in a multi panel figure using gridExtra
  list(CMIP = CMIP_panel, mean = mean_panel)
}


# calculate_percent_change: is a function that uses the summary data time contrtaints to subset the
# years from the amplitude data frame to calcualte the percent change in the mean for the various
# seasonal statistics
calculate_percent_change <- function(summary_data, amplitude_data){

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
    amplitude_data_30yrs_complete

  # Now calculate the percent change in the average seasonal statistic for every
  # variable / basin / model.
  amplitude_data_30yrs_complete %>%
    tidyr::gather(stat_type, value, mean_amplitude, mean_min, mean_max) %>%
    tidyr::spread(experiment, value) %>%
    mutate(dif = rcp85 - historical) %>%
    # mutate(percent_dif = 100 * dif / abs(historical)) %>%
    mutate(percent_dif = 100 * dif / abs(historical)) %>%
    select(variable, basin, model, stat_type, value = percent_dif)

}


# plot_percent_change: is a function that uses the long format of a percent change data frame and
# to create a boxplot that illustrates the spread of the individual model percent change and
# compares it with the percent change in the multi model mean. THis is meant to be an alternative
# to the percent change tables.
plot_percent_change <- function(percent_change_long, subtitle = NULL){

  cmip_percent_change <- filter(percent_change_long, model != "Multi-Model Mean")

  if(nrow(cmip_percent_change) <= 1){stop("No CMIP data found")}


  # Create the cmip percent change boxplot layer.
  cmip_percent_change %>%
    ggplot(aes(basin, value, fill = stat_type)) +
    geom_boxplot(outlier.shape = 1) ->
    boxplot_layer

  # Add the multi model mean percent change points on top.
  boxplot_layer +
    facet_wrap("variable", scales = "free") ->
    boxplot_points

  # Adjust figure aesthetics.
  boxplot_points +
    AGUoceans_SETTINGS +
    labs(y = "% Change",
         x = NULL,
         title = "Seasonal Stat Percent Change",
         subtitle = subtitle) +
    theme(legend.title = element_blank())

}


# plot_basin_trend: is a function that makes the plots for https://github.com/JGCRI/OA_variability/issues/21, the
# line plot of the trended annual means comparing basins for each variable.
plot_basin_trend <- function(data, line_size = 1, caption = ""){

  # plot_fxn: the internal line plotting function
  plot_fxn <- function(to_plot){

    # Create the y label
    variable_name <- unique(to_plot$variable)
    units_value   <- unique(to_plot$units)
    y_label <- paste0(variable_name, " ", units_value)

    # Create the line plot layer
    to_plot %>%
      ggplot(aes(year, value, color = basin, linetype = basin)) +
      geom_line(size = line_size) ->
      line_plot_layer

    # Add additional aestics
    line_plot_layer +
      AGUoceans_SETTINGS +
      labs(title = "Multi Model Annual Mean Average",
           caption = caption,
           y = y_label)
  }

  # Subset for the selected years
  data %>%
    filter(year >= 1990 & year <= 2100) ->
    data_1990_2100

  # Then determine the multi model annual mean for each basin / variable.
  data_1990_2100 %>%
    group_by(basin, variable, units, year, ensemble) %>%
    summarise(value = mean(value, na.rm = T)) %>%
    ungroup %>%
    mutate(model = "multi model annual average") ->
    multi_model_mean

  multi_model_mean %>%
    group_by(variable) %>%
    do(figure = plot_fxn(.)) ->
    unformated_output

  setNames(object = unformated_output$figure, nm = unformated_output$variable)

}

# plot_robustness: is a function that uses the amplitude_data frame to create line plots of the
# CMIP models amplitudes and compares it with the multi model mean +/- sd
plot_robustness <- function(data){


  vis.robustness <- function(data){

    # Save the variable name
    vari <- unique(data$variable)

    data %>%
      ggplot(aes(x = year, y = amp_mean)) +
      oceanpH::MY_SETTINGS +
      # Add the individual annual model amplitudes to the graph
      geom_line(aes(x = year, y = amplitude, color = model), size = 0.5) +
      # Add the distribution cloud ribbon thing
      geom_ribbon(aes(ymin = lower_bound, ymax = upper_bound, x=year, linetype=NA),
                  alpha = 0.5, color = "grey") +
      geom_line(aes(x = year, y = amp_mean), size = 2) +
      facet_wrap("basin", scales = "free", ncol = 2) +
      labs(title = paste0("Interannual Robustness\n", vari),
           y = paste0(vari, " amplitude"),
           caption = "The shaded grey region is equal to mean +/- sd \n\n") +
      AGUoceans_SETTINGS
  }



  # Then determine the all models average and sd for each year / basin / variable.
  # Calculate the upper and lower bounds of the distribution.
  data %>%
    group_by(year, variable, basin) %>%
    summarise(amp_mean = mean(amplitude), amp_sd = sd(amplitude)) %>%
    mutate(lower_bound = amp_mean - amp_sd, upper_bound = amp_mean + amp_sd) %>%
    ungroup ->
    amp_sum_stats

  # Join the model average and sd by year / basin / variable. Create an indicator
  # variable for weather of not the observation falls within the sameness
  # distribution. Latter on that indicator variable will be quantified as a percent value.
  data %>%
    left_join(amp_sum_stats, by = c("year", "variable", "basin")) %>%
    # Use 1 and 0 as logical indicators in wich 1 = TRUE and 0 = FALSE
    mutate(within = if_else(lower_bound <= amplitude & amplitude <= upper_bound, 1, 0)) %>%
    mutate(above_bounds = if_else(amplitude > upper_bound, 1, 0)) %>%
    mutate(below_bounds = if_else(amplitude < lower_bound, 1, 0)) ->
    amp_robustness_df


  # Calculate the different percents, I think that the most common one will be the binary percent
  # within/out the distribution but the above or below may be interesting as well, but I don't
  # really know. I will calculate them anyway.
  #
  # Find the number of counts for observations and the three indicator variables. These counts will
  # be used to calculate the percent of the observations.
  amp_robustness_df %>%
    #  group_by(model, variable, basin) %>%
    group_by(variable, basin) %>%
    summarise(within_count = sum(within),
              above_bounds_count = sum(above_bounds),
              below_bounds_count = sum(below_bounds),
              obs_count = n_distinct(year)) %>%
    ungroup ->
    amp_observations_count


  # Convert the logical counts from counts to observations.
  amp_observations_count %>%
    mutate(percent_within = 100 * within_count / obs_count,
           percent_above  = 100 * above_bounds_count / obs_count,
           percent_below  = 100 * below_bounds_count / obs_count) %>%
    select(variable, basin, percent_within, percent_above, percent_below) ->
    #select(model, variable, basin, percent_within, percent_above, percent_below) ->
    percent_robustness_df


  # Line Graphs ------------------------------------------------------------------------------

  # This section makes line graphs of the amplitude distribution, mean values, and
  # single models at a time
  #
  # Start by selecting the the data to plot
  amp_robustness_df %>%
    select(year, units, ensemble, experiment, variable, model, basin, amplitude,
           amp_mean, lower_bound, upper_bound) ->
    to_plot

  # For each variable / basin create a robustness line graph.
  to_plot %>%
    group_by(variable) %>%
    do(fig = vis.robustness(.)) ->
    robustness_figs_df

  # Save the figures in a list named by the
  setNames(object = robustness_figs_df$fig, nm = robustness_figs_df$variable)


}


# png_printer: a function that uses a for loop to print all of the figures in a falt list to a
# dir, uses a user defined basename and a the plot index name to name the .png
png_printer <- function(plot_list, basename, output_dir){

  plot_list_names <- names(plot_list)

  # Save all of the basin trends
  for(i in 1:length(plot_list)){

    file_name <- paste0(basename, "_", plot_list_names[i], ".png")
    plot <- plot_list[[i]]
    ggsave(filename = file.path(output_dir,file_name), plot = plot)

  }
}

# Load Data -------------------------------------------------------------------------------

# Import the trended data, as of now the trended data set should be the only data set that
# includes the Global basin
trened_path <- list.files(INPUT_DIR, "basin_mean.rda", full.names = T)
trened_data <- get(load(trened_path))

# Import the detrended data
detrened_path <- list.files(INPUT_DIR, "detrended_data.rda", full.names = T)
detrened_data <- get(load(detrened_path)) %>% filter(basin != "Global")

# Import the summary stat data frame that contains monthly mean values from the
# 30 year period.
path <- list.files(file.path(INPUT_DIR), "summary_stats", full.names = T)
summary_data <- get(load(path)) %>%  filter(basin != "Global")


# Import the amplitude data, seasonal stats will be summarised as percent change
# and ploted as a box plot
amplitude_path <- list.files(INPUT_DIR, "amplitude.rda", full.names = T)
amplitude_data <- get(load(amplitude_path)) %>%  filter(basin != "Global")


# Make Figures ------------------------------------------------------------------------------

# Seaonal Cycle
#
# Make the figures similar to Zhao and Zeng of the mean annual cycle from the 30 year
# periods. NOTE --- I do not know how to create the common legend yet. But there is an
# example on https://cran.r-project.org/web/packages/egg/vignettes/Ecosystem.html?utm_content=buffercef33&utm_medium=social&utm_source=twitter.com&utm_campaign=buffer

# Use the MEAN VALUES to make the individual panels.
summary_data %>%
  filter(value_type == "mean") %>%
  group_by(basin) %>%
  do(fig_4 = plot_seasonal_cycle(.)) ->
  figure_4_no_names

# Set names for figure 4
figure_4 <- setNames(object = figure_4_no_names$fig_4, nm = figure_4_no_names$basin)

ordered_seasonal_cycle <- list(figure_4$`N Pacific`$CMIP, figure_4$`N Pacific`$mean,
                     figure_4$`N Atlantic`$CMIP, figure_4$`N Atlantic`$mean,
                     figure_4$`S Pacific`$CMIP, figure_4$`S Pacific`$mean,
                     figure_4$`S Atlantic`$CMIP, figure_4$`S Atlantic`$mean)

# gridExtra::grid.arrange(grobs = ordered_seasonal_cycle, ncol = 4)

# Percent Change in Mean Seasonal Stat
#
# Frist start by calculating the percent change in the histroical to future seasonal amplitude and seasonal
# extreems.
percent_change <- calculate_percent_change(summary_data, amplitude_data)

# Now make the percent change box plot
fig_percent_change <- plot_percent_change(percent_change)


# Change in Basin Trend
#
# CH requested this figure in https://github.com/JGCRI/OA_variability/issues/21
# A line figure illustrating the change in annual mean in trended data for each variable comparing
# the different basins from 1990 to 2100
fig_basin_trend <- plot_basin_trend(trened_data, line_size = 1.5)


# Amplitude Robustness Figures
#
# How similar are the cmip5 seasonal amplitudes to one another?
fig_robustness <- plot_robustness(amplitude_data)


# Format Output ------------------------------------------------------------------------
# Create an empty list to store the output int
output = list()

output[["seasonal_cycle"]] <- ordered_seasonal_cycle
output[["percent_change"]] <- fig_percent_change
output[["basin_trends"]] <- fig_basin_trend
output[["robustness"]] <- fig_robustness

# Add attributes and save as an rda file
attributes(output)$script_name <- script_name
output_name <- paste0(gsub(".R", "", script_name), ".rda")
save(output, file = file.path(OUTPUT_DIR, output_name))


# Save as PNG
if(save_CH){

  # Define CH's png output directory
  CH_output_dir <- file.path(OUTPUT_DIR, "CH_pngs"); dir.create(CH_output_dir, F)

  png_printer(output$basin_trends, "basin_trends", CH_output_dir)
  png_printer(output$robustness, "robustness", CH_output_dir)


  # Save the multi panneled seasonal cycle figure
  output_name <- file.path(CH_output_dir, "seasonal_cycle.png")

  png(filename = output_name, width = 960, height = 960)
  gridExtra::grid.arrange(grobs = output$seasonal_cycle, ncol = 4)
  dev.off()


}
