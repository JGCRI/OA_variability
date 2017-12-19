# Purpose: This script includes the the figure genreators, large functions that generate a lot of figures at one time.
#
# Created by: Kalyn Dorheim
# Created on: December 14

# generate.figs_multiple_models -------------------------------------------------------------------------------------------

#' Generate the multiple model comparison figures.
#'
#' \code{generate.figs_multiple_models} This function creates time sereis, mean monthly, amplitude densitry, KS and
#' annual extreem figures for mulitple models (more than one model is plotted at a time). All of the figures are saved in a list
#' as .rda objects. I have figured out how I want print the figures in a pdf yet but that is low on my list of things to do.
#'
#' @param INPUT_DIR the directory path to the input data
#' @param OUTPUT_DIR the directory path to where the final figure list will be saved.
#' @param output_name is a string output name for the .rda object saved by the function, should not include the .rda end
#' @param print_pdf an optional logic paramter that controls weather or not the figures will be printed off in a pdf, this capability is not working yet.
#' @import ggplot2
#' @import dplyr
#' @return a flat list of percent agreement heat maps by variable and value type faceted by basin

generate.figs_multiple_models <- function(INPUT_DIR, OUTPUT_DIR, output_name, print_pdf = FALSE){

  # Script Functions ------------------------------------------------------------------------------

  # I think this will be usefull will probably move it to the vis_fxns script
  stats.year_labels <- function(data){

    data %>%
      dplyr::select(experiment, start_year, end_year) %>%
      dplyr::distinct() %>%
      dplyr::mutate(label = paste0(experiment, " : ", start_year, " - ", end_year)) %>%
      dplyr::select(label)

  }


  # Import Data ------------------------------------------------------------------------------

  # Data paths
  raw_path <- list.files(INPUT_DIR, "basin_mean.rda", full.names = TRUE)
  detrended_path <- list.files(INPUT_DIR, "detrended_data.rda", full.names = TRUE)
  summary_path   <- list.files(INPUT_DIR, "summary_stats.rda", full.names = TRUE)
  amplitude_path <- list.files(INPUT_DIR, "amplitude.rda", full.names = TRUE)
  extreme_path   <- list.files(INPUT_DIR, "max_min", full.names = TRUE)
  KS_path <- list.files(INPUT_DIR, "Kurtosis_Skewness", full.names = TRUE)


  # Load the data sets
  raw_data <- get(load(raw_path))
  detrended_data <- get(load(detrended_path))
  summary_data   <- get(load(summary_path))
  amplitude_data <- get(load(amplitude_path))
  extreme_data   <- get(load(extreme_path))

  # Load the list of the K and S dataframes, parse out the dataframes from the list
  KS_list  <- get(load(KS_path))
  KS_data  <- KS_list$K_S
  delta_KS <- KS_list$delta_K_S


  # Time Series Plots ------------------------------------------------------------------------------

  # Raw time series
  tseries_raw <- plot.time_series(raw_data)


  # Detrended time series
  tseries_detrended <- plot.time_series(detrended_data)


  # Amplitude time series
  # In order to plot annual seasonal amplitude change the column names to be compatible with the
  # plot.time_series function
  amplitude_data %>%
    rename(date = year, value = amplitude) %>%
    plot.time_series(subtitle = "Seasonal Amplitude", legend = FALSE) ->
    tseries_amplitude

  amplitude_data %>%
    rename(date = year, value = min) %>%
    plot.time_series(subtitle = "Annual Min Value", legend = FALSE) ->
    tseries_min

  amplitude_data %>%
    rename(date = year, value = max) %>%
    plot.time_series(subtitle = "Annual Max Value", legend = F) ->
    tseries_max


  # Monthly Means ------------------------------------------------------------------------------
  data <- tidyr::spread(summary_data, value_type, value)
  mean_monthly = list()

  var_list <- unique(data$variable)

  for(i in 1:length(var_list)){

    to_plot   <- filter(data, variable == var_list[i])
    yr_labels <- stats.year_labels(to_plot)
    variable  <- unique(to_plot$variable)

    to_plot %>%
      group_by(month, month_name, experiment, ensemble, basin, units) %>%
      rename(value = mean) %>%
      summarise(mean = mean(value), sd = sd(value)) %>%
      ggplot(aes(x = month, y = mean, color = experiment)) +
      geom_line(size = 2) +
      #  geom_ribbon(aes(ymin = mean - 2*sd, ymax = mean + 2*sd, x = month, group = experiment, linetype=NA), alpha = 0.25) +
      facet_wrap(facets = "basin", ncol = 3, scale = "free") +
      theme(text = element_text(size = 13)) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      #  labs(title = "All-Models mean monthly with 2 sigma band",
      #       y = paste0("Mean Detrended ", variable),
      labs(title = "All-Models mean monthly", y = paste0("Mean Detrended ", variable),
           caption = paste0(yr_labels[1,], "\n", yr_labels[2,], "/n/n")) +
      scale_x_discrete(limits = to_plot$month, labels = to_plot$month_name) +
      oceanpH::MY_SETTINGS ->
      mean_monthly[[paste0(variable)]]

  } # end of plotting for loop



  # Amplitude Distribution ---------------------------------------------------------------------
  amp_density_figs <- vis.amplitude_density(amplitude_data)


  # K / S Scatter Plots ----------------------------------------------------------------------

  # The K and S historical vs future K and S scatter plots, visualized by basin, variable,
  # (and should probably add by model....)
  KS_scatter_figs <- vis.KS_scatter_plots(KS_data)


  # Delta K and S scatter plots
  delta_KS_fig <- vis.delta_KS(delta_KS, boxplot = TRUE)



  # Annual Extreme Heat Maps ----------------------------------------------------------------

  # Start by calcualting the percent of max or min ocurring in each month by
  # year / basin / variable. Count the number of observations in each month
  # and divide by number of observations in each year.
  extreme_data %>%
    group_by(year, basin, variable, month_name, value_type) %>%
    summarise(count = n()) %>%
    ungroup %>%
    group_by(year, basin, variable, value_type) %>%
    mutate(percent = 100 * count / sum(count)) %>%
    ungroup ->
    percent_data

  # Since the percent_data only contains observations for the months of annual
  # extemes the figures will not show a full 12 month annual cycle. In order to
  # include all of the months in the figures, create at data frame for year 2101
  # and add to the data frame to plot.
  percent_data %>%
    select(basin, variable, value_type) %>%
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
  extreme_figs <- vis.annual_extremes(percent_data_2101)




  # Save Plots ------------------------------------------------------------------------------

  # Combine the time series plots into a list
  time_series = list(detrended = tseries_detrended, raw = tseries_raw, amplitude = tseries_amplitude,
                     min = tseries_min, max = tseries_max)

  # Save all of the figures in a single list
  all_models = list(time_series = time_series,
                    mean_monthly = mean_monthly,
                    distribution = amp_density_figs,
                    KS_scatter_figs = KS_scatter_figs,
                    delta_K = delta_KS_fig,
                    annual_extremes = extreme_figs)

  # Save figures asll .rda object
  rda_output_name <- paste0(output_name, ".rda")
  save(all_models, file = file.path(OUTPUT_DIR, rda_output_name))
  message("figures saved at ",  file.path(OUTPUT_DIR, rda_output_name))

  # If applicable save as a pdf
  if(print_pdf){

    message("you fool you have not figured out how to do this yet!")
  }
}


# generate.figs_models_individually -------------------------------------------------------------------------------------------

#' Generate the figures for all of the models indvidually
#'
#' \code{generate.figs_models_individually} This function creates time sereis, mean monthly, amplitude densitry, KS and
#' annual extreem figures for mulitple models (more than one model is plotted at a time). All of the figures are saved in a list
#' as .rda objects. I have figured out how I want print the figures in a pdf yet but that is low on my list of things to do.
#'
#' @param INPUT_DIR the directory path to the input data
#' @param OUTPUT_DIR the directory path to where the final figure list will be saved.
#' @param output_name is a string output name for the .rda object saved by the function, should not include the .rda end
#' @param print_pdf an optional logic paramter that controls weather or not the figures will be printed off in a pdf, this capability is not working yet.
#' @import ggplot2
#' @import dplyr
#' @return a tibble of models and figures

generate.figs_models_individually <- function(INPUT_DIR, OUTPUT_DIR, output_name, print_pdfs = FALSE){

  # Import data frames to plot
  raw_path <- list.files(INPUT_DIR, "basin_mean.rda", full.names = TRUE)
  detrended_path <- list.files(INPUT_DIR, "detrended_data.rda", full.names = TRUE)
  summary_path   <- list.files(INPUT_DIR, "summary_stats.rda", full.names = TRUE)
  amplitude_path <- list.files(INPUT_DIR, "amplitude.rda", full.names = TRUE)
  kurotsis_path  <- list.files(INPUT_DIR, "Kurtosis", full.names = TRUE)
  max_min_path   <- list.files(INPUT_DIR, "max_min", full.names = TRUE)

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

  save(FIGURES, file = file.path(OUTPUT_DIR, output_name))
  message("output saved at ", file.path(OUTPUT_DIR, output_name))

  # # Print pdfs--------------------------------------------------------------------------------------
  # # There is probably a better way to do this but it is low on my priority list.
  #
  # if(print_pdfs){ # Define the function for saving the figures
  #   flatten_function <- function(data){
  #
  #     model_name <- unique(data$model)
  #
  #     data %>%
  #       select(-model) %>%
  #       purrr::flatten(.) ->
  #       intermediate
  #
  #     out = list(raw_tseries = intermediate$raw_tseries, detrended_tseries = intermediate$detrended_tseries,
  #                amplitude_tseries = intermediate$amplitude_tseries, monthly_mean = intermediate$monthly_mean,
  #                amplitude_density = intermediate$amplitude_density, deltaKS_scatter = data$delatKS_scatter,
  #                KS_scatter = intermediate$KS_scatter,
  #                extreme_heat_map = intermediate$extreme_fig)
  #
  #     return(out)
  #
  #   }
  #
  #   FIGURES %>%
  #     group_by(model) %>%
  #     do(fig = flatten_function(.)) ->
  #     formated_figs
  #
  #   # Function for savin each model's figures as a pdfs
  #   save_pdfs_at <- file.path(OUTPUT_DIR, "pdfs")
  #   if(!file.exists(save_pdfs_at)){dir.create(save_pdfs_at, recursive = T)}
  #
  #   save_pdfs <- function(data, path = save_pdfs_at){
  #
  #     model_name <- unique(data$model)
  #
  #     to_print <- purrr::flatten(purrr::flatten(data$fig))
  #
  #     pdf(paste0(path, model_name, ".pdf"))
  #
  #     for(i in 1:length(to_print)){
  #       print(to_print[i])
  #     }
  #
  #     dev.off()
  #
  #     message("printed plots for ", model_name)
  #   }
  #
  #
  #   # If print_pdfs (defined in the Environment section of this script) then print all of the figures
  #   # off in pdfs for each model.
  #
  #   if(print_pdfs){
  #
  #     # Save each model as a separate pdf file
  #     formated_figs %>%
  #       group_by(model) %>%
  #       do(save_pdfs(.))}
  #   }
  #
  #
  # message("function complete")

}


# generate.robustness_index -------------------------------------------------------------------------------------------

#' Generate the data and figures related to the robustness analysis
#'
#' \code{generate.robustness_index} This calculates the robustness stats, saves the data in kables and also
#' plots the "cloud" robustness index time series.
#'
#' @param amplitude_data_path path to the amplitude data set
#' @param output_path the directory location where to save the output
#' @param output_rda is a string used in naming the output, shoudl end with a .rda of some sort
#' @import ggplot2
#' @import dplyr
#' @import knitr
#' @return a list of figres, kables, and data frames

generate.robustness_index <- function(amplitude_data_path, output_path, output_rda){

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
      facet_wrap("basin", scales = "free", ncol = 3) +
      labs(title = paste0("Interannual Robustness\n", vari),
           y = paste0(vari, " amplitude"),
           caption = "The shaded grey region is equal to mean +/- sd \n\n") +
      oceanpH::MY_SETTINGS
  }


  kable.robustness <- function(data, caption = ""){

    # Save the variable name for use in the caption
    vari <- unique(data$variable)

    # Drop the variable column and then make the kable
    data %>%
      select(-variable) %>%
      knitr::kable(format = "html", caption = paste0(vari, " ", caption)) %>%
      kableExtra::kable_styling("striped", full_width = T)
  }



  # Robustness/Sameness Index  ------------------------------------------------------------------------------

  # Import the amplitude data
  data <- get(load(amplitude_data_path)) %>%  ungroup


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
    group_by(model, variable, basin) %>%
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
    select(model, variable, basin, percent_within, percent_above, percent_below) ->
    percent_robustness_df


  # Line Graphs ------------------------------------------------------------------------------

  # This section makes line graphs of the amplitude distribution, mean values, and
  # single models at a time
  #
  # Start by selecting the the data to plot
  amp_robustness_df %>%
    select(year, units, ensemble, experiment, variable, basin, model, amplitude,
           amp_mean, lower_bound, upper_bound, model) ->
    to_plot

  # For each variable / basin create a robustness line graph.
  to_plot %>%
    group_by(variable) %>%
    do(fig = vis.robustness(.)) ->
    robustness_figs_df

  # Save the figures in a list named by the
  robustness_figs <- setNames(object = robustness_figs_df$fig, nm = robustness_figs_df$variable)



  # Sameness Kables ------------------------------------------------------------------------------

  # Format the data to make into the table. Control the number of digits and
  # select which percent to spread.

  # Within the distribution percent kable
  percent_robustness_df %>%
    mutate(percent_within = signif(percent_within, 3)) %>%
    select(basin, model, variable, percent_within) %>%
    spread(basin, percent_within) ->
    to_table

  # Use the kable.robustness function to create the robustness kables
  to_table %>%
    group_by(variable) %>%
    do(kable = kable.robustness(., "% within the distribution")) ->
    robustness_kable_df

  within_distribution <- setNames(robustness_kable_df$kable, nm = robustness_kable_df$variable)


  # Percent above the distribution
  percent_robustness_df %>%
    mutate(percent_above = signif(percent_above, 3)) %>%
    select(basin, model, variable, percent_above) %>%
    spread(basin, percent_above) ->
    to_table

  # Use the kable.robustness function to create the robustness kables
  to_table %>%
    group_by(variable) %>%
    do(kable = kable.robustness(., "% above the distribution")) ->
    robustness_kable_df

  above_distribution <- setNames(robustness_kable_df$kable, nm = robustness_kable_df$variable)



  # Percent below the distribution
  percent_robustness_df %>%
    mutate(percent_below = signif(percent_below, 3)) %>%
    select(basin, model, variable, percent_below) %>%
    spread(basin, percent_below) ->
    to_table

  # Use the kable.robustness function to create the robustness kables
  to_table %>%
    group_by(variable) %>%
    do(kable = kable.robustness(., "% blow the distribution")) ->
    robustness_kable_df

  below_distribution <- setNames(robustness_kable_df$kable, nm = robustness_kable_df$variable)


  # Percent Within Bar Plots ------------------------------------------------------------------------------

  percent_robustness_df %>%
    ggplot(aes(x = basin, y = percent_within,  fill = model, group = model)) +
    geom_col(position = "dodge") +
    facet_wrap("variable") +
    MY_SETTINGS +
    labs(title = "Percent Within Robust Distribution \nby Model",
         y = "% of total amplitude time series within ") ->
    percent_within_bar


  # Which Model(s) Most Consistent ------------------------------------------------------------------------

  percent_robustness_df %>%
    select(model, variable, basin, percent_within) %>%
    group_by(basin, variable) %>%
    filter(percent_within == max(percent_within)) %>%
    ungroup %>%
    group_by(model) %>%
    summarise(count = n()) %>%
    arrange(count) %>%
    knitr::kable() ->
    count_within

  percent_robustness_df %>%
    select(model, variable, basin, percent_within) %>%
    group_by(model) %>%
    summarise(mean = mean(percent_within)) %>%
    arrange(mean) %>%
    knitr::kable(digits = 2) ->
    mean_percent_within



  # Final Formating and save Script Products --------------------------------------------------------------

  out = list(within = within_distribution,
             above = above_distribution,
             below = below_distribution,
             figures = robustness_figs,
             fig_bar = percent_within_bar,
             count = count_within,
             mean = mean_percent_within)

  # save(out, file = paste0(OUTPUT_DIR,"/amplitude_sameness.rda"))
  save(out, file = file.path(output_path, output_rda))
  message("output saved as ", file.path(output_path, output_rda))

}

# End -----
