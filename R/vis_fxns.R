# -------------------------------------------------------------------------------------------
# Purpose: This script contains the vis family of functions. All of these
# functions are related to plotting/graphing cmip5 data in some way. This script
# contains the following functions: vis.is_outlier(), vis.exploratory(),
# vis.compare_id(), vis.add_fig_name() & vis.time_series() & plot.time_series()
#
# Created by: Dorheim, Kalyn
# Created on: August 24 2017
# Modified:   xxx
#
# Notes: vis.time_series need a better way to lable the time axis.

# Define my fav figure settings
MY_SETTINGS <- ggplot2::theme(text = ggplot2::element_text(size = 13)) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

# -------------------------------------------------------------------------------------------
# vis.is_outlier()
# ------------------------------------------------------------------------------
#' Identify outliers
#'
#' \code{vis.is_outlier}
#'
#' @param x is the vector to check for outliers, values that lay outside of the 1.5*IQR
#' @return an indicator varibale x if the the value is an outlier
#' @keywords internal

#   %>% dplyr::mutate(outlier = ifelse(is_outlier(value), paste0(model), NA)) %>%

vis.is_outlier <- function(x){
  return(x <= quantile(x, 0.25) - 1.5 * IQR(x) | x >= quantile(x, 0.75) + 1.5 * IQR(x))
}

# -------------------------------------------------------------------------------------------
# vis.compare_id()
# -------------------------------------------------------------------------------------------
#' Compare differnt data frames by the "id" variable
#'
#' \code{vis.compare_id} This function generates line plots based on some variable termed id
#'
#' @param df a data frame containing the information to plot
#' @importFrom dplyr %>%
#' @return A list containing a figure that compares the two data sets based on the id paramter

vis.compare_id <- function(df){

  # Set up
  #
  # My preferred figure settings
  my_settings  <- ggplot2::theme(text = element_text(size = 13)) +
    ggplot2::theme(axis.text.x = element_text(angle = 45, hjust = 1))


  # Drop any levels
  df <- droplevels(df)


  # Check requirements
  req_cols <- c("time", "value", "units", "id")
  missing  <- !req_cols %in% colnames(df)
  if(any(missing)){
    stop(paste0(req_cols[which(missing == TRUE)], sep = " "), "are missing from the data frame")
  }

  df %>%
    dplyr::ungroup() %>%
    dplyr::mutate(id = as.character(id)) %>%
    unique ->
    df

  # Collect the label information
  exp <- unique(df$experiment)
  en  <- unique(df$ensemble)
  var <- unique(df$variable)
  uni <- unique(df$units)

  # Make the figure that compares the different responses for the pulses in the
  # different years.
  df %>%
    ggplot2::ggplot(aes(x = time, y = value, fill = id, color = id)) +
    ggplot2::geom_line(size = 1.5) +
    ggplot2::labs(y = paste0(var," ", uni)) +
    my_settings +
    ggplot2::labs(caption = paste0(exp, "\n", en),
                  title = paste0("Compare some id\n add more info")) ->
    fig; fig

  # Save the figure for output.
  out <- list(fig = fig, data = df)

  # Return
  return(out)

} #end of vis.compare_id

# -------------------------------------------------------------------------------------------
# vis.add_fig_name()
# -------------------------------------------------------------------------------------------
#' Add names to figures stored in a list
#'
#' \code{vis.add_fig_name} This function adds names to figures that are stored in a list
#'
#' @param fig_list an object that contains multiple lists that are un-named, requires a column of names
#' @return A list of named figures

vis.add_fig_name <- function(fig_list){
  check.column(fig_list, "figure list", c("fig", "name"))
  stats::setNames(fig_list$fig, fig_list$name)

} # end of vis.add_fig_name


# -------------------------------------------------------------------------------------------
# internal.time_series
# -------------------------------------------------------------------------------------------
#' Make a figure of a concatenated time series
#'
#' \code{internal.time_series} This function adds names to figures that are stored in a list
#'
#' @param df aa data frame with information to plot
#' @return A figure of a timeseries


# To do i need to figure out a better way to label the x axis

internal.time_series <- function(input_df, subtitle = ""){

  # Checks
  #
  # Check the data frame for the required columns
  req_cols <- c("time", "units", "value", "model",  "experiment", "variable",
                "ensemble", "method", "basin")
  check.column(input_df, "to_plot", req_cols)

  df <- input_df

  # Set Up
  # -
  # Define the empty list to save the figures in.
  out_list = list()

  # Define my fav figure settings
  my_settings <- ggplot2::theme(text = element_text(size = 13)) +
    ggplot2::theme(axis.text.x = element_text(angle = 45, hjust = 1))


  var     <- unique(df$variable)
  units   <- unique(df$units)
  en      <- unique(df$ensemble)
  method  <- unique(df$method)

  df %>%
    ggplot(aes(x = time, y = value, group = model, color = model)) +
    geom_line(size = 1.2) +
    my_settings +
    facet_wrap(facets = "basin", ncol = 3, scales = "free") +
    labs(y = paste0(var," ", units), title = "Time Series",
         caption = paste0("Historical and future experiments plotted together for aesthetic purposes only"),
         subtitle = subtitle)

} # end of the vis.time_series


# -------------------------------------------------------------------------------------------
# plot.time_series
# ------------------------------------------------------------------------------
#' Genreate time series plots for the fld weighted means directly from pic
#'
#' \code{plot.time_series} for the internal basin mean dat aobject
#'
#' @param write_pdf a logical statement to write output as a pdf or not
#' @param data the data plot to use to make the figures
#' @param write_pdf a logical statement for writing the pdf or not
#' @param file_name is the name of the file created
#' @import dplyr
#' @import ggplot2
#' @import tidyr
#' @return a data frame with all of the meta information
#' @keywords internal

plot.time_series <- function(data, subtitle = ""){

  out = list()
  var_list <- unique(data$variable)

  for(i in 1:length(var_list)){

    to_plot   <- dplyr::filter(data, variable == var_list[i])
    variable  <- unique(to_plot$variable)

    out[[paste0(variable)]] <- internal.time_series(to_plot, subtitle)

  }

  return(out)
}

# -------------------------------------------------------------------------------------------
# vis.amplitude() - FLAG FOR REMOVAL
# ------------------------------------------------------------------------------
#' Line plots for a monthly amplitude data frame
#'
#' \code{vis.amplitude} for a single monthly amplitude data frame create a monthly
#' line plot
#'
#' @param input a single cmip observation amplitude data frame
#' @import ggplot2
#' @importFrom dplyr %>%
#' @return a list of figures of all of the monthly value types
vis.amplitude <- function(input){
  to_plot <- tidyr::unnest(input)
  info    <- cmip.meta(to_plot)

  type_list <- info$value_type

  info %>%
    dplyr::select(-value_type) %>%
    dplyr::distinct() ->
    info

  if(nrow(info) > 1) {
    stop("too many cmip observations")
  }

  out = list()

  # Use a for loop to generate a plot for each of the
  # value types.
  for(i in 1:length(type_list)){

    type <- type_list[i]

    to_plot %>%
      dplyr::filter(value_type == type) %>%
      dplyr::mutate(value = as.numeric(value)) %>%
      ggplot(aes(x = month, y = value, color = model)) +
      geom_line(size = 1.2) +
      theme(text = element_text(size = 13)) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      theme(legend.position = "none") +
      scale_x_discrete(limits = month_name_df$month, labels = month_name_df$month_name) +
      labs(y = paste0(type, " ", info$variable, " ", info$units),
           x = "Month",
           title = paste0("Monthly ", type, "\n",
                          info$basin, " ", info$model, " ", info$experiment, " ", info$ensemble)) ->
      out[[paste0("Monthly_",type)]]

  } # end of the value type for loop

  return(out)

} # end of the vis.amplitude function
# -------------------------------------------------------------------------------------------
# vis.monthly_mean()
# -------------------------------------------------------------------------------------------
#' Line plots for monthly mean values
#'
#' \code{vis.monthly_mean} visualize the monthly mean values with error bars
#'
#' @param data a summary stats data frame with desired grouping information for figures
#' @import ggplot2
#' @importFrom dplyr %>%
#' @return a list of figures of all of the monthly line plots

vis.monthly_mean <- function(data){

  # First format the input data frame into something we can print. Format the input data set
  data <- tidyr::spread(data, value_type, value)

  # Define the internal function for creating the year labels
  stats.year_labels <- function(data){

    data %>%
      dplyr::select(experiment, start_year, end_year) %>%
      dplyr::distinct() %>%
      dplyr::mutate(label = paste0(experiment, " : ", start_year, " - ", end_year)) %>%
      dplyr::select(label)

  } # end of internal function


  # Save the model name label.
  model_label <- unique(data$model)
  if(length(model_label) > 1){model_label <- "All models"}

  # Create the list where to store the outputs.
  out = list()

  # Save the variable list for the for loop
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
      labs(title = paste0(model_label , " mean monthly with 2 sigma band"),
           y = paste0("Mean Detrended ", variable),
           caption = paste0(yr_labels[1,], "\n", yr_labels[2,])) +
      scale_x_discrete(limits = to_plot$month, labels = to_plot$month_name) ->
      out[[paste0(variable)]]

  } # end of variable for loop

  # Return output
  return(out)
}




# -------------------------------------------------------------------------------------------
# vis.amplitude_density()
# -------------------------------------------------------------------------------------------

vis.amplitude_density <- function(data){

  # Create an empty list to save the different variable figures in
  out = list()

  # Save the model name label.
  model_label <- unique(data$model)
  if(length(model_label) > 1){model_label <- "All models"}

  # Save the variable list for the variable for loop.
  var_list <- unique(data$variable)

  # Variable plots for loop, that plots each variable individually.
  for(i in 1:length(var_list)){

    to_plot   <- dplyr::filter(data, variable == var_list[i])
    variable  <- unique(to_plot$variable)

    to_plot %>%
      ggplot(aes(x = amplitude, fill = experiment)) +
      geom_density(alpha=.5, na.rm = TRUE) +
      facet_wrap(facets = "basin", ncol = 3, scales = "free") +
      theme(text = element_text(size = 13)) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(x = "Seasonal Amplitude",
           y = "Density",
           title = paste0(model_label, " ", variable, " Amplitude Distrubution Plot")) ->
      out[[paste0(variable)]]

  } # end of plotting for loop

  # Return the list of figures
  return(out)

}


# -----
# End
