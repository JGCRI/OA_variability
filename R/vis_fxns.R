# ------------------------------------------------------------------------------
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
# ------------------------------------------------------------------------------
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


# ------------------------------------------------------------------------------
# vis.exploratory()
# ------------------------------------------------------------------------------
#' Make exploratory graphs
#'
#' \code{vis.exploratory} Uses an internal graphing function to create figures for
#' each unique variable/ensemble/experiment combination, I think it can easily be
#' modified so do comparison groups
#'
#' @param input_df is the data frame of processed cmip5 data to plot
#' @return a list containing boxplots and time series of mean monthly data, includes regular
#' and normalized figures.

# Idea: What would be cool is there was a way to control the grouping to
# allow for facet figures... but idk....
vis.exploratory <- function(input_df){

  # Checks
  # -------
  # Check the data frame for the required columns
  req_cols <- c("time", "units", "value", "model",  "experiment", "variable",
                "ensemble", "month", "year", "method")
  check.column(input_df, "to_plot", req_cols)

  df <- input_df

  # Define Internal Graphing Fxn
  # -----------------------------
  internal.func <- function(df){

    # Label Info
    # -----------
    # Save the cmip5 information and the cdo processing to be used in
    # figure labels and captions.
    ex     <- unique(df$experiment)
    en     <- unique(df$ensemble)
    var    <- unique(df$variable)
    units  <- unique(df$units)
    method <- unique(df$method)


    # Set Up
    # -----------
    # Define the empty list to save the figures in.
    list1 = list()
    list2 = list()

    # Define my fav figure settings
    my_settings <- ggplot2::theme(text = element_text(size = 13)) +
      ggplot2::theme(axis.text.x = element_text(angle = 45, hjust = 1))

    month_name_df <- data.frame(month = 1:12,
                                month_name = c("January", "February", "March", "April", "May", "June",
                                               "July", "August", "September", "October", "November", "December"))
    month_name = c("January", "February", "March", "April", "May", "June",
                   "July", "August", "September", "October", "November", "December")

    # Mean monthly data
    # -----------------
    df %>%
      group_by(ensemble, experiment, month, model) %>%
      summarise(value = mean(value)) %>%
      mutate(outlier = ifelse(vis.is_outlier(value), paste0(model), NA)) ->
      df_monthly


    # -----------
    # Figures
    # -----------


    # 1. Time Series
    # -------------
    # For each ensemble and experiment combination make a spaghetti
    # plot of the model's value with some trend line.

    df %>%
      ggplot(aes(x = time, y = value, group = model, color = model)) +
      geom_line(size = 1.2) +
      my_settings +
      theme(axis.text.x = element_text(colour="white")) +
      labs(y = paste0(var," ", units), title = "Time Series", caption = paste0(en,"\n",ex,"\n",method)) ->
      fig

    # Save the figure
    list1[["time_series"]] <- fig


    # 2. Monthly Line Plot
    # ------------------
    # Spaghetti plot of the monthly averages by model
    df_monthly %>%
      ggplot(aes(x = month, y = value, group = model, color = model)) +
      geom_line(size = 1.2) +
      my_settings +
      scale_x_discrete(limits = month_name_df$month, labels = month_name_df$month_name) +
      labs(y = paste0(var," ", units), title = "Mean Monthly", caption = paste0(en,"\n",ex,"\n",method)) ->
      fig

    # Save the figure
    list1[["mean_monthly"]] <- fig


    # 3. Monthly Boxplot
    # ------------------
    # Boxplot of the mean monthly values
    df %>%
      left_join(month_name_df, by = "month") %>%
      group_by(month) %>%
      mutate(outlier = ifelse(vis.is_outlier(value), paste0(model), NA)) %>%
      ggplot(aes(x = month, y = value, group = month)) +
      geom_boxplot() +
      geom_text(aes(label = outlier), na.rm = TRUE, size = 3, check_overlap = TRUE) +
      my_settings +
      scale_x_discrete(limits = month_name_df$month, labels = month_name_df$month_name) +
      labs(y = paste0(var," ", units), title = "Monthly", caption = paste0(en,"\n",ex,"\n",method)) ->
      fig

    # Save the figure
    list1[["boxplot"]][["monthly"]] <- fig


    # 4. Mean Monthly Boxplot
    # ------------------------
    # Boxplot of the mean monthly values
    df_monthly %>%
      ggplot(aes(x = month, y = value, group = month)) +
      geom_boxplot() +
      geom_text(aes(label = outlier), na.rm = TRUE, size = 3, check_overlap = TRUE) +
      my_settings +
      scale_x_discrete(limits = month_name_df$month, labels = month_name_df$month_name) +
      labs(y = paste0(var," ", units), title = "Mean Monthly", caption = paste0(en,"\n",ex,"\n",method)) ->
      fig

    # Save the figure
    list1[["boxplot"]][["mean_monthly"]] <- fig


    #----------------------
    # Normalized Plots
    #----------------------
    # Normalize the df and the monthly df by the mean values then
    # replace the original figure's data frame with the normalized data frame.
    df %>%
      group_by(experiment, ensemble, model) %>%
      mutate(value = value / mean(value)) ->
      new_df

    df_monthly %>%
      group_by(experiment, ensemble, model, month) %>%
      mutate(value = value / mean(value)) ->
      new_df_monthly


    # 5. Time Series
    # ---------------
    # Use the time series figure created above but replace the data with the normalized value
    list1[["time_series"]] %+% new_df +
      labs(title = "Normalized Time Series\n(normalized by the mean value)") ->
      fig

    # Save the figure
    list2[["time_series"]] <- fig


    # 6. Monthly Mean Series
    # ----------------------
    list1[["mean_monthly"]][["data"]] %>%
      group_by(ensemble, experiment, model) %>%
      mutate(value = value/mean(value)) ->
      new_df

    list1[["mean_monthly"]] %+% new_df +
      labs(title = "Mean Monthly Normalized by Mean") -> fig

    # Save the figure
    list2[["mean_monthly"]] <- fig


    # 7. Monthly Boxplot
    # ------------------
    list1[["boxplot"]][["monthly"]][["data"]] %>%
      group_by(ensemble, experiment, month) %>%
      mutate(value = value / mean(value)) ->
      new_df

    list1[["boxplot"]][["monthly"]] %+% new_df +
      labs(title = "Monthly Normalized by Mean") ->
      fig

    # Save the figure
    list2[["boxplot"]][["monthly"]] <- fig


    # 8. Mean Monthly Boxplot
    # -------------------------
    # Boxplot of the mean monthly values
    list1[["boxplot"]][["mean_monthly"]][["data"]] %>%
      mutate(value = value/mean(value)) ->
      new_df

    list1[["boxplot"]][["mean_monthly"]] %+% new_df +
      labs(title = "Mean Monthly Normalized by the Mean") ->
      fig

    # Save the figure
    list2[["boxplot"]][["mean_monthly"]] <- fig

    return(list(plots = list1, normalized = list2))

  } # end the internal function


  # Make Figures
  # -------------
  # Apply the internal graphing function to the data frame by method,
  # variable, ensemble, and experiment.
  df %>%
    group_by(method, variable, ensemble, experiment) %>%
    do(fig = internal.func(.)) ->
    out_list


  # Format Output
  # ---------------
  # Generate the names from the output
  names_df <- tidyr::unite(data = out_list, name, method, variable, ensemble, experiment, sep = "_")
  names <- names_df$name

  # Create the empty list to return
  out = list()

  # Fill the output list with the figures/names from the internal
  # function output.
  for(i in 1:length(names)){
    add_name <- names[i]
    out[[paste0(add_name)]] <- out_list$fig[[i]]
  }


  # Return
  # ------
  # Return the output
  return(out)


} #end of the exploratory function


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

  # # Make Figures
  # #
  # # Get the color list
  # colors <- vis.generate_colors(df)
  # df$id <- factor(df$id, levels = colors$id)

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
  #  ggplot2::scale_color_manual(values = paste0(colors$colors)) +
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

internal.time_series <- function(input_df){

  # Checks
  #
  # Check the data frame for the required columns
  req_cols <- c("time", "units", "value", "model",  "experiment", "variable",
                "ensemble", "month", "year", "method", "basin")
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
  bas     <- unique(df$basin)

  # 1. Time Series
  #
  # For each ensemble and experiment combination make a spaghetti
  # plot of the model's value with some trend line.

  df %>%
    ggplot(aes(x = time, y = value, group = model, color = model)) +
    geom_line(size = 1.2) +
    my_settings +
    labs(y = paste0(var," ", units), title = paste0(bas,"\nTime Series"), caption = paste0(en,"\n",method)) ->
    out_list

  return(out_list)


} # end of the vis.time_series


# ------------------------------------------------------------------------------
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

plot.time_series <- function(data, write_pdf = FALSE, file_name){

  # Make the experiment concatenated time series plot
  data %>%
    dplyr::group_by(variable, basin) %>%
    dplyr::do(fig = internal.time_series(.)) %>%
    dplyr::arrange(basin, variable) %>%
    tidyr::unite(name, basin, variable, remove = FALSE) %>%
    dplyr::mutate(fig = setNames(nm = name, fig)) %>%
    dplyr::select(-name) ->
    time_series

  if(write_pdf) {

    path <- system.file("outputs", package = "oceanpH")
    pdf(paste0(path, "/", file_name, ".pdf"), onefile = TRUE)

    invisible(lapply(time_series$fig, print))

    dev.off()

    message("Writing ", paste0(path,"/",file_name,".pdf"))
  } # end of the write pdf if statement

  return(time_series)
}



# End
