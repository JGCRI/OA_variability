# Purpose: building the code to do the AGUoceans figures see https://github.com/JGCRI/OA_variability/issues/17 for
# more details.
#
# Created by: Dorheim, Kalyn
# Created on: Jan 8 2018
# Modified:   xxx
#
# Notes:

# Setup Environment ------------------------------------------------------------------------------
# Libraries
library(ggplot2)
library(dplyr)
devtools::load_all()

script_name <- find_scriptName()

# Define Directories
BASE <- getwd()
INPUT_DIR  <- file.path(BASE, "output", "cmip", "AGUoceans")
OUTPUT_DIR <- file.path(BASE, "output", "cmip", "AGUoceans_figs"); dir.create(OUTPUT_DIR, F)


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
    scale_color_manual(name = "model",
                       values = c("grey", "black"),
                       breaks=c("CMIP5 Models", "Multi Model Median"),
                       labels=c("CMIP5 Models", "Multi Model Median")) +
    scale_x_date(date_breaks = '20 years') +
    theme(legend.title = element_blank(),
          legend.position = "bottom") +
    facet_wrap("basin", ncol = 1) +
    labs(title = vari,
         caption = caption)

}


# make_figure2: makes the second figure for GitHub issue 17 (see there for more details). This function
# make a box plot of the change in K and S for each model by basin / variable. It will also include
# points for the K and S from the multi model median amplitude distribution.
make_figure2 <- function(cmip_data, mean_data, caption = ""){

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
    geom_point(data = mean_data, aes(basin, delta, shape = stat_variable,
                                     group = factor(stat_variable, c("delta  K", "delta  S")),
                                     color = "Multi Model Median"),
               position = position_dodge(width = 0.75), size = 3) ->
     boxplot_median

  # Now specify what color and legend options, facet by variable, and add labels.
  boxplot_median +
    scale_color_manual(name = "delete this label",
                       values = c("black"),
                       breaks=c("Multi Model Median"),
                       labels=c("Multi Model Median")) +
    facet_wrap("variable") +
    theme(legend.title = element_blank()) +
    labs(x = "Basin",
         y = "Change (Future - Historical)",
         title = "Change in Amplitude Distribution\nKurtosis and Skewness")

}


# make_figure3: makes the third figure for the GitHub issue 17 (see there for more details). This function
# makes a 3 panel faceted figure for each basin / variable of annual min, annual max, and amplitude time
# series.
make_figure3 <- function(data, line_size = 1.5, caption = ""){

  # Save variable info for label
  vari <- unique(data$variable)

  # Change the data frame format from wide to long.
  data  %>%
    rename(Amplitude = amplitude, `Annual Minimum` = min, `Annual Maximum` = max) %>%
    tidyr::gather(value_type, value, Amplitude, `Annual Minimum`, `Annual Maximum`) ->
    long_data

  # Add factor information for the order time series plots will appear in the
  # faceted figure.
  long_data$value_type <- factor(long_data$value_type, labels = c("Annual Minimum", "Annual Maximum", "Amplitude"))


  # Find the median value for the time series.
  long_data %>%
    # Because we are faceting by the value type so that amplitude, min, and max all appear as separate time
    # series group by year and value_type in order to determine the median value type time series.
    group_by(year, value_type) %>%
    summarise(value = median(value)) %>%
    ungroup() %>%
    mutate(value_label = "CMIP5 Median", model = "Multi Model Median") ->
    data_median


  # Now start making the figure.
  # Make the fist layer of the plot with all of the cmip models individually.
  long_data %>%
    mutate(value_label = "CMIP5 Models") %>%
    ggplot(aes(year, value, group = model)) +
    geom_line(aes(color = "CMIP5 Models"), size = line_size)  +
    # Use the standardized script aesthetic settings.
    AGUoceans_SETTINGS ->
    cmip_models_layer

  # Add the time series median information to the plot.
  cmip_models_layer +
    geom_line(data = data_median, aes(year, value, color = "Multi Model Median"), size = line_size) ->
    cmip_models_median

  # Manipulate the color scheme and legend.
  cmip_models_median +
    scale_color_manual(name = "model",
                       values = c("grey", "black"),
                       breaks=c("CMIP5 Models", "Multi Model Median"),
                       labels=c("CMIP5 Models", "Multi Model Median")) +
    theme(legend.title = element_blank(),
          legend.position = "bottom") +
    facet_wrap("value_type", scales = "free") +
    labs(y = vari)

}

# Figure 1 ------------------------------------------------------------------------------
# Detreneded time series line plots.

# Import the detrended data
detrened_path <- list.files(INPUT_DIR, "detrended_data.rda", full.names = T)
detrened_data <- get(load(detrened_path))

# Make figure one for each variable / basin.
detrened_data %>%
  group_by(variable, basin) %>%
  do(fig_1 = make_figure1(.)) ->
  figures

# I do not know if this figure will be all that clear so here I make a zoomed in one
# show this to CH to see if she likes it better.
figures$fig_1[[1]] + coord_cartesian(xlim=as.Date(c("1861-01-01", "1863-01-01"))) +
  scale_x_date(date_breaks = '1 years') ->
  fig1_close_up_example


# Figure 2 ------------------------------------------------------------------------------
# The change in Kurtosis and Skewness box plots. The individual model delta K and S
# values have been calculated as part of the "driver" but we want to add the change
# in K and S for the multi model median or mean ?? distribution. This delta K and S
# value will have to be calculated now.

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
  multi_model_KS

multi_delta_KS <- multi_model_KS$delta_K_S


# Make the figure.
fig_2 <- make_figure2(cmip_delta_KS, multi_delta_KS)


# Figure 3 ------------------------------------------------------------------------------

# Make figure 3 using the amplitude_data tibble. Make a figure for each basin / variable
# combination and join it to the figures tibble.

amplitude_data %>%
  group_by(basin, variable) %>%
  do(fig_3 = make_figure3(.)) %>%
  left_join(figures, by = c("basin", "variable")) ->
  figures


# Format Output ------------------------------------------------------------------------
# Create an empty list to store the output int
output = list()

# formate_output_helper: is a function that unfortunately uses a for loop, I really need
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

# Save in a flat list
output <- format_output_helper(figures, "fig_1", output)
output <- format_output_helper(figures, "fig_3", output)
output[["fig_2"]] <- fig_2

# Add attributes and save as an rda file
attributes(output)$script_name <- script_name
output_name <- paste0(gsub(".R", "", script_name), ".rda")
save(output, file = file.path(OUTPUT_DIR, output_name))


# Save as PNG
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

    if(column_name == "fig_1"){
      ggsave(filename = png_name, plot = fig)
    } else {
      ggsave(filename = png_name, plot = fig, width = 11)

    }


  }

}

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

# End -----
