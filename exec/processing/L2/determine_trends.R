# ------------------------------------------------------------------------------
# Purpose: The purpose of this script to do do part 1 of the detrending analysis.
# Right now we are just interested in the different types of trends we see in the
# mean annual and monthly data.
#
# Created by: Dorheim, Kalyn
# Created on: September 13 2017
# Modified:   xxx
#
# Notes: Right now we are still working with the exploratory data not the
# real stuff from pic. When we get the real data there will most likly be
# some changes required by the code.
#
# Okay idk how to save all of the figure I made also debateable that was a
# watste of time.. booo fml.
# ------------------------------------------------------------------------------
# 0. Environment Set up
# ------------------------------------------------------------------------------
# # Define the base name directory, should be the same for the entire project
# BASE_NAME <- getwd()
#
# # Required Libraries
# devtools::load_all(BASE_NAME)
# library(dplyr)
# library(tidyr)
# library(ggplot2)

# Define the data and output directories
DATA_DIR <- paste0(BASE_NAME, "/exec/processing/exploratory/output/")
# OUT_DIR  <- paste0(BASE_NAEM, "/blah")

# The list to save output in
out = list()

# Define the base name for the output files.
out_fname <- ".csv"

# Script name
script_name <- ""

# ------------------------------------------------------------------------------
# 1. Import And Format Data
# ------------------------------------------------------------------------------
# Import the models to screen out of analysis
to_remove <- read.csv(paste0(BASE_NAME, "/exec/processing/L0/models_to_remove.csv"))

# Import the pH, tos, and CO3 data frames.
read.csv(paste0(DATA_DIR, "fldmean_monthly_tos.csv")) %>%
  bind_rows(read.csv(paste0(DATA_DIR, "fldmean_monthly_co3.csv"))) %>%
  bind_rows(read.csv(paste0(DATA_DIR, "fldmean_monthly_pH.csv")) %>% mutate(units = NA)) %>%
# Remove data for the models thate are found in the to_remove list
  filter(!model %in% to_remove$model) ->
  data

# ------------------------------------------------------------------------------
# 2. Annual Linear Regression
# ------------------------------------------------------------------------------
# Get the annual average for each ensemble/experiment/variable/model combination.
data %>%
  group_by(ensemble, experiment, variable, units, model, year) %>%
  summarise(value = mean(value)) %>%
  rename(time = year) %>%
  group_by(ensemble, experiment, variable, model) %>%

# Determine the linear fit and make figures.
  do(list = detrend.linear_fit(.)) %>%
  unite(-list, col = name, sep = "_") %>%
  list.add_list_name ->
  annual

# Extract the linear fit data frames.
annual_df <- data.frame()
for(i in 1:length(annual)){
  annual_df <- bind_rows(annual_df, annual[[i]]$coefficient_data)
}
annual_df$month <- "annual"

# ------------------------------------------------------------------------------
# 3. Monthly Linear Regression
# ------------------------------------------------------------------------------
# Get the annual average for each ensemble/experiment/variable/model combination.
data %>%
  select(-time) %>%
  ungroup %>%
  rename(time = year) %>%
  group_by(ensemble, experiment, variable, model, month) %>%

  # Determine the linear fit and make figures.
  do(list = detrend.linear_fit(.)) %>%
  unite(-list, col = name, sep = "_", remove = FALSE) %>%
  list.add_list_name ->
  month

# Now we need to add the month information to the data frames
# and the figures and then I need to save the data frame.
month_df <- data.frame()

# Start by determining the month name from the month list.
as_tibble(names(month)) %>%
  separate(value,into = c("ensemble", "experiment", "variable", "model", "month_num"), sep = "_") %>%
  mutate(month_num = as.integer(month_num)) %>%
  left_join(data.frame(month_num = 1:12,
                              month_name = c("January", "February", "March", "April", "May", "June",
                                             "July", "August", "September", "October", "November", "December")),
            by = "month_num") ->
  month_names

# The for loop
for(i in 1:length(month)){

  # Extract month name
  which_month <- month_names$month_name[i]

  # Add month to the figure
  month[[i]]$figures$data_v_fit + labs(subtitle = which_month) ->
    month[[i]]$figures$data_v_fit

  # Add month name to the coeffiecents data frame
  temp_data <- month[[i]]$coefficient_data
  temp_data$month <-   which_month

  # Concatenate the monthly data frames
  month_df <- bind_rows(month_df, temp_data)
}


# ------------------------------------------------------------------------------
# 4. Slope Tables
# ------------------------------------------------------------------------------
# Okay here look by monthly and annual... do a count of the number of models
# with trends ie slope != to 0 vs. slope = 0 and then look at the p value
# for significance...
annual_df %>%
  bind_rows(month_df) %>%
  filter(coefficient == "slope") %>%
  rename(value = Estimate) ->
  slope_df

# Table of counts for the number of models with trends
slope_df %>%
  select(experiment, model, month, value) %>%
  mutate(trend = ifelse(abs(value) >= 1e-6, "Yes", "No")) %>%
  group_by(experiment, month, trend) %>%
 # summarise(count = length(model), mean = mean(value)) %>%
  summarise(count = length(model)) %>%
  spread(trend, count) %>%
  knitr::kable() ->
  out[["table"]][["trend"]]

# Table of counts for the number of models with trends
slope_df %>%
  select(experiment, model, month, `Pr(>|t|)`) %>%
  mutate(p_val = ifelse(abs(`Pr(>|t|)`) <= 0.005, "Yes", "No")) %>%
  group_by(experiment, month, p_val) %>%
  summarise(count = length(model)) %>%
  spread(p_val, count) %>%
  knitr::kable() ->
  out[["table"]][["pval"]]

slope_df %>%
  filter(`Pr(>|t|)` > 0.005) %>%
  select(model) %>%
  unique ->
  out[["table"]][["insignificant"]]

slope_df %>%
  filter(`Pr(>|t|)` <= 0.005) %>%
  select(model) %>%
  unique ->
  out[["table"]][["significant"]]

# ------------------------------------------------------------------------------
# 5. Visualize Slopes
# ------------------------------------------------------------------------------
# Define my fav figure settings
my_settings <- ggplot2::theme(text = element_text(size = 13)) +
  ggplot2::theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Month Names Data frames
month_name_df <- data.frame(month_num = 1:12,
                            month_name = c("January", "February", "March", "April", "May", "June",
                                           "July", "August", "September", "October", "November", "December"))
slope_df %>%
  left_join(month_name_df, by = c("month" = "month_name")) %>%
  group_by(variable, experiment, month) %>%
  mutate(outlier = ifelse(vis.is_outlier(value), paste0(model), NA)) ->
  slope_df

# Annual slope boxplots
slope_df %>%
  filter(month == "annual") %>%
  group_by(experiment, variable) %>%
  mutate(norm = value/mean(value)) %>%
  ggplot(aes(x = experiment, y = norm, fill = variable)) +
  geom_text(aes(label = outlier), na.rm = TRUE, size = 3, check_overlap = TRUE) +
  geom_boxplot() +
  labs(y = "slope normalized by the mean", title = "Annual Change") ->
  out[["figures"]][["annual_slope"]]

# Montly
slope_df %>%
  filter(month != "annual") ->
  plot_month

plot_month$month <- factor(plot_month$month , levels=unique(month_name_df$month_name))

variable_list <- c("ph", "tos", "co3")

for(i in 1:length(variable_list)){

  v <- variable_list[i]

  plot_month %>%
    filter(variable == v) %>%
    group_by(experiment, variable, month) %>%
    mutate(norm = value/mean(value)) %>%
    ggplot(aes(x = month, y = norm, fill = experiment)) +
    geom_boxplot() +
    geom_text(aes(label = outlier), na.rm = TRUE, size = 3, check_overlap = TRUE) +
    my_settings +
    labs(y = "slope normalized by the mean", title = paste0("Annual Change in Monthly Mean\n", v)) ->
    out[["figures"]][["monthly_slope"]][[paste0(v)]]

}


# ----
# End
# messge("End of ", script_name)







