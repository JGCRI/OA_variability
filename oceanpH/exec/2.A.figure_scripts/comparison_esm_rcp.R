# Purpose: Are esmrcp and rcp different? Could this be due to carbon cycle feed backs??
# Date:    December 19 2017
# Notes:

# Environment --------------------------------------------------------------------------------------------
library(ggplot2)
library(dplyr)

script_name <- "comaprison_esm_rcp.R"

# Define Directories
BASE <- getwd()
OUTPUT_DIR <- file.path(BASE, "figs", "cmip")

# Import the detrended data sets from the rcp and esm experiments.
rcp85_data <- get(load(list.files(file.path(BASE, "output", "cmip", "rcp85"), "detrended_data.rda", full.names = T)))
esm85_data <- get(load(list.files(file.path(BASE, "output", "cmip", "esmrcp85"), "detrended_data.rda", full.names = T)))


# Script Functions ---------------------------------------------------------------------------------------
monthly_mean_plot <- function(df){

  # This function makes the line plots for the average year for a given basin / variable faceted by
  # the period. The idea is that this function will compare the average annual cycle of the regular
  # and the emissions driven experiments.

  vari <- unique(df$variable)
  bas  <- unique(df$basin)
  uni  <- unique(df$units)

  df %>%
    ggplot(aes(month, value, color = experiment)) +
    geom_line(size = 1.5) +
    oceanpH:::MY_SETTINGS +
    facet_grid(.~period) +
    labs(y = paste0(uni," ", vari),
         title = paste0(bas, " ", vari)) +
    scale_x_discrete(limits = df$month, labels = df$month_name)

}

annual_mean_plot <- function(df){

  # This function makes a line plot of the annual average for a single variable / basin. The
  # Idea is to use this function to look at the differences between the annual average for the
  # basins.

  vari <- unique(df$variable)
  bas  <- unique(df$basin)
  uni  <- unique(df$units)

  df %>%
    ggplot(aes(year, value, color = experiment)) +
    geom_line(size = 1.5) +
    oceanpH:::MY_SETTINGS +
    labs(title = paste0("Annual Mean", bas, " ", vari),
         y = paste0(vari, " ", uni))
}

# Format Data --------------------------------------------------------------------------------------------

# Before combining into a single data frame add an indicator for historical or future
rcp85_data <- mutate(rcp85_data, period = if_else(grepl("historical", experiment), "Historical", "Future"))
esm85_data <- mutate(esm85_data, period = if_else(grepl("Historical", experiment), "Historical", "Future"))


# Combine the rf and emissions driven experiments together, use a left join method instead of bind_rows to
# ensure that the experiment comparison will contain the same models.
rcp85_data %>%
  rename(rcp = value) %>%
  select(-date, -time, -experiment) %>%
  left_join(esm85_data %>%
              rename(esm = value) %>%
              select(-date, -time, -experiment),
            # Do not join by experiment, the are different experiments for that very reason.
            by = c("ensemble", "variable", "basin", "model", "units",
                   "method", "year", "month", "month_name", "period")) %>%
  na.omit %>%
  tidyr::gather(experiment, value, rcp, esm) %>%
  mutate(experiment = paste0(experiment,"_", period)) ->
  complete_data_set

# Using the complete data set determine the monthly average for each basin / model / variable.
complete_data_set %>%
  group_by(experiment, variable, basin, units, month, month_name, period) %>%
  summarise(value = mean(value)) %>%
  ungroup ->
  monthly_means

# Chronologically order the periods using a factor.
monthly_means$period <- factor(monthly_means$period, levels = c("Historical", "Future"), ordered = T)

# Get the annual average for each variable / basin.
complete_data_set %>%
  group_by(year, experiment, basin, variable) %>%
  summarise(value = mean(value)) %>%
  ungroup %>%
  mutate(experiment = if_else(grepl("esm", experiment), "Emissions", "RCP")) ->
  annual_mean

# Make Figures --------------------------------------------------------------------------------------------

# The mean month or average intra annual cycle for each bain / variable.
monthly_means %>%
  group_by(basin, variable) %>%
  do(monthly_fig = monthly_mean_plot(.)) %>%
  arrange(variable, basin)->
  monthly_figures

# The mean annual average for each absin / variable.
annual_mean %>%
  group_by(variable, basin) %>%
  do(annual_fig = annual_mean_plot(.)) ->
  annual_figures

# Reformat the figures into a flat list.
unformated_figures <- left_join(monthly_figures, annual_figures, by = c("basin", "variable"))

formated_figures = list()
for(i in 1:nrow(unformated_figures)){

  variable <- unformated_figures$variable[i]
  basin    <- unformated_figures$basin[i]

  formated_figures[[paste(variable)]][[paste(basin)]] <- list(monthly = unformated_figures$monthly_fig[[i]],
                                                annual = unformated_figures$annual_fig[[i]])

}


# Save --------------------------------------------------------------------------------------------------
attributes(formated_figures)[["script_name"]] <- script_name
save(formated_figures, file = file.path(OUTPUT_DIR, "esm_rcp_comparison.rda"))

# End ----
message("output saved at ", file.path(OUTPUT_DIR, "esm_rcp_comparison.rda"))


