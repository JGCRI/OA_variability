# ------------------------------------------------------------------------------
# Purpose: This script detrends the mean basin data using a fit to a third
# degree polynomial and then creates some plots
#
# Created by: Dorheim, Kalyn
# Created on: October 11 2017
# Modified:   xxx
#
# Notes: Spell checked on 10/11
# ------------------------------------------------------------------------------
# Set up the environment
# ------------------------------------------------------------------------------
library(dplyr); library(tidyr); library(ggplot2);
library(oceanpH)

# Create an empty list where to save the figures
out = list()

basin_detrened <- detrend(basin_mean)
# In order to save the basin_detrended data frame you may need to adjust the working
# directory.
# devtools::use_data(basin_detrened, pkg = "OA_variability", internal = TRUE, overwrite = TRUE)

# ------------------------------------------------------------------------------
# The CESM1 - BGC plots by variable
# ------------------------------------------------------------------------------

# Plot all models and variables
ex_list  <- unique(basin_detrened$experiment)
var_list <- unique(basin_detrened$variable)

# For loop to run through experiment and variable combinations.
for(i in 1:length(ex_list)){

  ex   <- ex_list[i]
  data1 <- dplyr::filter(basin_detrened, experiment == ex, model == "CESM1-BGC")

  for(j in 1:length(var_list)){

    va   <- var_list[j]
    data2 <- dplyr::filter(data1, variable == va)
    un   <- unique(data2$units)
    mo   <- unique(data2$model)

    data2$basin <- factor(data2$basin, levels = c("Global", "North Hemi", "South Hemi", "Arctic", "Atlantic", "Indian",
                                                  "Pacific Ocean", "Southern Ocean"))

    data2 %>%
      na.omit %>%
      group_by(basin, year) %>%
      summarise(value = mean(value)) %>%
      ungroup %>%
      ggplot(aes(x = year, y = value)) +
      geom_line(size = 2) +
      labs(title = paste0("Polynomial Detrending Comparison\nAnnual Mean\n", ex," ", va, " ", mo),
           y = paste0(va, " ", un),
           caption = "Time series was detrended and annual mean was plotted for aesthetic purposes.") +
      theme(text = element_text(size = 13)) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      facet_wrap(facets = "basin", ncol = 4) ->
      out[[paste0(ex)]][[paste0(va)]]

  }
}


# ------------------------------------------------------------------------------
# The CESM1 - BGC plots by basin
# ------------------------------------------------------------------------------
# Plot all models and variables
ex_list  <- unique(basin_detrened$experiment)
var_list <- unique(basin_detrened$variable)
ba_list  <- unique(basin_detrened$basin)

# For loop to run through experiment and variable combinations.
for(i in 1:length(ex_list)){

  ex   <- ex_list[i]
  data1 <- dplyr::filter(basin_detrened, experiment == ex, model == "CESM1-BGC")

  for(j in 1:length(var_list)){

    va   <- var_list[j]
    data2 <- dplyr::filter(data1, variable == va)

    for(k in 1:length(ba_list)){
      ba    <- ba_list[k]
      data3 <- dplyr::filter(data2, basin == ba)

      un   <- unique(data3$units)
      mo   <- unique(data3$model)

      data3 %>%
        na.omit %>%
        group_by(basin, year) %>%
        summarise(value = mean(value)) %>%
        ungroup %>%
        ggplot(aes(x = year, y = value)) +
        geom_line(size = 2) +
        labs(title = paste0("Polynomial Detrending Comparison\n", ba, " ",ex," ", va, " ", mo),
             y = paste0(va, " ", un),
             caption = "Time series was detrended and annual mean was plotted for aesthetic purposes.") +
        theme(text = element_text(size = 13)) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) ->
        out[[paste0(ba)]][[paste0(ex)]][[paste0(va)]]

    } # basin for loop
  } # variable for loop
} # ex for loop


# ------------------------------------------------------------------------------
# Save the output
# ------------------------------------------------------------------------------
path <- system.file("data/figs", package = "oceanpH", mustWork = TRUE)
save(out, file = paste0(path, "/detrend_cesm1.rda"))
