# ------------------------------------------------------------------------------
# Purpose: This script detrend the data using three different polynomials and then
# graphs the output. Currently the script is only set up to plot the detrened values
# for the CESM1-BGC model but could easily plot other models. Using the polynomial
# detrending the noise is large if we try to plot the entire time series so
# I use the annual mean value in the plot to visualize better.
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


# Create an empty list where to save the figures
out = list()

# The following function were defined in the R/detrend.R script but have been
# removed since we decied to use 3rd degree polynomail as our detrending method.

# ------------------------------------------------------------------------------
# internal.detrend_poly
# ------------------------------------------------------------------------------

internal.detrend_poly <- function(df){

  # Start by checking to make sure you are only detrending for a single model,
  # basin, experiment, ensemble member.
  en <- unique(df$ensemble);   mo <- unique(df$model)
  ex <- unique(df$experiment); ba <- unique(df$basin)
  va <- unique(df$variable)

  if(length(c(en, mo, ex, ba, va)) > 5){
    stop("too many cmip observations read into the internal detrend ploy funciton")
  }


  # # Linear Regression
  # fit  <- lm(df$value ~ df$time)
  # pred <- predict(fit, data.frame(x = df$time))

  # The first degree polynomail fit
  fit1  <- lm(df$value ~ poly(df$time, 1))
  pred1 <- predict(fit1, data.frame(x = df$time))

  # The second degree polynomail fit
  fit2  <- lm(df$value ~ poly(df$time, 2))
  pred2 <- predict(fit2, data.frame(x = df$time))

  # The third degree polynomail fit
  fit3  <- lm(df$value ~ poly(df$time, 3))
  pred3 <- predict(fit3, data.frame(x = df$time))

  # Detrend
  #
  # Now detrend the raw value by all of the fits.
  df %>%
    #  dplyr::mutate(`linear reg` = value - pred) %>%
    dplyr::mutate(`1st deg` = value - pred1) %>%
    dplyr::mutate(`2nd deg` = value - pred2) %>%
    dplyr::mutate(`3rd deg` = value - pred3) %>%
    dplyr::rename(`raw value` = value)

} # end of the interal.detrend_poly

# ------------------------------------------------------------------------------
# detrend.poly_compare
# ------------------------------------------------------------------------------


detrend.poly_compare <- function(data){

  # For all of the possible different model, ensemble, experiment, basin, combinations
  # detrend the basin means using different poly nomial degrees.
  data %>%
    dplyr::group_by(ensemble, experiment, variable, basin, model) %>%
    dplyr::do(out = internal.detrend_poly(.)) %>%
    tidyr::unnest() %>%
    tidyr::gather(`detrend method`, value, `1st deg`, `2nd deg`, `3rd deg`, `raw value`) %>% # removed `learn reg`
    dplyr::select(ensemble, experiment, variable, basin, model, time, units,
                  method, year, month, month_name, `detrend method`, value)

}# end of the detrend.poly_compare function



# ------------------------------------------------------------------------------
# Detrend
# ------------------------------------------------------------------------------
path <- list.files(path = "data", pattern = "CESM1_trended_basin_mean", full.names = TRUE)
basin_mean <- get(load(path))

basin_detrened <- detrend.poly_compare(basin_mean)

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
      filter(`detrend method` != "raw value") %>%
      na.omit %>%
      group_by(basin, year, `detrend method`) %>%
      summarise(value = mean(value)) %>%
      ungroup %>%
      ggplot(aes(x = year, y = value, col = `detrend method`)) +
      geom_line(size = 2) +
      labs(title = paste0("Polynomial Detrending Comparison\nAnnual Mean\n", ex," ", va, " ", mo),
           y = paste0(va, " ", un),
           caption = "Time series was detrended and annual mean was plotted for visual purposes.") +
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
        filter(`detrend method` != "raw value") %>%
        na.omit %>%
        rename(detrend_method = `detrend method`) %>%
        group_by(basin, year, detrend_method) %>%
        summarise(value = mean(value)) %>%
        ungroup %>%
        ggplot(aes(x = year, y = value)) +
        geom_line(size = 2) +
        labs(title = paste0("Polynomial Detrending Comparison\n", ba, " ",ex," ", va, " ", mo),
             y = paste0(va, " ", un),
             caption = "Time series was detrended and annual mean was plotted for visual purposes.") +
        theme(text = element_text(size = 13)) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        facet_wrap(facets = "detrend_method", ncol = 2, nrow = 2) ->
        out[[paste0(ba)]][[paste0(ex)]][[paste0(va)]]

    } # basin for loop
  } # variable for loop
} # ex for loop


# ------------------------------------------------------------------------------
# Save the output
# ------------------------------------------------------------------------------
#path <- system.file("data/figs", package = "oceanpH", mustWork = TRUE)
#save(out, file = paste0(path, "/detrending_polynomial_comparison.rda"))


