# ------------------------------------------------------------------------------
# Purpose: This script contains the functions related to detrending time series
# data for futher analysis.
#
# Created by: Dorheim, Kalyn
# Created on: xxx
# Modified:   xxx
#
# Notes:
# ------------------------------------------------------------------------------
# 1. detrend.linear_fit
# ------------------------------------------------------------------------------
detrend.linear_fit <- function(df, make_grid = FALSE){

  # Checks
  #
  # Column Check
  check.column(df, "detrend.linear_fit input", c("model", "ensemble", "experiment", "time", "value", "variable", "units"))

  # Check for NAs
  if(is.na(df$value)){
    stop("Missing value in detrend.linear_fit input.")
  }

  if(is.na(df$time)){
    stop("Missing time in detrend.linear_fit input.")
  }

  # Functional Code
  #
  # Firs make a list to store output in
  output = list()

  # Save CMIP5 information
  en <- unique(df$ensemble)
  ex <- unique(df$experiment)
  m  <- unique(df$model)
  v  <- unique(df$variable)
  u  <- unique(df$units)

  # Do the linear regression
  fit <- stats::lm(value~time, df)

  # Get the summary statistics about the linear regression
  # fit and format for output.
  sum_fit  <- base::summary(fit)

  tibble::as_tibble(sum_fit$coefficients) %>%
    dplyr::bind_cols(coefficient = c("y-intercept", "slope")) %>%
    dplyr::mutate(ensemble = en, experiment = ex, model = m, variable = v) ->
    output[["coefficient_data"]]


  # Figures

  # My perfered figure settings
  # My preferred figure settings
  my_settings  <- ggplot2::theme(text = element_text(size = 13)) +
    ggplot2::theme(axis.text.x = element_text(angle = 45, hjust = 1))

  # Determine the slope and intercept values
  slope_val <- fit$coefficients[2]
  inter_val <- fit$coefficients[1]

  # Determine the slope and intercept p values
  slope_p <- output$coefficient_data$`Pr(>|t|)`[2]
  inter_p <- output$coefficient_data$`Pr(>|t|)`[1]

  # Raw Data vs Fit
  #
  df %>%
    ggplot2::ggplot(aes(x = time, y = value)) +
    ggplot2::geom_line(size = 1.5) +
    ggplot2::geom_abline(slope = slope_val, intercept = inter_val, size = 1.5, color = 2) +
    my_settings +
    ggplot2::labs(y = paste0(v,"  ",u), title = paste0("Data vs. Fit\n", m," ",ex, " ",en),
                  caption = paste0("slope = ", signif(slope_val,3), "(",signif(slope_p,3),")\nintercept = ",signif(inter_val,3), "(",signif(inter_p,3),")")) ->
    output[["figures"]][["data_v_fit"]]

  # Residual vs. fitted value
  #
  cbind(residuals = sum_fit$residuals, fitted = fit$fitted.values) %>%
   as.data.frame %>%
   ggplot2::ggplot(aes(x = fitted, y = residuals)) +
   ggplot2::geom_point(size = 1.5) +
   ggplot2::geom_hline(yintercept = 0, size = 1.5, color = 2) +
   my_settings +
   ggplot2::labs(y = "Residuals", x = "Fitted Values", title = paste0("Residuals vs Fitted\n", m," ",v," ",ex, " ",en),
                 caption = paste0("slope = ", signif(slope_val,3), "(",signif(slope_p,3),")\nintercept = ",signif(inter_val,3), "(",signif(inter_p,3),")")) ->
   output[["figures"]][["residuals_v_fitted"]]

 # Histogram of the Residuals
 #
 ggplot2::qplot(x = sum_fit$residuals, geom = "histogram", bins = 20) +
   my_settings +
   ggplot2::labs(y = "Frequency", x = "Residual", title = paste0("Histogram of Residuals\n", m," ",v," ",ex, " ",en),
                 caption = paste0("slope = ", signif(slope_val,3), "(",signif(slope_p,3),")\nintercept = ",signif(inter_val,3), "(",signif(inter_p,3),")")) ->
   output[["figures"]][["residual_histogram"]]

 # Normal QQ plot
 #
 ggplot2::ggplot(df) +
   ggplot2::stat_qq(aes(sample = sum_fit$residuals)) +
   my_settings +
   ggplot2::labs(y = "Residual", x = "Theoretical Quantiles", title = paste0("Normal Q-Q\n", m," ",v," ",ex, " ",en),
                 caption = paste0("slope = ", signif(slope_val,3), "(",signif(slope_p),")\nintercept = ",signif(inter_val,3), "(",signif(inter_p,3),")")) ->
   output[["figures"]][["normal_qq"]]

 if(make_grid == TRUE) {

   # Save all of the figures in a gird for easish printing, this will increase the running time.
   output[["figures"]][["data_v_fit"]] -> p1
   output[["figures"]][["residuals_v_fitted"]] + ggplot2::labs(caption = "") -> p2
   output[["figures"]][["residual_histogram"]] + ggplot2::labs(caption = "") -> p3
   output[["figures"]][["normal_qq"]] + ggplot2::labs(caption = "") -> p4

   gridExtra::grid.arrange(p1, p2, p3, p4) ->
     output[["figures"]][["grid"]]

 } # make grid if statement


 # Return
 return(output)

} # end of the function


# ----
# End



