# ------------------------------------------------------------------------------
# Purpose: This script contains the functions related to de-trending time series
# data for further analysis.
#
# Created by: Dorheim, Kalyn
# Created on: October 10 2017
# Modified:   xxx
#
# Notes: Based off results from teh polynomial_detrending_comparison.R and disussion
# on 10/11 (slack) we decieded to go with the thrid degree polynomial detrending.
# ------------------------------------------------------------------------------
# internal.detrend
# ------------------------------------------------------------------------------
#' 3rd degree polynomial detrending for a single observation
#'
#' \code{internal.detrend} for a single model observation detrend the time series
#' by the 3rd degree polynomail
#'
#' @param df data frame containing a single cmip observation
#' @importFrom dplyr %>%
#' @return a data frame contiaing the raw values and detrended values
#' @noRd

internal.detrend_poly <- function(df){

  # The check was to make sure the detrending was happening correctly during the
  # devlopment period but it not needed now, may delet at a latter time?
  # # Start by checking to make sure you are only detrending for a single model,
  # # basin, experiment, ensemble member.
  # en <- unique(df$ensemble);   mo <- unique(df$model)
  # ex <- unique(df$experiment); ba <- unique(df$basin)
  # va <- unique(df$variable)
  #
  # if(length(c(en, mo, ex, ba, va)) > 5){
  #   stop("too many cmip observations read into the internal detrend ploy funciton")
  # }

  # The third degree polynomail fit
  fit  <- lm(df$value ~ poly(df$time, 3))
  pred <- predict(fit, data.frame(x = df$time))

  # Detrend
  #
  # Now detrend the raw value by all of the fits.
  dplyr::mutate(df, value = value - pred)

} # end of the interal.detrend_poly

# ------------------------------------------------------------------------------
# detrend
# ------------------------------------------------------------------------------
#' 3rd degree polynomail
#'
#' \code{detrend} Detrend time series data using different the 3rd degree
#' polynomial
#'
#' @param data a data frame containing the time sereis to detrend
#' @importFrom dplyr %>%
#' @return a data frame contiaing the detrended values
#' @export

detrend <- function(data){

  # For all of the possible different model, ensemble, experiment, basin, combinations
  # detrend the basin means using different poly nomial degrees.
  data %>%
    dplyr::group_by(ensemble, experiment, variable, basin, model) %>%
    dplyr::do(out = internal.detrend_poly(.)) %>%
    tidyr::unnest() %>%
    dplyr::select(-variable1, -basin1, -model1)

} # end of the detrend.poly_compare function


# ----
# End



