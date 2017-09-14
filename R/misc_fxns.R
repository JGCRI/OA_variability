# ------------------------------------------------------------------------------
# Purpose: This script contains the functions that I am on the fence about, I
# cannot tell if they are necessary or not... such as add_month_name()
#
# Created by: Dorheim, Kalyn
# Created on: xxx
# Modified:   xxx
#
# Notes:
# ------------------------------------------------------------------------------
# 1. add_month_name()
# ------------------------------------------------------------------------------
#' Add month name to a data frame
#'
#' \code{add_month_name} Adds the month name to a data frame that contains month
#' number
#'
#' @parm df the data frame to add the month name to, must contain column of numeric months
#' @importFrom dplyr %>%
#' @return a data frame that contains a column of the month name

add_month_name <- function(df){
  # Check for month column
  check.column(df, "input data frame", "month")

  # Data frame of the month names and month numbers
  month_names_df <- data.frame(month = 1:12,
                               month_names = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                                               "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))

  # Add the month name infromation to the data frame
  df %>%
    # Just incase the month number got converted to a string/character
    dplyr::mutate(month = as.numeric(month)) %>%
    dplyr::left_join(month_names_df, by = "month")

} # end of add_month_name
# ----
# End







