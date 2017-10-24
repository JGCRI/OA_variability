# ------------------------------------------------------------------------------
# Purpose: This script contains the check family of functions that check and
# validate the inputs for various functions used in this package.
#
# Created by: Dorheim, Kalyn
# Created on: Augut 17 2017
# Modified:   xxx
#
# Notes: This script contains the following: check.column()
# ------------------------------------------------------------------------------
# check.column()
# ------------------------------------------------------------------------------
#' Check input dataframes for the required columns
#'
#' \code{check.column} Checks to make sure that any data frames used as an input
#' contains all of the required columns. If the data frame does not then the a
#' stop error will occur
#'
#' @param df the data frame to check for required columns
#' @param required_columns the names of the columns to check for in the data frame
#' @param df_name the name of the df to return in the error message
#' @importFrom dplyr %>%
#' @return an intentional error message if the data frame does not pass the tests
#' @keywords internal


check.column <- function(df, df_name, required_columns){

  if(!is.data.frame(df)){
    stop("Data frame input required for ", paste0(df_name))
  }

  missing <- !required_columns %in% colnames(df)

  if(any(missing)){
    stop("The following column(s) are missing from ", paste0(df_name)," ", paste0(required_columns[which(missing == TRUE)], sep = ", "))
  }

} # end of column_check

