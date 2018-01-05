# Purpose: These are my base functions that I like to use in most of my projects.
#
# Created by: Dorheim, Kalyn
# Created on: Jan 5
# Modified:   xxx
#
# Notes:
#
# find_scriptName() ------------------------------------------------------------------------------

# idea: could do full file name not just the script name idk if want directory or not...


#' Find the current script name
#'
#' \code{ind_scriptName} Find the current script name. The idea is that it could be used to
#' find the script name and add it as an output flag in porjects where keeping track of the product
#' source script would be a useful thing to have.
#' @return string name of script

find_scriptName <- function(){basename(rstudioapi::getActiveDocumentContext()$path)}



