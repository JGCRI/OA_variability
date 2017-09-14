# -------------------------------------------------------------------------------------------
# Purpose: This script contains functions in the list family, all of this functions relate
# to working with lists. Functions defined here include: list.add_list_name()
#
# Created by: Dorheim, Kalyn
# Created on: September 13 2017
# Modified:   xxx
#
# Notes: ex
# -------------------------------------------------------------------------------------------
# 1. list.add_list_name()
# -------------------------------------------------------------------------------------------
#' Add names to a list stored in a list
#'
#' \code{list.add_list_name} This function adds names to an object that is stored in a list
#'
#' @param input_list an object that contains multiple lists that are un-named, requires a column of names
#' @return A list of named figures

list.add_list_name <- function(input_list){
  check.column(input_list, "input_list", c("list", "name"))
  stats::setNames(input_list$list, input_list$name)

} # end of list.add_list_name

# ----
# End








