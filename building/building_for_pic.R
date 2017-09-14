# ------------------------------------------------------------------------------
# Purpose: This usess the functions from the cmip5_files_fx.R the main purpose
# is to make sure that the functions will run on pic.
#
# Created by: Dorheim, Kalyn
# Created on: Augut 22 2017
# Modified:   xxx
#
# Notes:
# ------------------------------------------------------------------------------

list <- find_me(path = "/pic/projects/GCAM/CMIP5-CLynch/PH_extra",
                variable = "ph", domain = "Omon", experiment = "rcp85", ensemble = "r1i1p1")

mini_list <- list[1]; mini_list

df <- cmip_file_info(mini_list)

cdo.processor(df, cdo_command = "fldmean")


# ---
list <- find_me(path = "/pic/projects/GCAM/CMIP5-CLynch/PH_extra",
                variable = "ph", domain = "Omon", experiment = "rcp85", ensemble = "r1i1p1")

short <- list[1:2]

df <- cmip_file_info(short)

df %>%
  group_by(variable, model, experiment, ensemble) %>%
  do(results = cdo.processor(., cdo_command = "fldmean")) ->
  test

bind_rows(test$results ) -> out
