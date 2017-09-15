# ------------------------------------------------------------------------------
# Purpose: This script contains the cmip family of functions that are all
# related to finding, extracting, and checking cmip5 netcdfs on pic or wherever
# the netcdfs are saved. This script contains the
# following functions: cmip.find_me(), cmip.check_depth(), and cmip.file_info(),
#
# Created by: Dorheim, Kalyn
# Created on: Augut 10 2017
# Modified:   xxx
#
# Notes: Sucessful run on pic 8/23/17
# ------------------------------------------------------------------------------
# 1. cmip.find_me()
# ------------------------------------------------------------------------------
#' Find CMIP5 netcdfs of interest
#'
#' \code{cmip.find_me} returns a list of the pathes to the CMIP5 netcdf files of interest.
#' Unless otherwise specified this function is setup to search for any variable, model,
#' ensemble, domain, date .ncdf files. This function can search for multiple CMIP5 specificaitons
#' at a time for example if  you are interested in fidning all of the CMIP5 files for the rcp45
#' and 1pctCO2 experiements the experiment parameter would be specified as experiment = c("rcp45", "1pctCO2").
#'
#' @param path the directory to search for the CMIP5 files in
#' @param variable default is set to search for all letter/number combinations, may specify as a single or multiple search requirement(s)
#' @param domain default is set to search for all letter/number combinations, may specify as a single or multiple search requirement(s)
#' @param model default is set to search for all letter/number combinations, may specify as a single or multiple search requirement(s)
#' @param experiment default is set to search for all letter/number combinations, may specify as a single or multiple search requirement(s)
#' @param ensemble default is set to search for all letter/number combinations, may specify as a single or multiple search requirement(s)
#' @param time default is set to search for numeric dates, may specify as a single or multiple search requirement(s)
#' @importFrom dplyr %>%
#' @return a list of paths for CMIP5 netcdf files

# Example - will work on pic
# find_me(path = "/pic/projects/GCAM/CMIP5-CLynch/PH_extra", variable = "ph", domain = "Omon", experiment = "rcp85", ensemble = "r1i1p1" )

cmip.find_me <- function(path, variable = "[a-zA-Z0-9-]+", domain = "[a-zA-Z0-9-]+", model = "[a-zA-Z0-9-]+",
                    experiment = "[a-zA-Z0-9-]+", ensemble = "[a-zA-Z0-9-]+",  time = "[0-9]+)-([0-9]+"){

  # Collapse the pattern of CMIP5 file names, this will allow for the pattern
  # generated in the next step to search for multiple options if specified in
  # the function input.
  var_pattern    <- paste0(variable, collapse = "|")
  domain_pattern <- paste0(domain, collapse = "|")
  model_pattern  <- paste0(model, collapse = "|")
  exp_pattern    <- paste0(experiment, collapse = "|")
  ens_pattern    <- paste0(ensemble, collapse = "|")
  time_pattern   <- paste0(time, collapse = "|")

  # If statement for generating the cmip 5 serach pattern, it depends on
  # whether or not the netcdf is a constant or changes.
  if(domain_pattern != "fx"){
    # Generate the file pattern to serach for.
    file_pattern <- paste("(", var_pattern,
                          ")_(", domain_pattern,
                          ")_(", model_pattern,
                          ")_(", exp_pattern,
                          ")_(", ens_pattern,
                          ")_(", time_pattern,
                          ").nc$", sep = "")
  } else {
    # Generate the file pattern to serach for.
    file_pattern <- paste("(", var_pattern,
                          ")_(", domain_pattern,
                          ")_(", model_pattern,
                          ")_(", exp_pattern,
                          ")_(", ens_pattern,
                          ").nc$", sep = "")
  } # end of if statement



  # Serach directory specified in function input for files matching
  # the desired traits.
  list.files(path, pattern = file_pattern, full.names = TRUE, recursive = TRUE)

} # end of find me


# Check for depth function
# ------------------------------------------------------------------------------
# 2. cmip.check_depth()
# ------------------------------------------------------------------------------
#' Check to see if the CMIP5 netcdf file has information about the depth or not.
#'
#' \code{cmip.check_depth} imports a singular netcdf file and then checks to see if
#' the file contains any information about ocean depth or "lev".
#'
#' @param ncdf_paths the path to a single or vector of paths to cmip5 files
#' @importFrom dplyr %>%
#' @return a data frame containing the netcdf file path and the Y/N indicator of depth existence

# Example - list_ncdf %>% check_dpeth -> out (nice data frame format and v. quick)

cmip.check_depth <- function(ncdf_paths){

  output <- c()

  for(i in 1:length(ncdf_paths)){

    # Select the single ncdf to check
    ncdf_path <- ncdf_paths[i]


    # Open the ncdf4 file
    nc <- ncdf4::nc_open(ncdf_path)

    # Check to see if there is infromation about the level.
    # If there is no data about the ocean depth level then NA
    # will be returned. If there is depth information then the list
    # of the depths will be returned.Depth may be stored as lev or depth,
    # check for both.
    depth_values <- tryCatch(ncdf4::ncvar_get(nc, "lev"), error=function(e) {0})

    # Create a yes no indicators for depth.
    depth <- ifelse(length(depth_values) > 1, "Y", "N")

    # Now check the other depth option
    depth_values <- tryCatch(ncdf4::ncvar_get(nc, "depth"), error=function(e) {0})

    # Create a yes no indicators for depth.
    depth <- ifelse(length(depth_values) > 1, "Y", depth)

    # Compile the infromation into a list.
    output <- rbind(output, c(ncdf_path, depth))

  } # end of for loop

 names(output) <-c("path", "depth")
 return(output)

} # end check_depth


# ------------------------------------------------------------------------------
# 3. cmip.file_info()
# ------------------------------------------------------------------------------
#' Parse out cmip file information from the cmip netcdf file name
#'
#' \code{cmip.file_info} cmip5 netcdfs should all have the same file name, this
#' function splits the file name into relevant cmip5 file information such as
#' model name, ensemble member, experiment, ect.
#'
#' @param file_path a vector containing the path to cmip5 files to process
#' @importFrom dplyr %>%
#' @return a data frame file name and relevant cmip5 file information
#' @keywords internal

cmip.file_info <- function(file_path){

  # Convert the vector of paths to netcdf files into a tibble and
  # then isolate the file name from the path.
  tibble::as_tibble(file_path) %>%
    dplyr::rename("path" = value) %>%
    dplyr::mutate(filename = basename(path)) ->
    df

  # Determine how to parse out the cmip 5 information base on
  # domain. aka if the netcdf contains time series or just
  # meta data.
  meta_data <- unique(grepl("areacella_fx|sftlf_fx|areacello_fx", df$filename))

  if(meta_data == TRUE){
    # The data frames do not contain any time information.
    #
    # Now extract the cmip information from the file name.
    df %>%
      tidyr::separate(filename, into = c("variable", "domain", "model", "experiment",
                                         "ensemble"), sep = "_", remove = FALSE)
  } else {
    # The data frames contains time information and therefore
    # cmip 5 informaiton must be parsed to contain the time info.
    #
    # Now extract the cmip information from the file name.
    df %>%
      tidyr::separate(filename, into = c("variable", "domain", "model", "experiment",
                                         "ensemble", "time"), sep = "_", remove = FALSE) %>%
      # Remove time, it's unnecessary and export df
      dplyr::select(-time)
  } # end of the if meta data
} # end of cmip_file_info

# ---
# End
