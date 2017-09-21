# ------------------------------------------------------------------------------
# Purpose: This script contains all of the cdo family of functions that use the
# cdo in some capacity, whether it is is to analyze or process the netcdfs.
# The functions defined in this script include the following: cdo.concatenate()
# cdo.processor(), internal.sellonlat_cdo(), internal.sellonlat(), and
# cdo.sellonlat()
#
# Created by: Dorheim, Kalyn
# Created on: August 10 2017
# Modified:   xxx
#
# Notes: Remove the hard coded defaults related to pic?
# ------------------------------------------------------------------------------
# 1. cdo.concatenate function
# ------------------------------------------------------------------------------
#' Concatenate netcdfs and convert to absolute time
#'
#' \code{cdo.concatenate}
#'
#' @param df a data frame containing the a path to the netcdf files to concatenate together
#' @param cdo_path the path to the cdo to use, the default is set to the cdo installed on pic
#' @param ofile the file to write the concatenated netcdf to, default is set to my scratch file on pic
#' @return a concatenated netcdf with absolute time
#' @keywords internal

cdo.concatenate <- function(df, cdo_path, ofile){

  # Checks
  # ------------
  # Columns names
  check.column(df = df, df_name = "files to concatenate", required_columns = c("path", "model"))

  # Check to make sure concatenating only one file
  model <- unique(df$model)
  if(length( model ) > 1){
    stop("Problem with cdo.concatenate call -- about concatenate multiple models together")
  }

  # This function depends on CDO (https://code.zmaw.de/projects/cdo) being installed.
  # Check to make sure that it is installed at the location defined as the parameter.
  stopifnot(file.exists(cdo_path))


  # Functional Code
  # --------------------------------------------------
  # Select the path to the ncdf files to concatenate.
  ncdf_files <- df[["path"]]

  # Concatenate all files together into a single large temporary file we have to do this because files may contain
  # partial years, on the command line: cdo -a copy f1 f2 f3... temporary file (Russell's code)
  message("Concatenating ",  model," files and converting to absolute time.\n")
  system2(cdo_path, args = c("-a", "copy", ncdf_files, ofile), stdout = TRUE, stderr = TRUE)

} # end of cdo.concatenate


# ------------------------------------------------------------------------------
# 2. cdo.processor function
# ------------------------------------------------------------------------------
#' Concatenates files CMIP5 netcdfs together and then carries out a single cdo command line
#'
#' \code{cdo.processor}
#'
#' @param df is the dataframe of the file path and cmip5 information
#' @param cdo_path is the path of the location of the cdo used to process the netcdfs
#' @param cdo_command the name of the cdo command(s) that will be used to process the netcdf
#' @param ofile is the path to the location where to save a temporary netcdf
#' @return a data frame of the processed netcdf data and CMIP5 file information

# Notes: passed test on pic 8/23/2017

cdo.processor <- function(df, cdo_path = "/share/apps/netcdf/4.3.2/gcc/4.4.7/bin/cdo", cdo_command, ofile = "/pic/scratch/dorh012/tfile1"){

  # Checks
  # ------------
  # Columns names
  req_cols <- c("path", "model", "experiment", "ensemble")
  check.column(df = df, df_name = "files used in cdo.fldmean", required_columns = req_cols)

  # This function depends on CDO (https://code.zmaw.de/projects/cdo) being installed.
  # Check to make sure that it is installed at the location defined as the parameter.
  stopifnot(file.exists(cdo_path))

  message(df$path)

  # CMIP 5 info
  # ------------
  # Collect the CMIP5 run information ie, model name, experiment, ensemble ect and
  # store as a data frame.
  m   <- df$model %>%  unique
  en  <- df$ensemble %>% unique
  ex  <- df$experiment %>% unique
  var <- df$variable %>% unique
  cmip_info <- data.frame(model = m, ensemble = en, experiment = ex, variable = var)

  # Because the files read into this function will be concatenated together we want
  # to make sure that it only contains netcdfs for one CMIP5 model/ensemble/experiment.
  # Check that it is a unique combination here.
  if(dim(cmip_info)[1] > 1){
    stop("Error in cdo.fldmean, attempting to concatenate multiple CMIP5 model/ensemble/experiments together")
  } else if (dim(cmip_info)[1] < 1){
    stop("Error in cdo.fldmean, could not identify any CMIP5 netdfs to concatenate")
  }


  # Concatenate
  # --------------
  # Concatenate all files together into a single large temporary file we have to do this because files may contain
  # partial years, on the command line: cdo -a copy f1 f2 f3... temporary file (Russell's code).
  # Use the default settings from the cdo.fldmean for the path to the CDO and the ofile
  tfile <- paste0(ofile,"_2")
  cdo.concatenate(df, cdo_path = cdo_path, ofile = tfile)


  # Do the cdo
  # ------------
  message("Doing ", paste0(cdo_command))
  tryCatch(system2(cdo_path, args = c(cdo_command, tfile, ofile), stdout = TRUE, stderr = TRUE))


  # Save info in df
  # ------------------
  # Open the ncdf created by the lat cdo function
  nc     <- ncdf4::nc_open(ofile)
  name   <- ofile

  # Extract information about time from the ncdf created by cdo command. Uncomment the
  # t units if there is any
  time            <- ncdf4::ncvar_get(nc, "time")
  year            <- substr(time, 1, 4)
  month           <- substr(time, 5, 6)
  #tunits_format   <- ncdf4::ncatt_get(nc, "time", "units")$value
  #tunits_calander <- ncdf4::ncatt_get(nc, "time")$calendar
  #tunits          <- paste0(tunits_format,"~",tunits_calander)
  variable_units  <- ncdf4::ncatt_get(nc, var)$units
  result          <- ncdf4::ncvar_get(nc, var)

  # Create a data frame of the results and cmip5 info to return.
  out             <- data.frame(time, units = variable_units, value = result)
  #out$time_units  <- tunits
  out$year        <- year
  out$month       <- month
  out$model       <- m
  out$experiment  <- ex
  out$variable    <- var
  out$ensemble    <- en
  out$method_used <- cdo_command


  # Remove the netcds
  # ------------------
  file.remove(tfile)
  file.remove(ofile)

  # Output
  # -------
  # Return a data frame of weighted mean tas, time, model, ensemble, etcetera. info
  return(out)

} # cdo.processor

# ------------------------------------------------------------------------------
# 3. internal.sellonlat_cdo function
# ------------------------------------------------------------------------------
#' Applies a cdo operator to a defined geographical region
#'
#' \code{internal.sellonlat_cdo} is function that is used internally in the cdo.sellonlat
#' function.
#'
#' @param basin_df a data frame of the geographic boundaries of the defined region
#' @return a data frame of the processed netcdf data and CMIP5 file information for a single region
#' @keywords internal

# Successful run on pic 9/18

internal.sellonlat_cdo <- function(basin_df){

  # Save the basin information to be used in the sellonlatbox operator.
  basin_name  <- basin_df["basin"]; message(basin_name)
  south_bound <- as.numeric(basin_df["lat1"])
  north_bound <- as.numeric(basin_df["lat2"])

  # Determine if you need convert the lon coordinates aka
  # is goes from 0 to 360 instead of -180 to 180
  ncdf4::nc_open(area_weights) %>%
    ncdf4::ncvar_get(., "lon") %>%
    max ->
    lon_max

  # If the maximum longitude of the system is greater than 180 then
  # convert the negative lon coordinates.
  if(lon_max > 180){
    new_lon1 <- ifelse(as.numeric(basin_df["lon1"]) < 1, 180 + (360 + as.numeric(basin_df["lon1"])), as.numeric(basin_df["lon1"]))
    new_lon2 <- ifelse(as.numeric(basin_df["lon2"]) < 1, 180 + (360 + as.numeric(basin_df["lon2"])), as.numeric(basin_df["lon2"]))
    new_lon  <- c(new_lon1, new_lon2)

    west_bound <- min(new_lon)
    east_bound <- max(new_lon)

  } else {
    west_bound  <- as.numeric(basin_df["lon1"])
    east_bound  <- as.numeric(basin_df["lon2"])
  }


  # Format the basin information for use in the cdo command.
  lat_arguments <- paste0("sellonlatbox,",west_bound,",",east_bound,",",south_bound,",",north_bound)

  # Select basin area weights
  system2(internal_cdo_path, args = c(lat_arguments, area_weights, basin_weights), stdout = TRUE, stderr = TRUE )

  # Use the selected cdo command to process the data netcdf using the weights from the area netcdf for a specific
  # geographic area.
  system2(internal_cdo_path, args = c(cdo_operator, paste0("-setgridarea,",basin_weights), paste0("-",lat_arguments), concatenated, ofile), stdout = TRUE, stderr = TRUE )

  # Extract the processed variable from the output netcdf
  nc_out  <- ncdf4::nc_open(ofile)
  var_out <- ncdf4::ncvar_get(nc_out, v)
  var_uni <- ncdf4::ncvar_get(nc_out, v)["units"]
  tim_out <- ncdf4::ncvar_get(nc_out, "time")

  # Create and format a data frame for output.
  cbind(time = tim_out, value = var_out) %>%
    as.data.frame %>%
    dplyr::mutate(variable = paste0(v), units = paste0(var_uni), basin = paste0(basin_name), model = paste0(mo),
                  ensemble = paste0(en), experiment = paste0(ex), method = paste0(cdo_operator))

} # end of the internal.sellonlat_cdo function

# ------------------------------------------------------------------------------
# 4. internal.sellonlat function
# ------------------------------------------------------------------------------
#' Internal function that applies a cdo operator to all of the defined geographical regions
#'
#' \code{internal.sellonlat} is function that is used internally in the cdo.sellonlat function
#'
#' @param internal_df a data frame created in the cdo.sellonlat function
#' @return a data frame of the processed netcdf data for a single variable/model in all of the regions
#' @keywords internal

# Successful run on pic 9/18

internal.sellonlat <- function(internal_df){

  # First prepare the internal.sellonlat_cdo dependencies
  #
  # Determine the netcdf to use as the area weights in the cdo commands.
  area_weights <<- unique(internal_df$area_path)

  # Collect cmip information to add to the output in internal.sellonlat_cdo
  v   <<- unique(internal_df$variable)
  mo  <<- unique(internal_df$model)
  en  <<- unique(internal_df$ensemble)
  ex  <<- unique(internal_df$experiment)

  # Check the to make sure the internal.sellonlat_cdo requirements are met
  #
  # KALYN - might be able to remove these checks after you finalize the the big function
  #
  # Check to make sure that it is only one model and that there is only one
  # file being used to generate the weights.
  if(length(mo) > 1){
    stop("Observations from multiple models is being fed into the internal.sellonlat_cdo function")
  }
  if(length(area_weights) > 1){
    stop("Multiple netcdfs identified for use as area weights")
  }

  # Execute cdo commands
  #
  # First concatenate data observations
  internal_df %>%
    rename(path = data_path) ->
    df
  cdo.concatenate(df, cdo_path = internal_cdo_path, ofile = concatenated)

  # So I know that this does not look like it makes a lot of
  # sense not v clear but v fast... need to comment to make sure
  # it is clear what we are doing here...
  message("Processing ", mo, " by basin.\n")
  bind_rows(apply(defined_basins, 1, internal.sellonlat_cdo))

}

# ------------------------------------------------------------------------------
# 5. cdo.sellonlat function
# ------------------------------------------------------------------------------
#' The R functional form of the cdo operator sellonlat
#'
#' \code{cdo.sellonlat} is function that applies a cdo operator over a defined
#' geographical regions.
#'
#' @param area_input a data frame containing cmip info and the path to the cell area netcdfs
#' @param data_input a data frame containing cmip info and the path to the data netcdfs
#' @param defined_basins a data frame containing the lat/lon boundaries of an ocean basin
#' @param intermediate_output default set to BASE_NAME, it is the defined location where to save any intermediate output
#' @param cdo_path the path to the cdo to use
#' @param temp_dir the path to the location where to save the intermediate netcdfs from the cdo
#' @return a data frame of the all the processed netcdf data in all of the regions

# Successful run on pic 9/18

cdo.sellonlat <- function(area_input, data_input, defined_basins, cdo_operator = "fldmean", intermediate_output = BASE_NAME, cdo_path = "/share/apps/netcdf/4.3.2/gcc/4.4.7/bin/cdo", temp_dir = "/pic/scratch/dorh012/"){

  # Define global variables used by the internal functions.
  internal_cdo_path <<- cdo_path
  cdo_operator      <<- cdo_operator
  defined_basins    <<- defined_basins

  # Checks
  #
  # Columns names
  req_cols <- c("path", "model", "experiment", "ensemble")
  check.column(df = area_input, df_name = "area input data frame", required_columns = req_cols)
  check.column(df = data_input, df_name = "data input data frame", required_columns = req_cols)

  # This function depends on CDO (https://code.zmaw.de/projects/cdo) being installed.
  # Check to make sure that it is installed at the location defined as the parameter.
  #
  stopifnot(file.exists(paste0(internal_cdo_path)))

  # Check to make sure that the cdo operator chosen exists if not, then fatal
  # error will occur
  #
  operator_test <- system2(internal_cdo_path , args = c("-h ", cdo_operator))
  if(tryCatch(operator_test == 1)){
    stop("cdo_operator was not executable by cdo.")
  }

  # Check the intermediate directories.
  #
  if(tryCatch(file.exists(paste0(intermediate_output))) == FALSE){
    intermediate_output <- getwd()
    message(paste0("intermediate_output files saved at ", intermediate_output))
  }

  if(!file.exists(paste0(temp_dir))){
    stop("temp_dir, location for saving intermediate netcdfs does not exists.")
  }


  # Modify Input Data Frames
  #
  # Select only the required information from the input data frames and rename the path
  # columns to prepare to match area, data, and fraction paths together by model. In the
  # following steps this match will be used to generate the weights for the defined cdo
  # command.

  area_df <- dplyr::select(area_input, area_path = path, model)
  data_df <- dplyr::select(data_input, data_path = path, model, variable, ensemble, experiment)


  # Match Data Frames
  #
  # Match the area and data input files by model name so that the correct
  # area netcdf if used as the area weights.
  data_df %>%
    dplyr::left_join(area_df, by = "model") ->
    matched_paths

  # Determine which of the data files are missing either area or fraction input
  # save for output
  matched_paths %>%
    filter(is.na(data_path) | is.na(area_path)) ->
    missing_netcdfs

  if(nrow(missing_netcdfs) == 0){
 #   missing_netcdfs <- "None identified."
  }

  # Save the missing netcdfs
  write.csv(missing_netcdfs, paste0(intermediate_output,"missing_netcdfs.csv"), row.names = FALSE)

  # Select the data netcdf files to process that contain all the data file
  # and also the files needed to generate weights.
  matched_paths %>%
    filter(!is.na(data_path), !is.na(area_path)) ->
    to_process

  # Save the cmip file files to be processed
  write.csv(to_process, paste0(intermediate_output,"tobeprocessed.csv"), row.names = FALSE)


  # Make ncdfs to store intermediate outputs in
  #
  # After we passed the tests & checks and also the formating steps now make the
  # intermediate netcdf files. Define as a global variable inside the function
  concatenated  <<- paste0(temp_dir,"temp1")
  ofile         <<- paste0(temp_dir,"temp2")
  basin_weights <<- paste0(temp_dir, "temp3")


  # Use the internal functions to execute the sellonlat cdo operator.
  to_process %>%
    group_by(ensemble, experiment, model, variable) %>%
    do(out = internal.sellonlat(.)) ->
    output_list

  # Return
  return(bind_rows(output_list$out))

  # Clean Up
  file.remove(ofile)
  file.remove(concatenated)

}

# ----
# End
