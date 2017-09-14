# ------------------------------------------------------------------------------
# Purpose: This script contains all of the cdo family of functions that use the
# cdo in some capatcity, whether it is is to analyze or process the netcdfs.
# The functions defined in this script include the following: cdo.concatenate()
# and cdo.processor().
#
# Created by: Dorheim, Kalyn
# Created on: Augut 10 2017
# Modified:   xxx
#
# Notes: Sucessful run on pic 8/23/17
# ------------------------------------------------------------------------------
# 1. cdo.concatenate()
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
  check.column(df = df, df_name = "files to concatenate", required_columns = c("path"))

  # This function depends on CDO (https://code.zmaw.de/projects/cdo) being installed.
  # Check to make sure that it is installed at the location defined as the parameter.
  stopifnot(file.exists(cdo_path))


  # Functional Code
  # --------------------------------------------------
  # Select the path to the ncdf files to concatenate.
  ncdf_files <- df[["path"]]

  # Concatenate all files together into a single large temporary file we have to do this because files may contain
  # partial years, on the command line: cdo -a copy f1 f2 f3... temporary file (Russell's code)
  message("Concatenating files and converting to absolute time...")
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
  # Collect the CMIP5 run information ie, modle name, experiment, ensemble ect and
  # store as a data frame.
  m   <- df$model %>%  unique
  en  <- df$ensemble %>% unique
  ex  <- df$experiment %>% unique
  var <- df$variable %>% unique
  cmip_info <- data.frame(model = m, ensemble = en, experiment = ex, variable = var)

  # Because the files read into this function will be concactenated together we want
  # to make sure that it only contains netcdfs for one CMIP5 model/ensemble/experiment.
  # Check that it is a unqiue combination here.
  if(dim(cmip_info)[1] > 1){
    stop("Error in cdo.fldmean, attempting to concatenate mulitple CMIP5 model/ensemble/experiments togethere")
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


# ----
# End
