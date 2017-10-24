# ------------------------------------------------------------------------------
# Purpose: This script contains all of the cdo family of functions that use the
# cdo in some capacity, whether it is is to analyze or process the netcdfs.
#
# Created by: Dorheim, Kalyn
# Created on: August 10 2017
# Modified:   xxx
#
# Notes:
# ------------------------------------------------------------------------------
# cdo.concatenate_regrid function
# ------------------------------------------------------------------------------
#' Concatenate netcdfs, convert to absolute time, and regrid the netcdf.
#'
#' \code{cdo.concatenate_regrid}
#'
#' @param df a data frame containing the a path to the netcdf files to concatenate together then regrid
#' @param cdo_path the path to the cdo to use, the default is set to the cdo installed on pic
#' @param ofile the file to write the concatenated netcdf to, default is set to my scratch file on pic
#' @return a regrided, concatenated netcdf with absolute time
#' @keywords internal

cdo.concatenate_regrid <- function(df, cdo_path, ofile1, ofile2){

  # Check input columns and make sure you are concatenating files for one model together.
  check.column(df = df, df_name = "files to concatenate", required_columns = c("path", "model"))

  model <- unique(df$model)
  if(length( model ) > 1){
    stop("Problem with cdo.concatenate call -- about concatenate multiple models together")
  }

  # This function depends on CDO (https://code.zmaw.de/projects/cdo) being installed.
  # Check to make sure that it is installed at the location defined as the parameter.
  stopifnot(file.exists(cdo_path))


  # Select the path to the ncdf files to concatenate.
  ncdf_files <- df[["path"]]

  # Concatenate all files together into a single large temporary file we have to do this because files may contain
  # partial years, on the command line: cdo -a copy f1 f2 f3... temporary file (Russell's code)
  message("Concatenating ",  model," files and converting to absolute time.\n")
  system2(cdo_path, args = c("-a", "copy", ncdf_files, ofile1), stdout = TRUE, stderr = TRUE)
  system2(cdo_path, args = c("remapbil,r360x180", ofile1, ofile2), stdout = TRUE, stderr = TRUE)

} # end of cdo.concatenate


# ------------------------------------------------------------------------------
# sellonlat_operator
# ------------------------------------------------------------------------------
#' Applies a cdo operator to a defined geographical region
#'
#' \code{sellonlat_operator} is function that is used internally in the cdo.sellonlat
#' function. This function will use lat corrdinates (-90 to 90) and lon coordinates (0 to 360)
#' to select observations within a box drawn between the min ad max lat/lon.
#'
#' @param basin_df a data frame of the geographic boundaries of the defined region
#' @return a data frame of the processed netcdf data and CMIP5 file information for a single region
#' @keywords internal


sellonlat_operator <- function(basin_df){

  # Save the basin information to be used in the sellonlatbox operator.
  basin_name  <- basin_df["basin"]
  lon <- c(as.numeric(basin_df["lon1"]), as.numeric(basin_df["lon2"]))
  lat <- c(as.numeric(basin_df["lat1"]), as.numeric(basin_df["lat2"]))

  lon1 <- min(lon); lon2 <- max(lon)
  lat1 <- min(lat); lat2 <- max(lat)

  # Format the basin information for use in the cdo command.
  lat_arguments <- paste0("sellonlatbox,",lon1,",",lon2,",",lat1,",",lat2)

  # Use the selected cdo command to process the data netcdf for a specific geographic area.
  system2(internal_cdo_path, args = c(cdo_operator, paste0("-",lat_arguments), regrided, basinmean), stdout = TRUE, stderr = TRUE )

  # Extract the processed variable from the output netcdf. The expected output check prevents
  # a fatal error incase the cdo was unable to process the netcdfs. If the expect output
  # from the cdo does not exsist the this function will return dumby data with values of
  # -999 as output.
  expected_out <- file.exists(basinmean)
  if(expected_out == TRUE){

    nc_out  <- ncdf4::nc_open(basinmean)
    var_out <- ncdf4::ncvar_get(nc_out, v)
    var_uni <- ncdf4::ncatt_get(nc_out, v)["units"]
    tim_out <- ncdf4::ncvar_get(nc_out, "time")

    # Create and format a data frame for output.
    tibble::tibble(time = tim_out, value = var_out) %>%
      dplyr::mutate(variable = paste0(v), units = paste0(var_uni), basin = paste0(basin_name), model = paste0(mo),
                    ensemble = paste0(en), experiment = paste0(ex), method = paste0(cdo_operator)) ->
      out


  } else {

    # Create bad observation values to add to return.
    var_out <- c(-999, -999)
    var_uni <- c(-999,-999)
    tim_out <- c(-999,-999)

    tibble::tibble(time = c(-999, -999), value = c(-999,-999)) %>%
      dplyr::mutate(variable = paste0(v), units = paste0(var_uni), basin = paste0(basin_name), model = paste0(mo),
                    ensemble = paste0(en), experiment = paste0(ex), method = paste0(cdo_operator)) ->
      out

  }

  return(out)

} # end of the sellonlat_operator

# ------------------------------------------------------------------------------
# multiple_basins_sellonlat function
# ------------------------------------------------------------------------------
#' Internal function that applies a cdo operator to all of the defined geographical regions
#'
#' \code{multiple_basins_sellonlat} is function that is used internally in the cdo.sellonlat function
#'
#' @param internal_df a data frame created in the cdo.sellonlat function
#' @return a data frame of the processed netcdf data for a single variable/model in all of the regions
#' @keywords internal

# Successful run on pic 9/18

multiple_basins_sellonlat <- function(internal_df){

  # First prepare the internal.sellonlat_cdo dependencies
  #
  # Collect cmip information to add to the output in internal.sellonlat_cdo
  v   <<- unique(internal_df$variable)
  mo  <<- unique(internal_df$model)
  en  <<- unique(internal_df$ensemble)
  ex  <<- unique(internal_df$experiment)

  # Check the to make sure the internal.sellonlat_cdo requirements are met
  # Check to make sure that it is only one model and that there is only one
  # file being used to generate the weights.
  if(length(mo) > 1){
    stop("Observations from multiple models is being fed into the internal.sellonlat_cdo function")
  }

  # Make ncdfs to store intermediate outputs in
  #
  # After we passed the tests & checks and also the formating steps now make the
  # intermediate netcdf files. Define as a global variable inside the function
  concatenated  <<- paste0(temp_dir,"concatenated_", v, "_", mo)
  regrided      <<- paste0(temp_dir,"regrid_", v, "_", mo)
  basinmean    <<- paste0(temp_dir,"basinmean_", v, "_", mo)


  # First concatenate data observations
  df <- internal_df
  message(internal_df)
  cdo.concatenate_regrid(internal_df, cdo_path = internal_cdo_path, ofile1 = concatenated, ofile2 = regrided)

  # Progress statement
  message("Processing ", mo," ", ex, " ", v, " by basin.\n")

  # Do the sellonat operator for all the basins and format output into a data frame.
  out_list <- apply(defined_basins, 1, sellonlat_operator)
  out <- dplyr::bind_rows(out_list)

  # Clean Up
  file.remove(concatenated)
  file.remove(regrided)
  file.remove(basinmean)

  # Return output
  return(out)

}

# ------------------------------------------------------------------------------
# cdo.sellonlat function
# ------------------------------------------------------------------------------
#' The R functional form of the cdo operator sellonlat
#'
#' \code{cdo.sellonlat} is function that applies a cdo operator over a defined
#' geographical regions. Becasue of some issue with the version of dplyr on pic
#' this function returns a nested list and some minor post processing is required bleh.
#'
#' @param data_input a data frame containing cmip info and the path to the data netcdfs
#' @param defined_basins a data frame containing the lat/lon boundaries of an ocean basin
#' @param intermediate_output default set to BASE_NAME, it is the defined location where to save any intermediate output
#' @param cdo_path the path to the cdo to use
#' @param temp_dir the path to the location where to save the intermediate netcdfs from the cdo
#' @return a data frame of the all the processed netcdf data in all of the regions

# Successful run on pic 9/18

cdo.sellonlat <- function(data_input, defined_basins, cdo_operator = "fldmean", intermediate_output = BASE_NAME,
                          cdo_path = "/share/apps/netcdf/4.3.2/gcc/4.4.7/bin/cdo", temp_dir = "/pic/scratch/dorh012/"){

  # Define global variables used by the internal functions.
  internal_cdo_path <<- cdo_path
  cdo_operator      <<- cdo_operator
  defined_basins    <<- defined_basins
  temp_dir          <<- temp_dir


  # Chekcs
  check.column(df = data_input, df_name = "data input data frame", required_columns = c("path", "model", "experiment", "ensemble"))

  # This function depends on CDO (https://code.zmaw.de/projects/cdo) being installed.
  # Check to make sure that it is installed at the location defined as the parameter.
  stopifnot(file.exists(paste0(internal_cdo_path)))

  # Check to make sure that the cdo operator chosen exists if not, then fatal
  # error will occur
  operator_test <- system2(internal_cdo_path , args = c("-h ", cdo_operator))
  if(tryCatch(operator_test == 1)){
    stop("cdo_operator was not executable by cdo.")
  }

  # Check the intermediate directories.
  if(tryCatch(file.exists(paste0(intermediate_output))) == FALSE){
    intermediate_output <- getwd()
    message(paste0("intermediate_output files saved at ", intermediate_output))
  }

  if(!file.exists(paste0(temp_dir))){
    stop("temp_dir, location for saving intermediate netcdfs does not exists.")
  }


  # Select only the required information from the input data frames and rename the path
  # columns to prepare to match area, data, and fraction paths together by model. In the
  # following steps this match will be used to generate the weights for the defined cdo
  # command.
  to_process <- dplyr::select(data_input, path, model, variable, ensemble, experiment)

  # Save the cmip file files to be processed
  write.csv(to_process, paste0(intermediate_output,"tobeprocessed.csv"), row.names = FALSE)


  # Use the internal functions to execute the sellonlat cdo operator.
  to_process %>%
    group_by(ensemble, experiment, model, variable) %>%
    do(out = multiple_basins_sellonlat(.)) ->
    output_list
  out <- bind_rows(output_list$out)


  # Return
  return(out)

}

# ----
# End
