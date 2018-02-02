# Notes after getting the functions to work I had a few ideas, first of all could I subset
# for the years of interest before getting the amplitude? Should I include a check for monthly
# data? Could I have combined the cdo.percent change and the single_percentChange functions
# into a single function? okay until we get all of the complete stuff all in one place these functions
# are not gonna be that helpful


# cdo.concatenate function
#' Concatenate netcdfs and converts to absolute time
#'
#' \code{cdo.concatenate}
#'
#' @param df a data frame containing the a path to the netcdf files to concatenate together then regrid
#' @param cdo_path the path to the cdo to use, the default is set to the cdo installed on pic
#' @param ofile the file to write the concatenated netcdf to, default is set to my scratch file on pic
#' @return a regrided, concatenated netcdf with absolute time
#' @keywords internal

cdo.concatenate_map <- function(df, cdo_path, ofile1, showMessages = F){

  # Checks
  check.column(df = df, df_name = "files to concatenate", required_columns = c("path", "model"))

  df %>%
    select(model, variable, ensemble, experiment) %>%
    distinct ->
    unique_df

  if(nrow(unique_df) > 1){
    stop("Problem with cdo.concatenate call, about concatenate multiple models/ variable / ensembles together")
    }

  # This function depends on CDO (https://code.zmaw.de/projects/cdo) being installed.
  # Check to make sure that it is installed at the location defined as the parameter.
  stopifnot(file.exists(cdo_path))


  # Select the path to the ncdf files to concatenate.
  ncdf_files <- df[["path"]]

  # Concatenate all files together into a single large temporary file we have to do this because files may contain
  # partial years, on the command line: cdo -a copy f1 f2 f3... temporary file (Russell's code)
  if(showMessages){message("Concatenating ",  unique_df["model"]," files and converting to absolute time.\n")}
  system2(cdo_path, args = c("-a", "copy", ncdf_files, ofile1), stdout = TRUE, stderr = TRUE)

}



# cdo.period_mean_amplitude
#' Create nc of average seasonality for some time period
#'
#' \code{cdo.period_mean_amplitude}
#'
#' @param df a data frame containing the a path to the netcdf files to concatenate then calculate average seasonality
#' @param cdo_path the path to the cdo to use, the default is set to the cdo installed on pic
#' @param years a vector of the years to select to include in the period average, important the cdo does NOT
#'  use a range but instead will select values for each year specified in this vector.
#' @param nc_dir the directory location to save all of the intermediate ncs at.
#' @param cleanUp default set to T to remove all of the intermediate ncs, if set to F then the .nc will be saved at the nc_dir
#' @param showMessages default is set to F but when set to T then all of the messages will be printed out
#' @return the average seasonal amplitude .nc will be saved at the nc_dir, this function will return a string of the filepath

cdo.period_mean_amplitude <- function(df, cdo_path, years, nc_dir, cleanUp = T, showMessages = F){

  # Check if inputs exists
  dir.exists(nc_dir)

  # Create the file names
  df %>%
    mutate(file = ".nc") %>%
    select(model, experiment, variable, file, ensemble) %>%
    tidyr::unite(name, model, experiment, variable, ensemble, file) %>%
    distinct %>%
    .[["name"]]->
    nc_baseName

  # Define the specific nc files
  concatenated_nc <- file.path(nc_dir, paste0("concatenated_", nc_baseName))
  amplitude_nc <- file.path(nc_dir, paste0("amplitude_", nc_baseName))
  avgAmplitude_nc <- file.path(nc_dir, paste0("avgAmplitude_", nc_baseName))


  # Concatenate the cmip information data frame
  cdo.concatenate_map(df, cdo_path, concatenated_nc, showMessages)

  # Calculate amplitude the annual absolute range of value
  if(showMessages){message("Annual amplitude saved at ", nc_baseName)}
  system2(cdo_path, args = c("abs", "-sub", "-yearmax", concatenated_nc, "-yearmin", concatenated_nc, amplitude_nc), stdout = TRUE, stderr = TRUE)

  # Generate the selectYears_operator
  selectYears_operator <- paste0("-select,year=",paste(years, collapse = ","))
  if(showMessages){message("Average over ", paste(years, collapse = " ,"), " saved at ", nc_baseName)}
  system2(cdo_path, args = c("timavg", selectYears_operator, amplitude_nc, avgAmplitude_nc), stdout = TRUE, stderr = TRUE)

  # Clean up
  if(cleanUp){file.remove(concatenated_nc, amplitude_nc)}

  return(avgAmplitude_nc)

}


# cdo.percent_change_nc
#' cdo command for the percent change in average seasonality
#'
#' \code{cdo.percent_change_nc} This function is the cdo command used to create a percent change nc file. It is used
#' internally in the simple_percentChange which uses this cdo command to calculate the percent change from the
#' two nc files created by the cdo.period_mean_amplitude.
#'
#' @param cdo_path the path to the cdo to use, the default is set to the cdo installed on pic.
#' @param nc0 the file path to the file.nc that is used as the original value for in percent change calculation.
#' @param nc1 the file path to the file.nc that is used as the new value for in percent change calculation.
#' @param nc_dir the directory location to save the final file.nc at
#' @param cleanUp default set to T to remove all of the intermediate ncs, if set to F then the .nc will be saved at the nc_dir
#' @param showMessages default is set to F but when set to T then all of the messages will be printed out
#' @return the file path for the percent change.nc that is created by this function.

cdo.percent_change_nc <- function(cdo_path, nc0, nc1, nc_dir, cleanUp = T, showMessages = F){

  # Check inputs
  if(!any(file.exists(nc0), file.exists(nc1))){stop("cdo.percent_change_nc: one or more of the input nc files does not exists")}

  # Create the percent change file name
  percentChange_name <- gsub("avgAmplitude_", "percentChange", basename(nc0))
  percentChange_nc <- file.path(nc_dir, percentChange_name)

  # Calculate the percent change
  if(showMessages){message("Calculating percent change and saving at ", percentChange_nc)}
  system2(cdo_path, args = c("mulc,100","-div", "-sub", nc1, nc0, nc0, percentChange_nc), stdout = TRUE, stderr = TRUE)

  if(cleanUp){file.remove(nc0, nc1)}

  return(percentChange_nc)

  }


# single_percentChange
#' Create a single percent change nc for a model
#'
#' \code{single_percentChange} this function safely creates the percent change between 2 experiments for the
#' nc for a model / variable / ensemble member. The function is wrapped in the purrr::safely so it returns a list
#' of results and error messages if applicable.
#'
#' @param cmip_df data set of the cmip model / variable / ensemble with 2 experiments to process.
#' @param cdo_path the path to the cdo to use, the default is set to the cdo installed on pic.
#' @param experiment_years the data frame of the experiment, start year and end year to be used to subset the amplitude ncs. The order of the
#' experiments in this data frame will determine nc0 and nc1 used in cdo.percent_change_nc
#' @param nc_dir the directory location where to store the intermediate nc files
#' @param output_dir the directory location where to save the final nc at
#' @param cleanUp default set to T to remove all of the intermediate ncs, if set to F then the .nc will be saved at the nc_dir
#' @param showMessages default is set to F but when set to T then all of the messages will be printed out
#' @return a list of results the location of the nc file if the function was successful and a list of the error messages if an error occurred
#' at any point in time.

single_percentChange <- purrr::safely(function(cmip_df, cdo_path, experiment_years, nc_dir, output_dir, showMessages = F, cleanUp = T){

  # Check to make sure that the cmip data frame to process only contains one model / variable / ensemble but contains
  # information for the two experiments from the experiment years data frames.
  cmip_df %>%
    select(model, variable, ensemble) %>%
    distinct ->
    model_vari_en

  if(nrow(model_vari_en) > 1){stop("single_percentChange: attempting to combine ", model_vari_en)}

  experiment_list <- (unique(cmip_df$experiment))
  if(length(experiment_list) != 2){stop("single_percentChange: problem with number of experiments, attempting to process ", experiment_list)}

  missing_experiments <- !experiment_list %in% experiment_years$experiment
  if(any(missing_experiments)){stop("single_percentChange: there is a problem with the experiments in the cmip data frame are not compatible with the experiment years data frame")}


  # Now calculate average seasonal amplitude for the first experiment aka nc0 in the experiment_years
  experiment0 <- experiment_years[1, ]
  years0 <- experiment0$start_year:experiment0$end_year
  experiment0_cmip <- cmip_df[cmip_df$experiment == experiment0$experiment, ]
  nc0 <- cdo.period_mean_amplitude(experiment0_cmip, cdo_path, years0, nc_dir, cleanUp, showMessages)


  # Now calculate average seasonal amplitude for the first experiment aka nc0 in the experiment_years
  experiment1 <- experiment_years[2, ]
  years1 <- experiment1$start_year:experiment1$end_year
  experiment1_cmip <- cmip_df[cmip_df$experiment == experiment1$experiment, ]

  nc1 <- cdo.period_mean_amplitude(experiment1_cmip, cdo_path, years1, nc_dir, cleanUp, showMessages)

  # Now make the percent change nc and return the file name for the final nc product
  final_nc <- cdo.percent_change_nc(cdo_path, nc0, nc1, output_dir, cleanUp, showMessages)
  return(final_nc)
})


# batch_percentChange
#' Create a percent change nc file for every model / variable / ensemble nc file
#'
#' \code{batch_percentChange} this function safely creates the percent change between 2 experiments for every
#' nc for a model / variable / ensemble member.
#'
#' @param cmip_df data set of the cmip model / variable / ensemble with 2 experiments to process.
#' @param cdo_path the path to the cdo to use, the default is set to the cdo installed on pic.
#' @param experiment_years the data frame of the experiment, start year and end year to be used to subset the amplitude ncs. The order of the
#' experiments in this data frame will determine nc0 and nc1 used in cdo.percent_change_nc
#' @param nc_dir the directory location where to store the intermediate nc files
#' @param output_dir the directory location where to save the final nc at
#' @param cleanUp default set to T to remove all of the intermediate ncs, if set to F then the .nc will be saved at the nc_dir
#' @param showMessages default is set to F but when set to T then all of the messages will be printed out
#' @return a list of results the location of the nc file if the function was successful and a list of the error messages if an error occurred
#' at any point in time.

batch_percentChange <- function(cmip_df, cdo_path, experiment_years, nc_dir, output_dir, showMessages = F, cleanUp = T){

  to_process <- split(cmip_df, interaction(cmip_df$model, cmip_df$variable, cmip_df$ensemble, sep = "_"), drop = F)
  map_output <- purrr::map(to_process, function(.) {single_percentChange(., cdo_path, experiment_years, nc_dir, output_dir, showMessages, cleanUp)} )

  purrr::flatten(map_output)

}
