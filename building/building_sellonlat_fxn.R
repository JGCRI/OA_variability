# ------------------------------------------------------------------------------
# Purpose: functionalize the sellect lat lon box cdo operator for different regions
# BUILDING
#
# Created by: Dorheim, Kalyn
# Created on: Augut 31 2017
# Modified:   xxx
# ------------------------------------------------------------------------------
# I have lots of notes but I just wanted to start pseudo coding to see how the
# peices are fitting toghether here
# ------------
# inputs
# ------------
cdo_path = "/share/apps/netcdf/4.3.2/gcc/4.4.7/bin/cdo"
cdo_operator = "fldmean" # may be set as default but user can change
temp_dir = "/pic/scratch/dorh012/"
area_input <- data.frame(c(1), c(1), c(1), c(1), c(1))
data_input <- data.frame()
BASE_NAME <- "/pic/projects/GCAM/Dorheim/"
intermediate_output <- BASE_NAME

# ------------
# Checks
# ------------
# Columns names
req_cols <- c("path", "model", "experiment", "ensemble")
check.column(df = area_input, df_name = "area input data frame", required_columns = req_cols)
check.column(df = data_input, df_name = "data input data frame", required_columns = req_cols)

# This function depends on CDO (https://code.zmaw.de/projects/cdo) being installed.
# Check to make sure that it is installed at the location defined as the parameter.
#
stopifnot(file.exists(cdo_path))

# Check to make sure that the cdo operator chosen exsists if not, then fatal
# error will occur
#
operator_test <- system2(cdo_path, args = c("-h ", cdo_operator))
if(tryCatch(operator_test == 1)){
  stop("cdo_operator was not executable by cdo.")
}

# Check the intermediate directories.
#
if(tryCatch(file.exists(intermediate_output)) == FALSE){
  intermediate_output <- getwd()
  message(paste0("intermediate_output files saved at ", intermediate_output))
}

if(!file.exists(temp_dir)){
  stop("temp_dir, location for saving intermediate netcdfs does not exsist.")
  }


# --------------------------
# Modify Input Data Frames
# --------------------------
# Select only the required information from the input data frames and rename the path
# columns to prepare to match area, data, and fraction paths together by model. In the
# following steps this match will be used to generate the weights for the defined cdo
# command.

area_df <- dplyr::select(area_input, area_path = path, model)
data_df <- dplyr::select(data_input, data_path = path, model, variable, ensemble, experiment)


# Match Data Frames
# ------------------
# Match the input data frames by model.
data_df %>%
  dplyr::left_join(area_df, by = "model") ->
  matched_paths

# Determine which of the data files are missing either area or fraction input
# save for output (DECISION as a csv file or included in the list!!)
matched_paths %>%
  filter(is.na(data_path), is.na(area_path)) ->
  missing_netcdfs

# Save the missing netcdfs
write.csv(missing_netcdfs, paste0(intermediate_output,"missing_netcdfs.csv"), row.names = FALSE)

# Select the data netcdf files to process that contain all the data file
# and also the files needed to generate weights.
matched_paths %>%
  na.omit ->
  to_process

# Save the cmip file files to be processed
write.csv(to_process, paste0(intermediate_output,"tobeprocessed.csv"), row.names = FALSE)

# After we passed the tests & checks and also the formating steps now make the
# intermediate netcdf files
concatenated <- paste0(temp_dir,"temp1")
ofile        <- paste0(temp_dir,"temp2")

cdo.concatenate(to_process %>% rename(path = data_path), cdo_path, concatenated)

area_weights <- to_process$area_path

# I think I will execute this by pipeline group_by(basin) %>% do(x = internal.sellonlat) can i take in multiplt data frames>>> thats my big quation now

internal.sellonlat <- function(basin_df){

  basin_name  <- basin_df["basin"]
  west_bound  <- basin_df["lon1"]
  east_bound  <- basin_df["lon2"]
  south_bound <- basin_df["lat1"]
  north_bound <- basin_df["lat2"]

  lat_arguments <- paste0( "-sellonlatbox", ",", west_bound,",", east_bound,",", south_bound,",", north_bound, sep =",")

  system2(WHICH_CDO, args = c(cdo_operator, paste0("-setgridarea,",area_weights), lat_arguments, concatenated, ofile),
          stdout = TRUE, stderr = TRUE )

  # Extract the weighted mean tas from the single regular grid.
  ofile %>%
    nc_open %>%
    ncvar_get(variable) %>%
    matrix(ncol = 1) %>%
    rebane*(->
    value

  names(value) <- basin_name
  #Return a list of the annual weighted mean tas for a single region.
  return(value)
}




