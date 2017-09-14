# ------------------------------------------------------------------------------
# Purpose: play around with the cdo to figure out how to do the pH depth averages
#
# Created by: Dorheim, Kalyn
# Created on: Augut 10 2017
# Modified:   xxx
#
# Notes: this will not run on local needs connection to cdo
# ------------------------------------------------------------------------------
library(dplyr)

WHICH_CDO  <- "/share/apps/netcdf/4.3.2/gcc/4.4.7/bin/cdo"
pHdata     <- "/pic/projects/GCAM/CMIP5-CHartin/CMIP5_RCP85/ph/ph_Oyr_CNRM-CM5_rcp85_r1i1p1_2056-2065.nc"
ncdf_path    <- "/pic/projects/GCAM/CMIP5-CHartin/CMIP5_RCP85/ph/ph_Oyr_CNRM-CM5_rcp85_r1i1p1_2056-2065.nc"
outfile    <- "/pic/scratch/dorh012/outfile"
outfile2    <- "/pic/scratch/dorh012/outfile2"





tryCatch(system2(WHICH_CDO, args = c("yearmean","-fldmean", "-vertmean", pHdata), stdout = TRUE, stderr = TRUE ))






system2(WHICH_CDO, args = c("sinfov", pHdata), stdout = TRUE, stderr = TRUE )

system2(WHICH_CDO, args = c("yearmean", "-vertmean", pHdata, outfile2), stdout = TRUE, stderr = TRUE )
system2(WHICH_CDO, args = c("showyear", outfile2), stdout = TRUE, stderr = TRUE )

data <- nc_open(pHdata); pH_data <- ncvar_get(ncdf_path, "ph"); mean(pH_data)

pHdata %>%  nc_open -> data

# Check to see if there is a level if it exsists then
x <- tryCatch(ncvar_get(data, "depth"), error=function(e) {0})



if(exists("VARIABLES")) {
  varpattern <- paste(VARIABLES, collapse = "|")
  domaninpattern <- "Amon"
  experimentpattern <- paste(ensemblepattern, collapse = "|")
}

# If statement for ensembles to search for as specific in section 0 or serach for all ensembles.
if(length(ENSEMBLES) > 0){
  ensemblepattern <- paste(ENSEMBLES  , collapse = "|")
} else {
  ensemblepattern <- "[a-zA-Z0-9-]+" #all
}

domainpattern <- modelpattern <-  "[a-zA-Z0-9-]+"  # all

# Create the pattern for CMIP5 file names for the vairable of interest (temp, Co2, ect.) to serach for.
pattern_variables <- paste("(", varpattern,
                           ")_(", domaninpattern,
                           ")_(", modelpattern,
                           ")_(", experimentpattern,
                           ")_(", ensemblepattern,
                           ")_([0-9]{6})-([0-9]{6}",   #currently set up for 6 digit dates
                           ").nc$", sep = "")
variable_filelist <- list.files(CMIP5_DATA, pattern = pattern_variables, full.names = TRUE, recursive = TRUE)

# 1. TESTING TO SEE IF CAN EXTRACT DATA AND TIME VIA CDO


system2(WHICH_CDO, args = c("selvar,ph,time", outfile), stdout = TRUE, stderr = TRUE )

# ---
# Checking to see why this netcdf terminated my session on R
# ---
data <- "/pic/projects/GCAM/CMIP5-CLynch/PH_extra/ph_Omon_IPSL-CM5A-MR_historical_r1i1p1_185001-200512.nc"
WHICH_CDO  <- "/share/apps/netcdf/4.3.2/gcc/4.4.7/bin/cdo"
system2(WHICH_CDO, args = c("info", data), stdout = TRUE, stderr = TRUE )


