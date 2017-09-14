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

# Define cdo location
WHICH_CDO  <- "/share/apps/netcdf/4.3.2/gcc/4.4.7/bin/cdo"

# Data
pHdata     <- "/pic/projects/GCAM/CMIP5-CHartin/CMIP5_RCP85/ph/ph_Oyr_CNRM-CM5_rcp85_r1i1p1_2056-2065.nc"

# Define where to save data
outfile1    <- "/pic/scratch/dorh012/outfile1"
outfile2    <- "/pic/scratch/dorh012/outfile2"

# fldmean
system2(WHICH_CDO, args = c("fldmean", pHdata, outfile1), stdout = TRUE, stderr = TRUE )

# ----
# End
