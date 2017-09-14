
# ------------------------------------------------------------------------------
# 1. Set up the environment.
library(devtools)
library(roxygen2)
library(dplyr)

# Set the working directory to the package directory.
# PCKG_DIR <- "/pic/projects/GCAM/Dorheim/oceanpH/"
setwd(paste0(PCKG_DIR))

# Build and load the package.
devtools::load_all()


# 2. Functional Code
# Find the cmip 5 files
data_input <- pH_list <- cmip.find_me(path = "/pic/projects/GCAM/CMIP5-CLynch/PH_extra", variable = "ph",
                          domain = "Omon", experiment = c("rcp85", "historical"), ensemble = "r1i1p1") %>% cmip.file_info
area_input <- area_list <- cmip.find_me(path = "/pic/projects/GCAM/CMIP5-CHartin/landfrac", variable = "areacella", domain = "fx") %>% cmip.file_info
frac_input <- cmip.find_me(path = "/pic/projects/GCAM/CMIP5-CHartin/landfrac", variable = "sftlf", domain = "fx") %>% cmip.file_info


to_process %>%
  filter(variable == "ph", model == "CanESM2", experiment == "rcp85") ->
  to_process

fake <- function(basin_df){

  west_bound  <- basin_df["lon1"]
  east_bound  <- basin_df["lon2"]
  south_bound <- basin_df["lat1"]
  north_bound <- basin_df["lat2"]

  return(north_bound)
}



defined_basins <- data.frame(basin = c("pac", "at"), lon1 = c(90,90), lon2 = c(90,90), lat1 = c(90,90), lat2 = c(90,90))


x <- apply(defined_basins, 1, fake)

