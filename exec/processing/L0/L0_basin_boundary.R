# ------------------------------------------------------------------------------
# Purpose: This script generates wolrd maps of all of the basin boundaries
# in the defined_basins.csv saved in a list and a kable for the lab note book.
#
# Created by: Dorheim, Kalyn
# Created on: August 31 2017
# Modified:   xxx
#
# Notes: It can take a while for the map figures to generate... might want to
# export as png and then hard code the figure/figures into the lab note book and
# the comment out the map maps section? idk also make a kabel of the basin
# boundaries here.
# ------------------------------------------------------------------------------
# 0. Setup the Environment
# ------------------------------------------------------------------------------
# Set the base name, it should be the package name
# BASE_NAME <- should be set in the R markdown

# The code requires the following but these should already be loaded in the
# R markdown.
# # SLCFimpulse package
# devtools::load_all(BASE_NAME)

# # Additional required libraries
# library(dplyr)
# library(tidyr)
# library(ggplot2)
# library(knitr)


# Define the script name
script_name <- "L0_basin_boundary.R"


# The data directory for the files. Will need to make changes to names of the
# files being imported if recycle this code for another analysis.
DATA_DIR <- paste0(BASE_NAME,"/exec/data/")

# Name of the list where to save everything
out = list()
message("Output from ", script_name, " saved in a list called out.")


# ------------------------------------------------------------------------------
# 1. Import Data
# ------------------------------------------------------------------------------
# Load the maps library, this contains the world map data
library(maps)

# Import the defined basins csv file.
defined_basins <- read.csv(paste0(DATA_DIR,"defined_basins.csv"))

# ------------------------------------------------------------------------------
# 2. Maps of Basin Boundaries
# ------------------------------------------------------------------------------
# Make the base world map. For each basin we will had the boundaries to the
# layer.
map_data("world") %>%
  ggplot(aes(long, lat, group = group)) +
  geom_path() ->
  base_map

# Now go through and add the region boundaries and label to the map for all of the
# basins in the defined basin .csv file.

# Replace this with the data from the defined_basin.csv
df <- defined_basins

# This for loop adds the basin boundaries to the base map.
for(i in 1:length(df$basin)){

  # First determine the basin name and then subset the all basins data frame
  # for the lat and lon information for that single basin
   basin_name <- df$basin[i]
   basin_df <- dplyr::filter(df, basin == basin_name)

  base_map +
    # Add the lat bounds
      geom_hline(yintercept = basin_df$lat1, color = "red") +
      geom_hline(yintercept = basin_df$lat2, color = "red") +
    # Label the lat bounds
      annotate("text", c(-180, -180), c(basin_df$lat1, basin_df$lat2), label = c(basin_df$lat1, basin_df$lat2), color = "blue") +
    # Add the lon bounds
      geom_vline(xintercept = basin_df$lon1, color = "red") +
      geom_vline(xintercept = basin_df$lon2, color = "red") +
    # Label the lat bounds
      annotate("text", c(basin_df$lon1, basin_df$lon2), c(0, 0), label = c(basin_df$lon1, basin_df$lon2), color = "blue") +
    # Add map label and save in the output list
      labs(title = paste0(basin_name)) ->
      out[["maps"]][[paste0(basin_name)]]

} # end of add lat and lon for loop.

# ------------------------------------------------------------------------------
# 3. Create Kable of Bounary Information
# ------------------------------------------------------------------------------
df <- defined_basins

  knitr::kable(df, col.names = c("Basin", "Lower Lat", "Upper Lat", "East Lon", "West Lon")) ->
    out[["table"]]

# ----
# End
message("End ", script_name)
