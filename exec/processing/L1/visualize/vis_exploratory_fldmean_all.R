# ------------------------------------------------------------------------------
# Purpose: The main purpose of this script is to make the exploratory figures
# for the ocean pH project. Figures created by this script will visualize the
# pH, CO3, and tos.
#
# Created by: Dorheim, Kalyn
# Created on: August 29 2017
# Modified:   xxx
#
# Notes:
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
script_name <- "vis_exploratory_figs_fldmean.R"


# The data directory for the files. Will need to make changes to names of the
# files being imported if recycle this code for another analysis.
DATA_DIR <- paste0(BASE_NAME,"/exec/output/L1/")

# Name of the list where to save everything
out = list()


# ------------------------------------------------------------------------------
# 1. Import Data
# ------------------------------------------------------------------------------
ph  <- read.csv(paste0(DATA_DIR, "fldmean_monthly_ph.csv")) %>% mutate(units = NA) %>% filter(model != "MRI-ESM1")
message("pH figures do not include MRI-ESM1")
co3 <- read.csv(paste0(DATA_DIR, "fldmean_monthly_co3.csv"))
tos <- read.csv(paste0(DATA_DIR, "fldmean_monthly_tos.csv"))

# Combine into a single data frame
bind_rows(ph, co3, tos) %>%
  mutate(method_used = "global_average") ->
    df

# ------------------------------------------------------------------------------
# 2. Make Figures
# ------------------------------------------------------------------------------
fig <- vis.exploratory(df)

message("Figures for fldmean ph, co3, and tos are saved in a list called fig")


# ------------------------------------------------------------------------------
# 3. CESM pH figures
# ------------------------------------------------------------------------------
df %>%
  filter(model == "CESM1-BGC") %>%
  droplevels ->
  df2

CESM_figs <- vis.exploratory(df2)
message("Figures for pH CESM saved in list called CESM_figs")
# ----
# End
message("End ", script_name)

