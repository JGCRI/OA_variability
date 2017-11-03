# ------------------------------------------------------------------------------
# Purpose: This script makes the delta Kurtosis and Skewness kables.
#
# Created by: Dorheim, Kalyn
# Created on: October 11 2017
# Modified:   xxx
#
# Notes:
# ------------------------------------------------------------------------------
# Environment
# ------------------------------------------------------------------------------
library(dplyr); library(tidyr); library(knitr); library(kableExtra)

# Import data
KS_path <- list.files("data", "Kurtosis", full.names = TRUE)
KS_list <- get(load(KS_path))

# Parse out data frame of interest from the loaded list.
delta_KS <- KS_list$delta_K_S


# ------------------------------------------------------------------------------
# Kables
# ------------------------------------------------------------------------------
# Create the empty list to store the tables in
tables = list()


# ------------------------------------------------------------------------------
# ph kable
# ------------------------------------------------------------------------------
delta_KS %>%
  filter(variable == "ph") %>%
  mutate(value = signif(delta, 2)) %>%
  mutate(delta = gsub(x = stat_variable, replacement = "", "delta  ")) %>%
  select(-stat_variable) %>%
  spread(basin, value) %>%
  select(-ensemble, -variable, -units, -model) %>%
  knitr::kable(format = "html", caption = "Delta kurtosis & skewness table") %>%
  kable_styling("striped", full_width = T) %>%
  group_rows("CESM1-BGC", 1, 2) %>%
  group_rows("CMCC-CESM", 3, 4) %>%
  group_rows("GFDL-ESM2G", 5, 6) %>%
  group_rows("GFDL-ESM2M", 7, 8) %>%
  group_rows("HadGEM2-CC", 9, 10) %>%
  group_rows("HadGEM2-ES", 11, 12) %>%
  group_rows("IPSL-CM5A-MR", 13, 14) %>%
  group_rows("MPI-ESM-LR", 15, 16) %>%
  group_rows("MPI-ESM-MR", 17, 18) ->
  tables[["ph"]]


# ------------------------------------------------------------------------------
# spco2 kable
# ------------------------------------------------------------------------------
delta_KS %>%
  filter(variable == "spco2") %>%
  mutate(value = signif(delta, 2)) %>%
  mutate(delta = gsub(x = stat_variable, replacement = "", "delta  ")) %>%
  select(-stat_variable) %>%
  spread(basin, value) %>%
  select(-ensemble, -variable, -units, -model) %>%
  knitr::kable(format = "html", caption = "Delta kurtosis & skewness table") %>%
  kable_styling("striped", full_width = T) %>%
  group_rows("CESM1-BGC", 1, 2) %>%
  group_rows("CMCC-CESM", 3, 4) %>%
  group_rows("GFDL-ESM2G", 5, 6) %>%
  group_rows("GFDL-ESM2M", 7, 8) %>%
  group_rows("HadGEM2-CC", 9, 10) %>%
  group_rows("HadGEM2-ES", 11, 12) %>%
  group_rows("MPI-ESM-LR", 13, 14) %>%
  group_rows("MPI-ESM-MR", 15, 16) ->
  tables[["spco2"]]


# ------------------------------------------------------------------------------
# tos kable
# ------------------------------------------------------------------------------
delta_KS %>%
  filter(variable == "tos") %>%
  mutate(value = signif(delta, 2)) %>%
  mutate(delta = gsub(x = stat_variable, replacement = "", "delta  ")) %>%
  select(-stat_variable) %>%
  spread(basin, value) %>%
  select(-ensemble, -variable, -units, -model) %>%
  knitr::kable(format = "html", caption = "Delta kurtosis & skewness table") %>%
  kable_styling("striped", full_width = T) %>%
  group_rows("CESM1-BGC", 1, 2) %>%
  group_rows("CMCC-CESM", 3, 4) %>%
  group_rows("GFDL-ESM2G", 5, 6) %>%
  group_rows("GFDL-ESM2M", 7, 8) %>%
  group_rows("HadGEM2-CC", 9, 10) %>%
  group_rows("IPSL-CM5A-MR", 11, 12) %>%
  group_rows("MPI-ESM-LR", 13, 14) %>%
  group_rows("MPI-ESM-MR", 15, 16)  ->
  tables[["tos"]]


# ------------------------------------------------------------------------------
# co3 kable
# ------------------------------------------------------------------------------
delta_KS %>%
  filter(variable == "co3") %>%
  mutate(value = signif(delta, 2)) %>%
  mutate(delta = gsub(x = stat_variable, replacement = "", "delta  ")) %>%
  select(-stat_variable) %>%
  spread(basin, value) %>%
  select(-ensemble, -variable, -units, -model) %>%
  knitr::kable(format = "html", caption = "Delta kurtosis & skewness table") %>%
  kable_styling("striped", full_width = T) %>%
  group_rows("CESM1-BGC", 1, 2) %>%
  group_rows("CMCC-CESM", 3, 4) %>%
  group_rows("IPSL-CM5A-MR", 5, 6) %>%
  group_rows("MPI-ESM-LR", 7, 8) ->
  tables[["co3"]]

# ------------------------------------------------------------------------------
# Save
# ------------------------------------------------------------------------------
save(tables, file = "figs/delta_KS_tables.rda")

# ----
# End
