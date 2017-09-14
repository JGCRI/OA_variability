# ------------------------------------------------------------------------------
# Purpose: The main purpose of this script is to get summary information about
# the output from the fldmean pH analysis. It is set up to return a list about
# the number of files for different ensemble/experiment/model combinations
# initially found in the analysis and actually processed. Table of counts and
# a data frame of unprocessed cmip5 files are returned in a list called out.
#
# Created by: Dorheim, Kalyn
# Created on: August 24 2017
# Modified:   xxx
#
# Notes: In order to recycle code may need to make changes
# in section 0 where the environment is set up and also in section 1. where the
# csv files are imported.
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
script_name <- "ph_fldmean_output_info.R"


# The data directory for the files. Will need to make changes to names of the
# files being imported if recycle this code for another analysis.
DATA_DIR <- paste0(BASE_NAME,"/exec/output/L1/")

# Name of the list where to save everything
out = list()


# ------------------------------------------------------------------------------
# 1. Import Data
# ------------------------------------------------------------------------------
files_found <- read.csv(paste0(DATA_DIR, "cmip5_identified_fldmean_monthly_pH.csv"))
output      <- read.csv(paste0(DATA_DIR, "fldmean_monthly_pH.csv"))
depth       <- read.csv(paste0(DATA_DIR, "checked_for_depth_pH.csv"))


# ------------------------------------------------------------------------------
# 2. table of counts
# ------------------------------------------------------------------------------
# Count the relevant categories of the cmip5 files that were found.
apply(files_found, 2, unique) %>%
  lapply(length) %>%
  data.frame %>%
  select(`number of files` = path, model, domain, variable, experiment, ensemble) ->
  summary_files_found

# Count the relevant categories of the cmip5 files that were actually analyzed.
apply(output, 2, unique) %>%
  lapply(length) %>%
  data.frame %>%
  mutate(path = NA, domain = NA) %>%
  select(`number of files` = path, model, domain, variable, experiment, ensemble) ->
  summary_output

# Combine the summary counts into a single table, format and save for output.
df <- bind_rows(summary_files_found, summary_output)
rownames(df) <- c("CMIP5 files found", "CMIP5 processed")
tab <- knitr::kable(df, align = "c", format = "markdown")

# Save the table
out[["table_counts"]] <- tab


# ------------------------------------------------------------------------------
# 3. Experiment/Ensemble Counts
# ------------------------------------------------------------------------------
# Count the number of models in each experiment/ensemble.
#
# For all of the cmip5 files found
files_found %>%
  select(ensemble, experiment, model) %>%
  group_by(ensemble, experiment) %>%
  summarize(model = length(unique(model))) %>%
  ungroup %>%
  tidyr::spread(experiment, model) %>%
  knitr::kable(align = "c", format = "markdown") ->
  tab

out[["files_found"]][["ex_en_table"]] <- tab

# For the cmip5 files analyzed
output %>%
  select(ensemble, experiment, model) %>%
  group_by(ensemble, experiment) %>%
  summarize(model = length(unique(model))) %>%
  ungroup %>%
  tidyr::spread(experiment, model) %>%
  knitr::kable(align = "c", format = "markdown") ->
  tab

out[["files_processed"]][["ex_en_table"]] <- tab


# ------------------------------------------------------------------------------
# 4. Check for Unprocessed Files
# ------------------------------------------------------------------------------
# Check to see if there are any cmip5 files that were found but where unable to
# processed.
#
# Combine the files found and the processed file information into a single data
# frame. If a model, ensemble, experiment combination is missing in the output
# file then there should be NAs in the combined data frame.
files_found %>%
  select(path, model, ensemble, experiment) %>%
  left_join(output %>% select(model, ensemble, experiment), by = c("model", "ensemble", "experiment")) ->
  df

# Data frame of the netcdf files that are missing from the output aka unprocessed files
unprocessed <- filter(df, is.na(model), is.na(ensemble), is.na(experiment))

# If no files were identified as unprocessed return a message instead of an
# empty data frame.
if(dim(unprocessed)[1] == 0){
  unprocessed <- "No files identified were unprocessed"
}

out[["unprocessed_files"]] <- paste(unprocessed)
# ------------------------------------------------------------------------------
# 5. Check for depth
# ------------------------------------------------------------------------------
colnames(depth) <- c("file", "depth")

if(!"Y" %in% depth$dpeth){
  depth <- "None."
}
out[["depth"]] <- depth

message(script_name, " complete. Output saved in a list called out.")
# ----
# End
