# ------------------------------------------------------------------------------
# Purpose: This script creates tables for the markdown regarding the model
# observations that were filtered from the analysis and a table of experiments.
#
# Created by: Dorheim, Kalyn
# Created on: October 31 2017
#
# Notes:
# ------------------------------------------------------------------------------
# Set up the environment
library(dplyr); library(knitr)



# ------------------------------------------------------------------------------
# Model observations removed list
# ------------------------------------------------------------------------------
# Start by importing the dir path to the .csv used to remove model observations.
remove_list_path <- list.files("raw-data/assumptions", "to_remove", recursive = TRUE, full.names = TRUE)
to_remove_df     <- read.csv(remove_list_path) %>%  mutate(note = " ")


removed_path <- list.files("data/cmip", "removed", full.names = TRUE)
removed_df <- get(load(removed_path)) %>% mutate(note = "not all model observations were removed jsut for certain netcdfs")
removed_obs <- bind_rows(to_remove_df, removed_df)


# Save information as kable
observations_removed <- kable(removed_obs)


# ------------------------------------------------------------------------------
# Table of experimetns and variable
# ------------------------------------------------------------------------------
data_path <- list.files("data/cmip", "basin_mean", full.names = TRUE)
data      <- get(load(data_path))


# Create a dataframe of the unique model / variable / experiment combinations.
data %>%
  filter(basin == "Global", month_name == "Jan") %>%
  select(experiment, variable, model, year, value) %>%
  distinct() %>%
  spread(variable, value) ->
  ex_mo_variables

# create historical and future ids for each variable.
ex_mo_variables %>%
  filter(experiment == "historical") %>%
  mutate(ph_hist = if_else(is.na(ph), "  ", "hist")) %>%
  mutate(spco2_hist = if_else(is.na(spco2), "  ", "hist")) %>%
  mutate(tos_hist = if_else(is.na(tos), "  ", "hist")) %>%
  mutate(co3_hist = if_else(is.na(co3), "  ", "hist")) %>%
  select(model, ph_hist, spco2_hist, tos_hist, co3_hist) %>%
  distinct ->
  historical_ids

ex_mo_variables %>%
  filter(experiment != "historical") %>%
  mutate(ph_fut = if_else(is.na(ph), "  ", "fut")) %>%
  mutate(spco2_fut = if_else(is.na(spco2), "  ", "fut")) %>%
  mutate(tos_fut = if_else(is.na(tos), "  ", "fut")) %>%
  mutate(co3_fut = if_else(is.na(co3), "  ", "fut")) %>%
  select(model, ph_fut, spco2_fut, tos_fut, co3_fut) %>%
  distinct ->
  future_ids

# Combine the future and histoircal id data frame together and
# create the variable columns
historical_ids %>%
  inner_join(future_ids, by = "model") %>%
  unite(ph, ph_hist, ph_fut, sep = "  ") %>%
  unite(spco2, spco2_hist, spco2_fut, sep = "  ") %>%
  unite(co3, co3_hist, co3_fut, sep = "  ") %>%
  unite(tos, tos_hist, tos_fut, sep = "  ") ->
  EandV_table_df


# Format the data frame into a kable
EandV_table_df %>%
  kable(format = "markdown") ->
  EandV_table


# ------------------------------------------------------------------------------
# Save
# ------------------------------------------------------------------------------
tables = list(observations_removed = observations_removed, experiment_variable = EandV_table)
save(tables, file = "figs/cmip/observations_table.rda")


