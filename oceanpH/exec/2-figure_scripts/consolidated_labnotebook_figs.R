# Purpose: Finalize the figures for the consolidated lab note book for improved story telling.
#
# Date: December 19
#
# Notes:

# Environment -------------------------------------------------------------------------------------
library(dplyr)
library(ggplot2)

BASE <- getwd()
OUTPUT <- file.path(BASE, "figs", "consolidated_labnotebook")

# Load the mulitple figures
multiple_models_figs <- get(load(list.files(file.path(BASE, "figs", "cmip", "rcp85"), "comparing_multiple_models", full.names = T)))

script_name <- "consolidated_labnotebook_figs.R"

# Delta K and S -------------------------------------------------------------------------------------

# Parse out the delta K and S figures by the individual variable

all_delta <- multiple_models_figs$delta_K
all_data  <- all_delta$data

ph_delta  <- all_delta %+% filter(all_data, variable == "ph")
co3_delta <- all_delta %+% filter(all_data, variable == "co3")
tos_delta <- all_delta %+% filter(all_data, variable == "tos")
tas_delta <- all_delta %+% filter(all_data, variable == "tas")
spco2_delta <- all_delta %+% filter(all_data, variable == "spco2")
fgco2_delta <- all_delta %+% filter(all_data, variable == "fgco2")

delta = list(ph = ph_delta, co3 = co3_delta, tos = tos_delta, co3 = co3_delta,
             tas = tas_delta, spco2 = spco2_delta, fgco2 = fgco2_delta)

# Time Series -------------------------------------------------------------------------------------

# Parse out the amplitude, min and max annual time series from big fiugres list.
amp_figs <- multiple_models_figs$time_series$amplitude
min_figs <- multiple_models_figs$time_series$min
max_figs <- multiple_models_figs$time_series$max


# Use a for loop to loop through all of the variables.
time_series = list()
for(i in 1:length(amp_figs)){

  # Select the data layer from each ggplto object and group by basin. The
  # grouped data will replace the ggplot data layer to create figures for
  # each basin individually.
  amp_figs[[i]][["data"]] %>% group_by(basin) -> new_amp_data
  min_figs[[i]][["data"]] %>% group_by(basin) -> new_min_data
  max_figs[[i]][["data"]] %>% group_by(basin) -> new_max_data

  vari_name <- unique(new_amp_data$variable)

  # Pipe in grouped data into the digures and replace the figure's data frames.
  new_amp_data %>%
    do(amp_fig = amp_figs[[i]] %+% .) ->
    amp_by_basin

  new_min_data %>%
    do(min_fig = min_figs[[i]] %+% . ) ->
    min_by_basin

  new_max_data %>%
    do(max_fig = max_figs[[i]] %+% . ) ->
    max_by_basin

  # join the figures together and then arrange in a grid.
  left_join(amp_by_basin, min_by_basin, by = "basin") %>%
    left_join(max_by_basin, by = "basin") %>%
    ungroup ->
    separate_tseries

  # This will save all of the amplitude, min, and max time series as grobs that need to be printed with grid
  # extra gird.arrange().
  for(i in 1:nrow(separate_tseries)){
    gridExtra::arrangeGrob(grobs = list(separate_tseries$amp_fig[[i]],
                                        separate_tseries$min_fig[[i]],
                                        separate_tseries$max_fig[[i]]), nrow = 1) ->
      time_series[[paste0(vari_name)]][[paste0(separate_tseries$basin[i])]]
    }

}


# Monthly Means -------------------------------------------------------------------------------------
monthly_all_vari <- multiple_models_figs$mean_monthly
monthly_all_data <- monthly_all_vari$data

monthly_new = list()
for(i in 1:length(monthly_all_vari)){

  # Group the variable data by basin
  monthly_all_vari[[i]][["data"]] %>% group_by(basin) -> new_data

  # Save the variable name from the new data set
  vari_name <- names(monthly_all_vari[i])

  # Replace the data frame in the ggplo object with each group of basin data.
  new_data %>%
    do(monthly_fig = monthly_all_vari[[i]] %+% .) ->
    monthly_by_basin

  basin_figs <- setNames(nm = monthly_by_basin$basin, object = monthly_by_basin$monthly_fig)

  monthly_new[[vari_name]] <- basin_figs

}


# Save  ---------------------------------------------------------------------------------------------
# The original figures
original = list(monthly_all_vari = monthly_all_vari,
               amp_figs = amp_figs,
               min_figs = min_figs,
               max_figs = max_figs,
               all_delta = all_delta)

# Save the new formated and orignal figures in a single list
out = list(monthly_mean = monthly_new,
           time_series = time_series,
           delta = delta,
           original = original)
attributes(out)[["script_name"]] <- script_name

save(out, file = file.path(OUTPUT_DIR, "consolidated_labnotebook_figs.rda"))
message("figures saved at ", file.path(OUTPUT_DIR, "consolidated_labnotebook_figs.rda"))

# End ----
