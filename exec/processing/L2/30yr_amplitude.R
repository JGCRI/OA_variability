# ------------------------------------------------------------------------------
# Purpose: This script plots CESM1-BGC 30 year mean for a quick look at the amplitude
# see GitHub Issue # 1 for OA_variability.
#
# Created by: Dorheim, Kalyn
# Created on: October 10
# Modified:   xxx
#
# Notes: There is not too much insight that can be gained from this graph... must
# detrend the results first in order to have an insightful comparison. Figures are
# on GitHub and saved at /data/figs/CESM_raw_amplitude.RData
# ------------------------------------------------------------------------------

# Calculate the amplitude
amplitude <- get.amplitude_values(basin_mean)

# Select our example model
amplitude %>%
  filter(model == "CESM1-BGC") ->
  to_plot

# For all of the variables create basin plots for the
# monthly mean over the first/last 30 yr period.
out      <- list()
var_list <- unique(to_plot$variable)

for(i in 1:length(var_list)){

  m <- unique(to_plot$model)
  v <- var_list[i]

  to_plot %>%
    filter(variable == v) %>%
    tidyr::unnest() ->
    plot_df

  u <- unique(plot_df$units)

  # Set the basins factors for plotting purposes
  plot_df$basin <- factor(plot_df$basin, levels = c("Global", "North Hemi", "South Hemi", "Arctic", "Atlantic", "Indian",
                                                    "Pacific Ocean", "Southern Ocean"))
  # Make the grid line plot
  plot_df %>%
    filter(value_type == "mean") %>%
    ggplot(aes(x = month, y = value, color = experiment)) +
    geom_line(size = 1.2) +
    facet_wrap(facets = "basin", ncol = 4) +
    labs(title = paste0(m, "\n30 yr monthly mean"),
         y = paste0(v, " ", u)) +
    scale_x_discrete(limits =  unique(plot_df$month), labels =  unique(plot_df$month_name)) +
    ggplot2::theme(text = element_text(size = 13)) +
    ggplot2::theme(axis.text.x = element_text(angle = 45, hjust = 1)) ->
    out[[paste0(m)]][[paste0(v)]]

} # end of the for loop

# Save
# ------
# file_name <- paste0(getwd(), "/data/figs/CESM_raw_amplitude.RData")
# save(out, file = file_name)

# -----
# End


