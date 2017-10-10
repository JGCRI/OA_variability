# ------------------------------------------------------------------------------
# Purpose: the purpose of this script is to do linear detrending for the
# hisotorical and future time periods.
#
# We determined that there is something questionable going on with the detrending,
# either there is some sort of decadal pattern or the detrending needs to be with
# a 1st order or second order polynomail.
#
# This sciprt will is not intedned for being reused...
# ------------------------------------------------------------------------------
getwd()
# It should be the location of the package on the dev branch

# ------------------------------------------------------------------------------
# Untrended Time Series Plot
# ------------------------------------------------------------------------------
raw_time_series_rcp85 <- plot.time_series(basin_mean %>% filter(experiment == "historical"), write_pdf = FALSE, file_name = "untrened_tseries_rcp85")
# save(untreneded_time_series_rcp85, file = "./OA_variability/data/figs/untrened_tseries_rcp85.RData")
# load("./OA_variability/data/figs/untrened_tseries.RData")


# ------------------------------------------------------------------------------
# Detrend Data
# ------------------------------------------------------------------------------
# Now detrend the time serries for the basins
out <- detrend.linear(basin_mean)

# Extract the slope and the detrended basin means.
slope <- out$slope
detrened_basin_mean <- out$data

# # Save
# devtools::use_data(slope, pkg = "OA_variability", internal = FALSE, overwrite = TRUE)
# devtools::use_data(detrened_basin_mean, pkg = "OA_variability", internal = FALSE, overwrite = TRUE)


# ------------------------------------------------------------------------------
# Plot the Detrended vs Raw Global Data For the T Series
# ------------------------------------------------------------------------------
detrened_time_series <- plot.time_series(detrened_basin_mean %>% filter(experiment == "historical"), write_pdf = FALSE, file_name = "detrened_basin_mean_timeseries")
# save(detrened_time_series, file = "./OA_variability/data/figs/detrended_tseries.RData")

# For the purposes of the meetings on 10/10 I think that I am going to take a look at the future detrended vs
# undetrended figures.
left_join(raw_time_series_rcp85  %>%  rename(raw_fig = fig), detrened_time_series %>% rename(detrended_fig = fig),
          by = c("basin", "variable")) ->
  all_time_series

# Subset of the global time series
all_time_series %>%
  filter(basin == "Global") ->
  glb_t_series


p1 <- glb_t_series[1,]$raw_fig[[1]] +
  theme(legend.position = "none") +
  labs(caption = "", x = "", title = "Historical Raw Global T Series" ) +
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm")) +
  ggplot2::theme(axis.text.x = element_text(angle = 0, hjust = 1))

p2 <- glb_t_series[1,]$detrended_fig[[1]] +
  theme(legend.position = "none") +
  labs(caption = "", x = "", title = "Detrened Global T Series") +
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm")) +
  ggplot2::theme(axis.text.x = element_text(angle = 0, hjust = 1))

p3 <- glb_t_series[2,]$raw_fig[[1]] +
  theme(legend.position = "none") +
  labs(caption = "", x = "", title = "") +
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm")) +
  ggplot2::theme(axis.text.x = element_text(angle = 0, hjust = 1))

p4 <- glb_t_series[2,]$detrended_fig[[1]] +
  theme(legend.position = "none") +
  labs(caption = "", x = "", title = "'") +
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm")) +
  ggplot2::theme(axis.text.x = element_text(angle = 0, hjust = 1))

p5 <- glb_t_series[3,]$raw_fig[[1]] +
  theme(legend.position = "none") +
  labs(caption = "", title = "", x= "") +
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm")) +
  ggplot2::theme(axis.text.x = element_text(angle = 0, hjust = 1))


p6 <- glb_t_series[3,]$detrended_fig[[1]] +
  theme(legend.position = "none") +
  labs(caption = "", title = "", x = "") +
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm")) +
  ggplot2::theme(axis.text.x = element_text(angle = 0, hjust = 1))


plot <- gridExtra::grid.arrange(p1, p2, p3, p4, p5, p6)

 path <- "C:/Users/dorh012/Documents/GitHub/OA_variability/outputs/"
 pdf(paste0(path, "historical_tseries.pdf"), onefile = TRUE)
 gridExtra::grid.arrange(p1, p2, p3, p4, p5, p6)
 dev.off()

# ------------------------------------------------------------------------------
# Look at IPSL rcp 85
# ------------------------------------------------------------------------------
basin_mean %>%
  filter(grepl(x = model, pattern = "IPSL"), variable == "ph", basin == "Global") %>%
  filter(model != "IPSL-CM5B-LR") %>%
  filter(model != "IPSL-CM5A-LR") %>%
  ggplot(aes(x = time, y = value, color = model)) +
  geom_point() +
  labs(title = "IPSL rcp85 Time Series\nRaw Data",
       y = "tos C",
       caption = "rcp85") ->
  IPSL_p1

# basin_mean %>%
#   filter(grepl(x = model, pattern = "IPSL"), variable == "tos", experiment == "rcp85", basin == "Global") %>%
#   group_by(model, year) %>%
#   summarise(value = mean(value)) %>%
#   ggplot(aes(x = year, y = value, color = model)) +
#   geom_line() +
#   labs(title = "IPSL rcp85 \n Annual Average",
#        y = "tos C",
#        caption = "rcp85") ->
#   IPSL_p2


basin_mean %>%
  filter(grepl(x = model, pattern = "IPSL"), variable == "tos", experiment == "rcp85", basin == "Global") %>%
  filter(model != "IPSL-CM5B-LR") %>%
  ggplot(aes(x = month_name, y = value, color = model)) +
  geom_point() +
  labs(title = "IPSL rcp85 \n Month",
       y = "tos C",
       caption = "rcp85") ->
  IPSL_p2

# path <- "C:/Users/dorh012/Documents/GitHub/OA_variability/outputs/"
# pdf(paste0(path, "/rcp85_IPSL.pdf"), onefile = TRUE)
# gridExtra::grid.arrange(IPSL_p1, IPSL_p2)
# dev.off()


# -----
# End
