# File to create figure for downslope wind event duration for Estes Park (1979-2024)
# and a more detailed summary for during the East Troublesome Fire (October 2020)

# Inputs:
# et_event_summary.csv and et_winds.csv created in process_downslope_wind_data.R
# gby_station_data.csv downloaded from the CoAgMet website (https://coagmet.colostate.edu)
# for Granby, Colorado

# Output:
# Figure S1 - (a) wind speeds and downslope wind event classifications during the East
# Troublesome Fire and (b) Frequency of downslope wind events lasting five or more days
# near Estes Park (1979-2024)

library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)
library(cowplot)

setwd("downslope")

et_event_summary <- read.csv("et_event_summary.csv")

et_winds <- read.csv("et_winds.csv")
str(et_event_summary)
et_event_summary$date <- as.POSIXct(et_event_summary$date)
et_event_summary$date_end <- et_event_summary$date + days(et_event_summary$N_days-1) + hours(12)
et_event_summary$date_start <- et_event_summary$date - hours(12)
gby_station_data <- read.csv("gby_station_data.csv") 

gby_station_data$date_and_time <- as.POSIXct(gby_station_data$date_and_time, format = "%Y-%m-%d %H:%M:%S")

idx <- is.na(gby_station_data$date_and_time)
gby_station_data <- gby_station_data[!idx,]

data_exp <- data.frame(date_start = as.POSIXct("2020-10-21 05:20:00", tz = "America/Denver"), date_end = as.POSIXct("2020-10-22 05:20:00", tz = "America/Denver"))

p1 <- ggplot() + geom_rect(data = et_event_summary, mapping = aes(xmin = date_start, xmax = date_end, ymin = -Inf, ymax = Inf), fill = "lightblue", alpha = 0.5) +
  geom_rect(data = data_exp, mapping = aes(xmin = date_start, xmax = date_end, ymin = -Inf, ymax = Inf), fill = 'coral3', color = 'coral3', alpha = 0.3) +
  geom_point(data = gby_station_data, mapping = aes(x = date_and_time, y = Gust.Speed), color = 'darkgray') +
  geom_point(data = gby_station_data, mapping = aes(x = date_and_time, y = Wind), color = 'black') +
  xlab("Date") + ylab("Wind speed (m/s)") + 
  theme_bw() + 
  theme(
    axis.text = element_text(size = 12),       # tick labels
    axis.title = element_text(size = 12)       # axis titles
  ) +
  xlim(min(gby_station_data$date_and_time), max(gby_station_data$date_and_time))
p1

# Next, we want to show how frequently different lengths of downslope wind events occurred
# over the record

p2 <- ggplot() + geom_histogram(data = dplyr::filter(et_winds, count >= 5), 
                                mapping = aes(x = count), fill = 'black', color = 'black') +
  xlab("Number consecutive downslope wind days (N \u2265 5)") +
  ylab(paste0("Frequency (", as.character(min(et_winds$year)),"-", as.character(max(et_winds$year)), ")")) +
  theme_bw() +
  theme(
    axis.text = element_text(size = 12),       # tick labels
    axis.title = element_text(size = 12)       # axis titles
  )
p2

p <- plot_grid(p1, p2, labels = c("a. ", "b."), rel_widths = c(3, 2), label_x = c(-0.01, -0.01), ncol = 2)
p

ggsave("downslope_winds.png", plot = p, width = 12, height = 4, unit = "in")

