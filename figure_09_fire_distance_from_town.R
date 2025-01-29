# This script computes the distance of the fire from town for each time step
# and visualizes that for comparison with the number of buildings left to 
# evacuate

library(dplyr)
library(ggplot2)
library(sf)
library(cowplot)
library(lubridate)
library(tidyr)

# Read in the East Troublesome Fire perimeter time series and transform
# to the CRS used for the evacuation model output
ts <- st_read("data/east_troublesome_ts.gpkg") %>% st_transform(crs = 32613)

estes_park <- st_read("data/estes_park.shp") %>% st_transform(crs = 32613)

# Note that the simulations are in Mountain Time (beginning at 5AM)
sim_start_time <- dplyr::filter(ts, sim == "control")$dates[1]

time_hr <- as.numeric(difftime(ts$dates, sim_start_time, units = "hours"))

# Compute distance from town in kilometers
town_distance <- as.numeric(st_distance(ts, estes_park))/1000

ts <- mutate(ts, town_distance, time_hr)

idx <- ts$town_distance<0.5

ts_reaches_town <- ts[idx,]

# When does the control simulation reach town?
con_reaches_town <- ts_reaches_town %>% dplyr::filter(sim == "control") %>%
  dplyr::filter(dates == min(dates))
con_reaches_town

# When does the experiment simulation reach town?
exp_reaches_town <- ts_reaches_town %>% dplyr::filter(sim == "experiment") %>%
  dplyr::filter(dates == min(dates))

exp_reaches_town

my_pal <- palette.colors(palette = "Okabe-Ito", n = 9)[c(1,6,7)]

p1 <- ggplot() + geom_point(data = ts, mapping = aes(x = time_hr, y = town_distance,
                                               color = as.factor(sim)), alpha = 0.8) +
  xlab("Time Since Ignition (hours)") + ylab("Distance from Estes Park (km)") +
  scale_color_discrete(name = "Source", type = my_pal,
                       labels = c('a_op' = "operational", 'control' = "control",
                                  'experiment' = "experiment")) +
  geom_vline(xintercept = 9) +
  theme_bw(base_size = 12) +
  xlim(c(0, 64))

p1

#ggsave("distance_from_estes_park.png", dpi = 600)

# Next, add the number of evacuated people by time

evac_df <- read.csv("data/Estes_Park_Colorado_evac.csv")

# Add 6 hours to represent evacuations beginning at 11:30AM (6 hours after)
evac_df <- mutate(evac_df, time_since_ig = (tt + 360)/60)

total_buildings <- max(evac_df$bldgs_normal)

evac_stack_df <- pivot_longer(evac_df, cols = c(bldgs_normal, bldgs_stopgo), names_to = "condition") %>% mutate(N_town = total_buildings - value)

my_pal2 <- palette.colors(palette = "Okabe-Ito", n = 9)[c(4,2)]

p2 <- ggplot() + geom_point(data = evac_stack_df, 
                      mapping = aes(x = time_since_ig, y = N_town, color = condition)) +
  xlab("Time Since Ignition (hours)") + ylab("Number of Buildings to Evacuate") +
  scale_color_discrete(name = "Condition", type = my_pal2, labels = c("speed limit", "stop-and-go")) +
  geom_vline(xintercept = 9) +
  theme_bw(base_size = 12) +
  xlim(c(0, 64))

p2

panel <- plot_grid(p1, p2, align = "hv", ncol = 1, labels = c("a", "b"), 
                   label_fontface = "plain")

panel

ggsave("figures/figure_09_fire_distance_and_evac_time.png", plot = panel, width = 8, height = 8,
       units = "in", dpi = 600)


