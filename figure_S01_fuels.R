# Script to plot the Scott and Burgan 40 Fire Fuels Categories for 2019
# 2020 and 2022 to determine when the signal from the 2020 fire season 
# was added to the fuel data for Figure S1

library(raster)
library(dplyr)
library(ggplot2)
library(cowplot)

# Read in the 2019 fire behavior fuel models
fuel2019 <- raster("data/LC19_F40_200_estes_park.tif")

# Read in the 2020 fire behavior fuel models
fuel2020 <- raster("data/LC20_F40_200_estes_park.tif")

# Read in the 2022 fire behavior fuel models
fuel2022 <- raster("data/LC22_F40_220_estes_park.tif")

# Check: when does the difference from the 2020 fires appear?
fuel_diff_2019_2020 <- fuel2020-fuel2019
fuel_diff_2019_2022 <- fuel2022-fuel2019

# Prepare the raster data to be visualized with ggplot

# Difference between 2019 and 2020
fuel_diff_2019_2020_df <- fuel_diff_2019_2020 %>% as.data.frame(xy = TRUE)
fuel_diff_2019_2020_df <- mutate(fuel_diff_2019_2020_df, check = layer != 0)

# Difference between 2019 and 2022
fuel_diff_2019_2022_df <- fuel_diff_2019_2022 %>% as.data.frame(xy = TRUE)
fuel_diff_2019_2022_df <- mutate(fuel_diff_2019_2022_df, check = layer != 0)

fuel_diff_2019_2020_df$check <- as.integer(fuel_diff_2019_2020_df$check, length = 1)
fuel_diff_2019_2022_df$check <- as.integer(fuel_diff_2019_2022_df$check, length = 1)

# Plot the raster difference between 2019 and 2020
p1 <- ggplot() + geom_raster(data = fuel_diff_2019_2020_df, mapping = aes(x = x, y = y, fill = as.factor(check))) +
  scale_fill_viridis_d(name = "Change") + labs(x = "Easting (m)", y = "Northing (m)") + 
  ggtitle("Pixels that change between 2019 and 2020") +
  theme_bw()

# Plot the raster difference between 2019 and 2022
p2 <- ggplot() + geom_raster(data = fuel_diff_2019_2022_df, mapping = aes(x = x, y = y, fill = as.factor(check))) +
  scale_fill_viridis_d(name = "Change") + labs(x = "Easting (m)", y = "Northing (m)") + 
  ggtitle("Pixels that change between 2019 and 2022") +
  theme_bw()

pgrid <- plot_grid(p1, p2, ncol = 2, align = "hv", labels = c("a", "b"))
ggsave("figures/figure_S01_fuel_diff_panel.png", plot = pgrid, width = 12, height = 4, units = "in")

