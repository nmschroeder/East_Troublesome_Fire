# This script visualizes the result of the evacuation model simulations for
# Figure 8
#
# The evacuation models were completed in the script for Figure 5
# to keep the r5r code in one script
#
# The results from that script are read into this one
#
# This script also prepares a data frame to track the number of people
# evacuated over time to prepare for Figure 9 as well as the area evacuated
# over time
#

library(dplyr)
library(sf)
library(ggplot2)

# Read in the CSV files for the evacuation model runs
ttm_normal <- read.csv("data/ttm_normal.csv")
ttm_stopgo <- read.csv("data/ttm_stopgo.csv")

# Read in data frames for the corresponding hexagonal grid
hex_counts <- st_read("data/evacuation_building_counts.gpkg")
hex_polys <- st_read("data/evacuation_hex_polys.gpkg")

# Read in the roads for Colorado
rds_dir <- "downloads/Tiger_Colorado"

roads_fnames <- list.files(path = rds_dir, pattern = glob2rx("*.shp"), recursive = TRUE, full.names = TRUE)

roads_list <- lapply(roads_fnames, st_read)

roads <- do.call(rbind.data.frame, roads_list)

egress <- c("State Hwy 7", "US Hwy 36", "US Hwy 34")

exit_paths <- dplyr::filter(roads, FULLNAME %in% egress)

roi <- st_read("data/estes_park_circle.shp")
roi_utm <- st_transform(roi, crs = 32613)

exits <- st_read("data/exits.gpkg")

exit_paths_roi <- st_intersection(st_transform(exit_paths, 32613), roi_utm)


# Find optimal route for each origin (minimum travel time - what does p50 mean?)
fastest_route <- function(temp_df){
  idx <- which.min(temp_df$travel_time_p50)
  return(temp_df[idx,])
}

## Compute for normal traffic conditions

# Group by the starting points (centers of the hexagons) and find the destination
# with the shortest travel time
ttm_normal_optimal <- ttm_normal %>% group_by(from_id) %>% 
  dplyr::group_map(~fastest_route(.x), .keep = TRUE) 

# Bind all the data frame rows together into one big data frame
ttm_normal_optimal <- do.call(rbind.data.frame, ttm_normal_optimal) 

# Find maximum travel time of the optima
idx <- which.max(ttm_normal_optimal$travel_time_p50)
total_evac_time_normal <- ttm_normal_optimal[idx,]

## Repeat for stop-and-go traffic conditions

ttm_stopgo_optimal <- ttm_stopgo %>% group_by(from_id) %>% 
  dplyr::group_map(~fastest_route(.x), .keep = TRUE)

ttm_stopgo_optimal <- do.call(rbind.data.frame, ttm_stopgo_optimal)

# Find the maximum travel time of the optima
idx <- which.max(ttm_stopgo_optimal$travel_time_p50)
total_evac_time_stopgo <- ttm_stopgo_optimal[idx,]

# Compute area evacuated per 10-minute increment assuming optimal routes
ttm_normal_optimal$from_id <- as.integer(ttm_normal_optimal$from_id)
ttm_normal_optimal$travel_time_p50 <- as.numeric(ttm_normal_optimal$travel_time_p50)
ttm_normal_optimal <- arrange(ttm_normal_optimal, from_id)

hex_buildings <- dplyr::filter(hex_counts, buildings > 0) %>% arrange(id_polygons)

if (identical(hex_buildings$id_polygons, ttm_normal_optimal$from_id)){
  ttm_normal_optimal <- cbind(hex_buildings, ttm_normal_optimal)
}

ttm_stopgo_optimal$from_id <- as.integer(ttm_stopgo_optimal$from_id)
ttm_stopgo_optimal$travel_time_p50 <- as.numeric(ttm_stopgo_optimal$travel_time_p50)
ttm_stopgo_optimal <- arrange(ttm_stopgo_optimal, from_id)

if (identical(hex_buildings$id_polygons, ttm_stopgo_optimal$from_id)){
  ttm_stopgo_optimal <- cbind(hex_buildings, ttm_stopgo_optimal)
}

plot_roads <- roads %>% st_transform(crs = 32613) %>% st_intersection(st_transform(roi, crs = 32613))

ttm <- rbind.data.frame(mutate(ttm_normal_optimal, case = 1), mutate(ttm_stopgo_optimal, case = 2))

# Let's use the fill for the evacuation time here
p8_part1 <- ggplot() + geom_sf(data = roi, color = 'black', fill = 'black') + 
  geom_sf(data = plot_roads, color = "white", linewidth = 0.25) + 
  geom_sf(data = exit_paths_roi, color = 'white', linewidth = 0.75) +
  geom_sf(data = dplyr::filter(ttm, case == 1), color = NA,
          mapping = aes(fill = travel_time_p50)) + 
  scale_fill_viridis_c(name = "Travel time \n(minutes)", option = "E") + 
  scale_color_viridis_c(name = "Travel time \n(minutes)", option = "E") +
  #geom_sf(data = town_shp, color = 'gray', fill = NA, linewidth = 0.5) +
  geom_sf(data = exits, color = 'white', fill = 'coral', pch = 24, size = 4) +
  theme(legend.position = "bottom")


# Let's use the fill for the evacuation time here
p8_part2 <- ggplot() + geom_sf(data = roi, color = 'black', fill = 'black') + 
  geom_sf(data = plot_roads, color = "white", linewidth = 0.25) + 
  geom_sf(data = exit_paths_roi, color = 'white', linewidth = 0.75) +
  geom_sf(data = dplyr::filter(ttm, case == 2), color = NA,
          mapping = aes(fill = travel_time_p50)) + 
  scale_fill_viridis_c(name = "Travel time \n(minutes)", option = "F") + 
  scale_color_viridis_c(name = "Travel time \n(minutes)", option = "F") +
  #geom_sf(data = town_shp, color = 'gray', fill = NA, linewidth = 0.5) +
  geom_sf(data = exits, color = 'white', fill = 'coral', pch = 24, size = 4) +
  theme(legend.position = "bottom")


p8 <- plot_grid(p8_part1, p8_part2, labels = c("a", "b"),
                align = "hv",
                label_fontface = "plain",
                label_x = 0.05,
                label_y = 0.99)

p8

fname <- "figures/figure_08_evac_times_map.png"
ggsave(filename = fname, plot = p8, width = 11, height = 6, units = "in", dpi = 600)

fname2 <- "figures/figure_08_evac_times_map.pdf"
ggsave(filename = fname2, plot = p8, width = 11, height = 6, units = "in")


# Remove units
# https://stackoverflow.com/questions/46935207/removing-units-from-an-r-vector
clean_units <- function(x){
  attr(x,"units") <- NULL
  class(x) <- setdiff(class(x),"units")
  return(x)
}

km2perhex <- clean_units(st_area(hex_polys[1,])/(1000)^2)
km2perhex

dt <- 10

ttm_max <- max(ttm_stopgo_optimal$travel_time_p50 + dt, na.rm = TRUE)

tt <- seq(0, ttm_max, by = dt)

# Initialize zero vectors
evac_area_normal <- vector(length = length(tt))*0
evac_buildings_normal <- evac_area_normal
evac_area_stopgo <- evac_area_normal
evac_buildings_stopgo <- evac_area_normal

for (i in 2:length(tt)){
  # Normal traffic
  idx <- which(ttm_normal_optimal$travel_time_p50 <= tt[i])
  evac_area_normal[i] <- length(idx)*km2perhex
  evac_buildings_normal[i] <- sum(ttm_normal_optimal$buildings[idx])
  
  # Stop and go traffic
  idx <- which(ttm_stopgo_optimal$travel_time_p50 <= tt[i])
  evac_area_stopgo[i] <- length(idx)*km2perhex
  evac_buildings_stopgo[i] <- sum(ttm_stopgo_optimal$buildings[idx])
}

# We only have hexagons that have buildings in them, so we can use the number
# of rows for ttm_normal_optimal to compute the area evacuated
total_area <- nrow(ttm_normal_optimal)*km2perhex

center_town <- st_centroid(roi) %>% st_coordinates()

evac_df <- data.frame(place = town_name,
                      x = rep(center_town[1], times = length(tt)),
                      y = rep(center_town[2], times = length(tt)),
                      time_min = tt,
                      area_normal = evac_area_normal, bldgs_normal = evac_buildings_normal,
                      area_stopgo = evac_area_stopgo, bldgs_stopgo = evac_buildings_stopgo,
                      total_normal = rep(max(ttm_normal_optimal$travel_time_p50), times = length(tt)),
                      total_stopgo = rep(max(ttm_stopgo_optimal$travel_time_p50), times = length(tt)),
                      rate_normal_km2hr = rep((total_area/max(ttm_normal_optimal$travel_time_p50))*60, times = length(tt)),
                      rate_stopgo_km2hr = rep((total_area/max(ttm_stopgo_optimal$travel_time_p50))*60, times = length(tt)))

fname <- paste0(out_dir, "/", town_tag, "_evac.csv")
write.csv(evac_df, fname)

evac_stack_df <- pivot_longer(evac_df, cols = c(area_normal, area_stopgo), names_to = c("area"))

# Check plot of results
p <- ggplot(data = evac_stack_df) + geom_line(mapping = aes(x = time_min, y = value, color = area)) + 
  geom_point(mapping = aes(x = time_min, y = value, color = area)) +
  scale_color_discrete(name = "Evacuation\nScenario", labels = c("Speed Limit", "Stop-and-Go")) +
  xlab("Time (minutes)") + ylab(expression("Evacuated Area "~(km^2))) + ggtitle(town_name)
p

#ggsave(paste0(town_name, "_evacarea_by_time.png"), plot = p, width = 4, height = 4, units = "in", dpi = 600)

