options(java.parameters = "-Xmx30G")
options(timeout = max(600, getOption("timeout")))

## This code creates a hexagonal grid around Estes Park and computes
## the number of buildings per grid cell for Figure 5.
##
## We also run evacuation simulations for traffic leaving Estes Park
## following the speed limit and stop-and-go traffic conditions.
##
## Stop-and-go traffic conditions are assumed to be 4 km per hour.
##
## This code was adapted from evacuation model simulations written by 
## coauthor Ty Tuff.
##
## To run this file, please download a .pbf file for Colorado from 
## Open Street Map (https://download.geofabrik.de/north-america/us/colorado.html) 
## and place in the directory downloads/osm/colorado
##
## To include elevation as was done in this study, please copy-paste the
## file called LC20_Elev_220_estes_park.tif generated in 
## figure_00_prepare_LANDFIRE_data.R into the downloads/osm/colorado
## directory as well.
##

library(r5r)
library(sf)
library(ggplot2)
library(dplyr)
library(raster)
library(osmdata)
library(stringr)
library(cowplot)
library(tidyr)

set.seed(0)

town_name <- "Estes Park, Colorado"

rds_dir <- "downloads/Tiger_Colorado"

roads_fnames <- list.files(path = rds_dir, pattern = glob2rx("*.shp"), recursive = TRUE, full.names = TRUE)

roads_list <- lapply(roads_fnames, st_read)

roads <- do.call(rbind.data.frame, roads_list)

egress <- c("State Hwy 7", "US Hwy 36", "US Hwy 34")

exit_paths <- dplyr::filter(roads, FULLNAME %in% egress)

town_shp <- st_read("data/estes_park.shp")

roi <- st_read("data/estes_park_circle.shp")
roi_utm <- st_transform(roi, crs = 32613)

exits <- st_read("data/exits.gpkg")

exit_paths_roi <- st_intersection(st_transform(exit_paths, 32613), roi_utm)

# Check that our exits are on the region of interest boundary
p_roi <- ggplot() + geom_sf(data = roi) + geom_sf(data = town_shp, color = 'purple') + 
  geom_sf(data = exits, color = 'magenta') + geom_sf(data = exit_paths_roi)
p_roi
#town_bbox <- town_shp %>% st_buffer(dist = 5000) %>% st_transform(crs = 4326) %>% st_bbox()

# Create a filename tag from the town name
town_tag <- gsub(",", "", town_name) 
town_tag <- str_split(town_tag, pattern = " ")[[1]]
town_tag <- paste0(town_tag, collapse = "_")

theme_set(theme_bw(base_size = 12))

# Path to .pbf file
data_path <- "downloads/osm/colorado"
out_dir <- "data"

# Retrieve bounding box for region of interest 
#town_bbox <- getbb(town_name, format_out = "polygon")

town_bbox <- roi %>% st_transform(crs = 4326) %>% st_bbox()

# Unnest the lists
if (is.list(town_bbox)){
  town_bbox <- lapply(rapply(town_bbox, enquote, how="unlist"), eval)
  # Collect extent and building data for the town of interest
  
  building_list <- list()
  
  for (i in 1:length(town_bbox)){
    temp <- town_bbox[[i]]
    opq_temp <- opq(temp) %>% add_osm_feature(key = "building") %>% osmdata_sf()
    opq_temp <- opq_temp$osm_polygons %>% dplyr::select(osm_id, geometry)
    building_list[[i]] <- opq_temp
  }
  
  # Bind the building data together
  building_temp <- do.call(rbind.data.frame, building_list)
} else{
  temp <- town_bbox
  opq_temp <- opq(temp) %>% add_osm_feature(key = "building") %>% osmdata_sf()
  building_temp <- opq_temp$osm_polygons %>% dplyr::select(osm_id, geometry)
}

town_buildings <- distinct(building_temp)
str(building_temp)
str(town_buildings)

# Find the centroids of each building to serve as the building point
# locations
house_data <- town_buildings %>% st_transform(crs = 32613) %>% st_centroid() %>% st_transform(crs = 4326)

# Keep only the town buildings and centroids that are within our region of interest

idx <- roi %>% st_transform(crs = 32613) %>% st_intersects(st_transform(house_data, crs = 32613), sparse = FALSE)

# Just look at the buildings that are within the region of interest
town_buildings <- town_buildings[idx,]
house_data <- house_data[idx,]

ext <- extent(house_data) %>% as("SpatialPolygons")
crs(ext) <- "EPSG:4326"

roi_sp <- roi %>% st_transform(crs = 32613) %>% as_Spatial()


# Start the r5r core
r5r_core <- setup_r5(
  data_path,
  verbose = FALSE,
  temp_dir = FALSE,
  elevation = "TOBLER",
  overwrite = FALSE
)


# Set up the hexagonal grid 
h <- spsample(roi_sp, type = "hexagonal", cellsize = 100)

# Convert center points to hexagons
g <- HexPoints2SpatialPolygons(h, dx = 100)
g <- st_as_sf(g)

hex_polys <- cbind(seq(1, length(g$geometry)), g)

# Change the column names
colnames(hex_polys) <- c("id_polygons", "geometry")

st_write(hex_polys, "data/evacuation_hex_polys.gpkg", append = FALSE)

# Find the intersections between the hexagonal grids and the house data
intersection <- st_intersection(x = hex_polys, y = st_transform(house_data, crs = 32613))

int_result <- intersection %>% 
  dplyr::group_by(id_polygons) 


buildings <- rep(0, max(hex_polys$id_polygons))
hex_counts <- cbind(hex_polys, buildings)

int_count <- intersection %>% 
  dplyr::group_by(id_polygons, .drop = FALSE) %>% dplyr::count()

hex_counts$buildings[int_count$id_polygons] <- int_count$n

st_write(hex_counts, "data/evacuation_building_counts.gpkg", append = FALSE)

# Add intersections with roads and the extent here

street_net <- street_network_to_sf(r5r_core)

car_net <- dplyr::filter(street_net$edges, car == TRUE)

p1 <- ggplot() + 
  geom_sf(data = hex_counts, color = NA, mapping = aes(fill = buildings)) + 
  scale_fill_viridis_b(name = "Number of \nbuildings", option = "B", breaks = seq(0, 22, by = 2)) + 
  scale_color_viridis_b(name = "Number of \nbuildings", option = "B", breaks = seq(0, 22, by = 2)) + 
  geom_sf(data = exit_paths_roi, color = 'white') +
  geom_sf(data = exits, color = 'white', fill = 'coral', size = 4, pch = 24) +
  geom_sf(data = town_shp, fill = NA, color = "gray", linewidth = 0.5) + 
  theme(legend.key.height= unit(1, "cm"))
p1
fname <- "figures/figure_05_buildings_and_exits.png"
ggsave(filename = fname, plot = p1,  width = 6, height = 6, units = "in", dpi = 600)

fname2 <- "figures/figure_05_buildings_and_exits.pdf"
ggsave(filename = fname2, plot = p1,  width = 6, height = 6, units = "in")

# Continue to run the evacuation models and save their results
crds <- st_coordinates(hex_counts)
head(crds)

# Find the centers of the hexagons for the evacuation network origins
centroids <- hex_counts %>% st_centroid() %>% st_transform(crs = 4326)
center_coords <- centroids %>% st_coordinates()

centers <- cbind(centroids[,1],center_coords, centroids[,2])
colnames(centers)[1:4] <- c("id","lon","lat" ,"n_building" )
centers <- st_drop_geometry(centers[,1:4])

# write.csv(centers, paste0(out_dir, "/", town_tag, "_hex_polys.csv"))

# Find the evacuation times

# For our example, we want to have the origin points be all the hexagons with
# a count of at least 1

# routing inputs
mode <- c("CAR")
max_walk_dist <- 10000 # in meters
max_trip_duration <- 8*60L # in minutes


# Origin points are the hexagons that have more than zero buildings
origin_pts <- dplyr::filter(centers, n_building>0)

# Replace this with the location at which major highways leave the city
destination_pts <- st_coordinates(exits) %>% as.data.frame() %>% distinct()

N <- nrow(destination_pts)

id <- 1:nrow(destination_pts) %>% as.character()
population = rep(0, times=N)
schools = rep(0, times=N)
jobs = rep(0, times = N)
healthcare = rep(0, times=N)
destination_pts <- cbind(id, destination_pts, population, schools, jobs, healthcare)
colnames(destination_pts) <- c("id", "lon", "lat", "population", "schools", "jobs", "healthcare")

# Calculate travel time matrix (outputs are in minutes; speeds are in km/hr)
# These take a long time to run, so the output is already saved in the data directory
ttm_normal <- travel_time_matrix(r5r_core,
                                 origins = origin_pts,
                                 destinations = destination_pts,
                                 max_trip_duration = 8*60,
                                 mode = c("CAR"))

write.csv(ttm_normal, "data/ttm_normal.csv")


# Only choosing bicycle here to be able to control the speed
ttm_stopgo <- travel_time_matrix(r5r_core,
                                 origins = origin_pts,
                                 destinations = destination_pts,
                                 max_lts = 4, # "cyclists" will travel on any road
                                 max_trip_duration = 8*60,
                                 mode = c("BICYCLE"),
                                 bike_speed = 4)


write.csv(ttm_stopgo, "data/ttm_stopgo.csv")

# Read in the CSV files to skip the above evacuation runs
ttm_normal <- read.csv("data/ttm_normal.csv")
ttm_stopgo <- read.csv("data/ttm_stopgo.csv")
hex_counts <- st_read("data/evacuation_building_counts.gpkg")

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
p2_part1 <- ggplot() + geom_sf(data = roi, color = 'black', fill = 'black') + 
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
p2_part2 <- ggplot() + geom_sf(data = roi, color = 'black', fill = 'black') + 
  geom_sf(data = plot_roads, color = "white", linewidth = 0.25) + 
  geom_sf(data = exit_paths_roi, color = 'white', linewidth = 0.75) +
  geom_sf(data = dplyr::filter(ttm, case == 2), color = NA,
          mapping = aes(fill = travel_time_p50)) + 
  scale_fill_viridis_c(name = "Travel time \n(minutes)", option = "F") + 
  scale_color_viridis_c(name = "Travel time \n(minutes)", option = "F") +
  #geom_sf(data = town_shp, color = 'gray', fill = NA, linewidth = 0.5) +
  geom_sf(data = exits, color = 'white', fill = 'coral', pch = 24, size = 4) +
  theme(legend.position = "bottom")


p2 <- plot_grid(p2_part1, p2_part2, labels = c("a", "b"),
                align = "hv",
                label_fontface = "plain",
                label_x = 0.05,
                label_y = 0.99)

p2

fname <- "figures/figure_08_evac_times_map.png"
ggsave(filename = fname, plot = p2, width = 11, height = 6, units = "in", dpi = 600)

fname2 <- "figures/figure_08_evac_times_map.pdf"
ggsave(filename = fname2, plot = p2, width = 11, height = 6, units = "in")


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
p3 <- ggplot(data = evac_stack_df) + geom_line(mapping = aes(x = time_min, y = value, color = area)) + 
  geom_point(mapping = aes(x = time_min, y = value, color = area)) +
  scale_color_discrete(name = "Evacuation\nScenario", labels = c("Speed Limit", "Stop-and-Go")) +
  xlab("Time (minutes)") + ylab(expression("Evacuated Area "~(km^2))) + ggtitle(town_name)
p3

#ggsave(paste0(town_name, "_evacarea_by_time.png"), plot = p3, width = 4, height = 4, units = "in", dpi = 600)

r5r::stop_r5(r5r_core)
rJava::.jgc(R.gc = TRUE)
