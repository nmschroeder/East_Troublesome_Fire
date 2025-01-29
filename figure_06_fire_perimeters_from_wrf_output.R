############################################################################################################
## Goal: Generate fire perimeters from the WRF-Fire output for the control and experiment simulations
## and organize them with the operational event polygons from NIFC's 2020 Operational Data Archive
##
## Please download Operational Data Archive 2020 from 
## https://data-nifc.opendata.arcgis.com/datasets/ea843f7f091f4c7f9743798b64c864be/about
## and place the resulting geodatabase into the downloads folder and can access it at the 
## following location within this directory:
##
## downloads/Public_EventDataArchive_2020.gdb
## 
## This file outputs a time series of fire perimeters called
##
## data/east_troublesome_ts.gpkg
##
## and visualizes them in Figure 6 (figures/figure_06_fire_spread.png)
##
############################################################################################################

library(ggplot2)
library(raster)
library(dplyr)
library(ncdf4)
library(sf)
library(stringr)
library(viridis)

# Note the resolution of domain 02 (in meters)
dx <- 111.111114501953
dx4 <- dx/4

# CRS of WRF output
proj_string <- "+proj=lcc +lat_1=40.34917 +lat_2=40.34917 +lon_0=-105.6476" 

fcontrol <- list.files(path = "wrf_outputs/control/hgt_02_gm_0145000", 
                       pattern = glob2rx("wrfout_d02*"), full.names = TRUE)


fexp <- list.files(path = "wrf_outputs/experiment/hgt_02_gm_0145000", 
                       pattern = glob2rx("wrfout_d02*"), full.names = TRUE)

N <- length(fexp)

# Read in Estes Park polygon
estes_park <- st_read("data/estes_park.shp") %>% st_transform(crs = proj_string)

# Check
ggplot() + geom_sf(data = estes_park)

nc <- nc_open(fcontrol[1])
names(nc$var)

LAT <- ncvar_get(nc, varid = "FXLAT")
LON <- ncvar_get(nc, varid = "FXLONG")

dim(LAT)
dim(LON)

lat_df <- data.frame(lat = c(LAT), lon = c(LON))

rm(LAT, LON)

control_list <- list()
experiment_list <- list()

for (i in 1:N){

  nc <- nc_open(fcontrol[i])
  
  # List the names in your netcdf file
  #names(nc$var)
  
  # For the first one, use the initial perimeter
  if (i == 1){
    
    # Obtain the level set function matrix
    lfn_hist <- ncvar_get(nc, varid = "LFN_HIST") 
    
    # Convert it to a data frame
    lfn_df <- data.frame(lfn_hist = c(lfn_hist))
    
    # Column bind it to the latitude and longitude data frame; values where
    # lfn_hist are negative are places that have burned
    all_df <- cbind.data.frame(lat_df, lfn_df) %>% dplyr::filter(lfn_hist < 0)
    all_sf <- st_as_sf(all_df, coords = c("lon", "lat"), dim = "XY")
    all_sf <- all_sf$geometry %>% st_sfc(crs = 4326)
    
    #ggplot() + geom_sf(data = all_sf)
    
    
    all_utm <- all_sf %>% st_transform(crs = proj_string)
    boxes_lfnhist <- st_buffer(all_utm, dist = (dx4/2+1), endCapStyle = "SQUARE")
    
    #ggplot() + geom_sf(data = boxes_lfnhist)
    
    pgon_lfnhist <- st_combine(boxes_lfnhist) %>% st_union(by_feature = TRUE, is_coverage = TRUE)
    pgon_lfnhist <- st_sfc(pgon_lfnhist, crs = proj_string) %>% st_buffer(dist = -1)
    
    pgon_control <- pgon_lfnhist
    pgon_exp <- pgon_lfnhist
    
  
  } else{
    
    # Read in the FIRE_AREA variable from the netcdf file 
    wrf_control <- ncvar_get(nc, varid = "FIRE_AREA")
    
    wrf_control_df <- data.frame(fire_area = c(wrf_control))
    
    all_df <- cbind.data.frame(lat_df, wrf_control_df) %>% dplyr::filter(fire_area > 0)
    all_sf <- st_as_sf(all_df, coords = c("lon", "lat"), dim = "XY")
    all_sf <- all_sf$geometry %>% st_sfc(crs = 4326)
    
    
    all_utm <- all_sf %>% st_transform(crs = proj_string)
    boxes <- st_buffer(all_utm, dist = (dx4/2+1), endCapStyle = "SQUARE")
    
    
    pgon <- st_combine(boxes) %>% st_union(by_feature = TRUE, is_coverage = TRUE)
    pgon_control <- st_sfc(pgon, crs = proj_string) %>% st_buffer(dist = -1)
    
    # Much slower; do not use
    #control_perim <- rasterToPolygons(wrf_control)
    
    ## Experiment
    nc_exp <- nc_open(fexp[i])
    wrf_exp <- ncvar_get(nc_exp, varid = "FIRE_AREA") 
    
    wrf_exp_df <- data.frame(fire_area = c(wrf_exp))
    
    all_df <- cbind.data.frame(lat_df, wrf_exp_df) %>% dplyr::filter(fire_area > 0)
    all_sf <- st_as_sf(all_df, coords = c("lon", "lat"), dim = "XY")
    all_sf <- all_sf$geometry %>% st_sfc(crs = 4326)
    
    
    all_utm <- all_sf %>% st_transform(crs = proj_string)
    boxes <- st_buffer(all_utm, dist = (dx4/2+1), endCapStyle = "SQUARE")
    
    
    pgon <- st_combine(boxes) %>% st_union(by_feature = TRUE, is_coverage = TRUE)
    pgon_exp <- st_sfc(pgon, crs = proj_string) %>% st_buffer(dist = -1)
    
  }
    
    # Add the control polygon for this iteration to the list
    control_list[[i]] <- pgon_control
    
    # Add the experiment polygon for this iteration to the list
    experiment_list[[i]] <- pgon_exp
  
}


# Set up the control and experiment data frames
control_list_test <- lapply(control_list, as.data.frame)

control_sf <- do.call(rbind.data.frame, control_list_test) %>% st_as_sf(crs = proj_string)
str(control_sf)

sim <- rep("control", times = N)
id <- 1:N

control_df <- mutate(control_sf, sim, id)

experiment_list_test <- lapply(experiment_list, as.data.frame)
experiment_sf <- do.call(rbind.data.frame, experiment_list_test) %>% st_as_sf(crs = proj_string)

sim <- rep("experiment", times = N)
id <- 1:N

experiment_df <- mutate(experiment_sf, sim, id)

sim_sf <- rbind.data.frame(experiment_df, control_df)

st_write(sim_sf, "data/simulations.shp", append = FALSE)

# Can start from here, if the previous lines were already run
sim_sf <- st_read("data/simulations.shp")

# Add the date and time to each row; then pivot_longer and append the operational
# polygon information. We can try a facet_wrap plot where we choose the sim as the
# facet to wrap on; call sim = op for the operational fire polygons

# Use id to link the dates to the rows

# Extract the date pattern from the filenames
d <- str_extract(fcontrol, pattern = "[0-9]{4}-[0-9]{2}-[0-9]{2}_[0-9]{2}:[0-9]{2}:[0-9]{2}")

# Replace the underscore with a space to follow the time conventions
d <- gsub("_", " ", d)

# Convert to dates
dates <- as.POSIXct(d, tz = "UTC")

dates_df <- data.frame(id = 1:N, dates = dates[1:N])
str(dates_df)

sim_dates_df <- right_join(sim_sf, dates_df, by = "id")

# Next add the operational fire polygons; the id will no longer match 20-min
# intervals

op_dir <- "downloads/Public_EventDataArchive_2020.gdb"
op_fires <- st_read(op_dir, layer = "EventPolygon") 

event_pgons <- op_fires %>% dplyr::filter(IncidentName %in% c("East Troublesome", "EAST TROUBLESOME"))
str(event_pgons)

event_pgons$GDB_FROM_DATE <- as.POSIXct(event_pgons$GDB_FROM_DATE, tz = "UTC")
event_pgons$DateCurrent <- as.POSIXct(event_pgons$DateCurrent, tz = "UTC")

rm(op_fires)

event_pgons <- arrange(event_pgons, GDB_FROM_DATE)
N_events <- nrow(event_pgons)

final_event <- event_pgons[N_events-1,]

ggplot() + geom_sf(data = final_event)

ggplot() + geom_sf(data = event_pgons, fill = NA, mapping = aes(color = GDB_FROM_DATE))

# Identify events that intersect the final fire perimeter east of the Continental Divide
idx <- st_intersects(final_event, event_pgons, sparse = FALSE)

# Select only those events which include perimeters east of the Continental Divide
event_temp <- event_pgons[idx,]

test <- event_temp[1,]
plot(test)

# Select only the portion of the multipolygons
event_id <- function(mpgon, final_event){
  new_pgon <- mpgon
  temp <- mpgon$Shape %>% st_cast("POLYGON")
  temp_id <- st_intersects(final_event, temp, sparse = FALSE)
  if (sum(temp_id)>=1){
    temp_pgon <- temp[temp_id,] %>% st_combine()
    new_pgon$Shape <- temp_pgon
  } else{
      
    if (sum(temp_id)==0){
      new_pgon <- NULL
    } else{
      temp_pgon <- temp
      new_pgon$Shape <- temp_pgon
    }

  }
  
  return(new_pgon)
}

temp_list <- list()
for (i in 1:N_events){
  temp_df <- event_pgons[i,]
  new_df <- event_id(temp_df, final_event)
  temp_list[[i]] <- new_df
  
}

event_east <- do.call(rbind.data.frame, temp_list)[1:7,]

st_write(event_east, "data/event_east.gpkg", append = FALSE)

event_east <- st_read("data/event_east.gpkg")

event_east_time_cut <- event_east[1:7,]

ggplot() + geom_sf(data = event_east, fill = NA, mapping = aes(color = GDB_FROM_DATE))

event_df <- dplyr::select(event_east_time_cut, DateCurrent, geom) %>% 
  mutate(id = 1:nrow(event_east_time_cut), sim = "a_op") %>% 
  rename(dates = DateCurrent, geometry = geom) %>%
  st_transform(crs = proj_string)
event_df

p <- ggplot(data = event_df) + geom_sf() + facet_wrap(~dates) + 
  theme_bw() + 
  theme(axis.text.x = element_blank(), 
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank())
p

pgon_df <- rbind.data.frame(sim_dates_df, event_df[1:7,])
pgon_df$sim <- as.factor(pgon_df$sim)

# Save the East Troublesome time series
st_write(pgon_df, "data/east_troublesome_ts.gpkg", append = FALSE)

# # Can read it in to split this script into two parts
# pgon_df <- st_read("data/east_troublesome_ts.gpkg")

# Helpful function for plotting dates from Tony Ladson
# https://stackoverflow.com/questions/21311489/scatter-plot-with-ggplot2-colored-by-dates
as.Date_origin <- function(x){
  as.POSIXct(x, tz = "UTC")
}

p1 <- ggplot(data = event_df[1:7,], mapping = aes(color = dates)) + 
  geom_sf(fill = NA) +
  scale_color_viridis_c(name = "Date", labels = as.Date_origin) + 
  theme_bw() +
  theme(axis.ticks.x = element_blank(), axis.ticks.y = element_blank())
p1

p2 <- ggplot(data = dplyr::filter(pgon_df, sim == "control"), mapping = aes(color = dates)) + 
  geom_sf(fill = NA) +
  scale_color_viridis_c(name = "Date", labels = as.Date_origin) + 
  theme_bw() +
  theme(axis.ticks.x = element_blank(), axis.ticks.y = element_blank())
p2

sim.labs <- c("a. Operational", "b. Control", "c. Experiment")
names(sim.labs) <- c("a_op", "control", "experiment")

pgon_df$dates_utc <- as.POSIXct(pgon_df$dates, tz = "UTC")

p3 <- ggplot() + 
  geom_sf(data = estes_park, fill = 'black') +
  geom_sf(data = arrange(pgon_df, desc(dates_utc)), mapping = aes(color = dates_utc), fill = NA) + 
  scale_color_viridis_c(name = "Date", label = as.Date_origin, option = "A") + 
  theme_dark(base_size = 12) +
  theme(legend.position = "bottom", strip.text = element_text(size = 14), legend.key.width = unit(2, "in")) +
  facet_wrap(~sim, labeller = labeller(sim = sim.labs))
p3

ggsave("figures/figure_06_fire_spread.png", plot = p3, dpi = 600, width = 12, height = 5, units = "in")



