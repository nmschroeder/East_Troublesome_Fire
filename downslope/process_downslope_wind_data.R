# File to collate downslope wind event data from Abatzoglou et al. (2023) 
# near Estes Park, Colorado

# Inputs:
# US State Tigerline shapefile (tl_2012_us_state.shp)
# NIFC Interagency Fire perimeters (WFIGS_Interagency_Perimeters/Perimeters.shp)

# Outputs:
# Downslope wind record for Estes Park (et_winds.csv)
# Summary of consecutive downslope wind events during the East Troublesome Fire
# (et_event_summary.csv)

library(ncdf4)
library(raster)
library(sf)
library(dplyr)
library(stringr)
library(lubridate)
library(tidyverse)

setwd("downslope")

# Read in US States Tigerline files
states <- st_read("states/tl_2012_us_state.shp") 

# Read NIFC Interagency Fire Perimeters and select the East Troublesome Fire 
et_fire <- st_read("~/Data/WFIGS_Interagency_Perimeters/Perimeters.shp") %>% 
  dplyr::filter(poly_Incid == "East Troublesome")

et_fire_perims <- st_cast(et_fire, "POLYGON") 

et_fire_perims <- mutate(et_fire_perims, id = 1:nrow(et_fire_perims))

# Check results
ggplot() + geom_sf(data = et_fire_perims) + facet_wrap(~id)

# Collect just the Thompson Zone (portion east of the Continental Divide)
thompson_zone <- et_fire_perims[nrow(et_fire_perims),]

plot(thompson_zone)

tz_center <- st_centroid(thompson_zone)
xy <- st_coordinates(tz_center)

# Filter for Colorado
colorado <- dplyr::filter(states, NAME == "Colorado") %>% st_transform(crs = 4326)

fnames <- list.files(path = "data", pattern = ".nc$", full.names = TRUE)

# Go through each year and collect the raster data for each day
for (j in 1:length(fnames)){
  fname <- fnames[j]
  
  tag <- str_extract(fname, "downslopeday_[0-9]{4}.nc")
  year <- str_extract(tag, "[0-9]{4}")
  
  ds <- nc_open(fname)
  
  ds$var
  
  lon <- ncvar_get(ds, varid = "lon") 
  lon <- ifelse(lon >= 180, lon-360, lon)
  idx_order <- order(lon)
  lon_ordered <- lon[idx_order]
  lat <- ncvar_get(ds, varid = "lat")
  winds <- ncvar_get(ds, varid = "downslope")
  
  # Collect the dimensions of the 3D VPD array
  winds_dim <- dim(winds)
  
  # Look at the dimensions (N_lon x N_lat x N_days)
  winds_dim
  
  # The number of days is the last dimension
  N_lon <- winds_dim[1]
  N_lat <- winds_dim[2]
  N_days <- winds_dim[3]
  df_list <- list()
  for (i in 1:N_days){
    
      
    
    winds_t <- winds[idx_order,,i] %>% t() %>% raster() 
    
    # The function extent takes four unnamed arguments: xmin, xmax, ymin, ymax
    extent(winds_t) <- extent(min(lon), max(lon), min(lat), max(lat))
    
    # Assign the CRS to 4326 (usual lat/lon for WGS84)
    crs(winds_t) <- CRS("EPSG:4326")
    
    winds_crop <- crop(winds_t, colorado)
    
    winds_df <- as.data.frame(winds_crop, xy = TRUE)
    colnames(winds_df) <- c("x", "y", "ds")
    str(winds_df)
    
    #plot(winds_t)
    #ggplot() + geom_raster(data = winds_df, mapping = aes(x = x, y = y, fill = ds))
    
    winds_df <- arrange(winds_df, x, y) %>% mutate(pixel_id = 1:nrow(winds_df), year = year, day = i, fname = tag)
    df_list[[i]] <- winds_df
  }
  
  year_df <- do.call(rbind.data.frame, df_list)
  
  new_tag <- gsub(pattern = ".nc", replacement = "", tag)
  
  # Save each year of data into a csv file
  write.csv(year_df, paste0("output/", new_tag, ".csv"))
  
  nc_close(ds)

}


# Next item: for each pixel_id, read in every year of data
pixel_ids <- unique(year_df$pixel_id) %>% sort()
N_pixels <- length(pixel_ids)
N_char <- str_length(as.character(max(pixel_ids)))
csv_names <- list.files(path = "output", pattern = glob2rx("downslopeday_*.csv"), full.names = TRUE)
N_files <- length(csv_names)

# Why was this 70:N_pixels before? Needing to complete the loop? 
# Answer: it must have been based on which output tifs were still needed
for (i in 1:N_pixels){
  id <- pixel_ids[i]
  
  pixel_tag <- str_pad(id, width = N_char, side = "left", pad = "0")
  
  pixel_list <- list()
  for (j in 1:N_files){
    pixel_list[[j]] <- read.csv(csv_names[j]) %>% dplyr::select(-X) %>% dplyr::filter(pixel_id == id)
  }
  
  pixel_df <- do.call(rbind.data.frame, pixel_list)
  
  write.csv(pixel_df, paste0("output/pixel_", pixel_tag, ".csv"))
}

# Read in each pixel_id's data and collect the downslope wind events
pixel_fnames <- list.files(path = "output", pattern = glob2rx("pixel*.csv"), full.names = TRUE)

N_fnames <- length(pixel_fnames)

for (i in 1:N_fnames){
  
  pixel_df <- read.csv(pixel_fnames[i]) %>% dplyr::select(-X)
  pixel_tag <- str_extract(pixel_fnames[i], "(?<=_)[^_\\.]+(?=\\.)")
  N_time <- nrow(pixel_df)
  count <- 0
  event_id <- 1
  event_list <- list()
  temp_list <- list()
  flag <- 0
  for (j in 1:N_time){
    na_check <- is.na(pixel_df$ds[j])
    if (na_check){
      check <- FALSE
    } else{
      check <- pixel_df$ds[j] == 1
    }
    
    if (check){
      flag <- 1
      count <- count + 1
      temp_list[[count]] <- pixel_df[j,] %>% mutate(count, event_id)
    } else{
      if (flag == 1){
        flag <- 0
        count <- 0
        event_list[[event_id]] <- do.call(rbind.data.frame, temp_list)
        event_id <- event_id + 1
        temp_list <- list()
      }
    }
  }
  event_df <- do.call(rbind.data.frame, event_list)
  if (nrow(event_df) == 0){
    event_df <- pixel_df[1,] %>% mutate(count = NA, event_id = NA)
    event_df$year <- NA
    event_df$day <- NA
    event_df$fname <- NA
  } 
  write.csv(event_df, paste0("output/events_pixel_", pixel_tag, ".csv"))
}

fnames_event <- list.files(path = "output", pattern = glob2rx("events_pixel_*csv"), full.names = TRUE)

wind_events <- lapply(fnames_event, read.csv)
wind_df <- do.call(rbind.data.frame, wind_events)

str(wind_df)

x_diff <- abs(wind_df$x - xy[1])
y_diff <- abs(wind_df$y - xy[2])
check <- vector(length = length(x_diff))

for (i in 1:length(x_diff)){
  check[i] <- max(x_diff[i], y_diff[i])  
}

idx <- which.min(check)
et_pixel_id <- wind_df[idx,]$pixel_id

et_winds <- dplyr::filter(wind_df, pixel_id == et_pixel_id)
write.csv(et_winds, "et_winds.csv")

event_summary <- wind_df %>% group_by(pixel_id, event_id) %>% summarize(x = x[1], y = y[1], year = year[1], day = day[1], N_days = max(count))

date_values <- as.Date(event_summary$day - 1, origin = paste0(event_summary$year, "-01-01"))
event_summary$date <- date_values

et_event_summary <- dplyr::filter(event_summary, pixel_id == et_pixel_id, date >= as.Date("2020-10-01"), date <= as.Date("2020-11-01"))
st_write(et_event_summary, "et_event_summary.csv")
