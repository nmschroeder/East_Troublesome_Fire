############################################################################################################
## Goal: Determine the best surface fuel moisture content to use for the WRF simulations 
##
## For this file, we plot the final extent of three different surface fuel moisture content
## values to determine which one is the best fit with respect to the final fire perimeter
## from NIFC. Something to note is that the model does not consider firefighting efforts
## which may have a very large impact on the final extent. Because of this, we aimed to find 
## a WRF-generated perimeter that at least exceeded the final fire perimeter from NIFC.
##
## To run this file, please download the Colorado portion of the National Boundary Dataset from
## https://apps.nationalmap.gov/downloader/ OR use the URL below to download the zip file
## https://prd-tnm.s3.amazonaws.com/StagedProducts/GovtUnit/GPKG/GOVTUNIT_Colorado_State_GPKG.zip
##
## When completed, there should be a directory within the download directory called 
## GOVTUNIT_Colorado_State_GPKG and within that directory the geopackage data files 
## GOVTUNIT_Colorado_State_GPKG.gpkg (.gpkg, .jpg, .xml)
##
############################################################################################################

library(ggplot2)
library(raster)
library(dplyr)
library(ncdf4)
library(sf)
library(stringr)
library(viridis)

# This is the grid resolution for the inner domain of the WRF model
dx <- 111.111114501953

# WRF divides dx in 4 in both directions for the fire model
dx4 <- dx/4

# This PROJ string is created within WRF at the center coordinates of the region
# of interest
proj_string <- "+proj=lcc +lat_1=40.34917 +lat_2=40.34917 +lon_0=-105.6476" 

# List the different final results for the control runs for parameter selection
fcontrol <- list.files(path = "wrf_outputs/parameter_selection", 
                       pattern = glob2rx("wrfout_d02*10-25_03*00"), 
                       recursive = TRUE,
                       full.names = TRUE)

# Note how many parameter files we have
N <- length(fcontrol)

# Read in the NIFC final fire perimeter as the true final fire perimeter
perim_true <- st_read("data/fire_east.shp")

# # Collect Estes Park city boundary from USGS National Boundary Dataset
# estes_park <- st_read("downloads/GOVTUNIT_Colorado_State_GPKG/GOVTUNIT_Colorado_State_GPKG.gpkg", 
#                   layer = "GU_IncorporatedPlace") %>% 
#   dplyr::filter(place_name == "Estes Park") %>%
#   st_transform(crs = proj_string)
#   
# st_write(estes_park, "data/estes_park.shp", append = FALSE)

# Read in Estes Park polygon (filtered from USGS National Boundary Dataset)
estes_park <- st_read("data/estes_park.shp") 

# Here, we have some exploration of the city boundary and the variables
# in the WRF output
ggplot() + geom_sf(data = estes_park)

nc <- nc_open(fcontrol[1])
names(nc$var)

LAT <- ncvar_get(nc, varid = "FXLAT")
LON <- ncvar_get(nc, varid = "FXLONG")

dim(LAT)
dim(LON)

lat_df <- data.frame(lat = c(LAT), lon = c(LON))

rm(LAT, LON)

nc_close(nc)

# Begin processing the WRF output

control_list <- list()

for (i in 1:N){
  
  # Pull parameter information from the filename
  # hgt_AA_gm_BBBBBBB
  # tells you the wind at fire height in meters (AA)
  # and the fuel moisture used (BBBBBBB/1e6)
  
  parameter_tag <- str_extract(fcontrol[i], pattern = "hgt_[0-9]{2}_gm_[0-9]{7}")
  hgt <- str_extract(parameter_tag, pattern = "_[0-9]{2}_")
  hgt <- gsub("_", "", hgt) %>% as.numeric()
  
  gm <- str_extract(parameter_tag, pattern = "[0-9]{7}") %>% as.numeric()/1e6

  nc <- nc_open(fcontrol[i])
  
  # List the names in your netcdf file
  #names(nc$var)
    
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
    
  # Add the control polygon for this iteration to the list
  control_list[[i]] <- as.data.frame(pgon_control) %>% mutate(hgt = hgt, gm = gm, id = i, sim = "control")
  nc_close(nc)
}

# Row bind the results together
control_sf <- do.call(rbind.data.frame, control_list) %>% st_as_sf(crs = proj_string)

# Create a figure that shows the final fire perimeters for the different surface fuel moisture
# values (using the control winds) against the NIFC final fire perimeter and city boundary for
# Estes Park
p_control <- ggplot() + geom_sf(data = perim_true, fill = "black") + 
  geom_sf(data = estes_park, fill = 'darkgray') +
  geom_sf(data = control_sf, alpha = 0.1, mapping = aes(color = as.factor(gm), fill = as.factor(gm))) +
  scale_color_viridis_d(name = "Surface fuel\nmoisture content\n(kg/kg)") + 
  scale_fill_viridis_d(name = "Surface fuel\nmoisture content\n(kg/kg)") + 
  theme_bw()

# View it
p_control

ggsave("figures/figure_04_parameter_selection.png", p_control, dpi = 600, height = 3, width = 9, units = 'in')

