############################################################################################################
## Goal: Crop the WRF Preprocessing System (WPS) output files to the region of interest (the portion of the 
## East Troublesome Fire east of the Continental Divide). This is Part 1, creating the region of interest
## from the eastern portion of the East Troublesome Fire (also known as the Thompson Zone) which is shown
## in Figure S2 in the Supporting Information. In Part 2, we take the weather outputs from WPS 
## (which include preprocessed HRRR weather data) and summarize to create Figure 3. The R file for this
## analysis is figure_03_wind_speeds.R.
##
## To run this file, please make sure your working directory is the East_Troublesome_Fire directory.
##
## For this file, please download the NIFC Interagency Fire perimeter shapefile data 
## set to the download directory from the website 
## https://data-nifc.opendata.arcgis.com/datasets/nifc::interagencyfireperimeterhistory-all-years-view/about/
##
## When completed, there should be a directory within the download directory called 
## InterAgencyFirePerimeterHistory_All_Years_View and within that directory the shapefile data files for 
## InterAgencyFirePerimeterHistory_All_Years_View.shp (including .cpg, .dbf, .prj, .shp, and .shx)
##
############################################################################################################

library(dplyr)
library(sf)
library(ggplot2)

## Set variables
fire_name <- "East Troublesome"

# Create a CRS that matches the namelist.wps file entries from the WRF preprocessing system
# (uses the center coordinates of the domains)
proj_string <- "+proj=lcc +lat_1=40.34917 +lat_2=40.34917 +lon_0=-105.6476" 

# # Read in NIFC interagency fire perimeter for East Troublesome
# msg = "Please download the NIFC interagency shapefile at https://data-nifc.opendata.arcgis.com/datasets/nifc::interagencyfireperimeterhistory-all-years-view/about/
#         Unzip the zip file within the directory. -Nicole H-S"
# 
# # Try reading in the fire perimeters and throw an error telling users where to download the data if it is missing
# tryCatch(all_fire_perims <- read_sf("downloads/InterAgencyFirePerimeterHistory_All_Years_View/InterAgencyFirePerimeterHistory_All_Years_View.shp"), 
#          error = function(e) stop(msg)) 
# 
# # Filter for the fire of interest
# fire_perims <- all_fire_perims %>% dplyr::filter(INCIDENT %in% fire_name)
# 
# # Transform this data set to the WRF CRS
# fire_perims_utm <- fire_perims %>% st_transform(crs = proj_string)
# 
# # Cast this data type into a polygon object
# fire_sections <- st_cast(fire_perims_utm, "POLYGON")
# 
# # Check that we have the section of the fire east of the Continental Divide
# ggplot() + geom_sf(data = fire_sections[19:22,])
# 
# # Select the perimeters east of the Continental Divide
# fire_east <- fire_sections[19:22,]
# st_write(fire_east, "data/fire_east.shp")

fire_east <- st_read("data/fire_east.shp")

# Bounding box
bb <- st_bbox(fire_east)

# Buffer by b (2 km, in this case)
b <- 2000
bb_buffer <- bb + c(-b, -b, b, b)

# Create sf point objects using the buffer coordinates
p1 <- st_point(x = c(bb_buffer[1], bb_buffer[2]))
p2 <- st_point(x = c(bb_buffer[1], bb_buffer[4]))
p3 <- st_point(x = c(bb_buffer[3], bb_buffer[4]))
p4 <- st_point(x = c(bb_buffer[3], bb_buffer[2]))

# Create a polygon object for this spatial domain to be the region for our wind calculations
wind_region <- st_polygon(x = list(rbind(p1, p2, p3, p4, p1)), dim = "XY") %>% st_sfc(crs = proj_string)
st_write(wind_region, "wind_region.shp", append = FALSE)

# Plot this domain for the SI
p_wind <- ggplot() + geom_sf(data = fire_east) + geom_sf(data = wind_region, fill = NA, color = 'red') + theme_bw()

# Print it to the screen
p_wind

# Save the figure to the figures directory
ggsave("figures/figure_S02_wind_region.png", plot = p_wind)