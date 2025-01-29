# Read in LANDFIRE elevation and Scott and Burgan 40 Fire Behavior Fuel
# Model data and crop to a region around Estes Park
#
# To begin, download the following TIF files from LANDFIRE 
# (https://landfire.gov/data/FullExtentDownloads)
# to the downloads/LANDFIRE directory:
#
#  LC20_Elev_220.tif # 2020 Elevation
#  LC19_F40_200.tif  # 2019 Scott and Burgan 40 Behavior Fuel Model data
#  LC20_F40_200.tif  # 2020 Scott and Burgan 40 Behavior Fuel Model data
#  LC22_F40_220.tif  # 2022 Scott and Burgan 40 Behavior Fuel Model data
# 
# There is also code to create domain 01 and domain 02 from this study
# and to crop the fire behavior fuel models to the extent visualized
# in Figure 2 using QGIS

library(raster)

# Extent for cropping
ext <- c(xmin = -931050, xmax = -691020, ymin = 1897290, ymax = 2037300) %>% extent()

# List all the LANDFIRE TIF files in the downloads
fnames <- list.files(path = "downloads/LANDFIRE", pattern = glob2rx("L*.tif"), 
                     full.names = TRUE)

for (i in 1:length(fnames)){
  # Read in the raster data
  r <- raster(fnames[i])
  
  # Crop to our extent around Estes Park
  cropped <- crop(x = r, y = ext)
  
  # Remove the downloads/LANDFIRE part of the path
  new_name <- gsub(pattern = "downloads/LANDFIRE/", replacement = "", x = fnames[i])
  
  # replace .tif with _estes_park.tif to note our region
  new_name <- gsub(pattern = ".tif", replacement = "_estes_park.tif", x = new_name)
  
  # Save to the data directory
  writeRaster(cropped, paste0("data/", new_name), overwrite = TRUE)
}

# For clarity, the following represent domain 01 and domain 02 in this study

# Create a CRS that matches the namelist.wps file entries (uses the center
# coordinates of the domains)
proj_string <- "+proj=lcc +lat_1=40.34917 +lat_2=40.34917 +lon_0=-105.6476" 

center_xy <- st_point(x = c(-105.6476, 40.34917)) %>% st_sfc(crs = 4326) %>% st_transform(crs = proj_string)
domain02 <- st_bbox(center_xy) + c(-25000, -15000, 25000, 15000)

d02 <- extent(domain02)

# To resample the data for plotting using QGIS for Figure 2
fuel2020 <- raster("data/LC20_F40_200_estes_park.tif")

# Transform the data using a nearest neighbor method (because the 
# data is categorical)
fuel2020t <- projectRaster(fuel2020, crs = proj_string, method = 'ngb')

# Add some buffered room for the visualization
vis_ext <- d02 + c(-5000, 5000, -5000, 5000)

fuel2020_crop <- crop(fuel2020t, vis_ext)
writeRaster(fuel2020_crop, "data/figure_02_fuels.tif", overwrite = TRUE)

domain01 <- st_bbox(center_xy) + c(-100000, -60000, 100000, 60000)

d01 <- extent(domain01)

d01_p1 <- st_point(x = c(d01@xmin, d01@ymin))
d01_p2 <- st_point(x = c(d01@xmin, d01@ymax))
d01_p3 <- st_point(x = c(d01@xmax, d01@ymax))
d01_p4 <- st_point(x = c(d01@xmax, d01@ymin))
d01_sf <- st_polygon(x = list(rbind(d01_p1, d01_p2, d01_p3, d01_p4, d01_p1))) %>%
  st_sfc(crs = proj_string)

d02_p1 <- st_point(x = c(d02@xmin, d02@ymin))
d02_p2 <- st_point(x = c(d02@xmin, d02@ymax))
d02_p3 <- st_point(x = c(d02@xmax, d02@ymax))
d02_p4 <- st_point(x = c(d02@xmax, d02@ymin))
d02_sf <- st_polygon(x = list(rbind(d02_p1, d02_p2, d02_p3, d02_p4, d02_p1))) %>%
  st_sfc(crs = proj_string)

st_write(d01_sf, "data/domain_01.shp", append = FALSE)
st_write(d02_sf, "data/domain_02.shp", append = FALSE)


