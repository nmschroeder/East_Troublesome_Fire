################################################################################
## Goal: Estimate fire speed from the WRF-Fire fire perimeters from this study 
## and from the operational data archive event perimeters and visualize the
## speed comparison over the first 24 hours
##
## This file reads in the following time series generated in figure_06*.R:
##
## data/east_troublesome_ts.gpkg
##
## and a cropped version of the LANDFIRE Elevation product
## 
## data/LC20_Elev_estes_park.tif
##
## to compute the speeds and speed comparisons shown in Figure 7 
## (figures/figure_07_control_experiment_speed_24hrs.png)
##
################################################################################


library(ggplot2)
library(raster)
library(dplyr)
library(ncdf4)
library(sf)
library(stringr)
library(viridis)
library(concaveman)
library(nngeo)
library(units)
library(cowplot)

# The speed estimates involve sampling the fire perimeters, so a seed is needed
# to get the exact same results each time
set.seed(100)

# Determine your fraction of points per x meters for the speed calculations
x <- 100
points_per_meter <- 2/x

proj_string <- "+proj=lcc +lat_1=40.34917 +lat_2=40.34917 +lon_0=-105.6476" 

elev <- raster("data/LC20_Elev_220_estes_park.tif") 

# Read in the time series data with the WRF simulations and operational
# polygons
pgons <- st_read("data/east_troublesome_ts.gpkg")

ds <- res(elev)[1]

elev_t <- projectRaster(elev, crs = proj_string)

# Collect the unique simulation IDs from the time series
sim_id_unique <- unique(pgons$sim)

elevation_distance <- function(pt1, pt2, elev_t, proj_string){
  # For now, we're taking the resolution of the elevation raster file
  # and sampling about 100 times per pixel to approximate the arc length
  # integral
  
  class_check1 <- class(pt1)
  
  if (!(("sf" %in% class_check1) & ("data.frame" %in% class_check1))){
    pt1 <- st_as_sf(pt1)
  }
  
  class_check2 <- class(pt2)
  
  if (!(("sf" %in% class_check2) & ("data.frame" %in% class_check2))){
    pt2 <- st_as_sf(pt2)
  }
  
  ds <- res(elev_t)[1]/100
  
  # Compute the xy-distance between the two points in meters
  d_p1_p2 <- st_distance(pt1, pt2) %>% as.numeric()
  
  # Create a linestring between the two proposed points
  L <- rbind.data.frame(pt1, pt2) %>% summarize() %>% st_cast("LINESTRING")
  
  # Determine how many sampled points of length ds can fit on the linestring
  k <- floor(as.numeric(d_p1_p2/ds))
  
  # Create a regular sample (will start ds from point 1)
  if (k > 0){
    sample_pts <- st_sample(L, size = k, type = 'regular')
    # Use pt1 and the sample_pts for the algorithm
    s_pts <- sample_pts %>% st_cast("POINT") %>% st_as_sf()
    
    # This will have changed a small amount during sampling
    ds <- st_distance(s_pts[1,], s_pts[2,]) %>% as.numeric()
    
    # Find the xy-distance between pt1 and the sample points
    dsp1 <- st_distance(pt1, s_pts[1,]) %>% as.numeric()
    
    # Find the xy-distance between pt2 and the sample points
    dsp2 <- st_distance(pt2, s_pts[k,]) %>% as.numeric()
    
    # Add point 1 and point 2 to the sample points in order (1st and last)
    s_pts <- rbind.data.frame(pt1, s_pts, pt2)
    
    # Create an array with the xy-distance between each point
    ds <- c(dsp1, rep(ds, times = (k-1)), dsp2)
    
    # Collect the z-values for the sampled points
    z <- raster::extract(elev_t, s_pts)
    
    # Find the change in z between each point
    dz <- z[2:(k+2)] - z[1:(k+1)]
    
    # Find the length of each segment in 3D space
    dl <- sqrt(ds^2 + dz^2)
    
    # Sum up the little lengths to estimate the length of the entire segment
    # projected along the elevation raster
    l <- sum(dl)
    
  } else{
    # The segment is not long enough to create points along it, so 
    # just use the end points
    s_pts <- rbind.data.frame(pt1, pt2)
    
    # Collect the z-values for the sampled points
    z <- raster::extract(elev_t, s_pts)
    
    # Find the change in z between each point
    dz <- z[2] - z[1]
    l <- sqrt(d_p1_p2^2 + dz^2)
  }
  
  # Note that the sampled points are not necessarily the same distance from
  # the end points as they are from each other
  #p_test + geom_sf(data = sample_pts, color = 'magenta')
  
  # Return the distance traveled including changes in elevation
  return(l)
}

time_check <- vector(length = length(sim_id_unique))

L_df_list <- list()

for (m in 1:length(sim_id_unique)){
  
  sim_id <- sim_id_unique[m]
  
  pgons_seq <- pgons %>% rename(DateCurrent = dates) %>% dplyr::filter(sim == sim_id)
  
  dt <- pgons_seq$DateCurrent[2:nrow(pgons_seq)]-pgons_seq$DateCurrent[1:(nrow(pgons_seq)-1)]
  
  units(dt) <- "hours"
  
  pgons_seq$dt <- c(NA, dt)
  
  
  ptm <- proc.time()
  
  
  count <- 1
  fire_spread_list <- list()

    for (i in 2:nrow(pgons_seq)){ 
    
    # Assign the starting and ending polygons (pgon1 and pgon2, respectively)
    pgon1 <- pgons_seq[(i-1),] %>% st_cast("POLYGON") %>% st_remove_holes()
    pgon2 <- pgons_seq[i,] %>% st_cast("POLYGON") %>% st_remove_holes()
    
    # Determine how many perimeters there are for each time point
    N1 <- nrow(pgon1)
    N2 <- nrow(pgon2)
    
    # Determine which of the previous polygons intersect the current ones
    # Rows refer to polygons in pgon1 (previous) and columns refer to polygons in 
    # pgon2 (current)
    pgon_grid <- st_intersects(pgon1, pgon2, sparse = FALSE)
    
    for (j in 1:N2){
      
      # Convert the jth polygon in polygon 2 to a multilinestring
      pgon2_ls <- pgon2[j,] %>% st_cast("MULTILINESTRING")
      
      # Create a 1-km buffer around the current polygon 
      pgon2_buffer <- pgon2[j,] %>% st_buffer(dist = 200)
      
      # If none of the previous polygons are inside this one, then the fire
      # may have spotted from ember throw
      spot_check <- !any(pgon_grid[,j])
      
      if (spot_check){
        idx_grid <- 1:N1
        # Collect all the previous polygons and find the longest perimeter length to determine
        # the number of sample points we need to use to compare with the current one (might be
        # much smaller than the previous, since it's from ember throw)
        
        # Some spots are small, so we need more points to get an accurate estimate
        # of the shortest distance between the spot and the previous perimeter
        M1 <- pgon1 %>% st_cast("MULTILINESTRING") %>% st_length() %>% max() %>% as.numeric() * points_per_meter 
        M1 <- as.integer(M1)
        M2 <- as.integer(st_length(pgon2_ls)*points_per_meter)
        M <- max(c(M1, M2, 1))
        
      } else{
        
        # Otherwise, determine which of the previous polygons are inside the
        # current one to loop through them and find the optimal distances
        idx_grid <- which(pgon_grid[,j])
        
        # Let M be the number of points to sample based on the perimeter of polygon 2
        M <- as.integer(st_length(pgon2_ls) * points_per_meter)
        M <- max(c(M, 1)) # Just in case M gets assigned to zero...
        
      }
      
      # Create vectors that include the date and change in time
      DateCurrent <- rep(pgon2$DateCurrent[1], times = M)
      dt <- rep(pgon2$dt[1], times = M)
      
      # Sample multilinestring Y, M times
      Y <- st_sample(pgon2_ls, size = M, type = 'regular') %>% st_cast("POINT") %>% st_as_sf() %>% mutate(pt_id = 1:M)
      
      # Now we want to find the distances from each of these previous polygons
      # to the next one and use the shortest distances
      

      # Initialize a list for storing linestrings between matched points
      L_list <- list()
      
      D <- array(data = NA, dim = c(M, M, length(idx_grid)))
      
      X_list <- list()
      
      for (k in 1:length(idx_grid)){
        
        idx_val <- idx_grid[k]
        
        # Convert the kth polygon from the previous perimeter(s) to a multilinestring
        pgon1_ls <- pgon1[idx_val,] %>% st_cast("MULTILINESTRING")
        
        # Sample the linestring
        X_temp <- st_sample(pgon1_ls, size = M, type = 'regular') %>% st_cast("POINT") %>% st_as_sf() %>% mutate(pt_id = 1:M)
        
        # In the abbreviated version, we only want the shortest distance from polygon 1 to
        # polygon 2
        D[,,k] <- st_distance(X_temp, Y) 
        X_list[[k]] <- X_temp %>% mutate(k, idx_val)
      }
      
      X_df <- do.call(rbind.data.frame, X_list)
      
      
      # For each point in Y, we want to know the closest point in X
      D_min <- array(data = NA, dim = c(nrow(Y), length(idx_grid)))
      D_min_xlocs <- array(data = NA, dim = c(nrow(Y), length(idx_grid)))

      for (jj in 1:nrow(Y)){
        for (k in 1:length(idx_grid)){
          #Y_min_xloc[jj] <- which.min(D[,jj,])
          D_min_xlocs[jj,k] <- which(D[,jj,k] == min(D[,jj,k]))
          D_min[jj,k] <- min(D[,jj,k])
        }
      }
      
      # You still need to determine which of the previous perimeters is closest
      # and what the associated distance is
      
      D_min_by_yloc <- array(data = NA, dim = c(nrow(Y), 1))
      D_min_xloc <- array(data = NA, dim = c(nrow(Y), 1))
      D_min_kloc <- array(data = NA, dim = c(nrow(Y), 1))
      
      for (jj in 1:nrow(Y)){
        D_min_by_yloc[jj] <- min(D_min[jj,])
        D_min_kloc[jj] <- which.min(D_min[jj,])
        D_min_xloc[jj] <- D_min_xlocs[jj, D_min_kloc[jj]]
      }
      
      # Now we have an array of the minimum distances between each point in 
      # Y and each starting perimeter Xp
      
      # Now we want the MAXIMUM of these minima to find the fastest rate 
      # of spread
      fastest_jk <- which(D_min_by_yloc == max(D_min_by_yloc), arr.ind = TRUE)
      if (nrow(fastest_jk)>1){
        fastest_jk <- fastest_jk[1,]
      }
      
      # Need indices here instead of D_min[fastest_jk]
      D_fastest_val <- D_min_by_yloc[fastest_jk[1], fastest_jk[2]]
      yloc <- fastest_jk[1]
      kloc <- D_min_kloc[yloc]
      xloc <- D_min_xloc[yloc]
      
      
      pt1_df <- X_df %>% filter(k == kloc & pt_id == xloc) 
      pt1 <- pt1_df %>% dplyr::select(x)
      pt2 <- Y[yloc,] %>% dplyr::select(x)
      
      # Create a line string from the two points
      L_temp <- rbind(pt1, pt2) %>% st_as_sfc() %>% st_as_sf(crs = proj_string) %>% summarize() %>% st_cast("LINESTRING")
      L_list[[1]] <- L_temp
      delta_t <- dt[1]
      delta_t <- set_units(delta_t, "hours")
      d_xy <- 0.001*as.numeric(D_fastest_val) %>% set_units("km")
      v_xy <- d_xy/delta_t
      d_elev <- 0.001 * elevation_distance(pt1, pt2, elev_t, proj_string) %>% set_units("km")
      v_elev <- d_elev/delta_t
      
      if (as.numeric(v_xy) > 30){
        stop("please check values")
        p <- ggplot() + geom_sf(data = pgon1, fill = NA, color = 'darkgreen') + 
          geom_sf(data = pgon2, fill = NA, color = 'darkred')
        p + geom_sf(data = X, color = 'darkgreen') +
          geom_sf(data = pt1, color = 'green') + 
          geom_sf(data = pt2, color = 'red') + 
          geom_sf(data = L_temp, color = 'blue')
      }
      
      #ggplot() + geom_sf(data = pgon1, fill = NA, color = 'darkgreen') + geom_sf(data = pgon2, fill = NA, color = 'darkred') + geom_sf(data = L_temp, color = 'blue') + geom_sf(data = pt1, color = 'green')
      
      DateCurrent <- DateCurrent[1]
      dt <- dt[1]
      
      L_sf <- lapply(L_list, st_as_sf)
      L <- do.call(rbind.data.frame, L_sf)
      
      X_fit <- pt1_df %>% dplyr::select(-idx_val)
      Y_fit <- Y[yloc,]
      
      temp_df <- cbind.data.frame(rename(X_fit, id1 = pt_id, perimID = k), rename(Y_fit, y = x, id2 = pt_id), rename(L, L=x)) %>%
        mutate(DateCurrent, dt, d_xy, v_xy, d_elev, v_elev, pgon = i, Yj = j, Xk = k, spot = spot_check)
      
      fire_spread_list[[count]] <- temp_df
      
      count <- count + 1
      
      print(paste0("Working on ", count))
      
    }
    
  }

  
  # Bind the list of fire speed calculations together
  
  fire_spread_df <- do.call(rbind.data.frame, fire_spread_list)
  
  # Summarize the findings
  
  summary_list <- list()
  
  count <- 1
  
  for (i in 2:nrow(pgons_seq)){
    temp_df <- dplyr::filter(fire_spread_df, pgon == i)
    
    # Determine the number of points in Yj
    y_unique <- unique(temp_df$id2)
    
    for (j in 1:length(y_unique)){
      
      temp_j <- dplyr::filter(temp_df, id2 == y_unique[j])
      idx_keep <- which.min(temp_j$d_xy)
      summary_list[[count]] <- temp_j[idx_keep,]
      count <- count + 1
      
    }
    
  }
  
  summary_df <- do.call(rbind.data.frame, summary_list)
  
  L_df <- dplyr::select(summary_df, -x, -y)
  st_geometry(L_df) <- L_df$L
  
  fname_gpkg <- paste0("fastest_velocity_", sim_id, ".gpkg")
  
  L_df <- L_df %>% mutate(sim = sim_id_unique[m])
  
  
  L_df_list[[m]] <- L_df
  
  time_check[m] <- proc.time() - ptm

}

time_check/60

# Row bind the speed data frame from each data type (control, experiment, operational)
L_df <- do.call(rbind.data.frame, L_df_list)

L_control <- L_df %>% dplyr::filter(sim == "control")
L_exp <- L_df %>% dplyr::filter(sim == "experiment")

## Could choose to look at all three
## Group the results by simulation and polygon to summarize
#L_groups <- L_df %>% group_by(sim, pgon) %>% summarize(max_v = max(v_elev), date=DateCurrent[1])

# Need to save the time stamp
L_check <- rbind(L_control, L_exp) %>% group_by(sim, pgon) %>% summarize(max_v = max(v_elev), date = DateCurrent[1])

# Select a color palette for the visualization
my_pal <- palette.colors(palette = "Okabe-Ito", n = 9)[c(6,7)]

# Prepare the date and max velocity columns for plotting
L_check$date <- as.POSIXct(L_check$date, tz = "UTC") 
L_check$max_v <- as.numeric(L_check$max_v)

# Create the numerator (experiment results)
r_num <- L_check %>% dplyr::filter(sim == "experiment") %>% 
  arrange(date) 

# Create the denominator (control results)
r_den <- L_check %>% dplyr::filter(sim == "control") %>% 
  arrange(date) 

# Compare the speed from the experiment to that of the control
r <- (r_num$max_v)/(r_den$max_v)

# Include the ratio calculated above into this data frame
r_num <- r_num %>% mutate(r)

# Comparison of fastest speed from experiment vs. control
p_speed <- ggplot() + geom_point(L_check, mapping = aes(x = date, y = max_v, 
                                                        color = sim)) +
  xlab("Date") + ylab("Speed (km per hr)") +
  scale_color_discrete(name = "Simulation\nType", type = my_pal) +
  xlim(c(as.POSIXct("2020-10-22 11:00:00", tz = "UTC"), 
         as.POSIXct("2020-10-23 23:00:00", tz = "UTC"))) +
  ylim(c(0, 4.5)) +
  theme_bw(base_size = 14)


p_fraction <- ggplot() + geom_point(r_num, mapping = aes(x = date, y = r)) + 
  geom_hline(yintercept = 1, color = 'brown4') +
  xlab("Date") + ylab("Experiment:Control") +
  xlim(c(as.POSIXct("2020-10-22 11:00:00", tz = "UTC"), as.POSIXct("2020-10-23 23:00:00", tz = "UTC"))) +
  theme_bw(base_size = 14)

# Create a panel including the estimated fire speed and fraction for the
# experiment vs. control results (this plot shows the entire timeline)
panel <- plot_grid(p_speed, p_fraction, align = "hv", ncol = 1)
panel

# Focus on the next 24 hours
p_speed <- ggplot() + geom_point(L_check, mapping = aes(x = date, y = max_v, 
                                                        color = sim)) +
  xlab("Date") + ylab("Speed (km per hr)") +
  scale_color_discrete(name = "Simulation\nType", type = my_pal) +
  ylim(c(0, 3)) +
  xlim(c(as.POSIXct("2020-10-22 11:00:00", tz = "UTC"), as.POSIXct("2020-10-23 11:00:00", tz = "UTC"))) +
  theme_bw(base_size = 12)


p_fraction <- ggplot() + geom_point(r_num, mapping = aes(x = date, y = r)) + 
  geom_hline(yintercept = 1, color = 'black', linetype = "dashed") +
  xlab("Date") + ylab("Experiment:Control") +
  xlim(c(as.POSIXct("2020-10-22 11:00:00", tz = "UTC"), as.POSIXct("2020-10-23 11:00:00", tz = "UTC"))) +
  theme_bw(base_size = 12)
  

panel24 <- plot_grid(p_speed, p_fraction, align = "hv", ncol = 1,
                   labels = c("a", "b"), label_fontface = "plain" )
panel24

ggsave("figures/figure_07_control_experiment_speed_24hrs.png", plot = panel24, height = 6, width = 6, dpi = 600)




