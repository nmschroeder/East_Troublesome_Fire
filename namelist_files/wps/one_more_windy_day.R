# This file reads in the prescribed NETCDF4 files and changes their
# SIMULATION_START_DATE attribute to account for the one more windy day
# simulation. Then it saves a copy of the file with the simulation 
# experiment start date in the title.

library(ncdf4)
library(dplyr)

# Read in the meteorology CSV files
fnames_df <- read.csv("met_em_experiment.csv")

# Determine how many file names are in the the CSV file
N_files <- nrow(fnames_df)

for (i in 1:N_files){
  # We are going to read in the experimental met em file and then save it
  # with the control time attribute
    
    # Only go through the steps if the original file name is different from
    # the experimental file name (i.e., if we are rolling the time back by 24
    # hours)

    # Store the original file name
    fname <- paste0("orig_", fnames_df$orig_fnames[i])
      
    # Create a new name with the experiment time to which we want to assign
    # this time
    fcopy <- fnames_df$new_fnames[i]
      
    # Make a copy of the original file in the experiment directory
    file.copy(fname, fcopy)

    if (fnames_df$flag[i] == 1){
      
      # Read in the experiment file
      met_em <- nc_open(fcopy, write = TRUE)

      # Change the simulation start date variable to the experiment time
      ncatt_put(met_em, varid = 0, attname = "SIMULATION_START_DATE", fnames_df$var_names[i])
      ncvar_put(met_em, varid = "Times", vals = fnames_df$var_names[i])
  
      # Store the new time to the experiment file    
      nc_sync(met_em)
      
      # Close the nc file
      nc_close(met_em)
      
    }

}
