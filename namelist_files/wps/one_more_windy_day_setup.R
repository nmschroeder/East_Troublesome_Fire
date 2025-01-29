# Let's write a script to go back 24 hours, starting at 2020-10-22 1100 UTC

library(dplyr)

# Simulation timeline
d_utc <- vector(length = 89, mode = "character")

day <- 21
hour <- 11

for (i in 1:length(d_utc)){
  if (hour < 10){
    hh <- paste0("0", as.character(hour))
  } else{
    hh <- as.character(hour)
  }
  
  d_utc[i] <- paste0("2020-10-", as.character(day), "_", hh, ":00:00")
  hour <- hour + 1
  if (hour == 24){
    hour <- 0
    day <- day + 1
  }
}

d_utc_control <- d_utc[25:89]
d_utc_control

d_utc_experiment <- d_utc[1:65]
d_utc_experiment

fnames_control_domain1 <- paste0("met_em.d01.", d_utc_control, ".nc")
fnames_control_domain2 <- paste0("met_em.d02.", d_utc_control, ".nc")

fnames_experiment_domain1 <- paste0("met_em.d01.", d_utc_experiment, ".nc")
fnames_experiment_domain2 <- paste0("met_em.d02.", d_utc_experiment, ".nc")

fnames_control <- c(fnames_control_domain1, fnames_control_domain2)
fnames_experiment <- c(fnames_experiment_domain1, fnames_experiment_domain2)

fnames_df <- data.frame(orig_fnames = fnames_experiment, new_fnames = fnames_control, var_names = c(d_utc_control, d_utc_control))
str(fnames_df)

flag <- (fnames_df$orig_fnames != fnames_df$new_fnames)*1.0
flag

fnames_csv <- mutate(fnames_df, flag)
write.csv(fnames_csv, "met_em_experiment.csv", row.names = FALSE)
