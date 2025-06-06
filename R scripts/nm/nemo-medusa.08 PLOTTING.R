
## Visualise summarised nemo-medusa model output

#### Set up ####

rm(list=ls(all.names = TRUE))                                               # Wipe the brain

library(MiMeMo.tools)
library(furrr)
plan(multisession)                                                          # Instructions for parallel processing

TS <- readRDS("./Objects/TS.rds")                                           # Read in time series
vars_ts <- c("Salinity_avg", "Temperature_avg", "DIN_avg", 
             "Detritus_avg", "Phytoplankton_avg")                           # Variables to plot   

SP <- readRDS("./Objects/SPATIAL.rds")                                      # Read in spatial data
vars_sp <- str_remove(vars_ts, "_avg") %>%                                  # Tweak the variable names for spatial plots
  c("Speed")
  
#### Plotting ####
    
walk(vars_ts, ts_plot)                                                      # Save a time series figure for each variable.

future_map2(rep(SP, each = length(vars_sp)),                                # For each decade
            rep(vars_sp, times = length(SP)), point_plot,                   # And each variable
            .progress = TRUE)                                               # Plot spatial maps in parallel

# tic ("Plotting current figures")
# furrr::future_map(SP, stick_plot, zoom = zoom, pre = pre, .progress = TRUE)       # Plot currents in parallel
# toc()
