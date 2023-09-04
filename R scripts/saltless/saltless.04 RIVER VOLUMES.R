
#### Set up ####

rm(list=ls())                                                               # Wipe the brain

packages <- c("tidyverse", "sf", "furrr", "raster", "data.table")           # List packages
lapply(packages, library, character.only = TRUE)                            # Load packages
source("./R scripts/@_Region file.R")                                       # Define project region 
