
# Extract monthly significant wave height

#### Setup ####

rm(list=ls())

Packages <- c("MiMeMo.tools", "exactextractr", "raster")                    # List packages
lapply(Packages, library, character.only = TRUE)                            # Load packages
source("./R scripts/@_Region file.R")

domains <- readRDS("./Objects/Domains.rds") %>%                             # Import inshore polygon
  filter(Shore == "Inshore") %>% 
  st_transform(crs = 4326)

waves <- brick("../Shared data/Waves/waves.nc", varname = "swh") %>%        # Import significant wave height time series
  rotate()

#### Calculate mean disturbance per habitat area, weighting by cell coverage ####

values <- exact_extract(waves, domains, fun = 'mean') %>%                   # Extract inshore waves    
  pivot_longer(everything(), names_to = "Date", values_to = "Waves") %>%    # Move time steps into a single column
  mutate(Date = str_remove(Date, "mean.X")) %>%                             # Fix date string
  mutate(Date = as.POSIXct(Date, format = "%Y.%m.%d"))                      # Format as date

saveRDS(values, "./Objects/Significant wave height.rds")

ggplot(values) +
  geom_line(aes(x = Date, y = Waves)) +
  theme_minimal() +
#  scale_x_continuous(breaks = c(0, 3, 6, 9, 12)) +
  labs(y = "Mean significant wave height (m)", x = "Month", caption = "Data from ERA5 reanalysis") +
NULL

ggsave("./Figures/flows/Significant wave height.png", width = 16, height = 8, units = "cm")
