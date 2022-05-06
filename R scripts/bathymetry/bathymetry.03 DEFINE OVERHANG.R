
# Create an object defining the geographic extent of the model domain

#### Set up ####

rm(list=ls())                                                   

Packages <- c("tidyverse", "sf", "stars", "rnaturalearth", "raster")                  # List handy packages
lapply(Packages, library, character.only = TRUE)                            # Load packages

source("./R scripts/@_Region file.R")                                       # Define project region 

world <- ne_countries(scale = "medium", returnclass = "sf") %>%             # Get a world map
  st_transform(crs = crs)                                                   # Assign polar projection

EEZ <- read_sf("./Data/eez")

GEBCO <- read_stars("../Shared data/GEBCO_2020.nc")
st_crs(GEBCO) <- st_crs(EEZ)
GFW <- raster("../Shared data/distance-from-shore.tif")

crop <- as(extent(-20, -12, -10, -2), "SpatialPolygons")
crs(crop) <- crs(GEBCO)

#GEBCO <- crop(GEBCO, crop)
GFW <- crop(GFW, crop)

mask <- readRDS("./Objects/Domains.rds") %>%  filter(Shore == "Offshore")

#### Create land ####

land <- matrix(c(-6, -17,
                 -5, -17,
                 -5, -15,
                 -6, -15,
                 -6, -17),
               ncol = 2, byrow = T) %>% 
  shape() %>% 
  st_transform(4326) %>% 
  st_difference(st_transform(EEZ, 4326))

#### Polygons based on depth ####

Depths <- GEBCO[EEZ] %>% 
  st_as_stars()

Depths[[1]][Depths[[1]] > units::set_units(0, "m") | Depths[[1]] < units::set_units(-600, "m")] <- NA

Depths[[1]][is.finite(Depths[[1]])] <- units::set_units(-600, "m")

Bottom <- st_as_stars(Depths) %>%
  st_as_sf(merge = TRUE) %>%
  st_make_valid() %>%
  group_by(GEBCO_2020.nc) %>%
  summarise(Depth = abs(mean(GEBCO_2020.nc))) %>%
  st_make_valid()

ggplot(Bottom) +
  geom_sf(aes(fill = Depth), alpha = 0.2) +
  theme_minimal()

#### Cut to domain ####

clipped <- st_difference(mask, st_transform(Bottom, crs = st_crs(mask)))

ggplot(clipped) +
  geom_sf(aes(fill = Depth), alpha = 0.5)

#### Format to domains object ####

overhang <- transmute(clipped, 
                      Shore = "Offshore",
                      area = as.numeric(st_area(clipped)),
                      Elevation = exactextractr::exact_extract(raster("../Shared data/GEBCO_2020.nc"), clipped, "mean")) %>% 
  st_transform(crs = crs)

saveRDS(overhang, "./Objects/Overhang.rds")

