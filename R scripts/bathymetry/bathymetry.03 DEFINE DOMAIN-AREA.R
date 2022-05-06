
# Create an object defining the geographic extent of the model domain
# This is the more accurate polygon which will be used to calculate the mean depth and volume of the inshore zone.
# There is also a larger block domain object built to extract data from coarse resolution products (Nemo-MEDUSA and ERA-5)

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

#### New EEZ ####

#EEZ <- st_buffer(land, units::set_units(200 * 1852, "m")) 

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

#### Polygons based on distance ####

Distance <- st_buffer(land, units::set_units(4500, "m")) %>% 
  st_difference(land) %>% 
  transmute(Shore = "Inshore") %>% 
  rename(geometry = ".")


ggplot() +
  geom_sf(data = Distance) + 
  theme_minimal() 

exactextractr::exact_extract(raster("../Shared data/GEBCO_2020.nc"), Distance, "mean")


ggplot() +
  geom_sf(data = EEZ, fill = "white", size = 0.1) +
  geom_sf(data = Bottom, fill = "lightblue", size =0.1) +
  geom_sf(data = Distance, fill = "red", size = 0.1) +
  theme_minimal()

ggsave("./Figures/bathymetry/EEZ.png", width = 18, height = 10, units = "cm", dpi = 700)

#### Format to domains object ####

Offshore <- st_difference(st_transform(EEZ, 4326), st_buffer(land, units::set_units(2600, "m"))) %>%
  transmute(Shore = "Offshore") 

Domains <- bind_rows(Offshore, Distance) %>% 
  transmute(Shore = c("Offshore", "Inshore"),
            area = as.numeric(st_area(.)),
            Elevation = exactextractr::exact_extract(raster("../Shared data/GEBCO_2020.nc"), ., "mean")) %>% 
  st_transform(crs = crs) 

saveRDS(Domains, "./Objects/Domains-accurate.rds")

map <- ggplot() + 
  geom_sf(data = Domains, aes(fill = Shore), colour = NA) +
#  geom_sf(data = Region_mask, colour = "red", fill = NA) + 
  geom_sf(data = world, size = 0.1, fill = "black") +
  scale_fill_manual(values = c(Inshore = "red", Offshore = "yellow3"), name = "Zone") +
#  zoom +
  coord_sf(xlim = st_bbox(st_transform(EEZ,crs))[c(1,3)], ylim = st_bbox(st_transform(EEZ,crs))[c(2,4)]) +
  theme_minimal() +
  #  theme(axis.text = element_blank()) +
  labs(caption = "Final model area") +
  NULL
ggsave_map("./Figures/bathymetry/Domains.png", map)
