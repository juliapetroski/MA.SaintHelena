
## Set repeated commands specific to the project region
## This version is parameterised for the Barents sea

library(sf)

EPSG <- rgdal::make_EPSG()
EPSG2 <- filter(EPSG, str_detect(note, "Helen"))
crs <- 4710                                                              # Specify the map projection for the project

lims <- c(xmin = -231106.7, xmax = 864812.7, ymin = 3873354.4, ymax = 4570029.7)# Specify limits of plotting window, also used to clip data grids

zoom <- coord_sf(xlim = c(lims[["xmin"]], lims[["xmax"]]), ylim = c(lims[["ymin"]], lims[["ymax"]]), expand = FALSE) # Specify the plotting window for SF maps in this region

ggsave_map <- function(filename, plot) {
  ggsave(filename, plot, scale = 1, width = 12, height = 10, units = "cm", dpi = 500)
  
}                             # Set a new default for saving maps in the correct size
pre <- list(scale = 1, width = 12, height = 10, units = "cm", dpi = 500) # The same settings if you need to pass them to a function in MiMeMo.tools

SDepth <- 60                  # Shallow deep boundary
DDepth <- 600                  # Maximum depth

#### bathymetry.5 MODEL DOMAIN ####

shape <- function(matrix) {
  
shape <-  matrix %>% 
  list() %>% 
  st_polygon() %>% 
  st_sfc() %>% 
  st_sf(Region = "Saint Helena",.)
  st_crs(shape) <- st_crs(4326)                                        
  shape <- st_transform(shape, crs = crs)
  return(shape)
  
}                      # Convert a matrix of lat-lons to an sf polygon

Region_mask <- matrix(c(16.23, 70,
                        20.25, 68.5,
                        41, 66.8,
                        45.3, 65.5,
                        64, 68, 
                        57.492431, 70.736206,
                        52.984071, 71.835129,
                        54.408132, 73.261126,
                        67.9, 76.7,
                        71, 80,
                        68, 83.5,
                        0, 80,
                        0, 75,
                        16.23, 70),
                       ncol = 2, byrow = T) %>% 
  list() %>% 
  st_polygon() %>% 
  st_sfc() %>% 
  st_sf(Region = "Saint Helena",.)
st_crs(Region_mask) <- st_crs(4326)                                        
Region_mask <- st_transform(Region_mask, crs = crs)

#### expand polygon for sampling rivers ####

river_expansion <- matrix(c(13, 73,
                            0, 80,
                            0, 85,
                            63, 85,
                            73, 77,
                            30, 71,
                            13, 73),
                          ncol = 2, byrow = T) %>% 
  list() %>% 
  st_polygon() %>% 
  st_sfc() %>% 
  st_sf(Region = "Saint Helena",.)
st_crs(river_expansion) <- st_crs(4326)                                          
river_expansion <- st_transform(river_expansion, crs = 3035)


