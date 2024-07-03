library(dplyr)
library(scales)
library(ggplot2)
library(raster)
library(sf)
library(dplyr)
library(mapview)

# BBOX
bbox <- st_as_sfc(
  st_bbox(c(
    xmin = 4.1,
    xmax = 4.37,
    ymin = 49.95,
    ymax = 50.10),
    crs = st_crs(4326))
)

ndvi_wilma_breeding_df

ndvi_wilma_breeding_resampled <- aggregate(ndvi_wilma_breeding, fact = 2, fun = mean)

ndvi_raster_cropped <- crop(ndvi_wilma_breeding, extent(as(bbox, 'Spatial')))
ndvi_wilma_breeding_resampled_cropped <- crop(ndvi_wilma_breeding_resampled, extent(as(bbox, 'Spatial')))

# Convert wilma_breeding to an sf object if it is not already
wilma_breeding_sf <- st_as_sf(wilma_breeding, coords = c("longitude", "latitude"), crs = 4326)
# Crop the spatial points to the bounding box
wilma_breeding_cropped <- st_intersection(wilma_breeding_sf, bbox)

viridis_palette <- viridis(100, option = "D")


ls1_breeding = st_sfc(st_linestring(wilma_breeding_sp@coords), crs = 4326)

# Breeding close up #### # Take a screenshot of this output: you can zoom out a bit
require(leaflet)

library(dplyr)
library(lubridate)

# Assuming wilma_breeding_sp is already loaded and is a data frame

# Convert timestamp to POSIXct format if it's not already
wilma_breeding_sp$timestamp <- as.POSIXct(wilma_breeding_sp$timestamp, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

# Add columns for date and hour
wilma_breeding <- wilma_breeding %>%
  mutate(
    yday = yday(timestamp),
    hour = hour(timestamp),
    year = year(timestamp)
  )

# Resample data to one fix per hour
wilma_breeding_resampled <- wilma_breeding %>%
  group_by(year, yday, hour) %>%
  slice(1) %>% 
  ungroup() %>%
  select(-yday, -hour, -year)

wilma_breeding_resampled_sf <- st_as_sf(wilma_breeding_resampled, coords = c("longitude", "latitude"), crs = 4326)
ls1_breeding_wilma_breeding_resampled_sf = st_sfc(st_linestring(as(wilma_breeding_resampled_sf, 'Spatial')@coords), crs = 4326)

mapview(wilma_breeding_resampled_sf)

map <- mapview(wilma_breeding_resampled_sf, lwd = 0.5, cex = 3, col.region = 'white', legend = FALSE) +
  mapview(ls1_breeding_wilma_breeding_resampled_sf, legend = FALSE) +
  mapview(wilma_breeding_resampled_sf[1,], shape = 24, stroke = 2, cex = 5, col.region = 'blue', legend = FALSE) +
  mapview(wilma_breeding_resampled_sf[nrow(wilma_breeding_resampled_sf),], cex = 5, shape = 4, stroke = 2, col.region = 'darkred', legend = FALSE)+
  mapview(terra::rast(ndvi_wilma_breeding_resampled_cropped),
          col.regions = viridis_palette,  legend=FALSE)  

map@map <- map@map %>% addProviderTiles(providers$Esri.WorldImagery)
map


mapview(wilma_breeding_resampled_sf, lwd = 0.5, cex = 3, col.region = 'white', legend = FALSE, alpha = 0.5) +
  mapview(ls1_breeding_wilma_breeding_resampled_sf, legend = FALSE, alpha = 0.8, color='grey') +
  mapview(wilma_breeding_resampled_sf[1, ], shape = 24, stroke = 2, cex = 5, col.region = 'blue', legend = FALSE) +
  mapview(wilma_breeding_resampled_sf[nrow(wilma_breeding_resampled_sf), ], cex = 5, shape = 4, stroke = 2, col.region = 'darkred', legend = FALSE) +
  mapview(terra::rast(ndvi_wilma_breeding_resampled_cropped), col.regions = viridis_palette, legend = FALSE)


# # Plot using mapview
# mapview(ndvi_wilma_breeding_resampled_cropped)
# 
# mapview(raster:raster(ndvi_raster_cropped), maxpixels=558114) + mapview(wilma_breeding_cropped)
# mapview(ndvi_raster_cropped$NDVI)
# mapview(ndvi_wilma_breeding) + mapview(wilma_breeding_sp)
#   
# Create the NDVI plot in ggplot
ggplot() +
  geom_raster(data = ndvi_wilma_breeding_df, aes(x = x, y = y, fill = NDVI)) +
  # scale_fill_gradientn(
  # scale_fill_viridis_c(option = 6, na.value = "white") +
  # colors = ndvi_palette,
  # values = rescale(c(-1, 0, 1)),
  # na.value = "white",
  # name = "NDVI"
  scale_fill_viridis_c(na.value = "white") +
  coord_fixed() +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  geom_point(data = wilma_breeding,
             aes(x = longitude, y = latitude, group = tag_id),
             color = "grey49", size = 0.4,
             cex=3) +
  ggtitle('Migratory Journey of Wilma') +
  geom_point(data = wilma_breeding[1, ],
             aes(x = longitude, y = latitude, group = tag_id),
             color = "blue", size = 2, shape = 24, stroke = 2) +
  geom_point(data = wilma_breeding[nrow(wilma_breeding), ],
             aes(x = longitude, y = latitude, group = tag_id),
             color = "darkred", size = 2, shape = 4, stroke = 2) +
  theme(legend.position = "right")

# Print the plot
print(ndvi_breeding_wilma)
