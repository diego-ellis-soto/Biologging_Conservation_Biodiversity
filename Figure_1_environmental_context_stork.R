# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
#
#
# MPAB PNAS 2024
# Diego Ellis-Soto⟐e, Andrea Flack⟐d,f,g,  Ariana Strandburg-Peshkin⟐g,h,i, Timm A. Wild⟐d,i, Hannah J Williams⟐d,g,i, M. Teague O’Maraa-d*, 
#
# Plot White stork remote sensing variables
#
#
# Load White stork data from Public Movebank data repository + plot all Movebank white storks data
#
# Contact information: diego.ellissoto@yale.edu
#
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# 
require(sp)
require(tidyverse)
require(move2)
require(move)
require(sf)
require(lubridate)
require(mapview)
require(Orcs)
cm.cols1=function(x,bias=1) { colorRampPalette(c('grey90','steelblue4','steelblue1','gold','red1','red4'),bias=bias)(x)}
# --- --- --- --- --- --- --- ---
# Load Wilma White Stork data
# --- --- --- --- --- --- --- ---

wilma_garbage = read.csv('/gpfs/gibbs/pi/jetz/from_loomis/de293/Movebank_Digital_Data_Repistory/Indir/Stork_data/Wilma_GPS_foragingLandfill.csv')
wilma_breeding = read.csv('/gpfs/gibbs/pi/jetz/from_loomis/de293/Movebank_Digital_Data_Repistory/Indir/Stork_data/Wilma_GPS_breeding.csv')
wilma_migration = read.csv('/gpfs/gibbs/pi/jetz/from_loomis/de293/Movebank_Digital_Data_Repistory/Indir/Stork_data/Wilma_GPS_migration.csv')

wilma = read.csv('/gpfs/gibbs/pi/jetz/from_loomis/de293/Movebank_Digital_Data_Repistory/Indir/Stork_data/Wilma_GPS_completeTrack.csv') |>
  mutate(yday = yday(timestamp),
         year = year(timestamp),
         id_year_yday = paste0(
           tag_id, year, yday
         ))

one_p_pday = wilma |> 
  group_by(id_year_yday) |> 
  filter(latitude >= -90 & latitude <= 90 & longitude >= -180 & longitude <= 180) |>
  slice(1)

wilma_sp <- SpatialPointsDataFrame(
  wilma,
  coords = wilma[,c('longitude', 'latitude')], proj4string =CRS("+proj=longlat +datum=WGS84"))

one_p_pday_sp <- SpatialPointsDataFrame(
  one_p_pday,
  coords = one_p_pday[,c('longitude', 'latitude')], proj4string =CRS("+proj=longlat +datum=WGS84"))

wilma_breeding_sp <- SpatialPointsDataFrame(
  wilma_breeding,
  coords = wilma_breeding[,c('longitude', 'latitude')], proj4string =CRS("+proj=longlat +datum=WGS84"))

wilma_migration_sp <- SpatialPointsDataFrame(
  wilma_migration,
  coords = wilma_migration[,c('longitude', 'latitude')], proj4string =CRS("+proj=longlat +datum=WGS84"))

wilma_garbage_sp <- SpatialPointsDataFrame(
  wilma_garbage,
  coords = wilma_garbage[,c('longitude', 'latitude')], proj4string =CRS("+proj=longlat +datum=WGS84"))

one_p_pday_sf =sf:: st_as_sf(one_p_pday_sp)

crs_climb_rate = "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs" # Is the projection of the climb rate
wilma_migration_sp_reproj = spTransform(wilma_migration_sp, crs(crs_climb_rate))

wilma_breeding$year = year(wilma_breeding$timestamp)
wilma_breeding$yday = yday(wilma_breeding$timestamp)
wilma_breeding$hour = hour(wilma_breeding$timestamp)

wilma_breeding$year_yday_hour = paste(wilma_breeding$year, '-', wilma_breeding$yday, wilma_breeding$hour)

one_p_pday_breeding = wilma_breeding |> 
  group_by(year_yday_hour) |> 
  filter(latitude >= -90 & latitude <= 90 & longitude >= -180 & longitude <= 180) |>
  slice(1)

one_p_pday_breeding_sp <- SpatialPointsDataFrame(
  one_p_pday_breeding,
  coords = one_p_pday_breeding[,c('longitude', 'latitude')], proj4string =CRS("+proj=longlat +datum=WGS84"))

# stork_1 <- coords2Lines(coordinates(
#   wilma_breeding_sp[
#     wilma_breeding_sp$tag_id == unique(wilma_breeding_sp$tag_id)[1],]), 
#   ID = "individual_id")
# 
# crs(stork_1) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

# --- --- --- --- --- --- --- ---
# Load Remote sensing variables
# --- --- --- --- --- --- --- ---
# setwd('/gpfs/gibbs/pi/jetz/from_loomis/de293/Movebank_Digital_Data_Repistory/Indir/Remote_sensing/')
# H_mod = raster::raster('lulc-human-modification-terrestrial-systems-geographic-geotiff/lulc-human-modification-terrestrial-systems_geographic.tif')
# H_mod_wilma = crop(H_mod, wilma_sp)
# H_mod_wilma_breeding = crop(H_mod, wilma_breeding_sp)

# wind_suit = raster('Prediction Maps/Uplift Suitability Map/avgPrediction2014_10folds_continuous.tif')
# climb_rate = raster('Prediction Maps/Uplift Intensity Map/climbPrediction2014_valuesWithinStorkRange_onlyPresence_climbingRate_landscape.tif')

# wind_suit_reproj =  projectRaster(wind_suit, crs = crs(H_mod_wilma))
# climb_rate_reproj = projectRaster(climb_rate, crs = crs(H_mod_wilma))

# # wilma_migration_sp_reproj = spTransform(wilma_migration_sp, crs(climb_rate))
# 
# climb_rate_migration_wilma = crop(climb_rate, (extent(wilma_migration_sp_reproj) * 1.1 ) )
# climb_rate_migration_wilma_resampled <- aggregate(climb_rate_migration_wilma, fact = 2, fun = mean)
# wind_suit_wilma = crop(wind_suit, wilma_migration_sp_reproj)
# wind_suit_wilma_resampled <- aggregate(wind_suit_wilma, fact = 2, fun = mean)
# wind_suit_migration_wilma = crop(wind_suit, wilma_migration_sp_reproj)
# wind_suit_resampled = aggregate(wind_suit_migration_wilma, fact = 2, fun = mean)
# Store and load for Hannah: 

# writeRaster(wind_suit_wilma_resampled, file = '/gpfs/gibbs/pi/jetz/from_loomis/de293/Movebank_Digital_Data_Repistory/Outdir/For_Hannah/wind_suit_wilma_resampled.tif')
# writeRaster(climb_rate_migration_wilma_resampled, file = '/gpfs/gibbs/pi/jetz/from_loomis/de293/Movebank_Digital_Data_Repistory/Outdir/For_Hannah/climb_rate_migration_wilma_resampled.tif')
# writeRaster(H_mod_wilma_breeding, file = '/gpfs/gibbs/pi/jetz/from_loomis/de293/Movebank_Digital_Data_Repistory/Outdir/For_Hannah/H_mod_wilma_breeding.tif')

# --- --- --- --- --- --- --- ---
# These remote sensing layers have been cropped and stored above:
# --- --- --- --- --- --- --- ---

# Load Layers:
wind_suit_wilma_resampled= raster('/gpfs/gibbs/pi/jetz/from_loomis/de293/Movebank_Digital_Data_Repistory/Outdir/For_Hannah/wind_suit_wilma_resampled.tif')
climb_rate_migration_wilma_resampled = raster('/gpfs/gibbs/pi/jetz/from_loomis/de293/Movebank_Digital_Data_Repistory/Outdir/For_Hannah/climb_rate_migration_wilma_resampled.tif')
H_mod_wilma_breeding = raster('/gpfs/gibbs/pi/jetz/from_loomis/de293/Movebank_Digital_Data_Repistory/Outdir/For_Hannah/H_mod_wilma_breeding.tif')
H_mod_wilma = raster('/gpfs/gibbs/pi/jetz/from_loomis/de293/Movebank_Digital_Data_Repistory/Outdir/For_Hannah/H_mod_wilma.tif')

# Crop climb rate to migration extent:

# climb_rate_migration_wilma_df <- as.data.frame(climb_rate_migration_wilma, xy = TRUE)
climb_rate_migration_wilma_df <- as.data.frame(climb_rate_migration_wilma_resampled, xy = TRUE)
# climb_rate_migration = crop(climb_rate, wilma_migration_sp_reproj)
wilma_migration_sp_reproj_df = as.data.frame(wilma_migration_sp_reproj)
H_mod_wilma_breeding_df = as.data.frame(H_mod_wilma_breeding, xy = TRUE)

# Load Robinson world map projection
load('/gpfs/gibbs/pi/jetz/from_loomis/de293/Movebank_Digital_Data_Repistory/Outdir/For_Hannah/world_robinson.Rdata')

# --- --- --- ---# --- --- --- ---
# Wilma migrating and with wind uplift strength ####
# --- --- --- ---# --- --- --- ---

ggplot() +
  geom_raster(data = climb_rate_migration_wilma_df, aes(x = x, y = y, 
                                                        fill = 
                                                          climb_rate_migration_wilma_resampled)) +
  scale_fill_viridis_c(option = "plasma", na.value = "white") +
  coord_fixed() +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )+
  geom_point(data = wilma_migration_sp_reproj_df,
             aes(x = longitude.1, y = latitude.1, group = tag_id),
             color = "red", size = 0.4) +
  xlim(3900000, 4100000)+ theme(legend.position="none")


# --- --- --- ---# --- --- --- ---
# # Wilma Breeding: Human footprint ##### Very Ugly
# --- --- --- ---# --- --- --- ---

ggplot() +
  geom_raster(data = H_mod_wilma_breeding_df, aes(x = x, y = y, 
                                                  fill = 
                                                    H_mod_wilma_breeding)) +
  scale_fill_viridis_c(option = "plasma", na.value = "white") +
  coord_fixed() +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )+
  geom_point(data = wilma_breeding,
             aes(x = longitude, y = latitude, group = tag_id),
             color = "white", size = 0.4) +
  theme(legend.position="none")+ggtitle('Wilma: breeding across the human modification gradient')

# --- ---
# Plot only global human footprint:
# library(rworldmap)
# data('world')
# crs = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
# world <- fortify(spTransform(getMap(), CRS(paste0(crs))))
# save(world, file = '/gpfs/gibbs/pi/jetz/from_loomis/de293/Movebank_Digital_Data_Repistory/Outdir/For_Hannah/world_robinson.Rdata')


# H_mod_wilma_df <- as.data.frame(H_mod_wilma, xy = TRUE)
# H_mod_wilma_df$hmod = H_mod_wilma_df$lulc.human.modification.terrestrial.systems_geographic
# 
# # wilma_sp_reproject = spTransform(wilma_sp, crs(H_mod_wilma)) # Is wgs 84
# wilma_sp_reproject = spTransform(wilma_sp, '+proj=longlat +datum=WGS84 +no_defs')
# wilma_sp_reproject_df = data.frame(wilma_sp_reproject)
# 
# one_p_pday_reproject = wilma_sp_reproject_df |> 
#   group_by(id_year_yday) |> 
#   filter(latitude >= -90 & latitude <= 90 & longitude >= -180 & longitude <= 180) |>
#   slice(1)


# --- --- --- --- --- --- --- --- --- ---
# Plot Wilma Migration journey: Human Footprint ####
# --- --- --- --- --- --- --- --- --- ---


wilma_migration_sp_reproj_df = data.frame(wilma_migration_sp_reproj)
wilma_migration_sp_df = data.frame(wilma_migration_sp)

# H_mod_wilma = crop(H_mod, ( extent(wilma_migration_sp) * 1.1 ) ) 
# writeRaster(H_mod_wilma, file ='/gpfs/gibbs/pi/jetz/from_loomis/de293/Movebank_Digital_Data_Repistory/Outdir/For_Hannah/H_mod_wilma.tif')
H_mod_wilma_df <- as.data.frame(H_mod_wilma, xy = TRUE)

bounding_box_wilma_migration = bbox(extent(wilma_migration_sp) * 1.1)


migratiion_footprint = ggplot() +
  geom_raster(data = H_mod_wilma_df, aes(x = x, y = y, 
                                         fill = H_mod_wilma)) +
  scale_fill_viridis_c(option = "plasma", na.value = "white", name = "Human Modification") +
  coord_fixed() +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  # geom_line(data = one_p_pday_reproject,
  #           aes(x = longitude, y = latitude, group = tag_id),
  #           color = "white", size = 1) +
  geom_point(data = wilma_migration_sp_df,
             aes(x = longitude, y = latitude, group = tag_id),
             color = "white", size = 0.4) +
  ggtitle('Migratory journey of Wilma') +
  geom_point(data = wilma_migration_sp_df[1, ],
             aes(x = longitude, y = latitude, group = tag_id),
             color = "blue", size = 2, shape = 24, stroke = 2) +
  geom_point(data = wilma_migration_sp_df[nrow(wilma_migration_sp_df), ],
             aes(x = longitude, y = latitude, group = tag_id),
             color = "darkred", size = 2, shape = 4, stroke = 2) +
  theme(legend.position = "right")


ggsave(migratiion_footprint, file = '/gpfs/gibbs/pi/jetz/from_loomis/de293/Movebank_Digital_Data_Repistory/Outdir/Wilma_migration.pdf')

# climb_rate = raster('Prediction Maps/Uplift Intensity Map/climbPrediction2014_valuesWithinStorkRange_onlyPresence_climbingRate_landscape.tif')
# 
# # wind_suit_reproj =  projectRaster(wind_suit, crs = crs(H_mod_wilma))
# # climb_rate_reproj = projectRaster(climb_rate, crs = crs(H_mod_wilma))
# wilma_migration_sp_reproj = spTransform(wilma_migration_sp, crs(climb_rate))


# --- --- --- --- --- --- --- --- --- ---
# Plot Wilma Migration journey: Climb Rate ####
# --- --- --- --- --- --- --- --- --- ---


climb_wilma_migr = crop(climb_rate_migration_wilma_resampled,
                        ( extent(wilma_migration_sp_reproj) * 1.2 ) ) 

wilma_migration_sp_reproj_df = data.frame(wilma_migration_sp_reproj)
climb_wilma_migr_df <- as.data.frame(climb_wilma_migr, xy = TRUE)

ggplot() +
  geom_raster(data = climb_wilma_migr_df,
              aes(x = x, y = y, 
                  fill = 
                    climb_rate_migration_wilma_resampled)) +
  scale_fill_viridis_c(option = "plasma", na.value = "white", name = "Climbing rate ms-1") +
  coord_fixed() +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  geom_point(data = wilma_migration_sp_reproj_df,
             aes(x = longitude.1, y = latitude.1, group = tag_id),
             color = "red", size = 0.4) +
  ggtitle('Migratory journey of Wilma') +
  geom_point(data = wilma_migration_sp_reproj_df[1, ],
             aes(x = longitude.1, y = latitude.1, group = tag_id),
             color = "blue", size = 2, shape = 24, stroke = 2) +
  geom_point(data = wilma_migration_sp_reproj_df[nrow(wilma_migration_sp_reproj_df), ],
             aes(x = longitude.1, y = latitude.1, group = tag_id),
             color = "darkred", size = 2, shape = 4, stroke = 2) +
  theme(legend.position = "right")


# --- --- --- --- --- --- --- --- --- ---
# Wilma Garbage Dump Foraging ####
# --- --- --- --- --- --- --- --- --- ---

wilma_garbage_sp_sf = st_as_sf(wilma_garbage_sp)
ls1 = st_sfc(st_linestring(wilma_garbage_sp@coords), crs = 4326)


# Garbage close up #### # Take a screenshot of this output: you can zoom out a bit

require(leaflet)

map <- mapview(wilma_garbage_sp, lwd = 0.5, cex = 5, col.region = 'grey49', legend = FALSE) +
  mapview(ls1, col.region = 'grey49', legend = FALSE) +
  mapview(wilma_garbage_sp_sf[1,], shape = 24, stroke = 2, cex = 5, col.region = 'grey49', legend = FALSE) +
  mapview(wilma_garbage_sp_sf[nrow(wilma_garbage_sp_sf),], cex = 5, shape = 4, stroke = 2, col.region = 'grey49', legend = FALSE)

# Add Esri World Imagery as the basemap
map@map <- map@map %>% addProviderTiles(providers$Esri.WorldImagery)
map

# Here I zoomed out a bit and sreenshotted the gardage dump in a different tile provider:
map_garbage = mapview(wilma_garbage_sp_sf[1,], shape = 24, stroke = 2, col.region='blue')  
map_garbage@map <- map_garbage@map %>% addProviderTiles(providers$OpenStreetMap)
map_garbage

# --- --- --- --- --- --- --- --- ---
# White Stork Species range ####
# --- --- --- --- --- --- --- --- ---

# Upload Shapefile of White Stork into robinson projection:
crs = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

stork = st_read('/gpfs/gibbs/pi/jetz/from_loomis/de293/Movebank_Digital_Data_Repistory/Indir/Stork_data/Ciconia_ciconia.shp') 
stork_reproj = st_transform(stork, crs(crs))

stork_df <- fortify(stork)
stork_reproj_df = fortify(stork_reproj)

wilma_migration_sp_reproj_crs = st_transform(st_as_sf(wilma_migration_sp_reproj), crs(crs) )

one_p_pday_sf_reproj = st_transform(st_as_sf(one_p_pday_sf), crs(crs) )
one_p_pday_sf_reproj_df = fortify(one_p_pday_sf_reproj)
# world <- fortify(spTransform(getMap(), CRS(paste0(crs))))

bbox <- st_bbox(stork_reproj_df)

season_colors <- c("1" = "#4682B4",  # Light Blue
                   "2" = "gold2",  # Gold
                   "3" = "#FF7F50")  # Coral

season_labels <- c("1" = "Resident", 
                   "2" = "Breeding", 
                   "3" = "Non-breeding")


# --- --- --- --- ---
# Only species range
# --- --- --- --- ---

white_stork = ggplot() + geom_polygon(
  data = world,
  aes(x = long, y = lat, group = group),
  fill = "grey",
  color = "black",  # Set the outline color to black
  alpha = 0.3
) +
  geom_sf(data = stork_reproj_df, aes(geometry = geometry, fill = as.factor(season)), color = NA, alpha = 0.5) +
  # scale_fill_manual(values = c("1" = "blue", "2" = "green", "3" = "red"), name = "Season") +
  # scale_fill_manual(values = season_colors, name = "Season") +
  scale_fill_manual(values = season_colors, name = "Season", labels = season_labels) +
  theme_minimal() +
  labs(title = "White Stork",
       subtitle = "Ciconia ciconia",
       caption = "Source: Birdlife")+guides(fill = guide_legend(title = NULL))+
  coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]), ylim = c(bbox["ymin"], bbox["ymax"]))+
  theme(axis.title = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks = element_blank())

white_stork

# --- --- --- --- ---
# Now with the migration track on:Europe only
# --- --- --- --- ---


ggplot() + geom_polygon(
  data = world,
  aes(x = long, y = lat, group = group),
  fill = "grey",
  color = "black",  # Set the outline color to black
  alpha = 0.3
) +
  geom_sf(data = stork_reproj_df, aes(geometry = geometry, fill = as.factor(season)), color = NA, alpha = 0.5) +
  geom_sf(data = wilma_migration_sp_reproj_crs, aes(geometry = geometry), color = "red", size = 0.5) +
  scale_fill_manual(values = season_colors, name = "Season", labels = season_labels) +
  theme_minimal() +
  labs(title = "White Stork",
       subtitle = "Ciconia ciconia",
       caption = "Source: Birdlife")+guides(fill = guide_legend(title = NULL))+
  coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]), ylim = c(bbox["ymin"], bbox["ymax"]))+
  theme(axis.title = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks = element_blank())

# --- --- --- --- --- --- --- ---
# Add the entire migration track beyond Europe multi year trck
# --- --- --- --- --- --- --- ---

ggplot() + geom_polygon(
  data = world,
  aes(x = long, y = lat, group = group),
  fill = "grey",
  color = "black",  # Set the outline color to black
  alpha = 0.3
) +
  geom_sf(data = stork_reproj_df, aes(geometry = geometry, fill = as.factor(season)), color = NA, alpha = 0.5) +
  geom_sf(data = one_p_pday_sf_reproj_df, aes(geometry = geometry), color = "red", size = 0.5) +
  scale_fill_manual(values = season_colors, name = "Season", labels = season_labels) +
  theme_minimal() +
  labs(title = "White Stork",
       subtitle = "Ciconia ciconia",
       caption = "Source: Birdlife")+guides(fill = guide_legend(title = NULL))+
  coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]), ylim = c(bbox["ymin"], bbox["ymax"]))+
  theme(axis.title = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks = element_blank())

# ggsave(white_stork, file = '/gpfs/gibbs/pi/jetz/from_loomis/de293/Movebank_Digital_Data_Repistory/Outdir/continental_stork.pdf')


bbox <- st_bbox(wilma_migration_sp_reproj_crs)

stork_reproj$season_f = as.factor(stork_reproj$season)

# --- --- --- --- --- --- --- --- ---
# Alternatively can plot with mapview
# --- --- --- --- --- --- --- --- ---

mapview(wilma_migration_sp_reproj_crs, cex = 2, col.region='red') + mapview(stork_reproj, zcol = 'season_f')

mapview(one_p_pday_sf_reproj, cex = 3, col.region='white') + mapview(stork_reproj, zcol = 'season_f')


stork_mapview_image = mapview(wilma_migration_sp_reproj_crs, lwd = 0.5, cex = 5, col.region='red', legend = FALSE) + mapview(stork_reproj, zcol = 'season_f', legend = FALSE)
# Save 

mapshot(stork_mapview_image, 
        file = '/gpfs/gibbs/pi/jetz/from_loomis/de293/Movebank_Digital_Data_Repistory/Outdir/Stork_range_track.png',
        remove_controls = NULL )

mapview(wilma_garbage_sp, lwd = 0.5, cex = 5, col.region = 'grey49', legend = FALSE)


bbox_stork <- extent(stork_reproj)

# Plot using mapview with bounding box
mapview(wilma_migration_sp_reproj_crs, lwd = 0.5, cex = 5, col.regions = 'red', legend = FALSE, bbox = bbox_stork) +
  mapview(stork_reproj, zcol = 'season_f', legend = FALSE)

