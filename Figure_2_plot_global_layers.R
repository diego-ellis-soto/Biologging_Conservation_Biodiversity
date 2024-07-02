# --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
#
#
# Plot all global layers:
#
#
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---

# --- --- --- --- --- --- --- --- --- ---
# Load R package: ####
# --- --- --- --- --- --- --- --- --- ---



require(data.table)
library(tmap)
data("World")
require(tidyverse)
require(raster)
require(terra)
require(mapview)
require(rasterVis)
require(viridis)
data('world')
require(maptools)  ## For wrld_simpl
require(raster)
library(countrycode)
require(maps)
library(sp)
require(spData)
require(move)
library(rgdal)
library(mapproj)
library(spData)
library(rworldmap)
library(geosphere)
require(ggthemes)
require(viridis)
# library(spDataLarge)
crs = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
load(url("https://github.com/valentinitnelav/RandomScripts/blob/master/NaturalEarth.RData?raw=true"))
world <- fortify(spTransform(getMap(), CRS(paste0(crs))))
# NE_graticules_wt <- spTransform(NE_graticules, CRSobj = wt)
NE_box_proj        <- spTransform(NE_box, CRSobj = paste0(crs))
NE_graticules_proj <- spTransform(NE_graticules, CRSobj = paste0(crs))
prj.coord <- project(cbind(lbl.Y$lon, lbl.Y$lat), proj=paste0(crs))
lbl.Y.prj <- cbind(prj.coord, lbl.Y)
names(lbl.Y.prj)[1:2] <- c("X.prj","Y.prj")
prj.coord <- project(cbind(lbl.X$lon, lbl.X$lat), proj=paste0(crs))
lbl.X.prj <- cbind(prj.coord, lbl.X)
names(lbl.X.prj)[1:2] <- c("X.prj","Y.prj")



# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# Plot remote sensing variables
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---

setwd('/gpfs/gibbs/pi/jetz/from_loomis/de293/Movebank_Digital_Data_Repistory/')


PROJ <- "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs" 

H_mod = terra::rast('Indir/Remote_sensing/lulc-human-modification-terrestrial-systems-geographic-geotiff/lulc-human-modification-terrestrial-systems_geographic.tif')

H_mod_50 <- terra::aggregate(H_mod, fact=50) # Resample to 50km

biodiversity_carbon_water = terra::rast('Indir/Remote_sensing/BiodiversityCarbonWater/10km/minshort_speciestargetswithPA_carbon__water__esh10km_repruns10_ranked.tif') # Mollweide
biodiversity_only = terra::rast('Indir/Remote_sensing/BiodiversityOnly/10km/minshort_speciestargetswithPA_esh10km_repruns10_ranked.tif') # Mollweide
Forest_fragmentation = terra::rast('Indir/Remote_sensing/Forest_fragmentation/FFI2020.tif') # Vandg projecton

Travel_Time_city = terra::rast('Indir/Remote_sensing/travel_time_to_cities_6.tif') # WGS84

newcrs = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

world <- fortify(spTransform(getMap(), CRS(paste0(newcrs))))

# Reproject and aggregate to 50km
biodiversity_carbon_water_50 <- terra::aggregate(biodiversity_carbon_water, fact=50)
biodiversity_only_50 <- terra::aggregate(biodiversity_only, fact=50)
Forest_fragmentation_50 <- terra::aggregate(Forest_fragmentation, fact=50)

Travel_Time_city_50 <- terra::aggregate(Travel_Time_city, fact=50)

biodiversity_carbon_water_50_reproj = terra::project(biodiversity_carbon_water_50,
                                                     newcrs)

biodiversity_only_50_reproj = terra::project(biodiversity_only_50,
                                             newcrs)

Forest_fragmentation_50_reproj  = terra::project(Forest_fragmentation_50,
                                                 newcrs)

Travel_Time_city_50_reproj  = terra::project(Travel_Time_city_50,
                                             newcrs)
H_mod_50_reproj  = terra::project(H_mod_50,
                                  newcrs) 




loginStored<-movebankLogin(username="COVID-19_IBS", password="covid19ibs")

all_studies <- getMovebank(entity_type = "study", login=loginStored) %>% drop_na(main_location_long,
                                                                                 main_location_lat) |>
  dplyr::filter(is_test == 'false' & study_type=='research') |>
  dplyr::filter(!grepl('tmp', name))

all_studies_sp <- SpatialPointsDataFrame(
  all_studies,coords = all_studies[,c('main_location_long', 'main_location_lat')], proj4string =CRS("+proj=longlat +datum=WGS84"))

all_studies_sp_robin <- spTransform(all_studies_sp, crs(newcrs))
world <- fortify(spTransform(getMap(), CRS(paste0(newcrs))))
all_studies_sp_robin_df = data.frame(all_studies_sp_robin)

movebank_points_all = ggplot() +
  geom_map(data = world, map = world,
           aes(x = long, y = lat, map_id = id),
           color = "gray50", fill = "gray50", size = 0.25) +
  geom_polygon(data = NE_box_proj,
               aes(x = long, y = lat), colour = "black", fill = "transparent", size = 0.15)+
  # geom_raster(data = biodiversity_carbon_water_reproj_df, aes(x = x, y = y, fill = minshort_speciestargetswithPA_carbon__water__esh10km_repruns10_ranked)) +
  scale_fill_viridis_c(option = "plasma", na.value = "white") +
  coord_fixed() +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )+
  geom_path(data = NE_graticules_proj,
            aes(x = long, y = lat, group = group),
            linetype = "dotted", color = "grey50", size = 0.25)+
  xlab(NULL) +
  ylab(NULL)+ theme(legend.position="none")+theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )+ geom_point(data = all_studies_sp_robin_df,
                aes(x = main_location_long.1, y = main_location_lat.1),
                color = "black", size = 0.4, pch = 21) 

ggsave(movebank_points_all,  file = '/gpfs/gibbs/pi/jetz/from_loomis/de293/Movebank_Digital_Data_Repistory/Outdir/global_movebank_deployment_map.pdf')



# +geom_point(aes(fill=id), 
#             colour="black",pch=21, size=5))


# --- --- --- ---
# Not at 50km:
# --- --- --- ---
H_mod_reproj  = terra::project(H_mod,
                               newcrs) 


Forest_fragmentation_reproj  = terra::project(Forest_fragmentation,
                                              newcrs)

biodiversity_carbon_water_reproj = terra::project(biodiversity_carbon_water,
                                                  newcrs)

biodiversity_only_reproj = terra::project(biodiversity_only,
                                          newcrs)

Travel_Time_city_reproj  = terra::project(Travel_Time_city,
                                          newcrs)



# H_mod_50_reproj = raster::projectRaster(H_mod_50$HFI_50km_resampled,
#                                         crs =  crs)

H_mod_50_reproj_df <- as.data.frame(H_mod_50_reproj, xy = TRUE)
biodiversity_carbon_water_50_reproj_df <- as.data.frame(biodiversity_carbon_water_50_reproj, xy = TRUE)
biodiversity_only_50_df <- as.data.frame(biodiversity_only_50_reproj, xy = TRUE)
Forest_fragmentation_50_df <- as.data.frame(Forest_fragmentation_50_reproj, xy = TRUE)
Travel_Time_city_50_df <- as.data.frame(Travel_Time_city_50_reproj, xy = TRUE)

# Not at 50km:
Forest_fragmentation_reproj_df = as.data.frame(Forest_fragmentation_reproj, xy = TRUE)
biodiversity_carbon_water_reproj_df = as.data.frame(biodiversity_carbon_water_reproj, xy = TRUE)
biodiversity_only_reproj_df = as.data.frame(biodiversity_only_reproj, xy = TRUE)
Forest_fragmentation_reproj_df = as.data.frame(Forest_fragmentation_reproj, xy = TRUE)
Travel_Time_city_reproj_df = as.data.frame(Travel_Time_city_reproj, xy = TRUE)
H_mod_reproj_df <- as.data.frame(H_mod_reproj, xy = TRUE)

# --- ---
# Plot the entire map of the human footprint: 

human_footprint_global = ggplot() +
  geom_map(data = world, map = world,
           aes(x = long, y = lat, map_id = id),
           color = "gray50", fill = "gray50", size = 0.25) +
  geom_polygon(data = NE_box_proj,
               aes(x = long, y = lat), colour = "black", fill = "transparent", size = 0.15)+
  geom_raster(data = H_mod_50_reproj_df, aes(x = x, y = y, fill = `lulc-human-modification-terrestrial-systems_geographic`)) +
  scale_fill_viridis_c(option = "plasma", na.value = "white") +
  coord_fixed() +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )+
  geom_path(data = NE_graticules_proj,
            aes(x = long, y = lat, group = group),
            linetype = "dotted", color = "grey50", size = 0.25)+
  xlab(NULL) +
  ylab(NULL)+
  theme(legend.position="none")+
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )


ggsave(human_footprint_global,  file = '/gpfs/gibbs/pi/jetz/from_loomis/de293/Movebank_Digital_Data_Repistory/Outdir/global_hfi_map.pdf')

# --- ---
# Plot the entire map of forest fragmentation

global_forest_fragm = ggplot() +
  geom_map(data = world, map = world,
           aes(x = long, y = lat, map_id = id),
           color = "gray50", fill = "gray50", size = 0.25) +
  geom_polygon(data = NE_box_proj,
               aes(x = long, y = lat), colour = "black", fill = "transparent", size = 0.15)+
  geom_raster(data = Forest_fragmentation_reproj_df, aes(x = x, y = y, fill = FFI2020)) +
  scale_fill_viridis_c(option = "plasma", na.value = "white") +
  coord_fixed() +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )+
  geom_path(data = NE_graticules_proj,
            aes(x = long, y = lat, group = group),
            linetype = "dotted", color = "grey50", size = 0.25)+
  xlab(NULL) +
  ylab(NULL)+
  theme(legend.position="none")+
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )


ggsave(global_forest_fragm,  file = '/gpfs/gibbs/pi/jetz/from_loomis/de293/Movebank_Digital_Data_Repistory/Outdir/global_forest_fragm_map.pdf')


# --- ---
# Plot the entire map of the biodiversity carbon water priotization

global_biodiv_carbon_water = ggplot() +
  geom_map(data = world, map = world,
           aes(x = long, y = lat, map_id = id),
           color = "gray50", fill = "gray50", size = 0.25) +
  geom_polygon(data = NE_box_proj,
               aes(x = long, y = lat), colour = "black", fill = "transparent", size = 0.15)+
  geom_raster(data = biodiversity_carbon_water_reproj_df, aes(x = x, y = y, fill = minshort_speciestargetswithPA_carbon__water__esh10km_repruns10_ranked)) +
  scale_fill_viridis_c(option = "plasma", na.value = "white") +
  coord_fixed() +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )+
  geom_path(data = NE_graticules_proj,
            aes(x = long, y = lat, group = group),
            linetype = "dotted", color = "grey50", size = 0.25)+
  xlab(NULL) +
  ylab(NULL)+ theme(legend.position="none")+theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )

ggsave(global_biodiv_carbon_water,  file = '/gpfs/gibbs/pi/jetz/from_loomis/de293/Movebank_Digital_Data_Repistory/Outdir/global_biodiv_carbon_water_map.pdf')


global_biodiv_only = ggplot() +
  geom_map(data = world, map = world,
           aes(x = long, y = lat, map_id = id),
           color = "gray50", fill = "gray50", size = 0.25) +
  geom_polygon(data = NE_box_proj,
               aes(x = long, y = lat), colour = "black", fill = "transparent", size = 0.15)+
  geom_raster(data = biodiversity_only_reproj_df, aes(x = x, y = y, fill = minshort_speciestargetswithPA_esh10km_repruns10_ranked)) +
  scale_fill_viridis_c(option = "plasma", na.value = "white") +
  coord_fixed() +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )+
  geom_path(data = NE_graticules_proj,
            aes(x = long, y = lat, group = group),
            linetype = "dotted", color = "grey50", size = 0.25)+
  xlab(NULL) +
  ylab(NULL)+ 
  theme(legend.position="none")+
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )

ggsave(global_biodiv_only,  file = '/gpfs/gibbs/pi/jetz/from_loomis/de293/Movebank_Digital_Data_Repistory/Outdir/global_biodiv_only_map.pdf')

# Remove the ocean: 
Travel_Time_city_50_df[Travel_Time_city_50_df$travel_time_to_cities_6 >50000,]$travel_time_to_cities_6 = NA

global_travel_time = ggplot() +
  geom_map(data = world, map = world,
           aes(x = long, y = lat, map_id = id),
           color = "gray50", fill = "gray50", size = 0.25) +
  geom_polygon(data = NE_box_proj,
               aes(x = long, y = lat), colour = "black", fill = "transparent", size = 0.15)+
  geom_raster(data = Travel_Time_city_50_df, aes(x = x, y = y, fill = log(travel_time_to_cities_6))) +
  scale_fill_viridis_c(option = "plasma", na.value = "white") +
  coord_fixed() +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )+
  geom_path(data = NE_graticules_proj,
            aes(x = long, y = lat, group = group),
            linetype = "dotted", color = "grey50", size = 0.25)+
  xlab(NULL) +
  ylab(NULL)+ 
  # theme(legend.position="none")+
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )


ggsave(global_travel_time,  file = '/gpfs/gibbs/pi/jetz/from_loomis/de293/Movebank_Digital_Data_Repistory/Outdir/global_travel_time_map.pdf')

require(gridExtra)
all_maps = grid.arrange(human_footprint_global, 
                        global_forest_fragm, 
                        global_travel_time, 
                        global_biodiv_only,
                        ncol = 1)

ggsave(all_maps, file = '/gpfs/gibbs/pi/jetz/from_loomis/de293/Movebank_Digital_Data_Repistory/Outdir/ALL_GLOBAL_MAP_VARS.pdf')

# Add in the same order as the deployment:



all_histos = grid.arrange(density_hist_HMOD, 
                          forest_fragm, 
                          trav_time, 
                          Biodiversity_only,
                          ncol = 1)



both = grid.arrange(all_histos, all_maps, ncol = 2)

ggsave(both, file = '/gpfs/gibbs/pi/jetz/from_loomis/de293/Movebank_Digital_Data_Repistory/Outdir/Fig_1_PNAS_20240626_v2.pdf')

