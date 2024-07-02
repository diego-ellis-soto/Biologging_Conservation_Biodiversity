# --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
#
# Make a histogram of the human footprint
# Make a map with number of ocurrences
# Make a map with one point per day as tracks on Mollweide 
# Make a track of global human footprint Mollweide
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---

# --- --- --- --- --- --- --- --- --- ---
# Load R package: ####
# --- --- --- --- --- --- --- --- --- ---
require(sf)
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

# library(spDataLarge)
crs = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

world <- fortify(spTransform(getMap(), CRS(paste0(crs))))
load(url("https://github.com/valentinitnelav/RandomScripts/blob/master/NaturalEarth.RData?raw=true"))


# NE_graticules_wt <- spTransform(NE_graticules, CRSobj = wt)
NE_box_proj        <- spTransform(NE_box, CRSobj = paste0(crs))
NE_graticules_proj <- spTransform(NE_graticules, CRSobj = paste0(crs))
prj.coord <- project(cbind(lbl.Y$lon, lbl.Y$lat), proj=paste0(crs))
lbl.Y.prj <- cbind(prj.coord, lbl.Y)
names(lbl.Y.prj)[1:2] <- c("X.prj","Y.prj")
prj.coord <- project(cbind(lbl.X$lon, lbl.X$lat), proj=paste0(crs))
lbl.X.prj <- cbind(prj.coord, lbl.X)
names(lbl.X.prj)[1:2] <- c("X.prj","Y.prj")

load(url("https://github.com/valentinitnelav/RandomScripts/blob/master/NaturalEarth.RData?raw=true"))
# Get the world polygon
# world <- map_data("world")
require(viridis)


loginStored<-movebankLogin(username="COVID-19_IBS", password="covid19ibs")

all_studies <- getMovebank(entity_type = "study", login=loginStored) %>% drop_na(main_location_long,
                                                                                 main_location_lat) |>
  dplyr::filter(is_test == 'false' & study_type=='research') |>
  dplyr::filter(!grepl('tmp', name))

all_studies_sp <- SpatialPointsDataFrame(
  all_studies,coords = all_studies[,c('main_location_long', 'main_location_lat')], proj4string =CRS("+proj=longlat +datum=WGS84"))
crs = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
all_studies_sp <- spTransform(all_studies_sp, crs(crs))

all_studies_sp_sf = st_as_sf(all_studies_sp)
all_studies_sp_df = data.frame(all_studies_sp)

world <- fortify(spTransform(getMap(), CRS(paste0(crs))))
crs = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
world <- fortify(spTransform(getMap(), CRS(paste0(crs))))

require(viridis)

deployment_global = ggplot() +
  geom_polygon(
    data = world,
    aes(x = long, y = lat, group = group),
    fill = "grey",
    color = "black",  # Set the outline color to black
    alpha = 0.3
  ) +
  geom_polygon(data = NE_box_proj,
               aes(x = long, y = lat), colour = "black", fill = "transparent", size = 0.12)+
  # geom_point(data = one_p_pday, aes(x = location.long, y = location.lat)) +
  geom_bin2d(data = all_studies_sp_df, aes(x = main_location_long.1, y = main_location_lat.1),
             bins = 100) +
  theme_void() +
  # ylim(-90, 90) +
  # xlim(-180, 180) +
  viridis::scale_fill_viridis(
    trans = "log",
    option = 'C',
    breaks = c(1, 10, 100, 1000),
    name = "Number of daily animal locations"
  )+
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
  theme(legend.position="none")+
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )+xlab(NULL) +
  ylab(NULL)

ggsave(deployment_global, file = '/gpfs/gibbs/pi/jetz/from_loomis/de293/Movebank_Digital_Data_Repistory/Outdir/All_deployments_map_20240627.pdf')


