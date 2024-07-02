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
crs = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

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

load(url("https://github.com/valentinitnelav/RandomScripts/blob/master/NaturalEarth.RData?raw=true"))
# Get the world polygon
# world <- map_data("world")
require(viridis)


# --- --- --- --- --- --- --- --- --- ---
# ####
# --- --- --- --- --- --- --- --- --- ---

# --- --- --- --- --- --- --- --- --- ---
# Movement data: ####
# --- --- --- --- --- --- --- --- --- ---

outdir = '/gpfs/gibbs/pi/jetz/from_loomis/de293/Movebank_Digital_Data_Repistory/Indir/Movement_data_joint/'

public_movebank_data = fread(paste0(outdir, 'Public_repo_movebank_data.csv'))
# Subset by Ciconia Ciconia

public_movebank_data$year = year(public_movebank_data$timestamp)
public_movebank_data$yday = yday(public_movebank_data$timestamp)
public_movebank_data$indiv_animal_study_name = paste0(public_movebank_data$individual.local.identifier, '_', public_movebank_data$study.name)
public_movebank_data$week = week(public_movebank_data$timestamp)
public_movebank_data$ind_year_yday = paste0(public_movebank_data$individual.local.identifier,'_' ,
                                            public_movebank_data$year, '_',
                                            public_movebank_data$yday)

public_movebank_data$indiv_animal_study_name_year_yday = paste0(
  public_movebank_data$indiv_animal_study_name,'_' ,
  public_movebank_data$year, '_',
  public_movebank_data$yday
)

n_sample_per_week = public_movebank_data %>%
  group_by(indiv_animal_study_name, year, week) %>%
  count() %>%
  ungroup()

summary <- public_movebank_data %>%
  #   filter(individual_id %in% i_2020$individual_id) %>%
  dplyr::group_by(individual.taxon.canonical.name) %>%
  dplyr::summarize(n_individuals = n_distinct(indiv_animal_study_name),
                   n_locations = n(),
                   n_studies = n_distinct(study.name)) %>%
  dplyr::mutate(mean_locations = n_locations/n_individuals) %>%
  # left_join(.,species_list, by = c("taxon_canonical_name" ="scientific_name")) %>%
  dplyr::select(individual.taxon.canonical.name, n_individuals, n_locations,
                n_studies, mean_locations) %>%
  dplyr::arrange(individual.taxon.canonical.name, n_locations)

result <- summary %>%
  group_by(individual.taxon.canonical.name) %>%
  summarize(
    n_individuals = sum(n_individuals),
    n_locations = sum(n_locations),
    n_studies = sum(n_studies),
    mean_locations = mean(mean_locations)
  )

result %>%
  knitr::kable()


# Split to one point per day:
one_p_pday = public_movebank_data |> 
  group_by(indiv_animal_study_name_year_yday) |> 
  filter(location.lat >= -90 & location.lat <= 90 & location.long >= -180 & location.long <= 180) |>
  slice(1)

# ####

# --- --- --- --- --- --- --- --- --- ---
# Robinson reporject: ####
# --- --- --- --- --- --- --- --- --- ---

crs = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

# One point per day
one_p_pday_sp <- SpatialPointsDataFrame(
  one_p_pday,
  coords = one_p_pday[,c('location.long', 'location.lat')], proj4string =CRS("+proj=longlat +datum=WGS84"))

one_p_pday_sp_proj <- spTransform(one_p_pday_sp, CRSobj = paste0(crs))

# All data:
public_movebank_data_sub = public_movebank_data |> dplyr::select(location.long, location.lat, individual.taxon.canonical.name, V11) |>
  drop_na(location.long, location.lat)  |> 
  filter(location.lat >= -90 & location.lat <= 90 & location.long >= -180 & location.long <= 180)

public_movebank_data_sp = SpatialPointsDataFrame(
  public_movebank_data_sub,
  coords = public_movebank_data_sub[,c('location.long', 'location.lat')],
  proj4string =CRS("+proj=longlat +datum=WGS84"))

public_movebank_data_sp_proj <- spTransform(public_movebank_data_sp, CRSobj = paste0(crs))

public_movebank_data_sp_proj_df = as.data.frame(public_movebank_data_sp_proj) |> mutate(Longitude = location.long.1,
                                                                                        Latitude = location.lat.1,)


# ####

one_p_pday_sp_proj_df = as.data.frame(one_p_pday_sp_proj) |> mutate(Longitude = location.long.1,
                                                                    Latitude = location.lat.1,)

# Number of daily locations WGS 84 Movebank
daily_locs_digital_repo_wgs84 = ggplot() +
  geom_polygon(
    data = world,
    aes(x = long, y = lat, group = group),
    fill = "grey",
    color = "black",  # Set the outline color to black
    alpha = 0.3
  ) +
  # geom_point(data = one_p_pday, aes(x = location.long, y = location.lat)) +
  geom_bin2d(data = one_p_pday, aes(x = location.long, y = location.lat), bins = 100) +
  theme_void() +
  # ylim(-90, 90) +
  # xlim(-180, 180) +
  scale_fill_viridis(
    trans = "log",
    option = 'C',
    breaks = c(1, 100, 1000, 10000, 100000),
    name = "Number of daily animal locations"
  )


ggsave(
  daily_locs_digital_repo_wgs84,
  file = '/gpfs/gibbs/pi/jetz/from_loomis/de293/Movebank_Digital_Data_Repistory/Outdir/daily_locs_digital_repo_wgs84.pdf')

world <- fortify(spTransform(getMap(), CRS(paste0(crs))))
# All locations in Robinson Projection

movebank_global_public_map_daily_locs_rob = ggplot() + 
  geom_map(data = world, map = world,
           aes(x = long, y = lat, map_id = id),
           color = "gray50", fill = "gray50", size = 0.25) +
  geom_polygon(data = NE_box_proj,
               aes(x = long, y = lat), colour = "black", fill = "transparent", size = 0.15)+
  # geom_point(data = one_p_pday_sp_proj_df,
  #            aes(x = Longitude, y = Latitude, color = 'black'),
  #            alpha = 0.5, size = 1) +
  geom_bin2d(data = one_p_pday_sp_proj_df, aes(x = Longitude, y = Latitude), bins = 100) +
  geom_path(data = NE_graticules_proj,
            aes(x = long, y = lat, group = group),
            linetype = "dotted", color = "grey50", size = 0.25) +
  coord_equal() +
  theme_map() +
  theme(plot.title = element_text(hjust = 0.5, size = 20, face = 'bold'))+
  ggtitle('Global Distribution of \n Movebank Digital Data \n Repository locations')+
  # ylim(-90, 90) +
  # xlim(-180, 180) +
  scale_fill_viridis(
    trans = "log", 
    option = 'C',
    breaks = c(1, 100, 1000, 10000, 100000),
    name = "N. daily \n animal locations"
  )
ggsave(
  movebank_global_public_map_daily_locs_rob,
  file = '/gpfs/gibbs/pi/jetz/from_loomis/de293/Movebank_Digital_Data_Repistory/Outdir/daily_locs_digital_repo_robinson.pdf')


movebank_global_public_map_all_locs_rob = ggplot() + 
  geom_map(data = world, map = world,
           aes(x = long, y = lat, map_id = id),
           color = "gray50", fill = "gray50", size = 0.25) +
  geom_polygon(data = NE_box_proj,
               aes(x = long, y = lat), colour = "black", fill = "transparent", size = 0.15)+
  geom_path(data = NE_graticules_proj,
            aes(x = long, y = lat, group = group),
            linetype = "dotted", color = "grey50", size = 0.25) +
  # geom_point(data = one_p_pday_sp_proj_df,
  #            aes(x = Longitude, y = Latitude, color = 'black'),
  #            alpha = 0.5, size = 1) +
  geom_bin2d(data = public_movebank_data_sp_proj_df, aes(x = Longitude, y = Latitude), bins = 100) +
  coord_equal() +
  theme_map() +
  theme(plot.title = element_text(hjust = 0.5, size = 20, face = 'bold'))+
  ggtitle('Global Distribution of \n Movebank Digital Data \n Repository locations')+
  # ylim(-90, 90) +
  # xlim(-180, 180) +
  scale_fill_viridis(
    trans = "log", 
    option = 'C',
    breaks = c(1, 100, 1000, 10000, 100000),
    name = "N. \n animal locations"
  )


ggsave(movebank_global_public_map_all_locs_rob,
       file = '/gpfs/gibbs/pi/jetz/from_loomis/de293/Movebank_Digital_Data_Repistory/Outdir/movebank_global_public_map_all_locs_20240613.pdf')

# Make a density plot of the public movebank data repository: 
density_hist =   ggplot() +
  geom_density(aes(V11),alpha = .2, linewidth = 1.2,
               data = public_movebank_data)+
  theme_classic() + ylab('Frequency') + xlab('Global Human Modification')

ggsave(density_hist,
       file = '/gpfs/gibbs/pi/jetz/from_loomis/de293/Movebank_Digital_Data_Repistory/Outdir/movebank_global_public_human_modification_incomplete.pdf')



# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# Add Movebank Deployment locations next:
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---

loginStored<-movebankLogin(username="COVID-19_IBS", password="covid19ibs")

all_studies <- getMovebank(entity_type = "study", login=loginStored) %>% drop_na(main_location_long,
                                                                                 main_location_lat) |>
  dplyr::filter(is_test == 'false' & study_type=='research') |>
  dplyr::filter(!grepl('tmp', name))

all_studies_sp <- SpatialPointsDataFrame(
  all_studies,coords = all_studies[,c('main_location_long', 'main_location_lat')], proj4string =CRS("+proj=longlat +datum=WGS84"))

all_studies_sp <- spTransform(all_studies_sp, crs(H_mod_50))

all_studies_sp$H_mod <- raster::extract(H_mod$lulc.human.modification.terrestrial.systems_geographic, all_studies_sp)

all_studies_sp$H_mod_50km <- raster::extract(H_mod_50$HFI_50km_resampled, all_studies_sp)

all_studies_sf <- st_as_sf(all_studies_sp)
all_studies_sp_df = data.frame(all_studies_sp)

H_mod <- raster('/gpfs/gibbs/pi/jetz/from_loomis/de293/Movebank_Digital_Data_Repistory/Indir/Remote_sensing/lulc-human-modification-terrestrial-systems-geographic-geotiff/lulc-human-modification-terrestrial-systems_geographic.tif')
H_mod_50 <- aggregate(H_mod, fact=50)
# H_mod_50 = raster::raster(H_mod_50)
# writeRaster(H_mod_50, file = '/gpfs/gibbs/pi/jetz/from_loomis/de293/Movebank_Digital_Data_Repistory/Indir/Remote_sensing/HFI_50km_resampled.tif')
H_mod_50 = raster::raster('/gpfs/gibbs/pi/jetz/from_loomis/de293/Movebank_Digital_Data_Repistory/Indir/Remote_sensing/HFI_50km_resampled.tif')
crs = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"


H_mod_50_reproj = raster::projectRaster(H_mod_50$HFI_50km_resampled,
                                        crs =  crs)

H_mod_50_reproj_df <- as.data.frame(H_mod_50_reproj, xy = TRUE)

H_mod_50_reproj_df$H_mod_50 = H_mod_50_reproj_df$lulc.human.modification.terrestrial.systems_geographic



all_studies_sp_df = data.frame(all_studies_sp)
H_mod_df <- as.data.frame(H_mod_50, xy = TRUE) |> drop_na()

col_pal = c('#046C9A', 'azure3')

density_hist =   ggplot() +
  geom_density(aes(H_mod_50km),
               # fill = "H_mod_50km"),
               alpha = .2,
               data = all_studies_sp_df, linewidth = 0.8)  +
  # geom_density(aes(H_mod_50,fill = "H_mod_50km"),alpha = .2,data = H_mod_df, linewidth = 0.8)  +
  ggtitle(paste0(' Human Modification density histogram ')) +
  scale_fill_manual(values = col_pal) + theme_classic() + ylab('Density') + xlab('Human Modification') +
  theme(axis.text.x = element_text(face = "bold", size = 16 ,color='black'),
        axis.title.x = element_text(face = "bold", size = 16 ,color='black'),
        axis.text.y = element_text(face = "bold", size = 16 ,color='black'),
        axis.title.y = element_text(face = "bold", size = 16 ,color='black'))+xlim(0, 1) +
  theme(legend.position="none") # Remove legend

ggsave(density_hist, file = '/gpfs/gibbs/pi/jetz/from_loomis/de293/Movebank_Digital_Data_Repistory/Outdir/human_mod_deployment_locs_movebank.pdf')

# Need to do the movebank deployment locally:

# --- ---
# Plot the entire map of the human footprint: 

ggplot() +
  geom_map(data = world, map = world,
           aes(x = long, y = lat, map_id = id),
           color = "gray50", fill = "gray50", size = 0.25) +
  geom_polygon(data = NE_box_proj,
               aes(x = long, y = lat), colour = "black", fill = "transparent", size = 0.15)+
  geom_raster(data = H_mod_50_reproj_df, aes(x = x, y = y, fill = H_mod_50)) +
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
            linetype = "dotted", color = "grey50", size = 0.25)








# Manually download the Missing ones. 

# 










# Extract forest cover:

# Extract distance to nearest roads:

# 

# 


nrow(one_p_pday)

# public_movebank_data_sub = public_movebank_data |> slice(10000)

# 
# length(unique(public_movebank_data$tag.local.identifier))
# length(unique(public_movebank_data$study.name))
# public_movebank_data_sub |> group_by(ind_year_yday) |> slice(1)



# Compare this to the world:
# H_mod <- terra::rast('/gpfs/gibbs/pi/jetz/from_loomis/de293/Movebank_Digital_Data_Repistory/Indir/Remote_sensing/lulc-human-modification-terrestrial-systems-geographic-geotiff/lulc-human-modification-terrestrial-systems_geographic.tif')
H_mod <- raster('/gpfs/gibbs/pi/jetz/from_loomis/de293/Movebank_Digital_Data_Repistory/Indir/Remote_sensing/lulc-human-modification-terrestrial-systems-geographic-geotiff/lulc-human-modification-terrestrial-systems_geographic.tif')
H_mod_df <- as.data.frame(H_mod, xy = TRUE)

# Split by taxa? Look at Taxonomy:


# Thin to one point per day:

# Make a map of raster counts
ggplot(H_mod_df, aes(x = lulc.human.modification.terrestrial.systems_geographic)) +
  geom_density(alpha = 0.2) +
  theme_classic() +
  ylab('Frequency') +
  xlab('Global Human Modification')




# 
# 
# 
# # Store them:
# 
# # 
# 
# data(World, metro, rivers, land)
# # 
# # tmap_mode("plot")
# # ## tmap mode set to plotting
# # tm_shape(land) +
# #   tm_raster("trees", palette = terrain.colors(10))
# # 
# # tm_shape(land) +
# #   tm_raster("cover", palette = terrain.colors(10))
# # 
# # 
# # tm_shape(World) + tm_raster(H_mod)
# # tm_shape(World) +
# #   tm_polygons("HPI")
# 
# 
# H_mod_density = ggplot() +
#   geom_density(aes(lulc.human.modification.terrestrial.systems_geographic),alpha = .2,
#                data = H_mod)+
#   theme_classic() + ylab('Frequency') + xlab('Global Human Modification')
# 
# 
# # Thin to one point per day:
# 
# # Make a Mollweide map with one point per day
# 
# 
# # Histogram 
# 
# # One point per day
# 
# # Global map Mollweide
# 
# 
# # Plot for Number of Individuals
# # ggplot(result, aes(x = n_individuals, y = individual.taxon.canonical.name, fill = n_individuals)) +
# #   geom_bar(stat = "identity") +
# #   scale_fill_viridis_c() +
# #   theme_minimal() +
# #   ggtitle('Number of Individuals per Species') +
# #   xlab('Number of Individuals') +
# #   ylab('Species') +
# #   geom_text(aes(label = n_individuals), hjust = -0.2, color = "black", size = 3)
# 
# # Plot for Number of Locations
# # ggplot(result, aes(x = n_locations, y = individual.taxon.canonical.name, fill = n_locations)) +
# #   geom_bar(stat = "identity") +
# #   scale_fill_viridis_c() +
# #   theme_minimal() +
# #   ggtitle('Number of Locations per Species') +
# #   xlab('Number of Locations') +
# #   ylab('Species') +
# #   geom_text(aes(label = n_locations), hjust = -0.2, color = "black", size = 3)
# 
# 
# 
# movebank_global_public_map_all_locs_rob = ggplot() + 
#   geom_map(data = world, map = world,
#            aes(x = long, y = lat, map_id = id),
#            color = "gray50", fill = "gray50", size = 0.25) +
#   geom_path(data = NE_graticules_proj,
#             aes(x = long, y = lat, group = group),
#             linetype = "dotted", color = "grey50", size = 0.25) +
#   geom_polygon(data = NE_box_proj,
#                aes(x = long, y = lat), colour = "black", fill = "transparent", size = 0.15)+
#   # geom_point(data = one_p_pday, aes(x = location.long, y = location.lat)) +
#   geom_bin2d(data = public_movebank_data_sp_proj_df, aes(x = Longitude, y = Latitude), bins = 100) +
#   theme_void() +
#   # ylim(-90, 90) +
#   # xlim(-180, 180) +
#   scale_fill_viridis(
#     trans = "log", 
#     option = 'C',
#     breaks = c(1, 100, 1000, 10000, 100000),
#     name = "Number of animal locations"
#   )
# 
# 
# ggplot() + 
#   geom_map(data = world, map = world,
#            aes(x = long, y = lat, map_id = id),
#            color = "gray50", fill = "gray50", size = 0.25) +
#   geom_polygon(data = NE_box_proj,
#                aes(x = long, y = lat), colour = "black", fill = "transparent", size = 0.15)+
#   # geom_point(data = one_p_pday_sp_proj_df,
#   #            aes(x = Longitude, y = Latitude, color = 'black'),
#   #            alpha = 0.5, size = 1) +
#   geom_bin2d(data = one_p_pday_sp_proj_df, aes(x = Longitude, y = Latitude), bins = 100) +
#   geom_path(data = NE_graticules_proj,
#             aes(x = long, y = lat, group = group),
#             linetype = "dotted", color = "grey50", size = 0.25) +
#   coord_equal() +
#   theme_map() +
#   theme(plot.title = element_text(hjust = 0.5, size = 20, face = 'bold'))+
#   ggtitle('Global Distribution of \n Movebank Digital Data \n Repository locations')+
#   # ylim(-90, 90) +
#   # xlim(-180, 180) +
#   scale_fill_viridis(
#     trans = "log", 
#     option = 'C',
#     breaks = c(1, 100, 1000, 10000, 100000),
#     name = "N. daily \n animal locations"
#   )
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# ggplot() +
#   geom_map(data = world, map = world,
#            aes(x = long, y = lat, map_id = id),
#            color = "gray50", fill = "gray50", size = 0.25) +
#   geom_polygon(data = NE_box_proj,
#                aes(x = long, y = lat), colour = "black", fill = "transparent", size = 0.15) +
#   geom_point(data = one_p_pday_sp_proj_df,
#              aes(x = Longitude, y = Latitude, color = 'black'),
#              alpha = 0.5, size = 1) +
#   # scale_color_viridis_c(option = "plasma") +  # Apply the plasma color scale
#   geom_path(data = NE_graticules_proj,
#             aes(x = long, y = lat, group = group),
#             linetype = "dotted", color = "grey50", size = 0.25) +
#   coord_equal() +
#   theme_map() +
#   theme(plot.title = element_text(hjust = 0.5, size = 20, face = 'bold'))+
#   ggtitle('Global Distribution of \n Movebank Deployment locations')
# 
