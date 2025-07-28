
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# Add Movebank Deployment locations next:
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---


newcrs = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

# Load remote sensing variables:
H_mod = terra::rast('lulc-human-modification-terrestrial-systems-geographic-geotiff/lulc-human-modification-terrestrial-systems_geographic.tif')
H_mod_reproj = terra::project(H_mod,newcrs)

biodiversity_only = terra::rast('minshort_speciestargetswithPA_esh10km_repruns10_ranked.tif') # Mollweide
biodiversity_only_reproj = terra::project(biodiversity_only,newcrs)

Forest_fragmentation = terra::rast('FFI2020.tif') # Vandg projecton
Forest_fragmentation_reproj = terra::project(Forest_fragmentation,newcrs)

Travel_Time_city = terra::rast('travel_time_to_cities_6.tif') # WGS84
Travel_Time_city_reproj = terra::project(Travel_Time_city,newcrs)

# --- --- --- --- --- --- --- --- --- ---
# Reproject to 50:
# --- --- --- --- --- --- --- --- --- ---

H_mod_50 <- terra::aggregate(H_mod, fact=50) # Resample to 50km

H_mod_50_reproj = terra::project(H_mod_50,
               newcrs)

biodiversity_only_50 <- terra::aggregate(biodiversity_only, fact=5) # Resample to 50km

biodiversity_only_50_reproj = terra::project(biodiversity_only_50,
                                 newcrs)

Forest_fragmentation_50 <- terra::aggregate(Forest_fragmentation, fact=10)  # Resample to 50km

Forest_fragmentation_50_reproj = terra::project(Forest_fragmentation_50,
                                             newcrs)

Travel_Time_city_50 <- terra::aggregate(Travel_Time_city, fact=50) # Resample to 50km

Travel_Time_city_50_reproj = terra::project(Travel_Time_city_50,
                                                newcrs)



loginStored<-movebankLogin(username="", password="")

all_studies <- getMovebank(entity_type = "study", login=loginStored) %>% drop_na(main_location_long,
                                                                                 main_location_lat) |>
  dplyr::filter(is_test == 'false' & study_type=='research') |>
  dplyr::filter(!grepl('tmp', name)) |>
  dplyr::filter(!grepl('temporary', name)) |>
  dplyr::filter(!grepl('Test', name)) |>
  dplyr::filter(!grepl('test', name))

all_studies_sp <- SpatialPointsDataFrame(
  all_studies,coords = all_studies[,c('main_location_long', 
                                      'main_location_lat')],
  proj4string =CRS("+proj=longlat +datum=WGS84"))

all_studies_sp <- spTransform(all_studies_sp, newcrs)

all_studies_sp_vect = vect(all_studies_sp)

# Annotate:

all_studies_sp$H_mod <- terra::extract(
  H_mod_reproj, all_studies_sp_vect)[,2]

all_studies_sp$H_mod_50 <- terra::extract(
  H_mod_50_reproj, all_studies_sp_vect)[,2]

all_studies_sp$biodiversity_only <- terra::extract(
  biodiversity_only_reproj, all_studies_sp_vect)[,2]

all_studies_sp$biodiversity_only_50 <- terra::extract(
  biodiversity_only_50_reproj, all_studies_sp_vect)[,2]

all_studies_sp$biodiversity_only <- terra::extract(
  biodiversity_only_reproj, all_studies_sp_vect)[,2]

all_studies_sp$Forest_fragmentation <- terra::extract(
  Forest_fragmentation_reproj, all_studies_sp_vect)[,2]

all_studies_sp$Forest_fragmentation_50 <- terra::extract(
  Forest_fragmentation_50_reproj, all_studies_sp_vect)[,2]

all_studies_sp$Travel_Time_city <- terra::extract(
  Travel_Time_city_reproj, all_studies_sp_vect)[,2]

all_studies_sp$Travel_Time_city_50 <- terra::extract(
  Travel_Time_city_50_reproj, all_studies_sp_vect)[,2]

all_studies_sf <- st_as_sf(all_studies_sp)
all_studies_sp_df = data.frame(all_studies_sp)


col_pal = c('#046C9A', 'azure3')

density_hist_HMOD =   ggplot() +
  geom_density(aes(H_mod_50),
               # fill = "H_mod_50km"),
               alpha = .2,
               data = all_studies_sp_df, linewidth = 0.8)  +
  # geom_density(aes(H_mod_50,fill = "H_mod_50km"),alpha = .2,data = H_mod_df, linewidth = 0.8)  +
  # ggtitle(paste0(' Human Modification density histogram ')) +
  scale_fill_manual(values = col_pal) + theme_classic() + ylab('Density') + xlab('Human Modification') +
  theme(axis.text.x = element_text(size = 16 ,color='black'),
        axis.title.x = element_text(size = 16 ,color='black'),
        axis.text.y = element_text(size = 16 ,color='black'),
        axis.title.y = element_text(size = 16 ,color='black'))+xlim(0, 1) +
  theme(legend.position="none") # Remove legend


all_studies_sp_df$Travel_Time_city_50_6_hours = (all_studies_sp_df$Travel_Time_city_50 / 60)

trav_time = ggplot() +
  geom_density(aes(Travel_Time_city_50_6_hours),
               alpha = .2,
               data = all_studies_sp_df, linewidth = 0.8)  +
  # ggtitle(paste0(' Travel time density histogram ')) +
  scale_fill_manual(values = col_pal) + theme_classic() + ylab('Density') +
  xlab('Travel time in hours') +
  theme(axis.text.x = element_text(size = 16 ,color='black'),
        axis.title.x = element_text(size = 16 ,color='black'),
        axis.text.y = element_text(size = 16 ,color='black'),
        axis.title.y = element_text(size = 16 ,color='black'))+
  theme(legend.position="none")


forest_fragm = ggplot() +
  geom_density(aes(Forest_fragmentation_50),
               alpha = .2,
               data = all_studies_sp_df, linewidth = 0.8)  +
  #  ggtitle(paste0(' Forest fragmentation density histogram ')) +
  scale_fill_manual(values = col_pal) + theme_classic() + ylab('Density') + 
  xlab('Forest fragmentation') +
  theme(axis.text.x = element_text(size = 16 ,color='black'),
        axis.title.x = element_text(size = 16 ,color='black'),
        axis.text.y = element_text(size = 16 ,color='black'),
        axis.title.y = element_text(size = 16 ,color='black'))+
  theme(legend.position="none")+xlim(0,1)


Biodiversity_only = ggplot() +
  geom_density(aes(biodiversity_only_50),
               alpha = .2,
               data = all_studies_sp_df, linewidth = 0.8)  +
  # ggtitle(paste0(' Biodiversity Only density histogram ')) +
  scale_fill_manual(values = col_pal) + theme_classic() + ylab('Density') + 
  xlab('Biodiversity Only') +
  theme(axis.text.x = element_text(size = 16 ,color='black'),
        axis.title.x = element_text(size = 16 ,color='black'),
        axis.text.y = element_text(size = 16 ,color='black'),
        axis.title.y = element_text(size = 16 ,color='black'))+
  theme(legend.position="none")


require(gridExtra)
all_histos = grid.arrange(density_hist_HMOD, 
                          forest_fragm, 
                          trav_time, 
                          Biodiversity_only,
                          ncol = 1)

ggsave(all_histos,
       file = '/Users/diegoellis/Desktop/ALL_DEPLOYMENT_VARS.pdf')

