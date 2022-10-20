# In order to compare a focal field offices data to other aim plots in the same
# ESDs it may become necessary to classify plots from adjoining Field Offices. 
library(here)
library(sf)
library(tidyverse)

# set_here('/media/sagesteppe/ExternalHD/UFO_ESD_manual_classification')
here()
p <- file.path(here(), 'data/raw')
vector_data <- list.files(p, recursive = T, pattern = 'shp$')

administrative_boundaries <- st_read(
  file.path(p, '/BLM_National_Administrative_Units/admu.gdb'), 
  layer = 'blm_natl_admu_field_poly_webpub', quiet = T) %>% 
  filter(ADMIN_ST %in% c('CO', 'NM', 'UT'))

focal_FO <- administrative_boundaries %>% 
  filter(ADMU_NAME == 'UNCOMPAHGRE FIELD OFFICE')

fo_buffer <- focal_FO %>%
  st_centroid() %>% # find the center of the FO, 
  st_buffer(160934) # buffer a reasonable distance? How much data is enough ...? ;-)

mlras <- st_read( # ensure your area of interest is within the relevant MLRAS
  file.path(p, vector_data[grep('*MLRA*', vector_data)]), quiet = T) %>% 
  filter(MLRARSYM %in% c('34B', '35', '36', '48A')) %>% 
  st_transform(st_crs(fo_buffer))

ggplot() +
  geom_sf(data = focal_FO) +
  geom_sf(data = fo_buffer, fill = NA, lwd = 2, color = 'black') +
  theme_bw()

rm(administrative_boundaries, vector_data)

p2terradat <- file.path('/media/sagesteppe/ExternalHD/aimDB', 'data/raw')

AIM_summaries <- read.csv(file.path(p2terradat, 'BLM_Natl_AIM_TerrADat_Hub.csv')) %>% 
  filter(State %in% c('CO', 'NM', 'UT')) %>% 
  dplyr::select(PrimaryKey:State, County:EcologicalSiteId, 
                Latitude_NAD83:Longitude_NAD83, GlobalID) %>%
  mutate_all(na_if,"") %>% 
  mutate(across(.cols = everything(), ~ str_trim(.x, side = "both"))) %>% 
  mutate(EcologicalSiteId = replace_na(EcologicalSiteId, 'UNKNOWN')) %>%
  st_as_sf(coords = c('Longitude_NAD83', "Latitude_NAD83"), crs = 4269) %>% 
  st_transform(st_crs(fo_buffer))  %>% 
  filter(EcologicalSiteId == 'UNKNOWN') 

 # reduce spatial extent to focal areas 
AIM_summaries <- AIM_summaries[st_intersection(AIM_summaries, fo_buffer),] 
AIM_summaries <- AIM_summaries[st_intersection(AIM_summaries, mlras),]

plots2consider <-AIM_summaries %>% 
  mutate(Distance = st_distance(AIM_summaries, st_centroid(fo_buffer)) ) %>% 
  arrange(Distance)

plots2consider <- plots2consider[unlist(st_disjoint(focal_FO, plots2consider)),] 

plots2verify <- plots2consider %>% 
  arrange(Distance) %>% 
  slice_head(n = 250) 

ggplot() + # ensure the process worked as so: 
  geom_sf(data = fo_buffer, fill = NA, lwd = 2, color = 'black') +
  geom_sf(data = focal_FO,  fill = 'black') +
  geom_sf(data = plots2consider, color = 'red') +
  geom_sf(data = plots2verify, color = 'cyan')
  
p <- file.path(here(), 'data/processed')

plots2verify$Distance <- as.numeric(plots2verify$Distance)
plots2verify <- plots2verify %>% 
  st_drop_geometry() %>% 
  dplyr::select(-ProjectName, -EcologicalSiteId, -GlobalID, -Distance) %>% 
  mutate(ID = 1:n())

write.csv(plots2verify, file.path(p, 'plotsToVerify.csv'), row.names = F)

rm(mlras, AIM_summaries, plots2consider, fo_buffer, focal_FO,
   p2terradat, p , plots2verify)
