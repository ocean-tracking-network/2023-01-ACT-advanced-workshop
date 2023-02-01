library(glatos)
library(sf)
library(gganimate)
library(tidyverse)
library(pathroutr)
library(ggspatial)
library(sp)
library(raster)

setwd("YOUR/PATH/TO/data/act")

detection_events <- #create detections event variable
  read_otn_detections('proj58_matched_detections_2016.csv') %>% # reading detections
  false_detections(tf = 3600) %>%  #find false detections
  filter(passed_filter != FALSE) %>% 
  detection_events(location_col = 'station', time_sep=3600)

plot_data <- detection_events %>% 
  dplyr::select(animal_id, mean_longitude,mean_latitude, first_detection)

one_fish <- plot_data[plot_data$animal_id == "PROJ58-1218518-2015-09-16",]

one_fish <- one_fish %>% filter(mean_latitude < 38.90 & mean_latitude > 38.87) %>% 
  slice(155:160)

USA<-getData('GADM', country='USA', level=1)

shape_file <- USA[USA$NAME_1 == 'Maryland',]

md_polygon <- st_as_sf(shape_file)  %>% st_transform(5070)

path <- one_fish %>%  dplyr::select(mean_longitude,mean_latitude)

path <- SpatialPoints(path, proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs"))

path <-  st_as_sf(path)  %>% st_transform(5070)

ggplot() +
  # Maryland polygon
  geom_sf(data = md_polygon, fill = "cornsilk3", size = 0) +
  # path points
  geom_sf(data = path) +
  # convert path points to polgyon
  geom_sf(data = st_cast(st_union(path), 'POLYGON'), fill = NA) +
  # zoom in
  coord_sf(x = c(1660338, 1661668), y = c(1932102, 1932700)) +
  theme_void()

plot_path <- path %>% st_cast('MULTIPOINT') %>% summarise(do_union = FALSE) %>% st_cast('LINESTRING')

track_pts <- st_sample(plot_path, size = 10000, type = "regular")

vis_graph <- prt_visgraph(md_polygon, buffer = 150)

track_pts_fix <- prt_reroute(track_pts, md_polygon, vis_graph, blend = TRUE)

track_pts_fix <- prt_update_points(track_pts_fix, track_pts)

pathroutrplot <- ggplot()+
  geom_sf(data = md_polygon, fill = "cornsilk3", size = 0) +
  geom_sf(data = track_pts_fix) +
  coord_sf(x = c(1660338, 1661668), y = c(1932102, 1933000)) +
  theme_void()

pathroutrplot

pathroutrplot.animation <-
  pathroutrplot +
  transition_reveal(fid) +
  shadow_mark(past = T, future = F)

gganimate::animate(pathroutrplot.animation, nframes=200)
