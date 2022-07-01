library(ggplot2)
library(sf)
library(adehabitatHR)
library(cowplot)
library(ggspatial)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggspatial)
library(gganimate)
library(adehabitatHR)

#set world map data
mapWorld <- ne_countries(scale = "medium", returnclass = "sf")

#remove observations with period NA for plotting
plot_Lingcoddata <- periods_Lingcod %>% filter(!is.na(coarse.period))

#create map zoomed in around study site
mapPortO <- ggplot(data = mapWorld) + geom_sf() + 
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering()) +
  geom_point(data = periods_Lingcod, mapping = aes(x = Longitude, y = Latitude, color = survey.period)) +
  coord_sf(xlim = c(-124.485, -124.47), ylim = c(42.687, 42.715)) + 
  facet_wrap(vars(survey.period), nrow = 2)

mapPortO_coarse <- ggplot(data = mapWorld) + geom_sf() + 
  annotation_scale(location = "bl", width_hint = 0.5) +
  geom_point(data = plot_Lingcoddata, mapping = aes(x = Longitude, y = Latitude, color = coarse.period)) +
  coord_sf(xlim = c(-124.485, -124.47), ylim = c(42.687, 42.715)) + 
  facet_wrap(vars(coarse.period), nrow = 1, drop=TRUE)

#create animation over time of Dungeness crab detections (lat/long), color by period: render time ~
animate_Dungeness <- ggplot(data = mapWorld) + geom_sf() + 
  geom_point(data = periods_Dungeness, mapping = aes(x = Longitude, y = Latitude, color = survey.period), size = 3) +
  transition_time(Date.time.UTC) +
  labs(title = "Time: {frame_time}") +
  coord_sf(xlim = c(-124.485, -124.47), ylim = c(42.69, 42.698))


#plot locations of detections on map
ggplot(data = periods_BlackR, aes(x=Longitude, y=Latitude, color=survey.period)) + geom_point()
ggplot(data = periods_ChinaR, aes(x=Longitude, y=Latitude, color=survey.period)) + geom_point()
ggplot(data = periods_Dungeness, aes(x=Longitude, y=Latitude, color=survey.period)) + geom_point()
ggplot(data = periods_Lingcod, aes(x=Longitude, y=Latitude, color=survey.period)) + geom_point()
