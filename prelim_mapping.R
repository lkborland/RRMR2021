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

#create map zoomed in around study site
mapPortO <- ggplot(data = mapWorld) + geom_sf() + 
            annotation_scale(location = "bl", width_hint = 0.5) +
            annotation_north_arrow(location = "bl", which_north = "true",
                                    pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                                    style = north_arrow_fancy_orienteering()) +
            geom_point(data = prelim_periods_Dung, mapping = aes(x = Longitude, y = Latitude), color = "cyan") +
            coord_sf(xlim = c(-127, -120), ylim = c(42.5, 43))

#create animation over time of Dungeness crab detections (lat/long), color by period: render time ~5 min
mapPortO <- ggplot(data = mapWorld) + geom_sf() + 
  geom_point(data = prelim_periods_Dung, mapping = aes(x = Longitude, y = Latitude, color = prelim.period), size = 3) +
  transition_time(Date.time.UTC) +
  labs(title = "Time: {frame_time}") +
  coord_sf(xlim = c(-124.485, -124.47), ylim = c(42.69, 42.698))
