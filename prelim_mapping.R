library(ggplot2)
library(sf)
library(adehabitatHR)
library(cowplot)
library(ggspatial)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggspatial)

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


mapPortO <- ggplot(data = mapWorld) + geom_sf() + 
  geom_point(data = prelim_periods_Dung, mapping = aes(x = Longitude, y = Latitude, color = prelim.period)) +
  coord_sf(xlim = c(-124.485, -124.46), ylim = c(42.6855, 42.70))