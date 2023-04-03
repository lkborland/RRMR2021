# map for AN book chapter - receiver locations and zoom out with scale bar

library(sf)
library(terra)
library(dplyr)
library(spData)
library(spDataLarge)
library(tmap)    # for static and interactive maps
library(leaflet) # for interactive maps
library(ggplot2) # tidyverse data visualization package
library(shiny)
library(maps)
library(USAboundaries)
library(grid)
library(tmaptools)

oregon <- us_states(resolution = "high", states = "oregon")

locdat <- read.csv("E:\\MS research\\RRMR2021ReceiverLogs\\receiver_waypts.csv")
locdat_sf <- st_as_sf(locdat, coords = c('Lon', 'Lat'), crs = st_crs(oregon)$proj4string)
po_region <- st_bbox(c(xmin = -124.497, xmax = -124.4555,
                      ymin = 42.68760, ymax = 42.712),
                    crs = st_crs(oregon)) |> st_as_sfc()

porto <- data.frame(long  = -124.469616, lat = 42.701169)
porto_sf <- st_as_sf(porto, coords = c('long', 'lat'), crs = st_crs(oregon)$proj4string)

map_or <- tm_shape(oregon) + tm_polygons() +
  tm_style("col_blind") + tm_shape(porto_sf) +
  tm_symbols(size=.4, col = "#C7695F", shape = 17)#,col=Classification, shapeNA = NA, title = "Spaces")

pal8 <- c("#E6E6E6", "#B2DF8A", "#33A02C", "darkgreen")

RRMRreceivers_map <-
  tm_shape(RRMR_Rough, bbox = po_region) + 
  tm_raster("roughness", palette = "#E6E6E6", legend.show = FALSE) +
  tm_shape(locdat_sf, bbox = po_region) +
  tm_dots(size = .3, col = "#1B5094") +
  tm_shape(oregon) + tm_polygons() +
  tm_compass(type = "4star", position = c("left", "top"), text.size = .35) +
  tm_scale_bar(breaks = c(0, .25, .5), position = c("right", "bottom"), text.size = .45) +
  tm_layout(frame = TRUE)
  
  #tm_basemap("Esri.WorldTopoMap")
 # tm_layout(basemaps = leaflet::providers$MapTilesAPI.OSMEnglish)


#tmap_mode("plot")
#tmap_style("col_blind")

RRMRreceivers_map
print(map_or, vp = viewport(0.16, 0.17, width = 0.25, height = 0.3))
