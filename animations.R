### Animation
library(dplyr)
library(ggmap)
library(ggplot2)
library(gganimate)
library(ggimage)
library(magick)
library(animation)
library(devtools)
library(chron)

#set theme for animation
theme_set(theme_minimal(base_family = "Roboto Mono"))

#create variable for BR 1, period 1
fish1.1 <- periods_BlackR_accel %>% 
  filter(Transmitter == "A69-9007-12048") %>% filter(survey.period == "June 3-10")

June10_start <- ymd_hms("2021-06-10 00:00:01", tz = "US/Pacific")

fishBRA <- periods_BlackR_accel %>% 
  filter(Date.time.UTC >= June10_start & Date.time.UTC <= June17_end)

#set boxes Period and Time = NA so they work in the ggplot
#boxes$Period <- NA
#boxes$Time <- NA

#create plot with ggplot, geom_point for telemetry data and geom_segment for pool location
p <- ggplot(data = fish1.1) + 
  geom_point(aes(x=Longitude, y=Latitude, frame = Date.time.UTC), alpha = 0.7) + 
  geom_segment(data=boxes, aes(x=Easting, y=Northing, xend = Easting + delta_long, yend = Northing + delta_lat))
p
#animate the plot p by time interval 0.2, save as a gif called animate2: 
#500 obs takes about 2 min on my laptop
gganimate(p, title_frame = T, interval = 0.2)

ggplot() +
  borders("state", "oregon", colour = "gray90") +
  theme_map()


####O R ########################
ggplot(data = fish1.1, aes(x = Longitude, y = Latitude)) +
  #borders("state", "oregon", colour = "gray90") +
  #ggmap(get_stamenmap(bbox, maptype = "terrain-background", zoom = 2)) +
  theme_map() + 
  geom_point(data = fish1.1, aes(x = Longitude, y = Latitude), 
             colour = "#351C4D", alpha = 0.85) +
  geom_image(aes(image= "E:\\MS research\\RRMR2021ReceiverLogs\\BlackRF_ODFW.jpg")) +
  labs(title = "Date: {frame_time}") +
  transition_time(Date.time.UTC) +
  ease_aes("linear")


bbox <- c(left = -124.497, bottom = 42.68760, right = -124.4555, top = 42.705)

#latitude = c(49.38639, 50.68870, 50.77530, 49.86880, 39.31181, 37.05229)          
#longitude = c(-121.45063, -120.36646, 50, -97.40836, 76.71748, -119.19536)

register_google(key = "AIzaSyCBdz_hGOD7jyWN18QuUBMWaXClSKEDvdI")


ggplot(data = fishBRA, aes(x = Longitude, y = Latitude)) +
  #borders("state", "oregon", colour = "gray90") +
  ggmap(get_map(bbox, maptype = "satellite")) +
  theme_map() + 
  geom_point(data = fishBRA, aes(x = Longitude, y = Latitude, 
                                              color = Transmitter), 
             alpha = 0.85) +
  geom_image(aes(image= "E:\\MS research\\RRMR2021ReceiverLogs\\BlackRF_ODFW.jpg")) +
  labs(title = "Date: {frame_time}") +
  transition_time(Date.time.UTC) +
  ease_aes("linear")

## use this one! - need to change for during boat time (or use langseth location?)
BRA_animate <- ggmap(get_map(bbox, maptype = "satellite"))+
  #geom_point(data = fishBRA, aes(x = Longitude, y = Latitude, color = Transmitter), size = 1.5)+
  geom_image(data = fishBRA, aes(x = Longitude, y = Latitude, image = "E:\\MS research\\RRMR2021ReceiverLogs\\BlackRF_ODFW_trans.png"), size = 0.15) +
  theme_map() + 
  labs(title = "Date: {frame_time}") +
  transition_time(Date.time.UTC) +
  ease_aes("linear")
#print(BRA_animate)
#saveGIF(BRA_animate, "BRA_animate.gif")



#site_df = as.data.frame(cbind(latitude,longitude))





site_map = ggmap(get_stamenmap(bbox, maptype = "terrain-background", zoom = 2))+
  geom_point(data = site_df, aes(x = longitude, y = latitude), 
             size = 1.5, color = "orange")+
  geom_point(data = site_df, aes(x = longitude, y = latitude), 
             pch= 21, size = 2, color = "black")+
  theme_bw() + 
  labs(x = "Longitude", y = "Latitude")



RRMRmap <- get_map(location = c(lon = -124.47, lat = 42.69), zoom = "auto", 
                   maptype = 'satellite')
RRMRmap <- ggmap(RRMRmap)
RRMRmap
