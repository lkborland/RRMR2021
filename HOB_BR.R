## HOB tag analyses - Black Rockfish 
## Code modeled after, with permission, from L. Rasmusen, ODFW

library(sp)
library(rgdal)
library(raster)
library(readr)
library(tidyverse)
library(lubridate)
library(cowplot)
library(ggmap)
library(ggsn)
library(RColorBrewer)
library(viridis)

### Multi-beam load raster
RRMR_multi <- raster("D:\\MS research\\RRMR2021ReceiverLogs\\RFR_IslRk_Humbug_Region_4m1.tif")
RRMR_Rough <- terrain(RRMR_multi,opt="roughness",neighbors=8)
RRMR_TPI <- terrain(RRMR_multi,opt="TPI",neighbors=8)
RRMR_TRI <- terrain(RRMR_multi,opt="TRI",neighbors=8)
RRMR_Slope <- terrain(RRMR_multi,opt="slope",unit='degrees',neighbors=8)
RRMR_Aspect <- terrain(RRMR_multi,opt="aspect",unit="degrees",neighbors=8)

### Load Black Rockfish depth points from tag data
Black <- periods_BlackR_depth
Black$Species <- "Black"
Black$DATETIME <- Black$Date.time.UTC

### Set coordinate system
cord.dec <- SpatialPoints(cbind(Black$Longitude,Black$Latitude), proj4string=CRS("+proj=longlat"))
cord.UTM <- spTransform(cord.dec, crs(RRMR_multi))

### Extract data from multibeam
BathyDepth <- raster::extract(RRMR_multi,cord.UTM, method='simple', buffer=NULL, small=FALSE, cellnumbers=FALSE,fun=NULL, na.rm=FALSE, df=FALSE, factors=FALSE)
BathyRough <- raster::extract(RRMR_Rough,cord.UTM, method='simple', buffer=NULL, small=FALSE, cellnumbers=FALSE,fun=NULL, na.rm=FALSE, df=FALSE, factors=FALSE)
BathyTPI <- raster::extract(RRMR_TPI,cord.UTM, method='simple', buffer=NULL, small=FALSE, cellnumbers=FALSE,fun=NULL, na.rm=FALSE, df=FALSE, factors=FALSE)
BathyTRI <- raster::extract(RRMR_TRI,cord.UTM, method='simple', buffer=NULL, small=FALSE, cellnumbers=FALSE,fun=NULL, na.rm=FALSE, df=FALSE, factors=FALSE)
BathySlope <- raster::extract(RRMR_Slope,cord.UTM, method='simple', buffer=NULL, small=FALSE, cellnumbers=FALSE,fun=NULL, na.rm=FALSE, df=FALSE, factors=FALSE)
BathyAspect <- raster::extract(RRMR_Aspect,cord.UTM, method='simple', buffer=NULL, small=FALSE, cellnumbers=FALSE,fun=NULL, na.rm=FALSE, df=FALSE, factors=FALSE)

Black$BathyDepth <- BathyDepth
Black$BathyRough <- BathyRough
Black$BathyTPI <- BathyTPI
Black$BathyTRI <- BathyTRI
Black$BathySlope <- BathySlope
Black$BathyAspect <- BathyAspect

Black$DEPTH <- Black$Sensor.Value

Black$HOB <- abs(Black$BathyDepth)-abs(Black$DEPTH)
Black$HOB <- with(Black,ifelse(HOB<0,0,HOB))

#####################################################
#BlackSum <- Black %>% group_by(Hour) %>% summarise(AvgHOB=mean(HOB,na.rm=TRUE),HOBDev=sd(HOB,na.rm=TRUE))
#BlackSum$Species <- "Black"
#BlackSum$Reef <- "Seal_Rock"

### Plots with Reef Overlay
# Depth
multi <- as.data.frame(SR_multi)
Deacmulti <- multi
Blkmulti <- multi
Deacmulti$Species <- "Deacon"
Blkmulti$Species <- "Black"

colnames(multi) <- c("DEPTH","Species")

ggplot()+geom_density(data=dat,aes(x=DEPTH*-1,fill=Species,y=..scaled..),color=NA)+
  geom_density(data=multi,aes(x=DEPTH,y=..scaled..),color="black")+
  facet_wrap(~Species)+coord_flip()+
  theme_bw()+ylab("Scaled Density")+xlab("Depth (m)")+
  scale_fill_manual(values=c("Black","Orange3","Blue","Brown","Gold"))+
  theme(legend.position=c(0.85,0.25))

# Center Map
or_big <- get_map(location = c(left = -124.5, bottom = 44.2, right = -123.9, top = 44.7), 
                  source = "google", maptype = "terrain")

PP_Box <- data.frame(rbind(cbind(max(PP_Depth2$y),min(PP_Depth2$x)),
                           cbind(max(PP_Depth2$y),max(PP_Depth2$x)),
                           cbind(min(PP_Depth2$y),max(PP_Depth2$x)),
                           cbind(min(PP_Depth2$y),min(PP_Depth2$x)),
                           cbind(max(PP_Depth2$y),min(PP_Depth2$x))))
colnames(PP_Box) <- c("y","x")

ggmap(or_big) + 
  geom_path(data = PP_Box, aes(x = x, y = y)) +
  geom_path(data = SR_Box, aes(x = x, y = y)) +
  geom_path(data = SW_Box, aes(x = x, y = y)) +
  xlab("Longitude")+ylab("Latitude")