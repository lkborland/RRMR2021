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

setwd("//fwnew12/Home/RasmusLe/My Documents/Telemetry/HOB")

# Multi Beam Load Raster
SR_multi <- raster("SealRockUTM/prj.adf")
SR_Rough <- terrain(SR_multi,opt="roughness",neighbors=8)
SR_TPI <- terrain(SR_multi,opt="TPI",neighbors=8)
SR_TRI <- terrain(SR_multi,opt="TRI",neighbors=8)
SR_Slope <- terrain(SR_multi,opt="slope",unit='degrees',neighbors=8)
SR_Aspect <- terrain(SR_multi,opt="aspect",unit="degrees",neighbors=8)

#Load Black Points Data From Parker
Black <- read_csv("BRFTags.csv")
Black$Species <- "Black"
Black$DATETIME <- as.POSIXct(Black$DateTime, tz="America/Los_Angeles")

cord.dec = SpatialPoints(cbind(Black$Longitude,Black$Latitude), proj4string=CRS("+proj=longlat"))
cord.UTM <- spTransform(cord.dec, crs(SR_multi))

BathyDepth=raster::extract(SR_multi,cord.UTM, method='simple', buffer=NULL, small=FALSE, cellnumbers=FALSE,fun=NULL, na.rm=FALSE, df=FALSE, factors=FALSE)
BathyRough=raster::extract(SR_Rough,cord.UTM, method='simple', buffer=NULL, small=FALSE, cellnumbers=FALSE,fun=NULL, na.rm=FALSE, df=FALSE, factors=FALSE)
BathyTPI=raster::extract(SR_TPI,cord.UTM, method='simple', buffer=NULL, small=FALSE, cellnumbers=FALSE,fun=NULL, na.rm=FALSE, df=FALSE, factors=FALSE)
BathyTRI=raster::extract(SR_TRI,cord.UTM, method='simple', buffer=NULL, small=FALSE, cellnumbers=FALSE,fun=NULL, na.rm=FALSE, df=FALSE, factors=FALSE)
BathySlope=raster::extract(SR_Slope,cord.UTM, method='simple', buffer=NULL, small=FALSE, cellnumbers=FALSE,fun=NULL, na.rm=FALSE, df=FALSE, factors=FALSE)
BathyAspect=raster::extract(SR_Aspect,cord.UTM, method='simple', buffer=NULL, small=FALSE, cellnumbers=FALSE,fun=NULL, na.rm=FALSE, df=FALSE, factors=FALSE)

Black$BathyDepth <- BathyDepth
Black$BathyRough <- BathyRough
Black$BathyTPI <- BathyTPI
Black$BathyTRI <- BathyTRI
Black$BathySlope <- BathySlope
Black$BathyAspect <- BathyAspect

Black$DEPTH <- Black$'Avg Of Depth'

Black$HOB <- abs(Black$BathyDepth)-abs(Black$DEPTH)
Black$HOB <- with(Black,ifelse(HOB<0,0,HOB))

BlackSum <- Black %>% group_by(Hour) %>% summarise(AvgHOB=mean(HOB,na.rm=TRUE),HOBDev=sd(HOB,na.rm=TRUE))
BlackSum$Species <- "Black"
BlackSum$Reef <- "Seal_Rock"

# Deacons Test
setwd("//fwnew12/Home/RasmusLe/My Documents/Telemetry/HOB/DeaconTags")
Deacon <- read_csv("TRANSMITTER-TD01-CALC-POSITIONS.csv")
Deacon <- rbind(Deacon,read_csv("TRANSMITTER-TD02-CALC-POSITIONS.csv"))
Deacon <- rbind(Deacon,read_csv("TRANSMITTER-TD03-CALC-POSITIONS.csv"))
Deacon <- rbind(Deacon,read_csv("TRANSMITTER-TD04-CALC-POSITIONS.csv"))
Deacon <- rbind(Deacon,read_csv("TRANSMITTER-TD05-CALC-POSITIONS.csv"))
Deacon <- rbind(Deacon,read_csv("TRANSMITTER-TD06-CALC-POSITIONS.csv"))
Deacon <- rbind(Deacon,read_csv("TRANSMITTER-TD07-CALC-POSITIONS.csv"))
Deacon <- rbind(Deacon,read_csv("TRANSMITTER-TD08-CALC-POSITIONS.csv"))
Deacon <- rbind(Deacon,read_csv("TRANSMITTER-TD09-CALC-POSITIONS.csv"))
Deacon <- rbind(Deacon,read_csv("TRANSMITTER-TD10-CALC-POSITIONS.csv"))
Deacon <- rbind(Deacon,read_csv("TRANSMITTER-TD11-CALC-POSITIONS.csv"))
Deacon$Species <- "Deacon"
setwd("//fwnew12/Home/RasmusLe/My Documents/Telemetry/HOB")

Deacon <- Deacon[!is.na(Deacon$DEPTH),]
Deacon <- subset(Deacon,HPE<25)
Deacon$DATETIME <- as.POSIXct(Deacon$DATETIME, tz="Europe/London")
attr(Deacon$DATETIME, "tzone") <- "America/Los_Angeles" 
Deacon$Hour <- lubridate::hour(Deacon$DATETIME)

cord.dec <- SpatialPoints(cbind(Deacon$LON,Deacon$LAT), proj4string=CRS("+proj=longlat"))
cord.UTM <- spTransform(cord.dec, crs(SR_multi))

BathyDepth=raster::extract(SR_multi,cord.UTM, method='simple', buffer=NULL, small=FALSE, cellnumbers=FALSE,fun=NULL, na.rm=FALSE, df=FALSE, factors=FALSE)
BathyRough=raster::extract(SR_Rough,cord.UTM, method='simple', buffer=NULL, small=FALSE, cellnumbers=FALSE,fun=NULL, na.rm=FALSE, df=FALSE, factors=FALSE)
BathyTPI=raster::extract(SR_TPI,cord.UTM, method='simple', buffer=NULL, small=FALSE, cellnumbers=FALSE,fun=NULL, na.rm=FALSE, df=FALSE, factors=FALSE)
BathyTRI=raster::extract(SR_TRI,cord.UTM, method='simple', buffer=NULL, small=FALSE, cellnumbers=FALSE,fun=NULL, na.rm=FALSE, df=FALSE, factors=FALSE)
BathySlope=raster::extract(SR_Slope,cord.UTM, method='simple', buffer=NULL, small=FALSE, cellnumbers=FALSE,fun=NULL, na.rm=FALSE, df=FALSE, factors=FALSE)
BathyAspect=raster::extract(SR_Aspect,cord.UTM, method='simple', buffer=NULL, small=FALSE, cellnumbers=FALSE,fun=NULL, na.rm=FALSE, df=FALSE, factors=FALSE)

Deacon$BathyDepth <- BathyDepth
Deacon$BathyRough <- BathyRough
Deacon$BathyTPI <- BathyTPI
Deacon$BathyTRI <- BathyTRI
Deacon$BathySlope <- BathySlope
Deacon$BathyAspect <- BathyAspect

Deacon$HOB <- abs(Deacon$BathyDepth)-abs(Deacon$DEPTH)
Deacon$HOB <- with(Deacon,ifelse(HOB<0,0,HOB))

DeaconSum <- Deacon %>% group_by(Hour) %>% summarise(AvgHOB=mean(HOB,na.rm=TRUE),HOBDev=sd(HOB,na.rm=TRUE))
DeaconSum$Species <- "Deacon"
DeaconSum$Reef <- "Seal_Rock"
RkFishSum <- rbind(data.frame(BlackSum),data.frame(DeaconSum))

######################################################################################################################################
# Cape Perpetua
PP_multi <- raster("PerpetuaUTM/prj.adf")
PP_Rough <- terrain(PP_multi,opt="roughness",neighbors=8)
PP_TPI <- terrain(PP_multi,opt="TPI",neighbors=8)
PP_TRI <- terrain(PP_multi,opt="TRI",neighbors=8)
PP_Slope <- terrain(PP_multi,opt="slope",unit="degrees",neighbors=8)
PP_Aspect <- terrain(PP_multi,opt="aspect",unit="degrees",neighbors=8)
#Black
setwd("//fwnew12/Home/RasmusLe/My Documents/Telemetry/HOB/PerpetuaTags")
Blk <- read_csv("TAG-T228-CALC-POSITIONS.csv")
Blk <- rbind(Blk,read_csv("TAG-T224-CALC-POSITIONS.csv"))
Blk$Species <- "Black"
#Brown
Brn <- read_csv("TAG-T226-CALC-POSITIONS.csv")
Brn$Species <- "Brown"
#Copper
Cop <- read_csv("TAG-T100-CALC-POSITIONS.csv")
Cop <- rbind(Cop,read_csv("TAG-T103-CALC-POSITIONS.csv"))
Cop <- rbind(Cop,read_csv("TAG-T104-CALC-POSITIONS.csv"))
Cop <- rbind(Cop,read_csv("TAG-T249-CALC-POSITIONS.csv"))
Cop <- rbind(Cop,read_csv("TAG-T252-CALC-POSITIONS.csv"))
Cop <- rbind(Cop,read_csv("TAG-T254-CALC-POSITIONS.csv"))
Cop <- rbind(Cop,read_csv("TAG-T255-CALC-POSITIONS.csv"))
Cop <- rbind(Cop,read_csv("TAG-T256-CALC-POSITIONS.csv"))
Cop$Species <- "Copper"
#Quillback
Qul <- read_csv("TAG-T101-CALC-POSITIONS.csv")
Qul <- rbind(Qul,read_csv("TAG-T102-CALC-POSITIONS.csv"))
Qul <- rbind(Qul,read_csv("TAG-T105-CALC-POSITIONS.csv"))
Qul <- rbind(Qul,read_csv("TAG-T250-CALC-POSITIONS.csv"))
Qul <- rbind(Qul,read_csv("TAG-T251-CALC-POSITIONS.csv"))
Qul <- rbind(Qul,read_csv("TAG-T253-CALC-POSITIONS.csv"))
Qul <- rbind(Qul,read_csv("TAG-T223-CALC-POSITIONS.csv"))
Qul <- rbind(Qul,read_csv("TAG-T225-CALC-POSITIONS.csv"))
Qul <- rbind(Qul,read_csv("TAG-T227-CALC-POSITIONS.csv"))
Qul$Species <- "Quillback"

Perp <- rbind(Blk,Brn,Cop,Qul)

setwd("//fwnew12/Home/RasmusLe/My Documents/Telemetry/HOB")

Perp <- Perp[!is.na(Perp$DEPTH),]
Perp <- subset(Perp,HPE<25)
Perp$DATETIME <- as.POSIXct(Perp$DATETIME, tz="Europe/London")
attr(Perp$DATETIME, "tzone") <- "America/Los_Angeles" 
Perp$Hour <- lubridate::hour(Perp$DATETIME)

cord.dec <- SpatialPoints(cbind(Perp$LON,Perp$LAT), proj4string=CRS("+proj=longlat"))
cord.UTM <- spTransform(cord.dec, crs(PP_multi))

BathyDepth=raster::extract(PP_multi,cord.dec, method='simple', buffer=NULL, small=FALSE, cellnumbers=FALSE,fun=NULL, na.rm=FALSE, df=FALSE, factors=FALSE)
BathyRough=raster::extract(PP_Rough,cord.UTM, method='simple', buffer=NULL, small=FALSE, cellnumbers=FALSE,fun=NULL, na.rm=FALSE, df=FALSE, factors=FALSE)
BathyTPI=raster::extract(PP_TPI,cord.UTM, method='simple', buffer=NULL, small=FALSE, cellnumbers=FALSE,fun=NULL, na.rm=FALSE, df=FALSE, factors=FALSE)
BathyTRI=raster::extract(PP_TRI,cord.UTM, method='simple', buffer=NULL, small=FALSE, cellnumbers=FALSE,fun=NULL, na.rm=FALSE, df=FALSE, factors=FALSE)
BathySlope=raster::extract(PP_Slope,cord.UTM, method='simple', buffer=NULL, small=FALSE, cellnumbers=FALSE,fun=NULL, na.rm=FALSE, df=FALSE, factors=FALSE)
BathyAspect=raster::extract(PP_Aspect,cord.UTM, method='simple', buffer=NULL, small=FALSE, cellnumbers=FALSE,fun=NULL, na.rm=FALSE, df=FALSE, factors=FALSE)

Perp$BathyDepth <- BathyDepth
Perp$BathyRough <- BathyRough
Perp$BathyTPI <- BathyTPI
Perp$BathyTRI <- BathyTRI
Perp$BathySlope <- BathySlope
Perp$BathyAspect <- BathyAspect

Perp$HOB <- abs(Perp$BathyDepth)-abs(Perp$DEPTH)
Perp$HOB <- with(Perp,ifelse(HOB<0,0,HOB))

PerpSum <- Perp %>% group_by(Species,Hour) %>% filter(HOB>=0) %>%
summarise(AvgHOB=mean(HOB,na.rm=TRUE),HOBDev=sd(HOB,na.rm=TRUE))

PerpSum$Reef <- "Perpetua"

RkFishSum <- rbind(data.frame(RkFishSum),data.frame(PerpSum))

######################################################################################################################################
# Stonewall Bank
SW_multi <- raster("StonewallUTM/prj.adf")
SW_Rough <- terrain(SW_multi,opt="roughness",neighbors=8)
SW_TPI <- terrain(SW_multi,opt="TPI",neighbors=8)
SW_TRI <- terrain(SW_multi,opt="TRI",neighbors=8)
SW_Slope <- terrain(SW_multi,opt="slope",unit="degrees",neighbors=8)
SW_Aspect <- terrain(SW_multi,opt="aspect",unit="degrees",neighbors=8)
# Yelloweye
#2012
setwd("//fwnew12/Home/RasmusLe/My Documents/Telemetry/HOB/StonewallTags/2012")
Yelloweye <- read_csv("TAG-T01-CALC-POSITIONS.csv")
Yelloweye <- rbind(Yelloweye,read_csv("TAG-T02-CALC-POSITIONS.csv"))
Yelloweye <- rbind(Yelloweye,read_csv("TAG-T03-CALC-POSITIONS.csv"))
Yelloweye <- rbind(Yelloweye,read_csv("TAG-T04-CALC-POSITIONS.csv"))
Yelloweye <- rbind(Yelloweye,read_csv("TAG-T05-CALC-POSITIONS.csv"))
Yelloweye <- rbind(Yelloweye,read_csv("TAG-T06-CALC-POSITIONS.csv"))
Yelloweye <- rbind(Yelloweye,read_csv("TAG-T07-CALC-POSITIONS.csv"))
Yelloweye <- rbind(Yelloweye,read_csv("TAG-T08-CALC-POSITIONS.csv"))
Yelloweye <- rbind(Yelloweye,read_csv("TAG-T09-CALC-POSITIONS.csv"))
Yelloweye <- rbind(Yelloweye,read_csv("TAG-T10-CALC-POSITIONS.csv"))
Yelloweye <- rbind(Yelloweye,read_csv("TAG-T11-CALC-POSITIONS.csv"))

#2013
setwd("//fwnew12/Home/RasmusLe/My Documents/Telemetry/HOB/StonewallTags/2013")
Yelloweye13 <- read_csv("TRANSMITTER-T01-CALC-POSITIONS.csv")
Yelloweye13 <- rbind(Yelloweye13,read_csv("TRANSMITTER-T02-CALC-POSITIONS.csv"))
Yelloweye13 <- rbind(Yelloweye13,read_csv("TRANSMITTER-T03-CALC-POSITIONS.csv"))
Yelloweye13 <- rbind(Yelloweye13,read_csv("TRANSMITTER-T04-CALC-POSITIONS.csv"))
Yelloweye13 <- rbind(Yelloweye13,read_csv("TRANSMITTER-T05-CALC-POSITIONS.csv"))
Yelloweye13 <- rbind(Yelloweye13,read_csv("TRANSMITTER-T06-CALC-POSITIONS.csv"))
Yelloweye13 <- rbind(Yelloweye13,read_csv("TRANSMITTER-T07-CALC-POSITIONS.csv"))
Yelloweye13 <-select(Yelloweye13, -c(DETECTEDID))
colnames(Yelloweye13)[1] =c("TAG")

#Combine 2012 and 2013
Yelloweye <- rbind (Yelloweye,Yelloweye13)
Yelloweye$Species <- "Yelloweye"

setwd("//fwnew12/Home/RasmusLe/My Documents/Telemetry/HOB")

Yelloweye <- Yelloweye[!is.na(Yelloweye$DEPTH),]
Yelloweye <- subset(Yelloweye,HPE<25)
Yelloweye$DATETIME <- as.POSIXct(Yelloweye$DATETIME, tz="Europe/London")
attr(Yelloweye$DATETIME, "tzone") <- "America/Los_Angeles" 
Yelloweye$Hour <- lubridate::hour(Yelloweye$DATETIME)

cord.dec <- SpatialPoints(cbind(Yelloweye$LON,Yelloweye$LAT), proj4string=CRS("+proj=longlat"))
cord.UTM <- spTransform(cord.dec, crs(SW_multi))

BathyDepth=raster::extract(SW_multi,cord.dec, method='simple', buffer=NULL, small=FALSE, cellnumbers=FALSE,fun=NULL, na.rm=FALSE, df=FALSE, factors=FALSE)
BathyRough=raster::extract(SW_Rough,cord.UTM, method='simple', buffer=NULL, small=FALSE, cellnumbers=FALSE,fun=NULL, na.rm=FALSE, df=FALSE, factors=FALSE)
BathyTPI=raster::extract(SW_TPI,cord.UTM, method='simple', buffer=NULL, small=FALSE, cellnumbers=FALSE,fun=NULL, na.rm=FALSE, df=FALSE, factors=FALSE)
BathyTRI=raster::extract(SW_TRI,cord.UTM, method='simple', buffer=NULL, small=FALSE, cellnumbers=FALSE,fun=NULL, na.rm=FALSE, df=FALSE, factors=FALSE)
BathySlope=raster::extract(SW_Slope,cord.UTM, method='simple', buffer=NULL, small=FALSE, cellnumbers=FALSE,fun=NULL, na.rm=FALSE, df=FALSE, factors=FALSE)
BathyAspect=raster::extract(SW_Aspect,cord.UTM, method='simple', buffer=NULL, small=FALSE, cellnumbers=FALSE,fun=NULL, na.rm=FALSE, df=FALSE, factors=FALSE)

Yelloweye$BathyDepth <- BathyDepth
Yelloweye$BathyRough <- BathyRough
Yelloweye$BathyTPI <- BathyTPI
Yelloweye$BathyTRI <- BathyTRI
Yelloweye$BathySlope <- BathySlope
Yelloweye$BathyAspect <- BathyAspect

Yelloweye$HOB <- abs(Yelloweye$BathyDepth)-Yelloweye$DEPTH
Yelloweye$HOB <- with(Yelloweye,ifelse(HOB<0,0,HOB))

YelloweyeSum <- Yelloweye %>% group_by(Hour) %>% filter(HOB>=0) %>%
  summarise(AvgHOB=mean(HOB,na.rm=TRUE),HOBDev=sd(HOB,na.rm=TRUE))

YelloweyeSum$Species <- "Yelloweye"
YelloweyeSum$Reef <- "Stonewall"

RkFishSum <- rbind(data.frame(RkFishSum),data.frame(YelloweyeSum))

ggplot(RkFishSum,aes(Hour,AvgHOB))+geom_line()+
  xlab("Hour of Day")+ylab("Height off bottom (m)")+facet_wrap(~Species)

##########################################################################################################
# Combine Raw data into single massive dataframe
 dat <- rbind(
Black[,c("Species","DATETIME","Hour","HOB","DEPTH","BathyDepth","BathyTPI","BathySlope","BathyTRI","BathyRough","BathyAspect")],
Deacon[,c("Species","DATETIME","Hour","HOB","DEPTH","BathyDepth","BathyTPI","BathySlope","BathyTRI","BathyRough","BathyAspect")],
Perp[,c("Species","DATETIME","Hour","HOB","DEPTH","BathyDepth","BathyTPI","BathySlope","BathyTRI","BathyRough","BathyAspect")],
Yelloweye[,c("Species","DATETIME","Hour","HOB","DEPTH","BathyDepth","BathyTPI","BathySlope","BathyTRI","BathyRough","BathyAspect")]
)

rm(list=setdiff(ls(), "dat"))

ggplot(dat,aes(x=BathySlope,y=BathyRough))+geom_point()+
  facet_wrap(~Species,scales="free")+theme_bw()+xlab("Roughness")+ylab("Slope")

PlotDepth <- ggplot(dat,aes(x=DEPTH*-1,fill=Species))+geom_density(aes(y=..scaled..))+facet_wrap(~Species)+coord_flip()+
  theme_bw()+xlab("Depth (m)")+ylab("Scaled Density")+
  scale_fill_manual(values=c("Black","Orange3","Blue","Brown","Gold"))+
  theme(legend.position=c(0.85,0.25))

PlotSlope <- ggplot(dat,aes(x=BathySlope,fill=Species))+geom_density(aes(y=..scaled..))+facet_wrap(~Species)+
  theme_bw()+ylab("Scaled Density")+xlab("Slope (degrees)")+
  scale_fill_manual(values=c("Black","Orange3","Blue","Brown","Gold"))+
  theme(legend.position=c(0.85,0.25))

PlotRough <- ggplot(dat,aes(x=BathyRough,fill=Species))+geom_density(aes(y=..scaled..))+facet_wrap(~Species)+
  theme_bw()+ylab("Scaled Density")+xlab("Roughness")+
  scale_fill_manual(values=c("Black","Orange3","Blue","Brown","Gold"))+
  theme(legend.position=c(0.85,0.25))

PlotTPI <- ggplot(dat,aes(x=BathyTPI,fill=Species))+geom_density(aes(y=..scaled..))+facet_wrap(~Species)+
  theme_bw()+ylab("Scaled Density")+xlab("TPI")+
  scale_fill_manual(values=c("Black","Orange3","Blue","Brown","Gold"))+
  theme(legend.position=c(0.85,0.25))

PlotTRI <- ggplot(dat,aes(x=BathyTPI,fill=Species))+geom_density(aes(y=..scaled..))+facet_wrap(~Species)+
  theme_bw()+ylab("Scaled Density")+xlab("TRI")+
  scale_fill_manual(values=c("Black","Orange3","Blue","Brown","Gold"))+
  theme(legend.position=c(0.85,0.25))

PlotHOB <- ggplot(dat,aes(x=Hour,y=HOB,color=Species,fill=Species))+geom_smooth()+theme_bw()+
  ylab("Height off Bottom (m)")+xlab("Hour of Day")+
  scale_color_manual(values=c("Black","Orange3","Blue","Brown","Gold"))+
  scale_fill_manual(values=c("Black","Orange3","Blue","Brown","Gold"))

ggplot(dat,aes(x=DATETIME,fill=Species))+geom_histogram(bins=200)+
  scale_fill_manual(values=c("Black","Orange3","Blue","Brown","Gold"))+
  facet_grid(Species~.,scales="free_y")

dat %>% 
  group_by(Species)  %>%
  summarise(
    temp=max(DATETIME)
  )

### Plots with Reef Overlay
# Depth
multi <- as.data.frame(SR_multi)
Deacmulti <- multi
Blkmulti <- multi
Deacmulti$Species <- "Deacon"
Blkmulti$Species <- "Black"

multi <- as.data.frame(PP_multi)
multi$prj <- multi$prj*-1
Copmulti <- multi
Quilmulti <- multi
Copmulti$Species <- "Copper"
Quilmulti$Species <- "Quillback"

multi <- as.data.frame(SW_multi)
multi$Species <- "Yelloweye"

multi <- rbind(Blkmulti,Deacmulti,Copmulti,Quilmulti,multi)
colnames(multi) <- c("DEPTH","Species")

ggplot()+geom_density(data=dat,aes(x=DEPTH*-1,fill=Species,y=..scaled..),color=NA)+
  geom_density(data=multi,aes(x=DEPTH,y=..scaled..),color="black")+
  facet_wrap(~Species)+coord_flip()+
  theme_bw()+ylab("Scaled Density")+xlab("Depth (m)")+
  scale_fill_manual(values=c("Black","Orange3","Blue","Brown","Gold"))+
  theme(legend.position=c(0.85,0.25))

# Slope
Slope <- as.data.frame(SR_Slope)
DeacSlope <- Slope
BlkSlope <- Slope
DeacSlope$Species <- "Deacon"
BlkSlope$Species <- "Black"

Slope <- as.data.frame(PP_Slope)
CopSlope <- Slope
QuilSlope <- Slope
CopSlope$Species <- "Copper"
QuilSlope$Species <- "Quillback"

Slope <- as.data.frame(SW_Slope)
Slope$Species <- "Yelloweye"

Slope <- rbind(BlkSlope,DeacSlope,CopSlope,QuilSlope,Slope)
colnames(Slope) <- c("BathySlope","Species")

ggplot()+geom_density(data=dat,aes(x=BathySlope,fill=Species,y=..scaled..),color=NA)+
  geom_density(data=Slope,aes(x=BathySlope,y=..scaled..),color="black")+
  facet_wrap(~Species)+
  theme_bw()+ylab("Scaled Density")+xlab("Slope (degrees)")+
  scale_fill_manual(values=c("Black","Orange3","Blue","Brown","Gold"))+
  theme(legend.position=c(0.85,0.25))

# Rough
Rough <- as.data.frame(SR_Rough)
DeacRough <- Rough
BlkRough <- Rough
DeacRough$Species <- "Deacon"
BlkRough$Species <- "Black"

Rough <- as.data.frame(PP_Rough)
CopRough <- Rough
QuilRough <- Rough
CopRough$Species <- "Copper"
QuilRough$Species <- "Quillback"

Rough <- as.data.frame(SW_Rough)
Rough$Species <- "Yelloweye"

Rough <- rbind(BlkRough,DeacRough,CopRough,QuilRough,Rough)
colnames(Rough) <- c("BathyRough","Species")
Rough <- subset(Rough,BathyRough<=10)

ggplot()+geom_density(data=dat,aes(x=BathyRough,fill=Species,y=..scaled..),color=NA)+
  geom_density(data=Rough,aes(x=BathyRough,y=..scaled..),color="black")+
  facet_wrap(~Species)+xlim(c(0,10))+
  theme_bw()+ylab("Scaled Density")+xlab("Roughness")+
  scale_fill_manual(values=c("Black","Orange3","Blue","Brown","Gold"))+
  theme(legend.position=c(0.85,0.25))

##########################################################################################################
newproj="+proj=longlat +datum=WGS84"
SR_Depth2 <- as.data.frame(projectRaster(SR_multi,crs=newproj),xy=TRUE)
SR_Rough2 <- as.data.frame(projectRaster(SR_Rough,crs=newproj),xy=TRUE)
SR_Slope2 <- as.data.frame(projectRaster(SR_Slope,crs=newproj),xy=TRUE)

PP_Depth2 <- as.data.frame(projectRaster(PP_multi,crs=newproj),xy=TRUE)
PP_Rough2 <- as.data.frame(projectRaster(PP_Rough,crs=newproj),xy=TRUE)
PP_Slope2 <- as.data.frame(projectRaster(PP_Slope,crs=newproj),xy=TRUE)

SW_Depth2 <- as.data.frame(projectRaster(SW_multi,crs=newproj),xy=TRUE)
SW_Rough2 <- as.data.frame(projectRaster(SW_Rough,crs=newproj),xy=TRUE)
SW_Slope2 <- as.data.frame(projectRaster(SW_Slope,crs=newproj),xy=TRUE)

#Stonewall Plots
YE2 <- data.frame(cbind(mean(Yelloweye$LAT),mean(Yelloweye$LON)))
colnames(YE2) <- c("Lat","Long")

ggplot() + geom_raster(data = SW_Depth2,aes(x = x, y = y,fill = prj)) +
  scale_fill_viridis(name = "Depth (m)") + 
  geom_point(data=YE2,aes(x=Long,y=Lat),color="Black",size=5,alpha=0.5)+
  coord_quickmap()+xlab("Longitude")+ylab("Latitude")

ggplot() + geom_raster(data = SW_Rough2,aes(x = x, y = y,fill = roughness)) +
  scale_fill_viridis(name = "Roughness",limits=c(0,5),breaks=c(0,2.5,5)) + 
  geom_point(data=YE2,aes(x=Long,y=Lat),color="Black",size=5,alpha=0.5)+
  coord_quickmap()+xlab("Longitude")+ylab("Latitude")

ggplot() + geom_raster(data = SW_Slope2,aes(x = x, y = y,fill = slope)) +
  scale_fill_viridis(name = "Slope (degrees)",limits=c(0,50),breaks=c(0,25,50)) + 
  geom_point(data=YE2,aes(x=Long,y=Lat),color="Black",size=5,alpha=0.5)+
  coord_quickmap()+xlab("Longitude")+ylab("Latitude")

#Perpetua Plots
Qul2 <- data.frame(cbind(mean(Qul$LAT),mean(Qul$LON)))
colnames(Qul2) <- c("Lat","Long")
Cop2 <- data.frame(cbind(mean(Cop$LAT),mean(Cop$LON)))
colnames(Cop2) <- c("Lat","Long")

ggplot() + geom_raster(data = PP_Depth2,aes(x = x, y = y,fill = prj*-1)) +
  scale_fill_viridis(name = "Depth (m)") + 
  geom_point(data=Qul2,aes(x=Long,y=Lat),color="Brown",size=5,alpha=0.5)+
  geom_point(data=Cop2,aes(x=Long,y=Lat),color="Orange3",size=5,alpha=0.5)+
  coord_quickmap()+xlab("Longitude")+ylab("Latitude")

ggplot() + geom_raster(data = PP_Rough2,aes(x = x, y = y,fill = roughness)) +
  scale_fill_viridis(name = "Roughness",limits=c(0,2),breaks=c(0,1,2)) + 
  geom_point(data=Qul2,aes(x=Long,y=Lat),color="Brown",size=5,alpha=0.5)+
  geom_point(data=Cop2,aes(x=Long,y=Lat),color="Orange3",size=5,alpha=0.5)+
  coord_quickmap()+xlab("Longitude")+ylab("Latitude")

ggplot() + geom_raster(data = PP_Slope2,aes(x = x, y = y,fill = slope)) +
  scale_fill_viridis(name = "Slope (degrees)",limits=c(0,10),breaks=c(0,5,10)) + 
  geom_point(data=Qul2,aes(x=Long,y=Lat),color="Brown",size=5,alpha=0.5)+
  geom_point(data=Cop2,aes(x=Long,y=Lat),color="Orange3",size=5,alpha=0.5)+
  coord_quickmap()+xlab("Longitude")+ylab("Latitude")

#Seal Rock Plots
Black2 <- data.frame(cbind(mean(Black$Latitude),mean(Black$Longitude)))
colnames(Black2) <- c("Lat","Long")
Deacon2 <- data.frame(cbind(mean(Deacon$LAT),mean(Deacon$LON)))
colnames(Deacon2) <- c("Lat","Long")

ggplot() + geom_raster(data = SR_Depth2,aes(x = x, y = y,fill = prj)) +
  scale_fill_viridis(name = "Depth (m)") + 
  geom_point(data=Black2,aes(x=Long,y=Lat),color="Black",size=5,alpha=0.5)+
  geom_point(data=Deacon2,aes(x=Long,y=Lat),color="Blue",size=5,alpha=0.5)+
  coord_quickmap()+xlab("Longitude")+ylab("Latitude")

ggplot() + geom_raster(data = SR_Rough2,aes(x = x, y = y,fill = roughness)) +
  scale_fill_viridis(name = "Roughness",limits=c(0,10),breaks=c(0,5,10)) + 
  geom_point(data=Black2,aes(x=Long,y=Lat),color="Black",size=5,alpha=0.5)+
  geom_point(data=Deacon2,aes(x=Long,y=Lat),color="Blue",size=5,alpha=0.5)+
  coord_quickmap()+xlab("Longitude")+ylab("Latitude")

ggplot() + geom_raster(data = SR_Slope2,aes(x = x, y = y,fill = slope)) +
  scale_fill_viridis(name = "Slope (degrees)",limits=c(0,60),breaks=c(0,20,40,60)) + 
  geom_point(data=Black2,aes(x=Long,y=Lat),color="Black",size=5,alpha=0.5)+
  geom_point(data=Deacon2,aes(x=Long,y=Lat),color="Blue",size=5,alpha=0.5)+
  coord_quickmap()+xlab("Longitude")+ylab("Latitude")

# Center Map
or_big <- get_map(location = c(left = -124.5, bottom = 44.2, right = -123.9, top = 44.7), 
                  source = "google", maptype = "terrain")

PP_Box <- data.frame(rbind(cbind(max(PP_Depth2$y),min(PP_Depth2$x)),
                cbind(max(PP_Depth2$y),max(PP_Depth2$x)),
                cbind(min(PP_Depth2$y),max(PP_Depth2$x)),
                cbind(min(PP_Depth2$y),min(PP_Depth2$x)),
                cbind(max(PP_Depth2$y),min(PP_Depth2$x))))
colnames(PP_Box) <- c("y","x")

SR_Box <- data.frame(rbind(cbind(max(SR_Depth2$y),min(SR_Depth2$x)),
                           cbind(max(SR_Depth2$y),max(SR_Depth2$x)),
                           cbind(min(SR_Depth2$y),max(SR_Depth2$x)),
                           cbind(min(SR_Depth2$y),min(SR_Depth2$x)),
                           cbind(max(SR_Depth2$y),min(SR_Depth2$x))))
colnames(SR_Box) <- c("y","x")

SW_Box <- data.frame(rbind(cbind(max(SW_Depth2$y),min(SW_Depth2$x)),
                           cbind(max(SW_Depth2$y),max(SW_Depth2$x)),
                           cbind(min(SW_Depth2$y),max(SW_Depth2$x)),
                           cbind(min(SW_Depth2$y),min(SW_Depth2$x)),
                           cbind(max(SW_Depth2$y),min(SW_Depth2$x))))
colnames(SW_Box) <- c("y","x")

ggmap(or_big) + 
  geom_path(data = PP_Box, aes(x = x, y = y)) +
  geom_path(data = SR_Box, aes(x = x, y = y)) +
  geom_path(data = SW_Box, aes(x = x, y = y)) +
  xlab("Longitude")+ylab("Latitude")

