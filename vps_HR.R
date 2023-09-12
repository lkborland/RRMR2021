## script for VPS data

library(ggplot2)
library(ggthemes)
library(ggmap)
library(kdensity)
library(adehabitatHR)
library(sp)
library(sf)
#library(stats)

#upload VPS data from CSV
animal.positions <- periods_Lingcod
#as_tibble(read.csv("D:\\MS research\\VPS data\\VPS-Port Orford-01-Results-20220216\\results\\animal\\all.csv"))

animal.positions <- animal.positions %>% dplyr::group_by(Transmitter)

#convert time column data types to POSIX
#animal.positions$Time <- as.POSIXct(strptime(animal.positions$Time, "%Y-%m-%d %H:%M:%S"), tz = "UTC")
#animal.positions$Time <- with_tz(animal.positions$Time, "US/Pacific")



#example vps home range in may
## define time period for example in May 28
#May28_start <- ymd_hms("2021-05-28 00:00:01", tz = "US/Pacific")
#May28_end <- ymd_hms("2021-05-28 23:59:59", tz = "US/Pacific")

## subset data from May 28 as example for pre-noise period
#ex_may28_KUD65159 <- animal.positions %>% dplyr::filter(Time >= May28_start & Time <= May28_end) %>% dplyr::filter(Id == 65159)

## remove rows with NAs
#ex_may28_KUD65159 <- ex_may28_KUD65159[!is.na(ex_may28_KUD65159$Longitude) & !is.na(ex_may28_KUD65159$Latitude),]
ex_Lingcod.pre <- animal.positions[!is.na(animal.positions$Longitude) & !is.na(animal.positions$Latitude),]
ex_Lingcod.pre <- ex_Lingcod.pre %>% filter(coarse.period == "Before")
ex_Lingcod.during <- animal.positions[!is.na(animal.positions$Longitude) & !is.na(animal.positions$Latitude),]
ex_Lingcod.during <- ex_Lingcod.during %>% filter(coarse.period == "During")
ex_Lingcod.post <- animal.positions[!is.na(animal.positions$Longitude) & !is.na(animal.positions$Latitude),]
ex_Lingcod.post <- ex_Lingcod.post %>% filter(coarse.period == "After")

## remove animals with less than 5 relocations (constraint of KUD function)
ex_Lingcod.pre <- ex_Lingcod.pre %>% group_by(Transmitter) %>% filter(n() > 5)
ex_Lingcod.during <- ex_Lingcod.during %>% group_by(Transmitter) %>% filter(n() > 5)
ex_Lingcod.post <- ex_Lingcod.post %>% group_by(Transmitter) %>% filter(n() > 5)

## Create a copy of the object to make into a SpatialPointsDataFrame
### Only include three columns (id, x, and y coordinates) for estimating home ranges
#ex_may28_KUD.sp <- ex_may28_KUD65159[, c("Id", "Longitude", "Latitude")]
#ex_may28_KUD.sp <- ex_may28_KUD.sp %>% rename(xlong = Longitude, ylat = Latitude)
#coordinates(ex_may28_KUD.sp) <- c("xlong", "ylat")

ex_Lingcodpre.sp <- ex_Lingcod.pre[, c("Transmitter", "Longitude", "Latitude")]
coordinates(ex_Lingcodpre.sp) <- c("Longitude", "Latitude")

ex_Lingcoddur.sp <- ex_Lingcod.during[, c("Transmitter", "Longitude", "Latitude")]
coordinates(ex_Lingcoddur.sp) <- c("Longitude", "Latitude")

ex_Lingcodpost.sp <- ex_Lingcod.post[, c("Transmitter", "Longitude", "Latitude")]
coordinates(ex_Lingcodpost.sp) <- c("Longitude", "Latitude")

## Set the coordinate reference system (CRS)
#proj4string(ex_may28_KUD.sp) <- CRS("+init=epsg:4326")
proj4string(ex_Lingcodpre.sp) <- CRS("+init=epsg:4326")
proj4string(ex_Lingcoddur.sp) <- CRS("+init=epsg:4326")
proj4string(ex_Lingcodpost.sp) <- CRS("+init=epsg:4326")

## create kernels, produce image by ID
kernel.ref.Lingcodpre <- kernelUD(ex_Lingcodpre.sp, h = "href")  # href = the reference bandwidth
#image(kernel.ref.Lingcod) # plot
Lingcodpre.poly <- getverticeshr(kernel.ref.Lingcodpre, percent = 95)
#image(kernel.ref.Lingcod[[1]])
#plot(getverticeshr(kernel.ref.Lingcod[[1]], 95), add = TRUE)
kernel.ref.Lingcoddur <- kernelUD(ex_Lingcoddur.sp, h = "href")
Lingcoddur.poly <- getverticeshr(kernel.ref.Lingcoddur, percent = 95)

kernel.ref.Lingcodpost <- kernelUD(ex_Lingcodpost.sp, h = "href")
Lingcodpost.poly <- getverticeshr(kernel.ref.Lingcodpost, percent = 95)

##create polygons
#ex_Lingcod_KUD.poly <- getverticeshr(kernel.ref.Lingcod, percent = 95)
df_Lingcod.sp <- fortify(Lingcod.poly)
g <- ggplot(df_Lingcod.sp, aes(x = long, y = lat, fill = id)) +
  geom_polygon(alpha = .4) +
  ggthemes::scale_fill_gdocs() +
  coord_equal() +
  theme_void()
g
g + facet_wrap(~id)



#Lingcod
animal.positions <- periods_Lingcod
animal.positions <- animal.positions %>% dplyr::group_by(Transmitter)
ex_Lingcod <- animal.positions[!is.na(animal.positions$Longitude) & !is.na(animal.positions$Latitude),]
ex_Lingcod.sp <- ex_Lingcod[, c("Transmitter", "Longitude", "Latitude")]
coordinates(ex_Lingcod.sp) <- c("Longitude", "Latitude")
proj4string(ex_Lingcod.sp) <- CRS("+init=epsg:4326")
kernel.ref.Lingcod <- kernelUD(ex_Lingcod.sp, h = "href")
Lingcod.poly <- getverticeshr(kernel.ref.Lingcod, percent = 95)

df_Lingcod.sp <- fortify(Lingcod.poly)
g <- ggplot(df_Lingcod.sp, aes(x = long, y = lat, fill = id)) +
  geom_polygon(alpha = .4) +
  ggthemes::scale_fill_gdocs() +
  coord_equal() +
  theme_void()
g
g + facet_wrap(~id)

image(kernel.ref.Lingcod)
image(kernel.ref.Lingcod[[1]])
plot(getverticeshr(kernel.ref.Lingcod[[1]], 95), add = TRUE)

image(kernel.ref.Lingcod)
plot(getverticeshr(kernel.ref.Lingcod, percent = 75), add = TRUE)

image(kernel.ref.Lingcod[[1]])
plot(getverticeshr(kernel.ref.Lingcod[[1]], 95), add = TRUE)


#Dungeness
animal.positions <- periods_Dungeness
animal.positions <- animal.positions %>% dplyr::group_by(Transmitter)
ex_Dungeness <- animal.positions[!is.na(animal.positions$Longitude) & !is.na(animal.positions$Latitude),]
ex_Dungeness.sp <- ex_Dungeness[, c("Transmitter", "Longitude", "Latitude")]
coordinates(ex_Dungeness.sp) <- c("Longitude", "Latitude")
proj4string(ex_Dungeness.sp) <- CRS("+init=epsg:4326")
kernel.ref.Dungeness <- kernelUD(ex_Dungeness.sp, h = "href")
Dungeness.poly <- getverticeshr(kernel.ref.Dungeness, percent = 95)

df_Dungeness.sp <- fortify(Dungeness.poly)
g <- ggplot(df_Dungeness.sp, aes(x = long, y = lat, fill = id)) +
  geom_polygon(alpha = .4) +
  ggthemes::scale_fill_gdocs() +
  coord_equal() +
  theme_void()
g
g + facet_wrap(~id)




#Black rockfish
BR.positions.before <- BlackR_accel %>% filter(periods == "Before") %>% filter(Transmitter == "A69-9007-12048")
BR.positions.before <- BR.positions.before %>% dplyr::group_by(Transmitter)
ex_BlackR <- BR.positions.before[!is.na(BR.positions.before$Longitude) & !is.na(BR.positions.before$Latitude),]
ex_BlackR.sp <- ex_BlackR[, c("Transmitter", "Longitude", "Latitude")]
coordinates(ex_BlackR.sp) <- c("Longitude", "Latitude")
proj4string(ex_BlackR.sp) <- CRS("+init=epsg:4326")

#####################################
preBRcoord <- coordinates(ex_BlackR.sp)
preBRcoord_xlong <- preBRcoord[,1]
preBRcoord_ylat <- preBRcoord[,2]
preBRcoord <- data.frame(preBRcoord_xlong, preBRcoord_ylat)
pre_xlong_max <- #max(preBRcoord$preBRcoord_xlong) 
pre_xlong_min <- #min(preBRcoord$preBRcoord_xlong)
pre_ylat_max <- #max(preBRcoord$preBRcoord_ylat) 
pre_ylat_min <- #min(preBRcoord$preBRcoord_ylat)

x <- seq(-124.5, -124.459, by=.01)        
y <-seq(42.684, 42.72, by=.01)
xy <- expand.grid(x=x,y=y)
coordinates(xy) <- ~x+y    
gridded(xy) <- TRUE

proj4string(xy) <- CRS("+init=epsg:4326")
as(xy, "Spatial")
############################################

kernel.ref.BlackR <- kernelUD(ex_BlackR.sp, h = "href")#, grid = xy, extent = 1)
BlackR.poly <- getverticeshr(kernel.ref.BlackR, percent = 95)

df_BlackR.sp <- fortify(BlackR.poly)
g <- ggplot(df_BlackR.sp, aes(x = long, y = lat, fill = id)) +
  geom_polygon(alpha = .4) +
  ggthemes::scale_fill_gdocs() +
  coord_equal() +
  theme_void()
g
g + facet_wrap(~id)


############################################################





#China rockfish
animal.positions <- ChinaR_accel %>% filter(Transmitter == "A69-9007-13269") %>% filter(periods == "After")
animal.positions <- animal.positions %>% dplyr::group_by(Transmitter)
ex_ChinaR <- animal.positions[!is.na(animal.positions$Longitude) & !is.na(animal.positions$Latitude),]
ex_ChinaR.sp <- ex_ChinaR[, c("Transmitter", "Longitude", "Latitude")]
coordinates(ex_ChinaR.sp) <- c("Longitude", "Latitude")
proj4string(ex_ChinaR.sp) <- CRS("+init=epsg:4326")
kernel.ref.ChinaR <- kernelUD(ex_ChinaR.sp, h = "href")
ChinaR.poly <- getverticeshr(kernel.ref.ChinaR, percent = 95)

df_ChinaR.sp <- fortify(ChinaR.poly)
g <- ggplot(df_ChinaR.sp, aes(x = long, y = lat, fill = id)) +
  geom_polygon(alpha = .4) +
  ggthemes::scale_fill_gdocs() +
  coord_equal() +
  theme_void()
g
g + facet_wrap(~id)









#example vps home range in pre period, china
## define time period for pre period
June11_start <- ymd_hms("2021-06-11 00:00:01", tz = "US/Pacific")
June11_end <- ymd_hms("2021-06-11 23:59:59", tz = "US/Pacific")

## subset data from pre period, china rockfish as example for pre-noise period
ex_pre_KUD <- animal.positions %>% dplyr::filter(Time < June11_start) %>% dplyr::filter(Id == 13272 | Id == 13277 | Id == 13278)

## remove rows with NAs
ex_pre_KUD <- ex_pre_KUD[!is.na(ex_pre_KUD$Longitude) & !is.na(ex_pre_KUD$Latitude),]
## remove animals with less than 5 relocations (constraint of KUD function)
ex_pre_KUD <- ex_pre_KUD %>% group_by(Id) %>% filter(n() > 5)

## Create a copy of the object to make into a SpatialPointsDataFrame
### Only include three columns (id, x, and y coordinates) for estimating home ranges
ex_pre_KUD.sp <- ex_pre_KUD[, c("Id", "Longitude", "Latitude")]
ex_pre_KUD.sp <- ex_pre_KUD.sp %>% rename(xlong = Longitude, ylat = Latitude)
coordinates(ex_pre_KUD.sp) <- c("xlong", "ylat")

## Set the coordinate reference system (CRS)
proj4string(ex_pre_KUD.sp) <- CRS("+init=epsg:4326")

## create kernels, produce image by ID
kernel.ref.pre <- kernelUD(ex_pre_KUD.sp, h = "href")  # href = the reference bandwidth
image(kernel.ref.pre) # plot


#plotting example vps home ranges in pre period
## create polygons
ex_pre_KUD.poly <- getverticeshr(kernel.ref.pre, percent = 95)

## create colors by animal
color <- rep("green", nrow(ex_pre_KUD.sp@data))
#color[(ex_pre_KUD.sp@data$Id == 13277)] <- "red"
#color[(ex_pre_KUD.sp@data$Id == 13278)] <- "blue"
plot(ex_pre_KUD.poly, col = ex_pre_KUD.poly@data$Id)
plot(ex_pre_KUD.sp, add = TRUE, col = color, pch = 21)
#plot(mapPortO, add = TRUE)



#example vps home range in pre period, lingcod

## subset data from pre period, lingcod as example for pre-noise period
ex_pre_KUD <- animal.positions %>% dplyr::filter(Time < June11_start) %>% dplyr::filter(Id == 13249 | Id == 13260)

## remove rows with NAs
ex_pre_KUD <- ex_pre_KUD[!is.na(ex_pre_KUD$Longitude) & !is.na(ex_pre_KUD$Latitude),]
## remove animals with less than 5 relocations (constraint of KUD function)
ex_pre_KUD <- ex_pre_KUD %>% group_by(Id) %>% filter(n() > 5)

## Create a copy of the object to make into a SpatialPointsDataFrame
### Only include three columns (id, x, and y coordinates) for estimating home ranges
ex_pre_KUD.sp <- ex_pre_KUD[, c("Id", "Longitude", "Latitude")]
ex_pre_KUD.sp <- ex_pre_KUD.sp %>% rename(xlong = Longitude, ylat = Latitude)
coordinates(ex_pre_KUD.sp) <- c("xlong", "ylat")

## Set the coordinate reference system (CRS)
proj4string(ex_pre_KUD.sp) <- CRS("+init=epsg:4326")

## create kernels, produce image by ID
kernel.ref.pre <- kernelUD(ex_pre_KUD.sp, h = "href")  # href = the reference bandwidth
image(kernel.ref.pre) # plot


#plotting example vps home ranges in pre period
## create polygons
ex_pre_KUD.poly <- getverticeshr(kernel.ref.pre, percent = 95)

## create colors by animal
color <- rep("green", nrow(ex_pre_KUD.sp@data))
#color[(ex_pre_KUD.sp@data$Id == 13277)] <- "red"
#color[(ex_pre_KUD.sp@data$Id == 13278)] <- "blue"
plot(ex_pre_KUD.poly, col = ex_pre_KUD.poly@data$Id)
plot(ex_pre_KUD.sp, add = TRUE, col = color, pch = 21)
#plot(mapPortO, add = TRUE)




#example vps home range in pre period, dungeness

## subset data from pre period, dungeness as example for pre-noise period
ex_pre_KUD_D <- animal.positions %>% dplyr::filter(Time < June11_start) %>% dplyr::filter(Id >= 13279 & Id <= 13293)
ex_dur_KUD_D <- animal.positions %>% dplyr::filter(Time >= June11_start & Time <= June18_end) %>% dplyr::filter(Id >= 13279 & Id <= 13293)
ex_post_KUD_D <- animal.positions %>% dplyr::filter(Time > June18_end) %>% dplyr::filter(Id >= 13279 & Id <= 13293)

ex_pre_KUD_D <- ex_pre_KUD_D[!is.na(ex_pre_KUD_D$Longitude) & !is.na(ex_pre_KUD_D$Latitude),]
ex_dur_KUD_D <- ex_dur_KUD_D[!is.na(ex_dur_KUD_D$Longitude) & !is.na(ex_dur_KUD_D$Latitude),]
ex_post_KUD_D <- ex_post_KUD_D[!is.na(ex_post_KUD_D$Longitude) & !is.na(ex_post_KUD_D$Latitude),]
## remove animals with less than 5 relocations (constraint of KUD function)
ex_pre_KUD_D <- ex_pre_KUD_D %>% group_by(Id) %>% filter(n() > 5)
ex_dur_KUD_D <- ex_dur_KUD_D %>% group_by(Id) %>% filter(n() > 5)
ex_post_KUD_D <- ex_post_KUD_D %>% group_by(Id) %>% filter(n() > 5)

## Create a copy of the object to make into a SpatialPointsDataFrame
### Only include three columns (id, x, and y coordinates) for estimating home ranges
ex_pre_KUD_D.sp <- ex_pre_KUD_D[, c("Id", "Longitude", "Latitude")]
ex_pre_KUD_D.sp <- ex_pre_KUD_D.sp %>% rename(xlong = Longitude, ylat = Latitude)
coordinates(ex_pre_KUD_D.sp) <- c("xlong", "ylat")

ex_dur_KUD_D.sp <- ex_dur_KUD_D[, c("Id", "Longitude", "Latitude")]
ex_dur_KUD_D.sp <- ex_dur_KUD_D.sp %>% rename(xlong = Longitude, ylat = Latitude)
coordinates(ex_dur_KUD_D.sp) <- c("xlong", "ylat")

ex_post_KUD_D.sp <- ex_post_KUD_D[, c("Id", "Longitude", "Latitude")]
ex_post_KUD_D.sp <- ex_post_KUD_D.sp %>% rename(xlong = Longitude, ylat = Latitude)
coordinates(ex_post_KUD_D.sp) <- c("xlong", "ylat")

## Set the coordinate reference system (CRS)
proj4string(ex_pre_KUD_D.sp) <- CRS("+init=epsg:4326")
proj4string(ex_dur_KUD_D.sp) <- CRS("+init=epsg:4326")
proj4string(ex_post_KUD_D.sp) <- CRS("+init=epsg:4326")

## create kernels, produce image by ID
kernel.ref.pre.D <- kernelUD(ex_pre_KUD_D.sp, h = "href") 
kernel.ref.dur.D <- kernelUD(ex_dur_KUD_D.sp, h = "href") 
kernel.ref.post.D <- kernelUD(ex_post_KUD_D.sp, h = "href") # href = the reference bandwidth
image(kernel.ref.pre) # plot


#plotting example vps home ranges in pre period
## create polygons
ex_pre_KUD_D.poly <- getverticeshr(kernel.ref.pre.D, percent = 95)
ex_dur_KUD_D.poly <- getverticeshr(kernel.ref.dur.D, percent = 95)
ex_post_KUD_D.poly <- getverticeshr(kernel.ref.post.D, percent = 95)


D_KUD_area_pre <- as_tibble(ex_pre_KUD_D.poly)
D_KUD_area_during <- as_tibble(ex_dur_KUD_D.poly)
D_KUD_area_post <- as_tibble(ex_post_KUD_D.poly)

D_KUD_area_pre <- D_KUD_area_pre %>% mutate(survey.period = "pre")
D_KUD_area_during <- D_KUD_area_during %>% mutate(survey.period = "during")
D_KUD_area_post <- D_KUD_area_post %>% mutate(survey.period = "post")


ex_KUD_D_all <- bind_rows(D_KUD_area_during, D_KUD_area_post)
#ex_KUD_D_all <- ex_KUD_D_all %>% filter(!(id == "13280" | id == "12070" | id == "12071" | id == "12076" | id == "12077"))

t.test(area ~ survey.period, data = ex_KUD_D_all)

## create colors by animal
color <- rep("green", nrow(ex_pre_KUD.sp@data))
#color[(ex_pre_KUD.sp@data$Id == 13277)] <- "red"
#color[(ex_pre_KUD.sp@data$Id == 13278)] <- "blue"
plot(ex_pre_KUD.poly, col = ex_pre_KUD.poly@data$Id)
plot(ex_pre_KUD.sp, add = TRUE, col = color, pch = 21)
#plot(mapPortO, add = TRUE)




#example vps home range in pre period, black rockfish ###############################################################

## subset data from pre period, black rockfish as example for pre-noise period
ex_pre_KUD_BR <- animal.positions %>% dplyr::filter(Date.time.UTC < June11_start) %>% dplyr::filter(Transmitter >= 12048 & Transmitter <= 12077)
ex_dur_KUD_BR <- animal.positions %>% dplyr::filter(Date.time.UTC >= June11_start & Date.time.UTC <= June18_end) %>% dplyr::filter(Transmitter >= 12048 & Transmitter <= 12077)
ex_post_KUD_BR <- animal.positions %>% dplyr::filter(Date.time.UTC > June18_end) %>% dplyr::filter(Transmitter >= 12048 & Transmitter <= 12077)

## remove rows with NAs
ex_pre_KUD_BR <- ex_pre_KUD_BR[!is.na(ex_pre_KUD_BR$Longitude) & !is.na(ex_pre_KUD_BR$Latitude),]
ex_dur_KUD_BR <- ex_dur_KUD_BR[!is.na(ex_dur_KUD_BR$Longitude) & !is.na(ex_dur_KUD_BR$Latitude),]
ex_post_KUD_BR <- ex_post_KUD_BR[!is.na(ex_post_KUD_BR$Longitude) & !is.na(ex_post_KUD_BR$Latitude),]
## remove animals with less than 5 relocations (constraint of KUD function)
ex_pre_KUD_BR <- ex_pre_KUD_BR %>% group_by(Transmitter) %>% filter(n() > 5)
ex_dur_KUD_BR <- ex_dur_KUD_BR %>% group_by(Transmitter) %>% filter(n() > 5)
ex_post_KUD_BR <- ex_post_KUD_BR %>% group_by(Transmitter) %>% filter(n() > 5)

## Create a copy of the object to make into a SpatialPointsDataFrame
### Only include three columns (id, x, and y coordinates) for estimating home ranges
ex_pre_KUD_BR.sp <- ex_pre_KUD_BR[, c("Transmitter", "Longitude", "Latitude")]
ex_pre_KUD_BR.sp <- ex_pre_KUD_BR.sp %>% rename(xlong = Longitude, ylat = Latitude)
coordinates(ex_pre_KUD_BR.sp) <- c("xlong", "ylat")

ex_dur_KUD_BR.sp <- ex_dur_KUD_BR[, c("Transmitter", "Longitude", "Latitude")]
ex_dur_KUD_BR.sp <- ex_dur_KUD_BR.sp %>% rename(xlong = Longitude, ylat = Latitude)
coordinates(ex_dur_KUD_BR.sp) <- c("xlong", "ylat")

ex_post_KUD_BR.sp <- ex_post_KUD_BR[, c("Transmitter", "Longitude", "Latitude")]
ex_post_KUD_BR.sp <- ex_post_KUD_BR.sp %>% rename(xlong = Longitude, ylat = Latitude)
coordinates(ex_post_KUD_BR.sp) <- c("xlong", "ylat")


preBRcoord <- coordinates(ex_pre_KUD_BR.sp)
preBRcoord_xlong <- preBRcoord[,1]
preBRcoord_ylat <- preBRcoord[,2]
preBRcoord <- data.frame(preBRcoord_xlong, preBRcoord_ylat)
pre_xlong_max <- max(preBRcoord$preBRcoord_xlong) + 1
pre_xlong_min <- min(preBRcoord$preBRcoord_xlong) - 1
pre_ylat_max <- max(preBRcoord$preBRcoord_ylat) + 1
pre_ylat_min <- min(preBRcoord$preBRcoord_ylat) - 1

## Set the coordinate reference system (CRS)
proj4string(ex_pre_KUD_BR.sp) <- CRS("+init=epsg:4326")
proj4string(ex_dur_KUD_BR.sp) <- CRS("+init=epsg:4326")
proj4string(ex_post_KUD_BR.sp) <- CRS("+init=epsg:4326")

# Domain              
x <- seq(pre_xlong_min, pre_xlong_max, by=.001)        
y <-seq(pre_ylat_min, pre_ylat_max, by=.001)
xy <- expand.grid(x=x,y=y)
coordinates(xy) <- ~x+y    
gridded(xy) <- TRUE
class(xy)
proj4string(xy) <- CRS("+init=epsg:4326")
as(xy, "Spatial")

## create kernels, produce image by ID
kernel.ref.pre <- kernelUD(ex_pre_KUD_BR.sp, h = "href", grid = xy, extent = 2,  same4all =  FALSE)  # href = the reference bandwidth
image(kernel.ref.pre) # plot

kernel.ref.dur <- kernelUD(ex_dur_KUD_BR.sp, h = "href") 
kernel.ref.post <- kernelUD(ex_post_KUD_BR.sp, h = "href") 


#plotting example vps home ranges in pre period
## create polygons
ex_pre_KUD_BR.poly <- getverticeshr(kernel.ref.pre, percent = 95)
ex_dur_KUD_BR.poly <- getverticeshr(kernel.ref.dur, percent = 95)
ex_post_KUD_BR.poly <- getverticeshr(kernel.ref.post, percent = 95)


BR_KUD_area_during <- as_tibble(ex_dur_KUD_BR.poly)
BR_KUD_area_post <- as_tibble(ex_post_KUD_BR.poly)

BR_KUD_area_during <- BR_KUD_area_during %>% mutate(survey.period = "during")
BR_KUD_area_post <- BR_KUD_area_post %>% mutate(survey.period = "post")

ex_KUD_BR_all <- bind_rows(BR_KUD_area_during, BR_KUD_area_post)
ex_KUD_BR_all <- ex_KUD_BR_all %>% filter(!(id == "12069" | id == "12070" | id == "12071" | id == "12076" | id == "12077"))

  
  
## create colors by animal
color <- rep("green", nrow(ex_pre_KUD.sp@data))
#color[(ex_pre_KUD.sp@data$Id == 13277)] <- "red"
#color[(ex_pre_KUD.sp@data$Id == 13278)] <- "blue"
plot(ex_pre_KUD.poly, col = ex_pre_KUD.poly@data$Id)
plot(ex_pre_KUD.sp, add = TRUE, col = color, pch = 21)
#plot(mapPortO, add = TRUE)



t.test(area ~ survey.period, data = ex_KUD_BR_all, paired = TRUE)





#example vps home range in coarse during period, dungeness

## subset data from pre period, dungeness as example for pre-noise period
ex_pre_KUD <- animal.positions %>% dplyr::filter(Time >= June11_start & Time <= June18_end) %>% dplyr::filter(Id >= 13279 & Id <= 13293)

## remove rows with NAs
ex_pre_KUD <- ex_pre_KUD[!is.na(ex_pre_KUD$Longitude) & !is.na(ex_pre_KUD$Latitude),]
## remove animals with less than 5 relocations (constraint of KUD function)
ex_pre_KUD <- ex_pre_KUD %>% group_by(Id) %>% filter(n() > 5)

## Create a copy of the object to make into a SpatialPointsDataFrame
### Only include three columns (id, x, and y coordinates) for estimating home ranges
ex_pre_KUD.sp <- ex_pre_KUD[, c("Id", "Longitude", "Latitude")]
ex_pre_KUD.sp <- ex_pre_KUD.sp %>% rename(xlong = Longitude, ylat = Latitude)
coordinates(ex_pre_KUD.sp) <- c("xlong", "ylat")

## Set the coordinate reference system (CRS)
proj4string(ex_pre_KUD.sp) <- CRS("+init=epsg:4326")

## create kernels, produce image by ID
kernel.ref.pre <- kernelUD(ex_pre_KUD.sp, h = "href")  # href = the reference bandwidth
image(kernel.ref.pre) # plot


#plotting example vps home ranges in pre period
## create polygons
ex_pre_KUD.poly <- getverticeshr(kernel.ref.pre, percent = 95)

## create colors by animal
color <- rep("green", nrow(ex_pre_KUD.sp@data))
#color[(ex_pre_KUD.sp@data$Id == 13277)] <- "red"
#color[(ex_pre_KUD.sp@data$Id == 13278)] <- "blue"
plot(ex_pre_KUD.poly, col = ex_pre_KUD.poly@data$Id)
plot(ex_pre_KUD.sp, add = TRUE, col = color, pch = 21)
#plot(mapPortO, add = TRUE)




#example vps home range in after period, dungeness

## subset data from pre period, dungeness as example for pre-noise period
ex_pre_KUD <- animal.positions %>% dplyr::filter(Time > June18_end) %>% dplyr::filter(Id >= 13279 & Id <= 13293)

## remove rows with NAs
ex_pre_KUD <- ex_pre_KUD[!is.na(ex_pre_KUD$Longitude) & !is.na(ex_pre_KUD$Latitude),]
## remove animals with less than 5 relocations (constraint of KUD function)
ex_pre_KUD <- ex_pre_KUD %>% group_by(Id) %>% filter(n() > 5)

## Create a copy of the object to make into a SpatialPointsDataFrame
### Only include three columns (id, x, and y coordinates) for estimating home ranges
ex_pre_KUD.sp <- ex_pre_KUD[, c("Id", "Longitude", "Latitude")]
ex_pre_KUD.sp <- ex_pre_KUD.sp %>% rename(xlong = Longitude, ylat = Latitude)
coordinates(ex_pre_KUD.sp) <- c("xlong", "ylat")

## Set the coordinate reference system (CRS)
proj4string(ex_pre_KUD.sp) <- CRS("+init=epsg:4326")

## create kernels, produce image by ID
kernel.ref.pre <- kernelUD(ex_pre_KUD.sp, h = "href")  # href = the reference bandwidth
image(kernel.ref.pre) # plot


#plotting example vps home ranges in pre period
## create polygons
ex_pre_KUD.poly <- getverticeshr(kernel.ref.pre, percent = 95)

## create colors by animal
color <- rep("green", nrow(ex_pre_KUD.sp@data))
#color[(ex_pre_KUD.sp@data$Id == 13277)] <- "red"
#color[(ex_pre_KUD.sp@data$Id == 13278)] <- "blue"
plot(ex_pre_KUD.poly, col = ex_pre_KUD.poly@data$Id)
plot(ex_pre_KUD.sp, add = TRUE, col = color, pch = 21)
#plot(mapPortO, add = TRUE)






# KDE analysis with sf package 
library(sf)
nc <- st_read(system.file("shape/nc.shp", package="sf")) %>% st_transform(32031)
grid <- create_grid_hexagonal(nc, cell_size = 100000)
points <- st_sample(nc, 500) %>% st_as_sf()
kde_estimate_grid <- kde(points, band_width = 150000, grid = grid)
raster <- create_raster(nc, cell_size = 100000)
kde_estimate_raster <- kde(points, band_width = 150000, grid = raster)


KDE_Lingcod <- kde(x=Longitude, binned=TRUE, compute.cont=TRUE, xmin=-124.485, xmax=-124.47, bgridsize=c(151,151))
plot(fhat)

library(ks)
library(rgl)
kern <- read.table(file.choose(), sep=",")
ex_L_KDE <- kde(ex_Lingcod.sp)






#######
