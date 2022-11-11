library(maptools)
## Before
#China rockfish

##64 ########################
CR.positions.b64 <- ChinaR_accel %>% filter(periods== "Before")%>% filter(Transmitter == "A69-9007-13264")
CR.positions.b64 <- CR.positions.b64 %>% dplyr::group_by(Transmitter)
b64_ChinaR <- CR.positions.b64[!is.na(CR.positions.b64$Longitude) & !is.na(CR.positions.b64$Latitude),]
b64_ChinaR.sp <- b64_ChinaR[, c("Transmitter", "Longitude", "Latitude")]
coordinates(b64_ChinaR.sp) <- c("Longitude", "Latitude")
proj4string(b64_ChinaR.sp) <- CRS("+init=epsg:4326")

kernel.b64.ChinaR <- kernelUD(b64_ChinaR.sp, h = "href")
ChinaR.b64.poly <- getverticeshr(kernel.b64.ChinaR, percent = 95)

ChinaR.b64.sp <- fortify(ChinaR.b64.poly)
ChinaR.b64.df <- as.data.frame(ChinaR.b64.poly)

##65 ################
CR.positions.b65 <- ChinaR_accel %>% filter(periods== "Before")%>% filter(Transmitter == "A69-9007-13265")
CR.positions.b65 <- CR.positions.b65 %>% dplyr::group_by(Transmitter)
b65_ChinaR <- CR.positions.b65[!is.na(CR.positions.b65$Longitude) & !is.na(CR.positions.b65$Latitude),]
b65_ChinaR.sp <- b65_ChinaR[, c("Transmitter", "Longitude", "Latitude")]
coordinates(b65_ChinaR.sp) <- c("Longitude", "Latitude")
proj4string(b65_ChinaR.sp) <- CRS("+init=epsg:4326")

kernel.b65.ChinaR <- kernelUD(b65_ChinaR.sp, h = "href")
ChinaR.b65.poly <- getverticeshr(kernel.b65.ChinaR, percent = 95)

ChinaR.b65.sp <- fortify(ChinaR.b65.poly)
ChinaR.b65.df <- as.data.frame(ChinaR.b65.poly)


##66 ##################### - works with ALL dataset
CR.positions.b66 <- ChinaR_accel %>% filter(periods== "Before")%>% filter(Transmitter == "A69-9007-13266")
CR.positions.b66 <- CR.positions.b66 %>% dplyr::group_by(Transmitter)
b66_ChinaR <- CR.positions.b66[!is.na(CR.positions.b66$Longitude) & !is.na(CR.positions.b66$Latitude),]
b66_ChinaR.sp <- b66_ChinaR[, c("Transmitter", "Longitude", "Latitude")]
coordinates(b66_ChinaR.sp) <- c("Longitude", "Latitude")
proj4string(b66_ChinaR.sp) <- CRS("+init=epsg:4326")

kernel.b66.ChinaR <- kernelUD(b66_ChinaR.sp, h = "href")
ChinaR.b66.poly <- getverticeshr(kernel.b66.ChinaR, percent = 95)

ChinaR.b66.sp <- fortify(ChinaR.b66.poly)
ChinaR.b66.df <- as.data.frame(ChinaR.b66.poly)


##67
CR.positions.b67 <- ChinaR_accel %>% filter(periods== "Before")%>% filter(Transmitter == "A69-9007-13267")
CR.positions.b67 <- CR.positions.b67 %>% dplyr::group_by(Transmitter)
b67_ChinaR <- CR.positions.b67[!is.na(CR.positions.b67$Longitude) & !is.na(CR.positions.b67$Latitude),]
b67_ChinaR.sp <- b67_ChinaR[, c("Transmitter", "Longitude", "Latitude")]
coordinates(b67_ChinaR.sp) <- c("Longitude", "Latitude")
proj4string(b67_ChinaR.sp) <- CRS("+init=epsg:4326")

kernel.b67.ChinaR <- kernelUD(b67_ChinaR.sp, h = "href")
ChinaR.b67.poly <- getverticeshr(kernel.b67.ChinaR, percent = 95)

ChinaR.b67.sp <- fortify(ChinaR.b67.poly)
ChinaR.b67.df <- as.data.frame(ChinaR.b67.poly)

kmlPolygons(obj = ChinaR.b67.poly, kmlfile="ChinaR.b67.kml", col = "blue") # write kml file


##68 ###################################
CR.positions.b68 <- ChinaR_accel %>% filter(periods== "Before")%>% filter(Transmitter == "A69-9007-13268")
CR.positions.b68 <- CR.positions.b68 %>% dplyr::group_by(Transmitter)
b68_ChinaR <- CR.positions.b68[!is.na(CR.positions.b68$Longitude) & !is.na(CR.positions.b68$Latitude),]
b68_ChinaR.sp <- b68_ChinaR[, c("Transmitter", "Longitude", "Latitude")]
coordinates(b68_ChinaR.sp) <- c("Longitude", "Latitude")
proj4string(b68_ChinaR.sp) <- CRS("+init=epsg:4326")

kernel.b68.ChinaR <- kernelUD(b68_ChinaR.sp, h = "href")
ChinaR.b68.poly <- getverticeshr(kernel.b68.ChinaR, percent = 95)

ChinaR.b68.sp <- fortify(ChinaR.b68.poly)
ChinaR.b68.df <- as.data.frame(ChinaR.b68.poly)

##69 ############################ - works with ALL dataset
CR.positions.b69 <- ChinaR_accel %>% filter(periods== "Before")%>% filter(Transmitter == "A69-9007-13269")
CR.positions.b69 <- CR.positions.b69 %>% dplyr::group_by(Transmitter)
b69_ChinaR <- CR.positions.b69[!is.na(CR.positions.b69$Longitude) & !is.na(CR.positions.b69$Latitude),]
b69_ChinaR.sp <- b69_ChinaR[, c("Transmitter", "Longitude", "Latitude")]
coordinates(b69_ChinaR.sp) <- c("Longitude", "Latitude")
proj4string(b69_ChinaR.sp) <- CRS("+init=epsg:4326")

kernel.b69.ChinaR <- kernelUD(b69_ChinaR.sp, h = "href")
ChinaR.b69.poly <- getverticeshr(kernel.b69.ChinaR, percent = 95)

ChinaR.b69.sp <- fortify(ChinaR.b69.poly)
ChinaR.b69.df <- as.data.frame(ChinaR.b69.poly)

##70 ###################################3
CR.positions.b70 <- ChinaR_accel %>% filter(periods== "Before")%>% filter(Transmitter == "A69-9007-13270")
CR.positions.b70 <- CR.positions.b70 %>% dplyr::group_by(Transmitter)
b70_ChinaR <- CR.positions.b70[!is.na(CR.positions.b70$Longitude) & !is.na(CR.positions.b70$Latitude),]
b70_ChinaR.sp <- b70_ChinaR[, c("Transmitter", "Longitude", "Latitude")]
coordinates(b70_ChinaR.sp) <- c("Longitude", "Latitude")
proj4string(b70_ChinaR.sp) <- CRS("+init=epsg:4326")

kernel.b70.ChinaR <- kernelUD(b70_ChinaR.sp, h = "href")
ChinaR.b70.poly <- getverticeshr(kernel.b70.ChinaR, percent = 95)

ChinaR.b70.sp <- fortify(ChinaR.b70.poly)
ChinaR.b70.df <- as.data.frame(ChinaR.b70.poly)

##71 ############################### - works with ALL dataset
CR.positions.b71 <- ChinaR_accel %>% filter(periods== "Before")%>% filter(Transmitter == "A69-9007-13271")
CR.positions.b71 <- CR.positions.b71 %>% dplyr::group_by(Transmitter)
b71_ChinaR <- CR.positions.b71[!is.na(CR.positions.b71$Longitude) & !is.na(CR.positions.b71$Latitude),]
b71_ChinaR.sp <- b71_ChinaR[, c("Transmitter", "Longitude", "Latitude")]
coordinates(b71_ChinaR.sp) <- c("Longitude", "Latitude")
proj4string(b71_ChinaR.sp) <- CRS("+init=epsg:4326")

kernel.b71.ChinaR <- kernelUD(b71_ChinaR.sp, h = "href")
ChinaR.b71.poly <- getverticeshr(kernel.b71.ChinaR, percent = 95)

ChinaR.b71.sp <- fortify(ChinaR.b71.poly)
ChinaR.b71.df <- as.data.frame(ChinaR.b71.poly)


##72
CR.positions.b72 <- ChinaR_accel %>% filter(periods== "Before")%>% filter(Transmitter == "A69-9007-13272")
CR.positions.b72 <- CR.positions.b72 %>% dplyr::group_by(Transmitter)
b72_ChinaR <- CR.positions.b72[!is.na(CR.positions.b72$Longitude) & !is.na(CR.positions.b72$Latitude),]
b72_ChinaR.sp <- b72_ChinaR[, c("Transmitter", "Longitude", "Latitude")]
coordinates(b72_ChinaR.sp) <- c("Longitude", "Latitude")
proj4string(b72_ChinaR.sp) <- CRS("+init=epsg:4326")

kernel.b72.ChinaR <- kernelUD(b72_ChinaR.sp, h = "href")
ChinaR.b72.poly <- getverticeshr(kernel.b72.ChinaR, percent = 95)

ChinaR.b72.sp <- fortify(ChinaR.b72.poly)
ChinaR.b72.df <- as.data.frame(ChinaR.b72.poly)

kmlPolygons(obj = ChinaR.b72.poly, kmlfile="ChinaR.b72.kml", col = "blue")

##73 ################################### - works with ALL dataset
CR.positions.b73 <- ChinaR_accel %>% filter(periods== "Before")%>% filter(Transmitter == "A69-9007-13273")
CR.positions.b73 <- CR.positions.b73 %>% dplyr::group_by(Transmitter)
b73_ChinaR <- CR.positions.b73[!is.na(CR.positions.b73$Longitude) & !is.na(CR.positions.b73$Latitude),]
b73_ChinaR.sp <- b73_ChinaR[, c("Transmitter", "Longitude", "Latitude")]
coordinates(b73_ChinaR.sp) <- c("Longitude", "Latitude")
proj4string(b73_ChinaR.sp) <- CRS("+init=epsg:4326")

kernel.b73.ChinaR <- kernelUD(b73_ChinaR.sp, h = "href")
ChinaR.b73.poly <- getverticeshr(kernel.b73.ChinaR, percent = 95)

ChinaR.b73.sp <- fortify(ChinaR.b73.poly)
ChinaR.b73.df <- as.data.frame(ChinaR.b73.poly)

##74 ############################ - works with ALL dataset
CR.positions.b74 <- ChinaR_accel %>% filter(periods== "Before")%>% filter(Transmitter == "A69-9007-13274")
CR.positions.b74 <- CR.positions.b74 %>% dplyr::group_by(Transmitter)
b74_ChinaR <- CR.positions.b74[!is.na(CR.positions.b74$Longitude) & !is.na(CR.positions.b74$Latitude),]
b74_ChinaR.sp <- b74_ChinaR[, c("Transmitter", "Longitude", "Latitude")]
coordinates(b74_ChinaR.sp) <- c("Longitude", "Latitude")
proj4string(b74_ChinaR.sp) <- CRS("+init=epsg:4326")

kernel.b74.ChinaR <- kernelUD(b74_ChinaR.sp, h = "href")
ChinaR.b74.poly <- getverticeshr(kernel.b74.ChinaR, percent = 95)

ChinaR.b74.sp <- fortify(ChinaR.b74.poly)
ChinaR.b74.df <- as.data.frame(ChinaR.b74.poly)


##75 ################################### - works with ALL dataset
CR.positions.b75 <- ChinaR_accel %>% filter(periods== "Before")%>% filter(Transmitter == "A69-9007-13275")
CR.positions.b75 <- CR.positions.b75 %>% dplyr::group_by(Transmitter)
b75_ChinaR <- CR.positions.b75[!is.na(CR.positions.b75$Longitude) & !is.na(CR.positions.b75$Latitude),]
b75_ChinaR.sp <- b75_ChinaR[, c("Transmitter", "Longitude", "Latitude")]
coordinates(b75_ChinaR.sp) <- c("Longitude", "Latitude")
proj4string(b75_ChinaR.sp) <- CRS("+init=epsg:4326")

kernel.b75.ChinaR <- kernelUD(b75_ChinaR.sp, h = "href")
ChinaR.b75.poly <- getverticeshr(kernel.b75.ChinaR, percent = 95)

ChinaR.b75.sp <- fortify(ChinaR.b75.poly)
ChinaR.b75.df <- as.data.frame(ChinaR.b75.poly)


##76 ############################ - works with ALL Dataset
CR.positions.b76 <- ChinaR_accel %>% filter(periods== "Before")%>% filter(Transmitter == "A69-9007-13276")
CR.positions.b76 <- CR.positions.b76 %>% dplyr::group_by(Transmitter)
b76_ChinaR <- CR.positions.b76[!is.na(CR.positions.b76$Longitude) & !is.na(CR.positions.b76$Latitude),]
b76_ChinaR.sp <- b76_ChinaR[, c("Transmitter", "Longitude", "Latitude")]
coordinates(b76_ChinaR.sp) <- c("Longitude", "Latitude")
proj4string(b76_ChinaR.sp) <- CRS("+init=epsg:4326")

kernel.b76.ChinaR <- kernelUD(b76_ChinaR.sp, h = "href")
ChinaR.b76.poly <- getverticeshr(kernel.b76.ChinaR, percent = 95)

ChinaR.b76.sp <- fortify(ChinaR.b76.poly)
ChinaR.b76.df <- as.data.frame(ChinaR.b76.poly)


##77
CR.positions.b77 <- ChinaR_accel %>% filter(periods== "Before")%>% filter(Transmitter == "A69-9007-13277")
CR.positions.b77 <- CR.positions.b77 %>% dplyr::group_by(Transmitter)
b77_ChinaR <- CR.positions.b77[!is.na(CR.positions.b77$Longitude) & !is.na(CR.positions.b77$Latitude),]
b77_ChinaR.sp <- b77_ChinaR[, c("Transmitter", "Longitude", "Latitude")]
coordinates(b77_ChinaR.sp) <- c("Longitude", "Latitude")
proj4string(b77_ChinaR.sp) <- CRS("+init=epsg:4326")

kernel.b77.ChinaR <- kernelUD(b77_ChinaR.sp, h = "href")
ChinaR.b77.poly <- getverticeshr(kernel.b77.ChinaR, percent = 95)

ChinaR.b77.sp <- fortify(ChinaR.b77.poly)
ChinaR.b77.df <- as.data.frame(ChinaR.b77.poly)

kmlPolygons(obj = ChinaR.b77.poly, kmlfile="ChinaR.b77.kml", col = "blue")

##78
CR.positions.b78 <- ChinaR_accel %>% filter(periods== "Before")%>% filter(Transmitter == "A69-9007-13278")
CR.positions.b78 <- CR.positions.b78 %>% dplyr::group_by(Transmitter)
b78_ChinaR <- CR.positions.b78[!is.na(CR.positions.b78$Longitude) & !is.na(CR.positions.b78$Latitude),]
b78_ChinaR.sp <- b78_ChinaR[, c("Transmitter", "Longitude", "Latitude")]
coordinates(b78_ChinaR.sp) <- c("Longitude", "Latitude")
proj4string(b78_ChinaR.sp) <- CRS("+init=epsg:4326")

kernel.b78.ChinaR <- kernelUD(b78_ChinaR.sp, h = "href")
ChinaR.b78.poly <- getverticeshr(kernel.b78.ChinaR, percent = 95)

ChinaR.b78.sp <- fortify(ChinaR.b78.poly)
ChinaR.b78.df <- as.data.frame(ChinaR.b78.poly)

kmlPolygons(obj = ChinaR.b78.poly, kmlfile="ChinaR.b78.kml", col = "blue")


ChinaR.before.df <- bind_rows(#ChinaR.b66.df, 
                              #ChinaR.b67.df, 
                              #ChinaR.b69.df,
                              #ChinaR.b71.df,
                              ChinaR.b72.df, 
                              #ChinaR.b73.df,
                              #ChinaR.b74.df,
                              #ChinaR.b75.df,
                              #ChinaR.b76.df,
                              ChinaR.b77.df,
                              ChinaR.b78.df)
ChinaR.before.df <- ChinaR.before.df %>% rename(area.before = area)

ChinaR.before.poly <- bind(ChinaR.b67.poly, ChinaR.b69.poly, ChinaR.b72.poly,
                           ChinaR.b77.poly, ChinaR.b78.poly)

kmlPolygons(obj = ChinaR.before.poly, kmlfile="ChinaR.before.kml", col = "blue")


## During
### 64 ################
CR.positions.d64 <- ChinaR_accel %>% filter(periods== "During")%>% filter(Transmitter == "A69-9007-13264")
CR.positions.d64 <- CR.positions.d64 %>% dplyr::group_by(Transmitter)
d64_ChinaR <- CR.positions.d64[!is.na(CR.positions.d64$Longitude) & !is.na(CR.positions.d64$Latitude),]
d64_ChinaR.sp <- d64_ChinaR[, c("Transmitter", "Longitude", "Latitude")]
coordinates(d64_ChinaR.sp) <- c("Longitude", "Latitude")
proj4string(d64_ChinaR.sp) <- CRS("+init=epsg:4326")

kernel.d64.ChinaR <- kernelUD(d64_ChinaR.sp, h = "href")
ChinaR.d64.poly <- getverticeshr(kernel.d64.ChinaR, percent = 95)

ChinaR.d64.sp <- fortify(ChinaR.d64.poly)
ChinaR.d64.df <- as.data.frame(ChinaR.d64.poly)

### 65 ######################
CR.positions.d65 <- ChinaR_accel %>% filter(periods== "During")%>% filter(Transmitter == "A69-9007-13265")
CR.positions.d65 <- CR.positions.d65 %>% dplyr::group_by(Transmitter)
d65_ChinaR <- CR.positions.d65[!is.na(CR.positions.d65$Longitude) & !is.na(CR.positions.d65$Latitude),]
d65_ChinaR.sp <- d65_ChinaR[, c("Transmitter", "Longitude", "Latitude")]
coordinates(d65_ChinaR.sp) <- c("Longitude", "Latitude")
proj4string(d65_ChinaR.sp) <- CRS("+init=epsg:4326")

kernel.d65.ChinaR <- kernelUD(d65_ChinaR.sp, h = "href")
ChinaR.d65.poly <- getverticeshr(kernel.d65.ChinaR, percent = 95)

ChinaR.d65.sp <- fortify(ChinaR.d65.poly)
ChinaR.d65.df <- as.data.frame(ChinaR.d65.poly)

### 66 ####################### - works with all dataset
CR.positions.d66 <- ChinaR_accel %>% filter(periods== "During")%>% filter(Transmitter == "A69-9007-13266")
CR.positions.d66 <- CR.positions.d66 %>% dplyr::group_by(Transmitter)
d66_ChinaR <- CR.positions.d66[!is.na(CR.positions.d66$Longitude) & !is.na(CR.positions.d66$Latitude),]
d66_ChinaR.sp <- d66_ChinaR[, c("Transmitter", "Longitude", "Latitude")]
coordinates(d66_ChinaR.sp) <- c("Longitude", "Latitude")
proj4string(d66_ChinaR.sp) <- CRS("+init=epsg:4326")

kernel.d66.ChinaR <- kernelUD(d66_ChinaR.sp, h = "href")
ChinaR.d66.poly <- getverticeshr(kernel.d66.ChinaR, percent = 95)

ChinaR.d66.sp <- fortify(ChinaR.d66.poly)
ChinaR.d66.df <- as.data.frame(ChinaR.d66.poly)


### 67
CR.positions.d67 <- ChinaR_accel %>% filter(periods== "During")%>% filter(Transmitter == "A69-9007-13267")
CR.positions.d67 <- CR.positions.d67 %>% dplyr::group_by(Transmitter)
d67_ChinaR <- CR.positions.d67[!is.na(CR.positions.d67$Longitude) & !is.na(CR.positions.d67$Latitude),]
d67_ChinaR.sp <- d67_ChinaR[, c("Transmitter", "Longitude", "Latitude")]
coordinates(d67_ChinaR.sp) <- c("Longitude", "Latitude")
proj4string(d67_ChinaR.sp) <- CRS("+init=epsg:4326")

kernel.d67.ChinaR <- kernelUD(d67_ChinaR.sp, h = "href")
ChinaR.d67.poly <- getverticeshr(kernel.d67.ChinaR, percent = 95)

ChinaR.d67.sp <- fortify(ChinaR.d67.poly)
ChinaR.d67.df <- as.data.frame(ChinaR.d67.poly)

kmlPolygons(obj = ChinaR.d67.poly, kmlfile="ChinaR.d67.kml", col = "blue")

###68 ##############################
CR.positions.d68 <- ChinaR_accel %>% filter(periods== "During")%>% filter(Transmitter == "A69-9007-13268")
CR.positions.d68 <- CR.positions.d68 %>% dplyr::group_by(Transmitter)
d68_ChinaR <- CR.positions.d68[!is.na(CR.positions.d68$Longitude) & !is.na(CR.positions.d68$Latitude),]
d68_ChinaR.sp <- d68_ChinaR[, c("Transmitter", "Longitude", "Latitude")]
coordinates(d68_ChinaR.sp) <- c("Longitude", "Latitude")
proj4string(d68_ChinaR.sp) <- CRS("+init=epsg:4326")

kernel.d68.ChinaR <- kernelUD(d68_ChinaR.sp, h = "href")
ChinaR.d68.poly <- getverticeshr(kernel.d68.ChinaR, percent = 95)

ChinaR.d68.sp <- fortify(ChinaR.d68.poly)
ChinaR.d68.df <- as.data.frame(ChinaR.d68.poly)

###69
CR.positions.d69 <- ChinaR_accel %>% filter(periods== "During")%>% filter(Transmitter == "A69-9007-13269")
CR.positions.d69 <- CR.positions.d69 %>% dplyr::group_by(Transmitter)
d69_ChinaR <- CR.positions.d69[!is.na(CR.positions.d69$Longitude) & !is.na(CR.positions.d69$Latitude),]
d69_ChinaR.sp <- d69_ChinaR[, c("Transmitter", "Longitude", "Latitude")]
coordinates(d69_ChinaR.sp) <- c("Longitude", "Latitude")
proj4string(d69_ChinaR.sp) <- CRS("+init=epsg:4326")

kernel.d69.ChinaR <- kernelUD(d69_ChinaR.sp, h = "href")
ChinaR.d69.poly <- getverticeshr(kernel.d69.ChinaR, percent = 95)

ChinaR.d69.sp <- fortify(ChinaR.d69.poly)
ChinaR.d69.df <- as.data.frame(ChinaR.d69.poly)

kmlPolygons(obj = ChinaR.d69.poly, kmlfile="ChinaR.d69.kml", col = "blue")

##70 ##################################
CR.positions.d70 <- ChinaR_accel %>% filter(periods== "During")%>% filter(Transmitter == "A69-9007-13270")
CR.positions.d70 <- CR.positions.d70 %>% dplyr::group_by(Transmitter)
d70_ChinaR <- CR.positions.d70[!is.na(CR.positions.d70$Longitude) & !is.na(CR.positions.d70$Latitude),]
d70_ChinaR.sp <- d70_ChinaR[, c("Transmitter", "Longitude", "Latitude")]
coordinates(d70_ChinaR.sp) <- c("Longitude", "Latitude")
proj4string(d70_ChinaR.sp) <- CRS("+init=epsg:4326")

kernel.d70.ChinaR <- kernelUD(d70_ChinaR.sp, h = "href")
ChinaR.d70.poly <- getverticeshr(kernel.d70.ChinaR, percent = 95)

ChinaR.d70.sp <- fortify(ChinaR.d70.poly)
ChinaR.d70.df <- as.data.frame(ChinaR.d70.poly)

##71 #############################3
CR.positions.d71 <- ChinaR_accel %>% filter(periods== "During")%>% filter(Transmitter == "A69-9007-13271")
CR.positions.d71 <- CR.positions.d71 %>% dplyr::group_by(Transmitter)
d71_ChinaR <- CR.positions.d71[!is.na(CR.positions.d71$Longitude) & !is.na(CR.positions.d71$Latitude),]
d71_ChinaR.sp <- d71_ChinaR[, c("Transmitter", "Longitude", "Latitude")]
coordinates(d71_ChinaR.sp) <- c("Longitude", "Latitude")
proj4string(d71_ChinaR.sp) <- CRS("+init=epsg:4326")

kernel.d71.ChinaR <- kernelUD(d71_ChinaR.sp, h = "href")
ChinaR.d71.poly <- getverticeshr(kernel.d71.ChinaR, percent = 95)

ChinaR.d71.sp <- fortify(ChinaR.d71.poly)
ChinaR.d71.df <- as.data.frame(ChinaR.d71.poly)

##72
CR.positions.d72 <- ChinaR_accel %>% filter(periods== "During")%>% filter(Transmitter == "A69-9007-13272")
CR.positions.d72 <- CR.positions.d72 %>% dplyr::group_by(Transmitter)
d72_ChinaR <- CR.positions.d72[!is.na(CR.positions.d72$Longitude) & !is.na(CR.positions.d72$Latitude),]
d72_ChinaR.sp <- d72_ChinaR[, c("Transmitter", "Longitude", "Latitude")]
coordinates(d72_ChinaR.sp) <- c("Longitude", "Latitude")
proj4string(d72_ChinaR.sp) <- CRS("+init=epsg:4326")

kernel.d72.ChinaR <- kernelUD(d72_ChinaR.sp, h = "href")
ChinaR.d72.poly <- getverticeshr(kernel.d72.ChinaR, percent = 95)

ChinaR.d72.sp <- fortify(ChinaR.d72.poly)
ChinaR.d72.df <- as.data.frame(ChinaR.d72.poly)

kmlPolygons(obj = ChinaR.d72.poly, kmlfile="ChinaR.d72.kml", col = "blue")

##73 ################## - works with all dataset
CR.positions.d73 <- ChinaR_accel %>% filter(periods== "During")%>% filter(Transmitter == "A69-9007-13273")
CR.positions.d73 <- CR.positions.d73 %>% dplyr::group_by(Transmitter)
d73_ChinaR <- CR.positions.d73[!is.na(CR.positions.d73$Longitude) & !is.na(CR.positions.d73$Latitude),]
d73_ChinaR.sp <- d73_ChinaR[, c("Transmitter", "Longitude", "Latitude")]
coordinates(d73_ChinaR.sp) <- c("Longitude", "Latitude")
proj4string(d73_ChinaR.sp) <- CRS("+init=epsg:4326")

kernel.d73.ChinaR <- kernelUD(d73_ChinaR.sp, h = "href")
ChinaR.d73.poly <- getverticeshr(kernel.d73.ChinaR, percent = 95)

ChinaR.d73.sp <- fortify(ChinaR.d73.poly)
ChinaR.d73.df <- as.data.frame(ChinaR.d73.poly)

##74 ##################33 - works with all dataset
CR.positions.d74 <- ChinaR_accel %>% filter(periods== "During")%>% filter(Transmitter == "A69-9007-13274")
CR.positions.d74 <- CR.positions.d74 %>% dplyr::group_by(Transmitter)
d74_ChinaR <- CR.positions.d74[!is.na(CR.positions.d74$Longitude) & !is.na(CR.positions.d74$Latitude),]
d74_ChinaR.sp <- d74_ChinaR[, c("Transmitter", "Longitude", "Latitude")]
coordinates(d74_ChinaR.sp) <- c("Longitude", "Latitude")
proj4string(d74_ChinaR.sp) <- CRS("+init=epsg:4326")

kernel.d74.ChinaR <- kernelUD(d74_ChinaR.sp, h = "href")
ChinaR.d74.poly <- getverticeshr(kernel.d74.ChinaR, percent = 95)

ChinaR.d74.sp <- fortify(ChinaR.d74.poly)
ChinaR.d74.df <- as.data.frame(ChinaR.d74.poly)

##75 #################### - works with all dataset
CR.positions.d75 <- ChinaR_accel %>% filter(periods== "During")%>% filter(Transmitter == "A69-9007-13275")
CR.positions.d75 <- CR.positions.d75 %>% dplyr::group_by(Transmitter)
d75_ChinaR <- CR.positions.d75[!is.na(CR.positions.d75$Longitude) & !is.na(CR.positions.d75$Latitude),]
d75_ChinaR.sp <- d75_ChinaR[, c("Transmitter", "Longitude", "Latitude")]
coordinates(d75_ChinaR.sp) <- c("Longitude", "Latitude")
proj4string(d75_ChinaR.sp) <- CRS("+init=epsg:4326")

kernel.d75.ChinaR <- kernelUD(d75_ChinaR.sp, h = "href")
ChinaR.d75.poly <- getverticeshr(kernel.d75.ChinaR, percent = 95)

ChinaR.d75.sp <- fortify(ChinaR.d75.poly)
ChinaR.d75.df <- as.data.frame(ChinaR.d75.poly)

##76 #########################
CR.positions.d76 <- ChinaR_accel %>% filter(periods== "During")%>% filter(Transmitter == "A69-9007-13276")
CR.positions.d76 <- CR.positions.d76 %>% dplyr::group_by(Transmitter)
d76_ChinaR <- CR.positions.d76[!is.na(CR.positions.d76$Longitude) & !is.na(CR.positions.d76$Latitude),]
d76_ChinaR.sp <- d76_ChinaR[, c("Transmitter", "Longitude", "Latitude")]
coordinates(d76_ChinaR.sp) <- c("Longitude", "Latitude")
proj4string(d76_ChinaR.sp) <- CRS("+init=epsg:4326")

kernel.d76.ChinaR <- kernelUD(d76_ChinaR.sp, h = "href")
ChinaR.d76.poly <- getverticeshr(kernel.d76.ChinaR, percent = 95)

ChinaR.d76.sp <- fortify(ChinaR.d76.poly)
ChinaR.d76.df <- as.data.frame(ChinaR.d76.poly)

##77
CR.positions.d77 <- ChinaR_accel %>% filter(periods== "During")%>% filter(Transmitter == "A69-9007-13277")
CR.positions.d77 <- CR.positions.d77 %>% dplyr::group_by(Transmitter)
d77_ChinaR <- CR.positions.d77[!is.na(CR.positions.d77$Longitude) & !is.na(CR.positions.d77$Latitude),]
d77_ChinaR.sp <- d77_ChinaR[, c("Transmitter", "Longitude", "Latitude")]
coordinates(d77_ChinaR.sp) <- c("Longitude", "Latitude")
proj4string(d77_ChinaR.sp) <- CRS("+init=epsg:4326")

kernel.d77.ChinaR <- kernelUD(d77_ChinaR.sp, h = "href")
ChinaR.d77.poly <- getverticeshr(kernel.d77.ChinaR, percent = 95)

ChinaR.d77.sp <- fortify(ChinaR.d77.poly)
ChinaR.d77.df <- as.data.frame(ChinaR.d77.poly)

kmlPolygons(obj = ChinaR.d77.poly, kmlfile="ChinaR.d77.kml", col = "blue")

##78
CR.positions.d78 <- ChinaR_accel %>% filter(periods== "During")%>% filter(Transmitter == "A69-9007-13278")
CR.positions.d78 <- CR.positions.d78 %>% dplyr::group_by(Transmitter)
d78_ChinaR <- CR.positions.d78[!is.na(CR.positions.d78$Longitude) & !is.na(CR.positions.d78$Latitude),]
d78_ChinaR.sp <- d78_ChinaR[, c("Transmitter", "Longitude", "Latitude")]
coordinates(d78_ChinaR.sp) <- c("Longitude", "Latitude")
proj4string(d78_ChinaR.sp) <- CRS("+init=epsg:4326")

kernel.d78.ChinaR <- kernelUD(d78_ChinaR.sp, h = "href")
ChinaR.d78.poly <- getverticeshr(kernel.d78.ChinaR, percent = 95)

ChinaR.d78.sp <- fortify(ChinaR.d78.poly)
ChinaR.d78.df <- as.data.frame(ChinaR.d78.poly)

kmlPolygons(obj = ChinaR.d78.poly, kmlfile="ChinaR.d78.kml", col = "blue")

ChinaR.dur.df <- bind_rows(#ChinaR.d66.df,
                           #ChinaR.d67.df, 
                           ChinaR.d69.df,
                           ChinaR.d72.df, 
                           #ChinaR.d73.df,
                           #ChinaR.d74.df,
                           #ChinaR.d75.df,
                           ChinaR.d77.df, 
                           ChinaR.d78.df)
ChinaR.dur.df <- ChinaR.dur.df %>% rename(area.dur = area)

ChinaR.dur.poly <- bind(ChinaR.d69.poly, ChinaR.d72.poly,
                           ChinaR.d77.poly, ChinaR.d78.poly)

kmlPolygons(obj = ChinaR.dur.poly, kmlfile="ChinaR.dur.kml", col = "blue")



## After
### 64 ################
CR.positions.a64 <- ChinaR_accel %>% filter(periods== "After")%>% filter(Transmitter == "A69-9007-13264")
CR.positions.a64 <- CR.positions.a64 %>% dplyr::group_by(Transmitter)
a64_ChinaR <- CR.positions.a64[!is.na(CR.positions.a64$Longitude) & !is.na(CR.positions.a64$Latitude),]
a64_ChinaR.sp <- a64_ChinaR[, c("Transmitter", "Longitude", "Latitude")]
coordinates(a64_ChinaR.sp) <- c("Longitude", "Latitude")
proj4string(a64_ChinaR.sp) <- CRS("+init=epsg:4326")

kernel.a64.ChinaR <- kernelUD(a64_ChinaR.sp, h = "href")
ChinaR.a64.poly <- getverticeshr(kernel.a64.ChinaR, percent = 95)

ChinaR.a64.sp <- fortify(ChinaR.a64.poly)
ChinaR.a64.df <- as.data.frame(ChinaR.a64.poly)

### 65 ######################
CR.positions.a65 <- ChinaR_accel %>% filter(periods== "After")%>% filter(Transmitter == "A69-9007-13265")
CR.positions.a65 <- CR.positions.a65 %>% dplyr::group_by(Transmitter)
a65_ChinaR <- CR.positions.a65[!is.na(CR.positions.a65$Longitude) & !is.na(CR.positions.a65$Latitude),]
a65_ChinaR.sp <- a65_ChinaR[, c("Transmitter", "Longitude", "Latitude")]
coordinates(a65_ChinaR.sp) <- c("Longitude", "Latitude")
proj4string(a65_ChinaR.sp) <- CRS("+init=epsg:4326")

kernel.a65.ChinaR <- kernelUD(a65_ChinaR.sp, h = "href")
ChinaR.a65.poly <- getverticeshr(kernel.a65.ChinaR, percent = 95)

ChinaR.a65.sp <- fortify(ChinaR.a65.poly)
ChinaR.a65.df <- as.data.frame(ChinaR.a65.poly)

### 66 #####################
CR.positions.a66 <- ChinaR_accel %>% filter(periods== "After")%>% filter(Transmitter == "A69-9007-13266")
CR.positions.a66 <- CR.positions.a66 %>% dplyr::group_by(Transmitter)
a66_ChinaR <- CR.positions.a66[!is.na(CR.positions.a66$Longitude) & !is.na(CR.positions.a66$Latitude),]
a66_ChinaR.sp <- a66_ChinaR[, c("Transmitter", "Longitude", "Latitude")]
coordinates(a66_ChinaR.sp) <- c("Longitude", "Latitude")
proj4string(a66_ChinaR.sp) <- CRS("+init=epsg:4326")

kernel.a66.ChinaR <- kernelUD(a66_ChinaR.sp, h = "href")
ChinaR.a66.poly <- getverticeshr(kernel.a66.ChinaR, percent = 95)

ChinaR.a66.sp <- fortify(ChinaR.a66.poly)
ChinaR.a66.df <- as.data.frame(ChinaR.a66.poly)

### 67
##67
CR.positions.a67 <- ChinaR_accel %>% filter(periods== "After")%>% filter(Transmitter == "A69-9007-13267")
CR.positions.a67 <- CR.positions.a67 %>% dplyr::group_by(Transmitter)
a67_ChinaR <- CR.positions.a67[!is.na(CR.positions.a67$Longitude) & !is.na(CR.positions.a67$Latitude),]
a67_ChinaR.sp <- a67_ChinaR[, c("Transmitter", "Longitude", "Latitude")]
coordinates(a67_ChinaR.sp) <- c("Longitude", "Latitude")
proj4string(a67_ChinaR.sp) <- CRS("+init=epsg:4326")

kernel.a67.ChinaR <- kernelUD(a67_ChinaR.sp, h = "href")
ChinaR.a67.poly <- getverticeshr(kernel.a67.ChinaR, percent = 95)

ChinaR.a67.sp <- fortify(ChinaR.a67.poly)
ChinaR.a67.df <- as.data.frame(ChinaR.a67.poly)

kmlPolygons(obj = ChinaR.a67.poly, kmlfile="ChinaR.a67.kml", col = "blue")

### 68 ######################
CR.positions.a68 <- ChinaR_accel %>% filter(periods== "After")%>% filter(Transmitter == "A69-9007-13268")
CR.positions.a68 <- CR.positions.a68 %>% dplyr::group_by(Transmitter)
a68_ChinaR <- CR.positions.a68[!is.na(CR.positions.a68$Longitude) & !is.na(CR.positions.a68$Latitude),]
a68_ChinaR.sp <- a68_ChinaR[, c("Transmitter", "Longitude", "Latitude")]
coordinates(a68_ChinaR.sp) <- c("Longitude", "Latitude")
proj4string(a68_ChinaR.sp) <- CRS("+init=epsg:4326")

kernel.a68.ChinaR <- kernelUD(a68_ChinaR.sp, h = "href")
ChinaR.a68.poly <- getverticeshr(kernel.a68.ChinaR, percent = 95)

ChinaR.a68.sp <- fortify(ChinaR.a68.poly)
ChinaR.a68.df <- as.data.frame(ChinaR.a68.poly)



### 69
CR.positions.a69 <- ChinaR_accel %>% filter(periods== "After")%>% filter(Transmitter == "A69-9007-13269")
CR.positions.a69 <- CR.positions.a69 %>% dplyr::group_by(Transmitter)
a69_ChinaR <- CR.positions.a69[!is.na(CR.positions.a69$Longitude) & !is.na(CR.positions.a69$Latitude),]
a69_ChinaR.sp <- a69_ChinaR[, c("Transmitter", "Longitude", "Latitude")]
coordinates(a69_ChinaR.sp) <- c("Longitude", "Latitude")
proj4string(a69_ChinaR.sp) <- CRS("+init=epsg:4326")

kernel.a69.ChinaR <- kernelUD(a69_ChinaR.sp, h = "href")
ChinaR.a69.poly <- getverticeshr(kernel.a69.ChinaR, percent = 95)

ChinaR.a69.sp <- fortify(ChinaR.a69.poly)
ChinaR.a69.df <- as.data.frame(ChinaR.a69.poly)

kmlPolygons(obj = ChinaR.a69.poly, kmlfile="ChinaR.a69.kml", col = "blue")

##70 ##################################
CR.positions.a70 <- ChinaR_accel %>% filter(periods== "After")%>% filter(Transmitter == "A69-9007-13270")
CR.positions.a70 <- CR.positions.a70 %>% dplyr::group_by(Transmitter)
a70_ChinaR <- CR.positions.a70[!is.na(CR.positions.a70$Longitude) & !is.na(CR.positions.a70$Latitude),]
a70_ChinaR.sp <- a70_ChinaR[, c("Transmitter", "Longitude", "Latitude")]
coordinates(a70_ChinaR.sp) <- c("Longitude", "Latitude")
proj4string(a70_ChinaR.sp) <- CRS("+init=epsg:4326")

kernel.a70.ChinaR <- kernelUD(a70_ChinaR.sp, h = "href")
ChinaR.a70.poly <- getverticeshr(kernel.a70.ChinaR, percent = 95)

ChinaR.a70.sp <- fortify(ChinaR.a70.poly)
ChinaR.a70.df <- as.data.frame(ChinaR.a70.poly)

##71 #############################3
CR.positions.a71 <- ChinaR_accel %>% filter(periods== "After")%>% filter(Transmitter == "A69-9007-13271")
CR.positions.a71 <- CR.positions.a71 %>% dplyr::group_by(Transmitter)
a71_ChinaR <- CR.positions.a71[!is.na(CR.positions.a71$Longitude) & !is.na(CR.positions.a71$Latitude),]
a71_ChinaR.sp <- a71_ChinaR[, c("Transmitter", "Longitude", "Latitude")]
coordinates(a71_ChinaR.sp) <- c("Longitude", "Latitude")
proj4string(a71_ChinaR.sp) <- CRS("+init=epsg:4326")

kernel.a71.ChinaR <- kernelUD(a71_ChinaR.sp, h = "href")
ChinaR.a71.poly <- getverticeshr(kernel.a71.ChinaR, percent = 95)

ChinaR.a71.sp <- fortify(ChinaR.a71.poly)
ChinaR.a71.df <- as.data.frame(ChinaR.a71.poly)


##72
CR.positions.a72 <- ChinaR_accel %>% filter(periods== "After")%>% filter(Transmitter == "A69-9007-13272")
CR.positions.a72 <- CR.positions.a72 %>% dplyr::group_by(Transmitter)
a72_ChinaR <- CR.positions.a72[!is.na(CR.positions.a72$Longitude) & !is.na(CR.positions.a72$Latitude),]
a72_ChinaR.sp <- a72_ChinaR[, c("Transmitter", "Longitude", "Latitude")]
coordinates(a72_ChinaR.sp) <- c("Longitude", "Latitude")
proj4string(a72_ChinaR.sp) <- CRS("+init=epsg:4326")

kernel.a72.ChinaR <- kernelUD(a72_ChinaR.sp, h = "href")
ChinaR.a72.poly <- getverticeshr(kernel.a72.ChinaR, percent = 95)

ChinaR.a72.sp <- fortify(ChinaR.a72.poly)
ChinaR.a72.df <- as.data.frame(ChinaR.a72.poly)

kmlPolygons(obj = ChinaR.a72.poly, kmlfile="ChinaR.a72.kml", col = "blue")

##73 ######################## works with new data
CR.positions.a73 <- ChinaR_accel %>% filter(periods== "After")%>% filter(Transmitter == "A69-9007-13273")
CR.positions.a73 <- CR.positions.a73 %>% dplyr::group_by(Transmitter)
a73_ChinaR <- CR.positions.a73[!is.na(CR.positions.a73$Longitude) & !is.na(CR.positions.a73$Latitude),]
a73_ChinaR.sp <- a73_ChinaR[, c("Transmitter", "Longitude", "Latitude")]
coordinates(a73_ChinaR.sp) <- c("Longitude", "Latitude")
proj4string(a73_ChinaR.sp) <- CRS("+init=epsg:4326")

kernel.a73.ChinaR <- kernelUD(a73_ChinaR.sp, h = "href")
ChinaR.a73.poly <- getverticeshr(kernel.a73.ChinaR, percent = 95)

ChinaR.a73.sp <- fortify(ChinaR.a73.poly)
ChinaR.a73.df <- as.data.frame(ChinaR.a73.poly)

##74 ##################################### works new
CR.positions.a74 <- ChinaR_accel %>% filter(periods== "After")%>% filter(Transmitter == "A69-9007-13274")
CR.positions.a74 <- CR.positions.a74 %>% dplyr::group_by(Transmitter)
a74_ChinaR <- CR.positions.a74[!is.na(CR.positions.a74$Longitude) & !is.na(CR.positions.a74$Latitude),]
a74_ChinaR.sp <- a74_ChinaR[, c("Transmitter", "Longitude", "Latitude")]
coordinates(a74_ChinaR.sp) <- c("Longitude", "Latitude")
proj4string(a74_ChinaR.sp) <- CRS("+init=epsg:4326")

kernel.a74.ChinaR <- kernelUD(a74_ChinaR.sp, h = "href")
ChinaR.a74.poly <- getverticeshr(kernel.a74.ChinaR, percent = 95)

ChinaR.a74.sp <- fortify(ChinaR.a74.poly)
ChinaR.a74.df <- as.data.frame(ChinaR.a74.poly)

##75 doesn't work new
CR.positions.a75 <- ChinaR_accel %>% filter(periods== "After")%>% filter(Transmitter == "A69-9007-13275")
CR.positions.a75 <- CR.positions.a75 %>% dplyr::group_by(Transmitter)
a75_ChinaR <- CR.positions.a75[!is.na(CR.positions.a75$Longitude) & !is.na(CR.positions.a75$Latitude),]
a75_ChinaR.sp <- a75_ChinaR[, c("Transmitter", "Longitude", "Latitude")]
coordinates(a75_ChinaR.sp) <- c("Longitude", "Latitude")
proj4string(a75_ChinaR.sp) <- CRS("+init=epsg:4326")

kernel.a75.ChinaR <- kernelUD(a75_ChinaR.sp, h = "href")
ChinaR.a75.poly <- getverticeshr(kernel.a75.ChinaR, percent = 95)

ChinaR.a75.sp <- fortify(ChinaR.a75.poly)
ChinaR.a75.df <- as.data.frame(ChinaR.a75.poly)

kmlPolygons(obj = ChinaR.a75.poly, kmlfile="ChinaR.a75.kml", col = "blue")

##76 #########################
CR.positions.a76 <- ChinaR_accel %>% filter(periods== "After")%>% filter(Transmitter == "A69-9007-13276")
CR.positions.a76 <- CR.positions.a76 %>% dplyr::group_by(Transmitter)
a76_ChinaR <- CR.positions.a76[!is.na(CR.positions.a76$Longitude) & !is.na(CR.positions.a76$Latitude),]
a76_ChinaR.sp <- a76_ChinaR[, c("Transmitter", "Longitude", "Latitude")]
coordinates(a76_ChinaR.sp) <- c("Longitude", "Latitude")
proj4string(a76_ChinaR.sp) <- CRS("+init=epsg:4326")

kernel.a76.ChinaR <- kernelUD(a76_ChinaR.sp, h = "href")
ChinaR.a76.poly <- getverticeshr(kernel.a76.ChinaR, percent = 95)

ChinaR.a76.sp <- fortify(ChinaR.a76.poly)
ChinaR.a76.df <- as.data.frame(ChinaR.a76.poly)

##77 doesn/t work new
CR.positions.a77 <- ChinaR_accel %>% filter(periods== "After")%>% filter(Transmitter == "A69-9007-13277")
CR.positions.a77 <- CR.positions.a77 %>% dplyr::group_by(Transmitter)
a77_ChinaR <- CR.positions.a77[!is.na(CR.positions.a77$Longitude) & !is.na(CR.positions.a77$Latitude),]
a77_ChinaR.sp <- a77_ChinaR[, c("Transmitter", "Longitude", "Latitude")]
coordinates(a77_ChinaR.sp) <- c("Longitude", "Latitude")
proj4string(a77_ChinaR.sp) <- CRS("+init=epsg:4326")

kernel.a77.ChinaR <- kernelUD(a77_ChinaR.sp, h = "href")
ChinaR.a77.poly <- getverticeshr(kernel.a77.ChinaR, percent = 95)

ChinaR.a77.sp <- fortify(ChinaR.a77.poly)
ChinaR.a77.df <- as.data.frame(ChinaR.a77.poly)

kmlPolygons(obj = ChinaR.a77.poly, kmlfile="ChinaR.a77.kml", col = "blue")

##78 doesn't work new
CR.positions.a78 <- ChinaR_accel %>% filter(periods== "After")%>% filter(Transmitter == "A69-9007-13278")
CR.positions.a78 <- CR.positions.a78 %>% dplyr::group_by(Transmitter)
a78_ChinaR <- CR.positions.a78[!is.na(CR.positions.a78$Longitude) & !is.na(CR.positions.a78$Latitude),]
a78_ChinaR.sp <- a78_ChinaR[, c("Transmitter", "Longitude", "Latitude")]
coordinates(a78_ChinaR.sp) <- c("Longitude", "Latitude")
proj4string(a78_ChinaR.sp) <- CRS("+init=epsg:4326")

kernel.a78.ChinaR <- kernelUD(a78_ChinaR.sp, h = "href")
ChinaR.a78.poly <- getverticeshr(kernel.a78.ChinaR, percent = 95)

ChinaR.a78.sp <- fortify(ChinaR.a78.poly)
ChinaR.a78.df <- as.data.frame(ChinaR.a78.poly)

kmlPolygons(obj = ChinaR.a78.poly, kmlfile="ChinaR.a78.kml", col = "blue")

ChinaR.after.df <- bind_rows(#ChinaR.a67.df, 
                             ChinaR.a69.df,
                            ChinaR.a72.df,
                           ChinaR.a75.df, ChinaR.a77.df, ChinaR.a78.df)
ChinaR.after.df <- ChinaR.after.df %>% rename(area.after = area)

ChinaR.after.poly <- bind(ChinaR.a67.poly, ChinaR.a69.poly, ChinaR.a72.poly,
                          ChinaR.a75.poly,
                        ChinaR.a77.poly, ChinaR.a78.poly)

kmlPolygons(obj = ChinaR.after.poly, kmlfile="ChinaR.after.kml", col = "blue")


#dataframe of areas of individuals by three periods
ChinaR.all.df <- ChinaR.before.df %>% 
  full_join(ChinaR.dur.df, by='id') %>% 
  full_join(ChinaR.after.df, by='id')

#repeated measures anova
CR_ANOVA <- ChinaR.all.df %>%
  gather(key = "period", value = "area", area.before, area.dur, area.after) %>%
  convert_as_factor(id, period)
periods.lvl <- c("area.before", "area.dur", "area.after")
#head(BR_ANOVA, 3)

CR_ANOVA %>%
  group_by(period) %>%
  get_summary_stats(area, type = "mean_sd")

CR_ANOVA$period <- factor(CR_ANOVA$period, levels = periods.lvl[c(1:3)])

bxp <- ggplot(CR_ANOVA, aes(x = period, y = area), add = "point") + geom_boxplot()
bxp


out <- boxplot.stats(ChinaR.before.df$area)$out
out_ind <- which(ChinaR.before.df$area %in% c(out))
out_ind

violin_CR_KUD <- ggplot(CR_ANOVA, aes(x=period, y=area, fill=period)) + 
  geom_violin(trim=TRUE)  +
  #scale_x_discrete(limits=c("May 20-June 10", "June 11", "June 12-16", "June 16", "June 17", "June 18", "June 19-July 11")) +
  #labs(x = "Period of survey", y = "Acceleration values", title = "Dungeness Crab Acceleration by Period", fill="Period",
  #     caption = "Preliminary analyses") 
  scale_fill_manual(values = noise_colors_repeat, breaks = periods.lvl, labels = KUD_labels)
violin_CR_KUD



pwc <- cr.aov %>%
  pairwise_t_test(
    area ~ period, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
pwc

#gotta remove NAs for the repeated measures ANOVA
cr.aov <- CR_ANOVA %>% drop_na()

cr_aov <- anova_test(data = cr.aov, dv = area, wid = id, within = period)
get_anova_table(cr_aov)


#regular ole ANOVA
summary(aov(area ~ period, data = CR_ANOVA))

which(ChinaR.before.df$area.before %in% c(max(ChinaR.before.df$area.before)))
which(ChinaR.dur.df$area.dur %in% c(max(ChinaR.dur.df$area.dur)))
which(ChinaR.after.df$area.after %in% c(max(ChinaR.after.df$area.after)))