### spatial UD for animals 
library(rgdal)
library(rgeos)
library(raster)
library(adehabitatHR)
library(tidyverse)
library(cowplot)
library(showtext)
showtext_auto()

Lingcod_accel <- periods_Lingcod


#kmlPolygons(obj = Lingcod.p1.poly, kmlfile="Lingcod.p1.kml", col = "blue")


## Second try Period 1!!
fish1 <- Lingcod_accel %>% dplyr::filter(Transmitter == "A69-9007-13249")
fish1 <- fish1[,c("Latitude", "Longitude", "survey.period", "detect_day", "detect_time", "Transmitter")]
fishall <- Lingcod_accel
fishall <- fishall[,c("Latitude", "Longitude", "survey.period", "detect_day", "detect_time", "Transmitter")]
fishlist <- split(fishall, f = fishall$Transmitter)
fishes <- unique(fishall$Transmitter)
fishes <- fishes[!is.na(fishes)]
lcperiodslist <- split(fish1, f = c(fish1$survey.period))
fishcols <- length(fishlist)
outdf <- data.frame(matrix(ncol = fishcols, nrow = 0))

for (i in seq_along(fishes)){
  fishind <- fishes[i]
  LC.positions.p1 <- Lingcod_accel %>% filter(survey.period == "May 10-17")%>% filter(Transmitter == fishind)
  LC.positions.p1 <- LC.positions.p1 %>% dplyr::group_by(Transmitter)
  p1_Lingcod <- LC.positions.p1[!is.na(LC.positions.p1$Longitude) & !is.na(LC.positions.p1$Latitude),]
  p1_Lingcod.sp <- p1_Lingcod[, c("Transmitter", "Longitude", "Latitude")]
  
  skip_to_next <- FALSE
  
  tryCatch({coordinates(p1_Lingcod.sp) <- c("Longitude", "Latitude") 
  proj4string(p1_Lingcod.sp) <- CRS("+init=epsg:4326")}, 
  error = function(e) { skip_to_next <<- TRUE})
  
  if(skip_to_next) { next }    
  
  skip_to_next <- FALSE
  
  tryCatch(kernel.p1.Lingcod <- kernelUD(p1_Lingcod.sp, h = "href"), 
           error = function(e) { skip_to_next <<- TRUE})
  
  if(skip_to_next) { next }     
  
  skip_to_next <- FALSE
  
  tryCatch(Lingcod.p1.poly <- getverticeshr(kernel.p1.Lingcod, percr_cent = 95), 
           error = function(e) { skip_to_next <<- TRUE})
  
  if(skip_to_next) { next }     
  
  filename1 <- paste(fishind, "p0", "kml", sep = ".")
  
  
  Lingcod.p1.sp <- fortify(Lingcod.p1.poly)
  Lingcod.p1.df <- as.data.frame(Lingcod.p1.poly)
  
  j <- sub(".*9007-", "", fishind)
  
  assign(paste0("Lingcod.p0.", j, ".poly"), Lingcod.p1.poly)
  kmlPolygons(obj = Lingcod.p1.poly, kmlfile= filename1, col = "blue") # write kml file
  
  assign(paste0("Lingcod.p0.", j), Lingcod.p1.df)
  
}

for (i in seq_along(fishes)){
  fishind <- fishes[i]
  LC.positions.p1 <- Lingcod_accel %>% filter(survey.period == "May 18-25")%>% filter(Transmitter == fishind)
  LC.positions.p1 <- LC.positions.p1 %>% dplyr::group_by(Transmitter)
  p1_Lingcod <- LC.positions.p1[!is.na(LC.positions.p1$Longitude) & !is.na(LC.positions.p1$Latitude),]
  p1_Lingcod.sp <- p1_Lingcod[, c("Transmitter", "Longitude", "Latitude")]
  
  skip_to_next <- FALSE
  
  tryCatch({coordinates(p1_Lingcod.sp) <- c("Longitude", "Latitude") 
  proj4string(p1_Lingcod.sp) <- CRS("+init=epsg:4326")}, 
  error = function(e) { skip_to_next <<- TRUE})
  
  if(skip_to_next) { next }    
  
  skip_to_next <- FALSE
  
  tryCatch(kernel.p1.Lingcod <- kernelUD(p1_Lingcod.sp, h = "href"), 
           error = function(e) { skip_to_next <<- TRUE})
  
  if(skip_to_next) { next }     
  
  skip_to_next <- FALSE
  
  tryCatch(Lingcod.p1.poly <- getverticeshr(kernel.p1.Lingcod, percr_cent = 95), 
           error = function(e) { skip_to_next <<- TRUE})
  
  if(skip_to_next) { next }     
  
  filename1 <- paste(fishind, "p1", "kml", sep = ".")
  
  Lingcod.p1.sp <- fortify(Lingcod.p1.poly)
  Lingcod.p1.df <- as.data.frame(Lingcod.p1.poly)
  
  j <- sub(".*9007-", "", fishind)
  
  assign(paste0("Lingcod.p1.", j, ".poly"), Lingcod.p1.poly)
  kmlPolygons(obj = Lingcod.p1.poly, kmlfile= filename1, col = "blue") # write kml file
  
  assign(paste0("Lingcod.p1.", j), Lingcod.p1.df)
  
}

for (i in seq_along(fishes)){
  fishind <- fishes[i]
  LC.positions.p1 <- Lingcod_accel %>% filter(survey.period == "May 26-June 2")%>% filter(Transmitter == fishind)
  LC.positions.p1 <- LC.positions.p1 %>% dplyr::group_by(Transmitter)
  p1_Lingcod <- LC.positions.p1[!is.na(LC.positions.p1$Longitude) & !is.na(LC.positions.p1$Latitude),]
  p1_Lingcod.sp <- p1_Lingcod[, c("Transmitter", "Longitude", "Latitude")]
  
  skip_to_next <- FALSE
  
  tryCatch({coordinates(p1_Lingcod.sp) <- c("Longitude", "Latitude") 
  proj4string(p1_Lingcod.sp) <- CRS("+init=epsg:4326")}, 
  error = function(e) { skip_to_next <<- TRUE})
  
  if(skip_to_next) { next }    
  
  skip_to_next <- FALSE
  
  tryCatch(kernel.p1.Lingcod <- kernelUD(p1_Lingcod.sp, h = "href"), 
           error = function(e) { skip_to_next <<- TRUE})
  
  if(skip_to_next) { next }     
  
  skip_to_next <- FALSE
  
  tryCatch(Lingcod.p1.poly <- getverticeshr(kernel.p1.Lingcod, percr_cent = 95), 
           error = function(e) { skip_to_next <<- TRUE})
  
  if(skip_to_next) { next }     
  
  filename1 <- paste(fishind, "p2", "kml", sep = ".")
  
  Lingcod.p1.sp <- fortify(Lingcod.p1.poly)
  Lingcod.p1.df <- as.data.frame(Lingcod.p1.poly)
  
  j <- sub(".*9007-", "", fishind)
  
  assign(paste0("Lingcod.p2.", j, ".poly"), Lingcod.p1.poly)
  kmlPolygons(obj = Lingcod.p1.poly, kmlfile= filename1, col = "blue") # write kml file
  
  assign(paste0("Lingcod.p2.", j), Lingcod.p1.df)
  
}

for (i in seq_along(fishes)){
  fishind <- fishes[i]
  LC.positions.p1 <- Lingcod_accel %>% filter(survey.period == "June 3-10")%>% filter(Transmitter == fishind)
  LC.positions.p1 <- LC.positions.p1 %>% dplyr::group_by(Transmitter)
  p1_Lingcod <- LC.positions.p1[!is.na(LC.positions.p1$Longitude) & !is.na(LC.positions.p1$Latitude),]
  p1_Lingcod.sp <- p1_Lingcod[, c("Transmitter", "Longitude", "Latitude")]
  
  skip_to_next <- FALSE
  
  tryCatch({coordinates(p1_Lingcod.sp) <- c("Longitude", "Latitude") 
  proj4string(p1_Lingcod.sp) <- CRS("+init=epsg:4326")}, 
  error = function(e) { skip_to_next <<- TRUE})
  
  if(skip_to_next) { next }    
  
  skip_to_next <- FALSE
  
  tryCatch(kernel.p1.Lingcod <- kernelUD(p1_Lingcod.sp, h = "href"), 
           error = function(e) { skip_to_next <<- TRUE})
  
  if(skip_to_next) { next }     
  
  skip_to_next <- FALSE
  
  tryCatch(Lingcod.p1.poly <- getverticeshr(kernel.p1.Lingcod, percr_cent = 95), 
           error = function(e) { skip_to_next <<- TRUE})
  
  if(skip_to_next) { next }     
  
  filename1 <- paste(fishind, "p3", "kml", sep = ".")
  
  Lingcod.p1.sp <- fortify(Lingcod.p1.poly)
  Lingcod.p1.df <- as.data.frame(Lingcod.p1.poly)
  
  j <- sub(".*9007-", "", fishind)
  
  assign(paste0("Lingcod.p3.", j, ".poly"), Lingcod.p1.poly)
  kmlPolygons(obj = Lingcod.p1.poly, kmlfile= filename1, col = "blue") # write kml file
  
  assign(paste0("Lingcod.p3.", j), Lingcod.p1.df)
  
}

for (i in seq_along(fishes)){
  fishind <- fishes[i]
  LC.positions.p1 <- Lingcod_accel %>% filter(survey.period == "June 11")%>% filter(Transmitter == fishind)
  LC.positions.p1 <- LC.positions.p1 %>% dplyr::group_by(Transmitter)
  p1_Lingcod <- LC.positions.p1[!is.na(LC.positions.p1$Longitude) & !is.na(LC.positions.p1$Latitude),]
  p1_Lingcod.sp <- p1_Lingcod[, c("Transmitter", "Longitude", "Latitude")]
  
  skip_to_next <- FALSE
  
  tryCatch({coordinates(p1_Lingcod.sp) <- c("Longitude", "Latitude") 
  proj4string(p1_Lingcod.sp) <- CRS("+init=epsg:4326")}, 
  error = function(e) { skip_to_next <<- TRUE})
  
  if(skip_to_next) { next }    
  
  skip_to_next <- FALSE
  
  tryCatch(kernel.p1.Lingcod <- kernelUD(p1_Lingcod.sp, h = "href"), 
           error = function(e) { skip_to_next <<- TRUE})
  
  if(skip_to_next) { next }     
  
  skip_to_next <- FALSE
  
  tryCatch(Lingcod.p1.poly <- getverticeshr(kernel.p1.Lingcod, percr_cent = 95), 
           error = function(e) { skip_to_next <<- TRUE})
  
  if(skip_to_next) { next }     
  
  filename1 <- paste(fishind, "11", "kml", sep = ".")
  
  Lingcod.p1.sp <- fortify(Lingcod.p1.poly)
  Lingcod.p1.df <- as.data.frame(Lingcod.p1.poly)
  
  j <- sub(".*9007-", "", fishind)
  
  assign(paste0("Lingcod.11.", j, ".poly"), Lingcod.p1.poly)
  kmlPolygons(obj = Lingcod.p1.poly, kmlfile= filename1, col = "blue") # write kml file
  
  assign(paste0("Lingcod.11.", j), Lingcod.p1.df)
  
}

for (i in seq_along(fishes)){
  fishind <- fishes[i]
  LC.positions.p1 <- Lingcod_accel %>% filter(survey.period == "June 12-16")%>% filter(Transmitter == fishind)
  LC.positions.p1 <- LC.positions.p1 %>% dplyr::group_by(Transmitter)
  p1_Lingcod <- LC.positions.p1[!is.na(LC.positions.p1$Longitude) & !is.na(LC.positions.p1$Latitude),]
  p1_Lingcod.sp <- p1_Lingcod[, c("Transmitter", "Longitude", "Latitude")]
  
  skip_to_next <- FALSE
  
  tryCatch({coordinates(p1_Lingcod.sp) <- c("Longitude", "Latitude") 
  proj4string(p1_Lingcod.sp) <- CRS("+init=epsg:4326")}, 
  error = function(e) { skip_to_next <<- TRUE})
  
  if(skip_to_next) { next }    
  
  skip_to_next <- FALSE
  
  tryCatch(kernel.p1.Lingcod <- kernelUD(p1_Lingcod.sp, h = "href"), 
           error = function(e) { skip_to_next <<- TRUE})
  
  if(skip_to_next) { next }     
  
  skip_to_next <- FALSE
  
  tryCatch(Lingcod.p1.poly <- getverticeshr(kernel.p1.Lingcod, percr_cent = 95), 
           error = function(e) { skip_to_next <<- TRUE})
  
  if(skip_to_next) { next }     
  
  filename1 <- paste(fishind, "p5", "kml", sep = ".")
  
  Lingcod.p1.sp <- fortify(Lingcod.p1.poly)
  Lingcod.p1.df <- as.data.frame(Lingcod.p1.poly)
  
  j <- sub(".*9007-", "", fishind)
  
  assign(paste0("Lingcod.p5.", j, ".poly"), Lingcod.p1.poly)
  kmlPolygons(obj = Lingcod.p1.poly, kmlfile= filename1, col = "blue") # write kml file
  
  assign(paste0("Lingcod.p5.", j), Lingcod.p1.df)
  
}

for (i in seq_along(fishes)){
  fishind <- fishes[i]
  LC.positions.p1 <- Lingcod_accel %>% filter(survey.period == "June 18")%>% filter(Transmitter == fishind)
  LC.positions.p1 <- LC.positions.p1 %>% dplyr::group_by(Transmitter)
  p1_Lingcod <- LC.positions.p1[!is.na(LC.positions.p1$Longitude) & !is.na(LC.positions.p1$Latitude),]
  p1_Lingcod.sp <- p1_Lingcod[, c("Transmitter", "Longitude", "Latitude")]
  
  skip_to_next <- FALSE
  
  tryCatch({coordinates(p1_Lingcod.sp) <- c("Longitude", "Latitude") 
  proj4string(p1_Lingcod.sp) <- CRS("+init=epsg:4326")}, 
  error = function(e) { skip_to_next <<- TRUE})
  
  if(skip_to_next) { next }    
  
  skip_to_next <- FALSE
  
  tryCatch(kernel.p1.Lingcod <- kernelUD(p1_Lingcod.sp, h = "href"), 
           error = function(e) { skip_to_next <<- TRUE})
  
  if(skip_to_next) { next }     
  
  skip_to_next <- FALSE
  
  tryCatch(Lingcod.p1.poly <- getverticeshr(kernel.p1.Lingcod, percr_cent = 95), 
           error = function(e) { skip_to_next <<- TRUE})
  
  if(skip_to_next) { next }     
  
  filename1 <- paste(fishind, "18", "kml", sep = ".")
  
  Lingcod.p1.sp <- fortify(Lingcod.p1.poly)
  Lingcod.p1.df <- as.data.frame(Lingcod.p1.poly)
  
  j <- sub(".*9007-", "", fishind)
  
  assign(paste0("Lingcod.18.", j, ".poly"), Lingcod.p1.poly)
  kmlPolygons(obj = Lingcod.p1.poly, kmlfile= filename1, col = "blue") # write kml file
  
  assign(paste0("Lingcod.18.", j), Lingcod.p1.df)
  
}

for (i in seq_along(fishes)){
  fishind <- fishes[i]
  LC.positions.p1 <- Lingcod_accel %>% filter(survey.period == "June 19-26")%>% filter(Transmitter == fishind)
  LC.positions.p1 <- LC.positions.p1 %>% dplyr::group_by(Transmitter)
  p1_Lingcod <- LC.positions.p1[!is.na(LC.positions.p1$Longitude) & !is.na(LC.positions.p1$Latitude),]
  p1_Lingcod.sp <- p1_Lingcod[, c("Transmitter", "Longitude", "Latitude")]
  
  skip_to_next <- FALSE
  
  tryCatch({coordinates(p1_Lingcod.sp) <- c("Longitude", "Latitude") 
  proj4string(p1_Lingcod.sp) <- CRS("+init=epsg:4326")}, 
  error = function(e) { skip_to_next <<- TRUE})
  
  if(skip_to_next) { next }    
  
  skip_to_next <- FALSE
  
  tryCatch(kernel.p1.Lingcod <- kernelUD(p1_Lingcod.sp, h = "href"), 
           error = function(e) { skip_to_next <<- TRUE})
  
  if(skip_to_next) { next }     
  
  skip_to_next <- FALSE
  
  tryCatch(Lingcod.p1.poly <- getverticeshr(kernel.p1.Lingcod, percr_cent = 95), 
           error = function(e) { skip_to_next <<- TRUE})
  
  if(skip_to_next) { next }     
  
  filename1 <- paste(fishind, "p7", "kml", sep = ".")
  
  Lingcod.p1.sp <- fortify(Lingcod.p1.poly)
  Lingcod.p1.df <- as.data.frame(Lingcod.p1.poly)
  
  j <- sub(".*9007-", "", fishind)
  
  assign(paste0("Lingcod.p7.", j, ".poly"), Lingcod.p1.poly)
  kmlPolygons(obj = Lingcod.p1.poly, kmlfile= filename1, col = "blue") # write kml file
  
  assign(paste0("Lingcod.p7.", j), Lingcod.p1.df)
  
}

for (i in seq_along(fishes)){
  fishind <- fishes[i]
  LC.positions.p1 <- Lingcod_accel %>% filter(survey.period == "June 27-July 4")%>% filter(Transmitter == fishind)
  LC.positions.p1 <- LC.positions.p1 %>% dplyr::group_by(Transmitter)
  p1_Lingcod <- LC.positions.p1[!is.na(LC.positions.p1$Longitude) & !is.na(LC.positions.p1$Latitude),]
  p1_Lingcod.sp <- p1_Lingcod[, c("Transmitter", "Longitude", "Latitude")]
  
  skip_to_next <- FALSE
  
  tryCatch({coordinates(p1_Lingcod.sp) <- c("Longitude", "Latitude") 
  proj4string(p1_Lingcod.sp) <- CRS("+init=epsg:4326")}, 
  error = function(e) { skip_to_next <<- TRUE})
  
  if(skip_to_next) { next }    
  
  skip_to_next <- FALSE
  
  tryCatch(kernel.p1.Lingcod <- kernelUD(p1_Lingcod.sp, h = "href"), 
           error = function(e) { skip_to_next <<- TRUE})
  
  if(skip_to_next) { next }     
  
  skip_to_next <- FALSE
  
  tryCatch(Lingcod.p1.poly <- getverticeshr(kernel.p1.Lingcod, percr_cent = 95), 
           error = function(e) { skip_to_next <<- TRUE})
  
  if(skip_to_next) { next }     
  
  filename1 <- paste(fishind, "p8", "kml", sep = ".")
  
  Lingcod.p1.sp <- fortify(Lingcod.p1.poly)
  Lingcod.p1.df <- as.data.frame(Lingcod.p1.poly)
  
  j <- sub(".*9007-", "", fishind)
  
  assign(paste0("Lingcod.p8.", j, ".poly"), Lingcod.p1.poly)
  kmlPolygons(obj = Lingcod.p1.poly, kmlfile= filename1, col = "blue") # write kml file
  
  assign(paste0("Lingcod.p8.", j), Lingcod.p1.df)
  
}

for (i in seq_along(fishes)){
  fishind <- fishes[i]
  LC.positions.p1 <- Lingcod_accel %>% filter(survey.period == "July 5-12")%>% filter(Transmitter == fishind)
  LC.positions.p1 <- LC.positions.p1 %>% dplyr::group_by(Transmitter)
  p1_Lingcod <- LC.positions.p1[!is.na(LC.positions.p1$Longitude) & !is.na(LC.positions.p1$Latitude),]
  p1_Lingcod.sp <- p1_Lingcod[, c("Transmitter", "Longitude", "Latitude")]
  
  skip_to_next <- FALSE
  
  tryCatch({coordinates(p1_Lingcod.sp) <- c("Longitude", "Latitude") 
  proj4string(p1_Lingcod.sp) <- CRS("+init=epsg:4326")}, 
  error = function(e) { skip_to_next <<- TRUE})
  
  if(skip_to_next) { next }    
  
  skip_to_next <- FALSE
  
  tryCatch(kernel.p1.Lingcod <- kernelUD(p1_Lingcod.sp, h = "href"), 
           error = function(e) { skip_to_next <<- TRUE})
  
  if(skip_to_next) { next }     
  
  skip_to_next <- FALSE
  
  tryCatch(Lingcod.p1.poly <- getverticeshr(kernel.p1.Lingcod, percr_cent = 95), 
           error = function(e) { skip_to_next <<- TRUE})
  
  if(skip_to_next) { next }     
  
  filename1 <- paste(fishind, "p9", "kml", sep = ".")
  
  Lingcod.p1.sp <- fortify(Lingcod.p1.poly)
  Lingcod.p1.df <- as.data.frame(Lingcod.p1.poly)
  
  j <- sub(".*9007-", "", fishind)
  
  assign(paste0("Lingcod.p9.", j, ".poly"), Lingcod.p1.poly)
  kmlPolygons(obj = Lingcod.p1.poly, kmlfile= filename1, col = "blue") # write kml file
  
  assign(paste0("Lingcod.p9.", j), Lingcod.p1.df)
  
}

## find centroid of all KUDs, all periods
library(sf)
library(ggplot2)
library(rgeos)

#each individual
LCfishpoly_listp1 <- list()
LCfishpoly_listp1 <- c(Lingcod.p1.13249.poly, Lingcod.p1.13250.poly, Lingcod.p1.13251.poly, Lingcod.p1.13254.poly,
                     Lingcod.p1.13255.poly, Lingcod.p1.13256.poly)

LCfishpoly_listp2 <- list()
LCfishpoly_listp2 <- c(Lingcod.p2.13249.poly, Lingcod.p2.13254.poly, Lingcod.p2.13255.poly, Lingcod.p2.13256.poly,
                     Lingcod.p2.13258.poly, Lingcod.p2.13260.poly)

LCfishpoly_listp3 <- list()
LCfishpoly_listp3 <- c(Lingcod.p3.13249.poly, Lingcod.p3.13250.poly, Lingcod.p3.13254.poly, Lingcod.p3.13255.poly,
                     Lingcod.p3.13256.poly, Lingcod.p3.13260.poly)


LCfishpoly_listp5 <- list()
LCfishpoly_listp5 <- c(Lingcod.p5.13249.poly, Lingcod.p5.13250.poly, Lingcod.p5.13251.poly, Lingcod.p5.13255.poly,
                     Lingcod.p5.13256.poly)

LCfishpoly_list18 <- list()
LCfishpoly_list18 <- c(Lingcod.18.13249.poly, Lingcod.18.13255.poly, Lingcod.18.13263.poly)

LCfishpoly_listp7 <- list()
LCfishpoly_listp7 <- c(Lingcod.p7.13249.poly, Lingcod.p7.13253.poly, Lingcod.p7.13254.poly, Lingcod.p7.13255.poly,
                     Lingcod.p7.13256.poly, Lingcod.p7.13263.poly)

LCfishpoly_listp8 <- list()
LCfishpoly_listp8 <- c(Lingcod.p8.13249.poly, Lingcod.p8.13253.poly, Lingcod.p8.13254.poly, Lingcod.p8.13255.poly,
                     Lingcod.p8.13256.poly, Lingcod.p8.13258.poly)

LCfishpoly_listp9 <- list()
LCfishpoly_listp9 <- c(Lingcod.p9.13249.poly, Lingcod.p9.13253.poly, Lingcod.p9.13254.poly, Lingcod.p9.13255.poly,
                     Lingcod.p9.13256.poly, Lingcod.p9.13263.poly)



for (i in seq_along(LCfishpoly_listp1)){
  fishind <- LCfishpoly_listp1[[i]]
  
  dat <- fishind
  fishname <- dat$id
  
  sp_cent <- gCentroid(dat, byid = TRUE)
  sp_cent <- sp_cent %>% st_as_sf()
  
  filename1 <- paste("Lingcod", fishname, "p1", "centroid", sep = ".")
  assign(filename1, sp_cent)
  
}

for (i in seq_along(LCfishpoly_listp2)){
  fishind <- LCfishpoly_listp2[[i]]
  
  dat <- fishind
  fishname <- dat$id
  
  sp_cent <- gCentroid(dat, byid = TRUE)
  sp_cent <- sp_cent %>% st_as_sf()
  
  filename1 <- paste("Lingcod", fishname, "p2", "centroid", sep = ".")
  assign(filename1, sp_cent)
  
}

for (i in seq_along(LCfishpoly_listp3)){
  fishind <- LCfishpoly_listp3[[i]]
  
  dat <- fishind
  fishname <- dat$id
  
  sp_cent <- gCentroid(dat, byid = TRUE)
  sp_cent <- sp_cent %>% st_as_sf()
  
  filename1 <- paste("Lingcod", fishname, "p3", "centroid", sep = ".")
  assign(filename1, sp_cent)
  
}


for (i in seq_along(LCfishpoly_listp5)){
  fishind <- LCfishpoly_listp5[[i]]
  
  dat <- fishind
  fishname <- dat$id
  
  sp_cent <- gCentroid(dat, byid = TRUE)
  sp_cent <- sp_cent %>% st_as_sf()
  
  filename1 <- paste("Lingcod", fishname, "p5", "centroid", sep = ".")
  assign(filename1, sp_cent)
  
}

for (i in seq_along(LCfishpoly_list18)){
  fishind <- LCfishpoly_list18[[i]]
  
  dat <- fishind
  fishname <- dat$id
  
  sp_cent <- gCentroid(dat, byid = TRUE)
  sp_cent <- sp_cent %>% st_as_sf()
  
  filename1 <- paste("Lingcod", fishname, "18", "centroid", sep = ".")
  assign(filename1, sp_cent)
  
}

for (i in seq_along(LCfishpoly_listp7)){
  fishind <- LCfishpoly_listp7[[i]]
  
  dat <- fishind
  fishname <- dat$id
  
  sp_cent <- gCentroid(dat, byid = TRUE)
  sp_cent <- sp_cent %>% st_as_sf()
  
  filename1 <- paste("Lingcod", fishname, "p7", "centroid", sep = ".")
  assign(filename1, sp_cent)
  
}

for (i in seq_along(LCfishpoly_listp8)){
  fishind <- LCfishpoly_listp8[[i]]
  
  dat <- fishind
  fishname <- dat$id
  
  sp_cent <- gCentroid(dat, byid = TRUE)
  sp_cent <- sp_cent %>% st_as_sf()
  
  filename1 <- paste("Lingcod", fishname, "p8", "centroid", sep = ".")
  assign(filename1, sp_cent)
  
}

for (i in seq_along(LCfishpoly_listp9)){
  fishind <- LCfishpoly_listp9[[i]]
  
  dat <- fishind
  fishname <- dat$id
  
  sp_cent <- gCentroid(dat, byid = TRUE)
  sp_cent <- sp_cent %>% st_as_sf()
  
  filename1 <- paste("Lingcod", fishname, "p9", "centroid", sep = ".")
  assign(filename1, sp_cent)
  
}

area_13249 <- rbind(Lingcod.p1.13249, Lingcod.p2.13249, Lingcod.p3.13249,
                    Lingcod.p5.13249, Lingcod.18.13249, Lingcod.p7.13249, Lingcod.p8.13249, Lingcod.p9.13249)
area_13249 <- area_13249 %>% mutate(id = "13249")
area13249 <- data.frame(area = area_13249$area, id = area_13249$id, per = c("p1", "p2", "p3", "p5", "18", "p7", "p8", "p9"))

area_13250 <- rbind(Lingcod.p1.13250, Lingcod.p3.13250,
                    Lingcod.p5.13250)
area_13250 <- area_13250 %>% mutate(id = "13250")
area13250 <- data.frame(area = area_13250$area, id = area_13250$id, per = c("p1", "p3", "p5"))
