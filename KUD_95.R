### spatial UD for animals 
library(rgdal)
library(rgeos)
library(raster)
library(adehabitatHR)
library(tidyverse)
library(cowplot)
library(showtext)
showtext_auto()

BlackR_accel <- periods_BlackR_accel


#kmlPolygons(obj = BlackR.p1.poly, kmlfile="BlackR.p1.kml", col = "blue")


## Second try Period 1!!
fish1 <- BlackR_accel %>% dplyr::filter(Transmitter == "A69-9007-12048")
fish1 <- fish1[,c("Latitude", "Longitude", "survey.period", "detect_day", "detect_time", "Transmitter")]
fishall <- BlackR_accel
fishall <- fishall[,c("Latitude", "Longitude", "survey.period", "detect_day", "detect_time", "Transmitter")]
fishlist <- split(fishall, f = fishall$Transmitter)
fishes <- unique(fishall$Transmitter)
fishes <- fishes[!is.na(fishes)]
brperiodslist <- split(fish1, f = c(fish1$survey.period))
fishcols <- length(fishlist)
outdf <- data.frame(matrix(ncol = fishcols, nrow = 0))

for (i in seq_along(fishes)){
  fishind <- fishes[i]
  BR.positions.p1 <- BlackR_accel %>% filter(survey.period == "May 10-17")%>% filter(Transmitter == fishind)
  BR.positions.p1 <- BR.positions.p1 %>% dplyr::group_by(Transmitter)
  p1_BlackR <- BR.positions.p1[!is.na(BR.positions.p1$Longitude) & !is.na(BR.positions.p1$Latitude),]
  p1_BlackR.sp <- p1_BlackR[, c("Transmitter", "Longitude", "Latitude")]
  
  skip_to_next <- FALSE
  
  tryCatch({coordinates(p1_BlackR.sp) <- c("Longitude", "Latitude") 
  proj4string(p1_BlackR.sp) <- CRS("+init=epsg:4326")}, 
  error = function(e) { skip_to_next <<- TRUE})
  
  if(skip_to_next) { next }    
  
  skip_to_next <- FALSE
  
  tryCatch(kernel.p1.BlackR <- kernelUD(p1_BlackR.sp, h = "href"), 
           error = function(e) { skip_to_next <<- TRUE})
  
  if(skip_to_next) { next }     
  
  skip_to_next <- FALSE
  
  tryCatch(BlackR.p1.poly <- getverticeshr(kernel.p1.BlackR, percr_cent = 95), 
           error = function(e) { skip_to_next <<- TRUE})
  
  if(skip_to_next) { next }     
  
  filename1 <- paste(fishind, "p0", "kml", sep = ".")
  
  BlackR.p1.sp <- fortify(BlackR.p1.poly)
  BlackR.p1.df <- as.data.frame(BlackR.p1.poly)
  
  j <- sub(".*9007-", "", fishind)
  
  assign(paste0("BlackR.p0.", j, ".poly"), BlackR.p1.poly)
  
  assign(paste0("BlackR.p0.", j), BlackR.p1.df)
  
}

for (i in seq_along(fishes)){
  fishind <- fishes[i]
  BR.positions.p1 <- BlackR_accel %>% filter(survey.period == "May 18-25")%>% filter(Transmitter == fishind)
  BR.positions.p1 <- BR.positions.p1 %>% dplyr::group_by(Transmitter)
  p1_BlackR <- BR.positions.p1[!is.na(BR.positions.p1$Longitude) & !is.na(BR.positions.p1$Latitude),]
  p1_BlackR.sp <- p1_BlackR[, c("Transmitter", "Longitude", "Latitude")]
  
  skip_to_next <- FALSE
  
  tryCatch({coordinates(p1_BlackR.sp) <- c("Longitude", "Latitude") 
  proj4string(p1_BlackR.sp) <- CRS("+init=epsg:4326")}, 
  error = function(e) { skip_to_next <<- TRUE})
  
  if(skip_to_next) { next }    
  
  skip_to_next <- FALSE
  
  tryCatch(kernel.p1.BlackR <- kernelUD(p1_BlackR.sp, h = "href"), 
           error = function(e) { skip_to_next <<- TRUE})
  
  if(skip_to_next) { next }     
  
  skip_to_next <- FALSE
  
  tryCatch(BlackR.p1.poly <- getverticeshr(kernel.p1.BlackR, percr_cent = 95), 
           error = function(e) { skip_to_next <<- TRUE})
  
  if(skip_to_next) { next }     
  
  filename1 <- paste(fishind, "p1", "kml", sep = ".")
  
  BlackR.p1.sp <- fortify(BlackR.p1.poly)
  BlackR.p1.df <- as.data.frame(BlackR.p1.poly)
  
  j <- sub(".*9007-", "", fishind)
  
  assign(paste0("BlackR.p1.", j, ".poly"), BlackR.p1.poly)
  
  assign(paste0("BlackR.p1.", j), BlackR.p1.df)
  
}

for (i in seq_along(fishes)){
  fishind <- fishes[i]
  BR.positions.p1 <- BlackR_accel %>% filter(survey.period == "May 26-June 2")%>% filter(Transmitter == fishind)
  BR.positions.p1 <- BR.positions.p1 %>% dplyr::group_by(Transmitter)
  p1_BlackR <- BR.positions.p1[!is.na(BR.positions.p1$Longitude) & !is.na(BR.positions.p1$Latitude),]
  p1_BlackR.sp <- p1_BlackR[, c("Transmitter", "Longitude", "Latitude")]
  
  skip_to_next <- FALSE
  
  tryCatch({coordinates(p1_BlackR.sp) <- c("Longitude", "Latitude") 
  proj4string(p1_BlackR.sp) <- CRS("+init=epsg:4326")}, 
  error = function(e) { skip_to_next <<- TRUE})
  
  if(skip_to_next) { next }    
  
  skip_to_next <- FALSE
  
  tryCatch(kernel.p1.BlackR <- kernelUD(p1_BlackR.sp, h = "href"), 
           error = function(e) { skip_to_next <<- TRUE})
  
  if(skip_to_next) { next }     
  
  skip_to_next <- FALSE
  
  tryCatch(BlackR.p1.poly <- getverticeshr(kernel.p1.BlackR, percr_cent = 95), 
           error = function(e) { skip_to_next <<- TRUE})
  
  if(skip_to_next) { next }     
  
  filename1 <- paste(fishind, "p2", "kml", sep = ".")
  
  BlackR.p1.sp <- fortify(BlackR.p1.poly)
  BlackR.p1.df <- as.data.frame(BlackR.p1.poly)
  
  j <- sub(".*9007-", "", fishind)
  
  assign(paste0("BlackR.p2.", j, ".poly"), BlackR.p1.poly)
  
  assign(paste0("BlackR.p2.", j), BlackR.p1.df)
  
}

for (i in seq_along(fishes)){
  fishind <- fishes[i]
  BR.positions.p1 <- BlackR_accel %>% filter(survey.period == "June 3-10")%>% filter(Transmitter == fishind)
  BR.positions.p1 <- BR.positions.p1 %>% dplyr::group_by(Transmitter)
  p1_BlackR <- BR.positions.p1[!is.na(BR.positions.p1$Longitude) & !is.na(BR.positions.p1$Latitude),]
  p1_BlackR.sp <- p1_BlackR[, c("Transmitter", "Longitude", "Latitude")]
  
  skip_to_next <- FALSE
  
  tryCatch({coordinates(p1_BlackR.sp) <- c("Longitude", "Latitude") 
  proj4string(p1_BlackR.sp) <- CRS("+init=epsg:4326")}, 
  error = function(e) { skip_to_next <<- TRUE})
  
  if(skip_to_next) { next }    
  
  skip_to_next <- FALSE
  
  tryCatch(kernel.p1.BlackR <- kernelUD(p1_BlackR.sp, h = "href"), 
           error = function(e) { skip_to_next <<- TRUE})
  
  if(skip_to_next) { next }     
  
  skip_to_next <- FALSE
  
  tryCatch(BlackR.p1.poly <- getverticeshr(kernel.p1.BlackR, percr_cent = 95), 
           error = function(e) { skip_to_next <<- TRUE})
  
  if(skip_to_next) { next }     
  
  filename1 <- paste(fishind, "p3", "kml", sep = ".")
  
  BlackR.p1.sp <- fortify(BlackR.p1.poly)
  BlackR.p1.df <- as.data.frame(BlackR.p1.poly)
  
  j <- sub(".*9007-", "", fishind)
  
  assign(paste0("BlackR.p3.", j, ".poly"), BlackR.p1.poly)
  
  assign(paste0("BlackR.p3.", j), BlackR.p1.df)
  
}

for (i in seq_along(fishes)){
  fishind <- fishes[i]
  BR.positions.p1 <- BlackR_accel %>% filter(survey.period == "June 11")%>% filter(Transmitter == fishind)
  BR.positions.p1 <- BR.positions.p1 %>% dplyr::group_by(Transmitter)
  p1_BlackR <- BR.positions.p1[!is.na(BR.positions.p1$Longitude) & !is.na(BR.positions.p1$Latitude),]
  p1_BlackR.sp <- p1_BlackR[, c("Transmitter", "Longitude", "Latitude")]
  
  skip_to_next <- FALSE
  
  tryCatch({coordinates(p1_BlackR.sp) <- c("Longitude", "Latitude") 
  proj4string(p1_BlackR.sp) <- CRS("+init=epsg:4326")}, 
  error = function(e) { skip_to_next <<- TRUE})
  
  if(skip_to_next) { next }    
  
  skip_to_next <- FALSE
  
  tryCatch(kernel.p1.BlackR <- kernelUD(p1_BlackR.sp, h = "href"), 
           error = function(e) { skip_to_next <<- TRUE})
  
  if(skip_to_next) { next }     
  
  skip_to_next <- FALSE
  
  tryCatch(BlackR.p1.poly <- getverticeshr(kernel.p1.BlackR, percr_cent = 95), 
           error = function(e) { skip_to_next <<- TRUE})
  
  if(skip_to_next) { next }     
  
  filename1 <- paste(fishind, "11", "kml", sep = ".")
  
  BlackR.p1.sp <- fortify(BlackR.p1.poly)
  BlackR.p1.df <- as.data.frame(BlackR.p1.poly)
  
  j <- sub(".*9007-", "", fishind)
  
  assign(paste0("BlackR.11.", j, ".poly"), BlackR.p1.poly)
  
  assign(paste0("BlackR.11.", j), BlackR.p1.df)
  
}

for (i in seq_along(fishes)){
  fishind <- fishes[i]
  BR.positions.p1 <- BlackR_accel %>% filter(survey.period == "June 12-16")%>% filter(Transmitter == fishind)
  BR.positions.p1 <- BR.positions.p1 %>% dplyr::group_by(Transmitter)
  p1_BlackR <- BR.positions.p1[!is.na(BR.positions.p1$Longitude) & !is.na(BR.positions.p1$Latitude),]
  p1_BlackR.sp <- p1_BlackR[, c("Transmitter", "Longitude", "Latitude")]
  
  skip_to_next <- FALSE
  
  tryCatch({coordinates(p1_BlackR.sp) <- c("Longitude", "Latitude") 
  proj4string(p1_BlackR.sp) <- CRS("+init=epsg:4326")}, 
  error = function(e) { skip_to_next <<- TRUE})
  
  if(skip_to_next) { next }    
  
  skip_to_next <- FALSE
  
  tryCatch(kernel.p1.BlackR <- kernelUD(p1_BlackR.sp, h = "href"), 
           error = function(e) { skip_to_next <<- TRUE})
  
  if(skip_to_next) { next }     
  
  skip_to_next <- FALSE
  
  tryCatch(BlackR.p1.poly <- getverticeshr(kernel.p1.BlackR, percr_cent = 95), 
           error = function(e) { skip_to_next <<- TRUE})
  
  if(skip_to_next) { next }     
  
  filename1 <- paste(fishind, "p5", "kml", sep = ".")
  
  BlackR.p1.sp <- fortify(BlackR.p1.poly)
  BlackR.p1.df <- as.data.frame(BlackR.p1.poly)
  
  j <- sub(".*9007-", "", fishind)
  
  assign(paste0("BlackR.p5.", j, ".poly"), BlackR.p1.poly)
  
  assign(paste0("BlackR.p5.", j), BlackR.p1.df)
  
}

for (i in seq_along(fishes)){
  fishind <- fishes[i]
  BR.positions.p1 <- BlackR_accel %>% filter(survey.period == "June 18")%>% filter(Transmitter == fishind)
  BR.positions.p1 <- BR.positions.p1 %>% dplyr::group_by(Transmitter)
  p1_BlackR <- BR.positions.p1[!is.na(BR.positions.p1$Longitude) & !is.na(BR.positions.p1$Latitude),]
  p1_BlackR.sp <- p1_BlackR[, c("Transmitter", "Longitude", "Latitude")]
  
  skip_to_next <- FALSE
  
  tryCatch({coordinates(p1_BlackR.sp) <- c("Longitude", "Latitude") 
  proj4string(p1_BlackR.sp) <- CRS("+init=epsg:4326")}, 
  error = function(e) { skip_to_next <<- TRUE})
  
  if(skip_to_next) { next }    
  
  skip_to_next <- FALSE
  
  tryCatch(kernel.p1.BlackR <- kernelUD(p1_BlackR.sp, h = "href"), 
           error = function(e) { skip_to_next <<- TRUE})
  
  if(skip_to_next) { next }     
  
  skip_to_next <- FALSE
  
  tryCatch(BlackR.p1.poly <- getverticeshr(kernel.p1.BlackR, percr_cent = 95), 
           error = function(e) { skip_to_next <<- TRUE})
  
  if(skip_to_next) { next }     
  
  filename1 <- paste(fishind, "18", "kml", sep = ".")
  
  BlackR.p1.sp <- fortify(BlackR.p1.poly)
  BlackR.p1.df <- as.data.frame(BlackR.p1.poly)
  
  j <- sub(".*9007-", "", fishind)
  
  assign(paste0("BlackR.18.", j, ".poly"), BlackR.p1.poly)
  
  assign(paste0("BlackR.18.", j), BlackR.p1.df)
  
}

for (i in seq_along(fishes)){
  fishind <- fishes[i]
  BR.positions.p1 <- BlackR_accel %>% filter(survey.period == "June 19-26")%>% filter(Transmitter == fishind)
  BR.positions.p1 <- BR.positions.p1 %>% dplyr::group_by(Transmitter)
  p1_BlackR <- BR.positions.p1[!is.na(BR.positions.p1$Longitude) & !is.na(BR.positions.p1$Latitude),]
  p1_BlackR.sp <- p1_BlackR[, c("Transmitter", "Longitude", "Latitude")]
  
  skip_to_next <- FALSE
  
  tryCatch({coordinates(p1_BlackR.sp) <- c("Longitude", "Latitude") 
  proj4string(p1_BlackR.sp) <- CRS("+init=epsg:4326")}, 
  error = function(e) { skip_to_next <<- TRUE})
  
  if(skip_to_next) { next }    
  
  skip_to_next <- FALSE
  
  tryCatch(kernel.p1.BlackR <- kernelUD(p1_BlackR.sp, h = "href"), 
           error = function(e) { skip_to_next <<- TRUE})
  
  if(skip_to_next) { next }     
  
  skip_to_next <- FALSE
  
  tryCatch(BlackR.p1.poly <- getverticeshr(kernel.p1.BlackR, percr_cent = 95), 
           error = function(e) { skip_to_next <<- TRUE})
  
  if(skip_to_next) { next }     
  
  filename1 <- paste(fishind, "p7", "kml", sep = ".")
  
  BlackR.p1.sp <- fortify(BlackR.p1.poly)
  BlackR.p1.df <- as.data.frame(BlackR.p1.poly)
  
  j <- sub(".*9007-", "", fishind)
  
  assign(paste0("BlackR.p7.", j, ".poly"), BlackR.p1.poly)
  
  assign(paste0("BlackR.p7.", j), BlackR.p1.df)
  
}

for (i in seq_along(fishes)){
  fishind <- fishes[i]
  BR.positions.p1 <- BlackR_accel %>% filter(survey.period == "June 27-July 4")%>% filter(Transmitter == fishind)
  BR.positions.p1 <- BR.positions.p1 %>% dplyr::group_by(Transmitter)
  p1_BlackR <- BR.positions.p1[!is.na(BR.positions.p1$Longitude) & !is.na(BR.positions.p1$Latitude),]
  p1_BlackR.sp <- p1_BlackR[, c("Transmitter", "Longitude", "Latitude")]
  
  skip_to_next <- FALSE
  
  tryCatch({coordinates(p1_BlackR.sp) <- c("Longitude", "Latitude") 
  proj4string(p1_BlackR.sp) <- CRS("+init=epsg:4326")}, 
  error = function(e) { skip_to_next <<- TRUE})
  
  if(skip_to_next) { next }    
  
  skip_to_next <- FALSE
  
  tryCatch(kernel.p1.BlackR <- kernelUD(p1_BlackR.sp, h = "href"), 
           error = function(e) { skip_to_next <<- TRUE})
  
  if(skip_to_next) { next }     
  
  skip_to_next <- FALSE
  
  tryCatch(BlackR.p1.poly <- getverticeshr(kernel.p1.BlackR, percr_cent = 95), 
           error = function(e) { skip_to_next <<- TRUE})
  
  if(skip_to_next) { next }     
  
  filename1 <- paste(fishind, "p8", "kml", sep = ".")
  
  BlackR.p1.sp <- fortify(BlackR.p1.poly)
  BlackR.p1.df <- as.data.frame(BlackR.p1.poly)
  
  j <- sub(".*9007-", "", fishind)
  
  assign(paste0("BlackR.p8.", j, ".poly"), BlackR.p1.poly)
  
  assign(paste0("BlackR.p8.", j), BlackR.p1.df)
  
}

for (i in seq_along(fishes)){
  fishind <- fishes[i]
  BR.positions.p1 <- BlackR_accel %>% filter(survey.period == "July 5-12")%>% filter(Transmitter == fishind)
  BR.positions.p1 <- BR.positions.p1 %>% dplyr::group_by(Transmitter)
  p1_BlackR <- BR.positions.p1[!is.na(BR.positions.p1$Longitude) & !is.na(BR.positions.p1$Latitude),]
  p1_BlackR.sp <- p1_BlackR[, c("Transmitter", "Longitude", "Latitude")]
  
  skip_to_next <- FALSE
  
  tryCatch({coordinates(p1_BlackR.sp) <- c("Longitude", "Latitude") 
  proj4string(p1_BlackR.sp) <- CRS("+init=epsg:4326")}, 
  error = function(e) { skip_to_next <<- TRUE})
  
  if(skip_to_next) { next }    
  
  skip_to_next <- FALSE
  
  tryCatch(kernel.p1.BlackR <- kernelUD(p1_BlackR.sp, h = "href"), 
           error = function(e) { skip_to_next <<- TRUE})
  
  if(skip_to_next) { next }     
  
  skip_to_next <- FALSE
  
  tryCatch(BlackR.p1.poly <- getverticeshr(kernel.p1.BlackR, percr_cent = 95), 
           error = function(e) { skip_to_next <<- TRUE})
  
  if(skip_to_next) { next }     
  
  filename1 <- paste(fishind, "p9", "kml", sep = ".")
  
  BlackR.p1.sp <- fortify(BlackR.p1.poly)
  BlackR.p1.df <- as.data.frame(BlackR.p1.poly)
  
  j <- sub(".*9007-", "", fishind)
  
  assign(paste0("BlackR.p9.", j, ".poly"), BlackR.p1.poly)
  
  assign(paste0("BlackR.p9.", j), BlackR.p1.df)
  
}

#################3 STOP HERE

for (i in seq_along(fishes)){
  fishind <- fishes[i]
  BR.positions.p1 <- BlackR_accel %>% filter(survey.period == "July 5-12")%>% filter(Transmitter == fishind)
  BR.positions.p1 <- BR.positions.p1 %>% dplyr::group_by(Transmitter)
  p1_BlackR <- BR.positions.p1[!is.na(BR.positions.p1$Longitude) & !is.na(BR.positions.p1$Latitude),]
  p1_BlackR.sp <- p1_BlackR[, c("Transmitter", "Longitude", "Latitude")]
  
  skip_to_next <- FALSE
  
  tryCatch({coordinates(p1_BlackR.sp) <- c("Longitude", "Latitude") 
  proj4string(p1_BlackR.sp) <- CRS("+init=epsg:4326")}, 
  error = function(e) { skip_to_next <<- TRUE})
  
  if(skip_to_next) { next }    
  
  skip_to_next <- FALSE
  
  tryCatch(kernel.p1.BlackR <- kernelUD(p1_BlackR.sp, h = "href"), 
           error = function(e) { skip_to_next <<- TRUE})
  
  if(skip_to_next) { next }     
  
  skip_to_next <- FALSE
  
  tryCatch(BlackR.p1.poly <- getverticeshr(kernel.p1.BlackR, percent = 95), 
           error = function(e) { skip_to_next <<- TRUE})
  
  if(skip_to_next) { next }     
  
  filename1 <- paste(fishind, "p9", "kml", sep = ".")
  
  #kmlPolygons(obj = BlackR.p1.poly, kmlfile= filename1, col = "blue") # write kml file

  
  BlackR.p1.sp <- fortify(BlackR.p1.poly)
  BlackR.p1.df <- as.data.frame(BlackR.p1.poly)
  
  j <- sub(".*9007-", "", fishind)
  
  assign(paste0("BlackR.p9.", j, ".poly"), BlackR.p1.poly)
  
  assign(paste0("BlackR.p9.", j), BlackR.p1.df)
  #outdf[i] <- BlackR.p1.df
  
}





i <- 14 ## update for each iteration instead of copy paste and change transmitter
fishind <- fishes[i]
BR.positions.p1 <- BlackR_accel %>% filter(survey.period == "May 18-25") %>% filter(Transmitter == fishind)
BR.positions.p1 <- BR.positions.p1 %>% dplyr::group_by(Transmitter)
p1_BlackR <- BR.positions.p1[!is.na(BR.positions.p1$Longitude) & !is.na(BR.positions.p1$Latitude),]
p1_BlackR.sp <- p1_BlackR[, c("Transmitter", "Longitude", "Latitude")]
coordinates(p1_BlackR.sp) <- c("Longitude", "Latitude")
proj4string(p1_BlackR.sp) <- CRS("+init=epsg:4326")

kernel.p1.BlackR <- kernelUD(p1_BlackR.sp, h = "href")
BlackR.p1.poly <- getverticeshr(kernel.p1.BlackR, percent = 95)

BlackR.p1.sp <- fortify(BlackR.p1.poly)
BlackR.p1.df <- as.data.frame(BlackR.p1.poly)
fishind

BlackR.p1.12050 <- BlackR.p1.df
BlackR.p1.12056 <- BlackR.p1.df
BlackR.p1.12054 <- BlackR.p1.df
BlackR.p1.12052 <- BlackR.p1.df
BlackR.p1.12070 <- BlackR.p1.df
BlackR.p1.12060 <- BlackR.p1.df
BlackR.p1.12066 <- BlackR.p1.df
BlackR.p1.12058 <- BlackR.p1.df
BlackR.p1.12076 <- BlackR.p1.df
BlackR.p1.12068 <- BlackR.p1.df
BlackR.p1.12048 <- BlackR.p1.df
BlackR.p1.12074 <- BlackR.p1.df
BlackR.p1.12062 <- BlackR.p1.df
BlackR.p1.12064 <- BlackR.p1.df



#May 26-June 2
i <- 14 ## update for each iteration instead of copy paste and change transmitter
fishind <- fishes[i]
BR.positions.p2 <- BlackR_accel %>% filter(survey.period == "May 26-June 2") %>% filter(Transmitter == fishind)
BR.positions.p2 <- BR.positions.p2 %>% dplyr::group_by(Transmitter)
p2_BlackR <- BR.positions.p2[!is.na(BR.positions.p2$Longitude) & !is.na(BR.positions.p2$Latitude),]
p2_BlackR.sp <- p2_BlackR[, c("Transmitter", "Longitude", "Latitude")]
coordinates(p2_BlackR.sp) <- c("Longitude", "Latitude")
proj4string(p2_BlackR.sp) <- CRS("+init=epsg:4326")

kernel.p2.BlackR <- kernelUD(p2_BlackR.sp, h = "href")
BlackR.p2.poly <- getverticeshr(kernel.p2.BlackR, percent = 95)

BlackR.p2.sp <- fortify(BlackR.p2.poly)
BlackR.p2.df <- as.data.frame(BlackR.p2.poly)
fishind

BlackR.p2.12050 <- BlackR.p2.df
BlackR.p2.12056 <- BlackR.p2.df
BlackR.p2.12054 <- BlackR.p2.df
BlackR.p2.12052 <- BlackR.p2.df
BlackR.p2.12070 <- BlackR.p2.df
BlackR.p2.12060 <- BlackR.p2.df
BlackR.p2.12066 <- BlackR.p2.df
BlackR.p2.12058 <- BlackR.p2.df
BlackR.p2.12076 <- BlackR.p2.df
BlackR.p2.12068 <- BlackR.p2.df
BlackR.p2.12048 <- BlackR.p2.df
BlackR.p2.12074 <- BlackR.p2.df
BlackR.p2.12062 <- BlackR.p2.df
BlackR.p2.12064 <- BlackR.p2.df

#June 3-10
i <- 14 ## update for each iteration instead of copy paste and change transmitter
fishind <- fishes[i]
BR.positions.p3 <- BlackR_accel %>% filter(survey.period == "June 3-10") %>% filter(Transmitter == fishind)
BR.positions.p3 <- BR.positions.p3 %>% dplyr::group_by(Transmitter)
p3_BlackR <- BR.positions.p3[!is.na(BR.positions.p3$Longitude) & !is.na(BR.positions.p3$Latitude),]
p3_BlackR.sp <- p3_BlackR[, c("Transmitter", "Longitude", "Latitude")]
coordinates(p3_BlackR.sp) <- c("Longitude", "Latitude")
proj4string(p3_BlackR.sp) <- CRS("+init=epsg:4326")

kernel.p3.BlackR <- kernelUD(p3_BlackR.sp, h = "href")
BlackR.p3.poly <- getverticeshr(kernel.p3.BlackR, percent = 95)

BlackR.p3.sp <- fortify(BlackR.p3.poly)
BlackR.p3.df <- as.data.frame(BlackR.p3.poly)
fishind

BlackR.p3.12050 <- BlackR.p3.df
BlackR.p3.12056 <- BlackR.p3.df
BlackR.p3.12054 <- BlackR.p3.df
BlackR.p3.12052 <- BlackR.p3.df
BlackR.p3.12070 <- BlackR.p3.df
BlackR.p3.12060 <- BlackR.p3.df
BlackR.p3.12066 <- BlackR.p3.df
BlackR.p3.12058 <- BlackR.p3.df
BlackR.p3.12076 <- BlackR.p3.df
BlackR.p3.12068 <- BlackR.p3.df
BlackR.p3.12048 <- BlackR.p3.df
BlackR.p3.12074 <- BlackR.p3.df
BlackR.p3.12062 <- BlackR.p3.df
BlackR.p3.12064 <- BlackR.p3.df


#June 11
i <- 14 ## update for each iteration instead of copy paste and change transmitter
fishind <- fishes[i]
BR.positions.11 <- BlackR_accel %>% filter(survey.period == "June 11") %>% filter(Transmitter == fishind)
BR.positions.11 <- BR.positions.11 %>% dplyr::group_by(Transmitter)
p4_BlackR <- BR.positions.11[!is.na(BR.positions.11$Longitude) & !is.na(BR.positions.11$Latitude),]
p4_BlackR.sp <- p4_BlackR[, c("Transmitter", "Longitude", "Latitude")]
coordinates(p4_BlackR.sp) <- c("Longitude", "Latitude")
proj4string(p4_BlackR.sp) <- CRS("+init=epsg:4326")

kernel.11.BlackR <- kernelUD(p4_BlackR.sp, h = "href")
BlackR.11.poly <- getverticeshr(kernel.11.BlackR, percent = 95)

BlackR.11.sp <- fortify(BlackR.11.poly)
BlackR.11.df <- as.data.frame(BlackR.11.poly)
fishind

BlackR.11.12050 <- BlackR.11.df
#### 12056 didn't hear from on June 11
################################## 12054 didn't hear from on June 11 
BlackR.11.12052 <- BlackR.11.df
BlackR.11.12070 <- BlackR.11.df
BlackR.11.12060 <- BlackR.11.df
BlackR.11.12066 <- BlackR.11.df
## not enough relocations for fish 12058
BlackR.11.12076 <- BlackR.11.df
## not enough relocations for fish 12068
BlackR.11.12048 <- BlackR.11.df
BlackR.11.12074 <- BlackR.11.df
BlackR.11.12062 <- BlackR.11.df
BlackR.11.12064 <- BlackR.11.df


#June 12-16
i <- 14 ## update for each iteration instead of copy paste and change transmitter
fishind <- fishes[i]
BR.positions.p5 <- BlackR_accel %>% filter(survey.period == "June 12-16") %>% filter(Transmitter == fishind)
BR.positions.p5 <- BR.positions.p5 %>% dplyr::group_by(Transmitter)
p5_BlackR <- BR.positions.p5[!is.na(BR.positions.p5$Longitude) & !is.na(BR.positions.p5$Latitude),]
p5_BlackR.sp <- p5_BlackR[, c("Transmitter", "Longitude", "Latitude")]
coordinates(p5_BlackR.sp) <- c("Longitude", "Latitude")
proj4string(p5_BlackR.sp) <- CRS("+init=epsg:4326")
kernel.p5.BlackR <- kernelUD(p5_BlackR.sp, h = "href")
BlackR.p5.poly <- getverticeshr(kernel.p5.BlackR, percent = 95)
BlackR.p5.sp <- fortify(BlackR.p5.poly)
BlackR.p5.df <- as.data.frame(BlackR.p5.poly)
fishind

BlackR.p5.12050 <- BlackR.p5.df
#### not enough relocations for 12056
BlackR.p5.12054 <- BlackR.p5.df
BlackR.p5.12052 <- BlackR.p5.df
BlackR.p5.12070 <- BlackR.p5.df
BlackR.p5.12060 <- BlackR.p5.df
BlackR.p5.12066 <- BlackR.p5.df
BlackR.p5.12058 <- BlackR.p5.df
BlackR.p5.12076 <- BlackR.p5.df
BlackR.p5.12068 <- BlackR.p5.df
BlackR.p5.12048 <- BlackR.p5.df
BlackR.p5.12074 <- BlackR.p5.df
BlackR.p5.12062 <- BlackR.p5.df
BlackR.p5.12064 <- BlackR.p5.df


#June 18
i <- 14 ## update for each iteration instead of copy paste and change transmitter
fishind <- fishes[i]
BR.positions.18 <- BlackR_accel %>% filter(survey.period == "June 18") %>% filter(Transmitter == fishind)
BR.positions.18 <- BR.positions.18 %>% dplyr::group_by(Transmitter)
p6_BlackR <- BR.positions.18[!is.na(BR.positions.18$Longitude) & !is.na(BR.positions.18$Latitude),]
p6_BlackR.sp <- p6_BlackR[, c("Transmitter", "Longitude", "Latitude")]
coordinates(p6_BlackR.sp) <- c("Longitude", "Latitude")
proj4string(p6_BlackR.sp) <- CRS("+init=epsg:4326")
kernel.18.BlackR <- kernelUD(p6_BlackR.sp, h = "href")
BlackR.18.poly <- getverticeshr(kernel.18.BlackR, percent = 95)
BlackR.18.sp <- fortify(BlackR.18.poly)
BlackR.18.df <- as.data.frame(BlackR.18.poly)
fishind

BlackR.18.12050 <- BlackR.18.df
BlackR.18.12048 <- BlackR.18.df
BlackR.18.12052 <- BlackR.18.df
# not enough relocations for 12054
# not enough relocations for 12056
# not enough relocations for 12058
BlackR.18.12060 <- BlackR.18.df
BlackR.18.12062 <- BlackR.18.df
BlackR.18.12064 <- BlackR.18.df
BlackR.18.12066 <- BlackR.18.df
BlackR.18.12068 <- BlackR.18.df
BlackR.18.12070 <- BlackR.18.df
BlackR.18.12074 <- BlackR.18.df
BlackR.18.12076 <- BlackR.18.df


#June 19 - 26
i <- 14 ## update for each iteration instead of copy paste and change transmitter
fishind <- fishes[i]
BR.positions.p7 <- BlackR_accel %>% filter(survey.period == "June 19-26") %>% filter(Transmitter == fishind)
BR.positions.p7 <- BR.positions.p7 %>% dplyr::group_by(Transmitter)
p7_BlackR <- BR.positions.p7[!is.na(BR.positions.p7$Longitude) & !is.na(BR.positions.p7$Latitude),]
p7_BlackR.sp <- p7_BlackR[, c("Transmitter", "Longitude", "Latitude")]
coordinates(p7_BlackR.sp) <- c("Longitude", "Latitude")
proj4string(p7_BlackR.sp) <- CRS("+init=epsg:4326")
kernel.p7.BlackR <- kernelUD(p7_BlackR.sp, h = "href")
BlackR.p7.poly <- getverticeshr(kernel.p7.BlackR, percent = 95)
BlackR.p7.sp <- fortify(BlackR.p7.poly)
BlackR.p7.df <- as.data.frame(BlackR.p7.poly)
fishind

BlackR.p7.12048 <- BlackR.p7.df
BlackR.p7.12050 <- BlackR.p7.df
BlackR.p7.12052 <- BlackR.p7.df
BlackR.p7.12054 <- BlackR.p7.df
BlackR.p7.12056 <- BlackR.p7.df
BlackR.p7.12058 <- BlackR.p7.df
BlackR.p7.12060 <- BlackR.p7.df
BlackR.p7.12062 <- BlackR.p7.df
BlackR.p7.12064 <- BlackR.p7.df
BlackR.p7.12066 <- BlackR.p7.df
BlackR.p7.12068 <- BlackR.p7.df
BlackR.p7.12070 <- BlackR.p7.df
BlackR.p7.12074 <- BlackR.p7.df
BlackR.p7.12076 <- BlackR.p7.df




#June 27 - July 4
i <- 2 ## update for each iteration instead of copy paste and change transmitter
fishind <- fishes[i]
BR.positions.p8 <- BlackR_accel %>% filter(survey.period == "June 27-July 4") %>% filter(Transmitter == fishind)
BR.positions.p8 <- BR.positions.p8 %>% dplyr::group_by(Transmitter)
p8_BlackR <- BR.positions.p8[!is.na(BR.positions.p8$Longitude) & !is.na(BR.positions.p8$Latitude),]
p8_BlackR.sp <- p8_BlackR[, c("Transmitter", "Longitude", "Latitude")]
coordinates(p8_BlackR.sp) <- c("Longitude", "Latitude")
proj4string(p8_BlackR.sp) <- CRS("+init=epsg:4326")
kernel.p8.BlackR <- kernelUD(p8_BlackR.sp, h = "href")
BlackR.p8.poly <- getverticeshr(kernel.p8.BlackR, percent = 95)
BlackR.p8.sp <- fortify(BlackR.p8.poly)
BlackR.p8.df <- as.data.frame(BlackR.p8.poly)
fishind

BlackR.p8.12048 <- BlackR.p8.df
BlackR.p8.12050 <- BlackR.p8.df



#######################################################################################

## Before
#Black rockfish - 12048
BR.positions.b48 <- BlackR_accel %>% filter(survey.period == "June 3-10") %>% filter(Transmitter == "A69-9007-12048")
BR.positions.b48 <- BR.positions.b48 %>% dplyr::group_by(Transmitter)
kud48_BlackR <- BR.positions.b48[!is.na(BR.positions.b48$Longitude) & !is.na(BR.positions.b48$Latitude),]
kud48_BlackR <- kud48_BlackR[, c("Transmitter", "Longitude", "Latitude")]
coordinates(kud48_BlackR) <- c("Longitude", "Latitude")
proj4string(kud48_BlackR) <- CRS("+init=epsg:4326")

kernel.48.BlackR <- kernelUD(kud48_BlackR, h = "href")
BlackR48.poly <- getverticeshr(kernel.48.BlackR, percent = 95)

BlackR48.sp <- fortify(BlackR48.poly)
BlackR48.df <- as.data.frame(BlackR48.poly)

kmlPolygons(obj = BlackR48.poly, kmlfile="BlackR.b48.kml", col = "blue") # write kml file

#g <- ggplot(BlackR76.sp, aes(x = long, y = lat, fill = id)) +
#  geom_polygon(alpha = .4) +
#  ggthemes::scale_fill_gdocs() +
#  coord_equal() +
#  theme_void()
#g

#Black rockfish - 12049 ###########################3
BR.positions.b49 <- BlackR_accel %>% filter(survey.period == "June 3-10") %>% filter(Transmitter == "A69-9007-12049")
BR.positions.b49 <- BR.positions.b49 %>% dplyr::group_by(Transmitter)
kud49_BlackR <- BR.positions.b49[!is.na(BR.positions.b49$Longitude) & !is.na(BR.positions.b49$Latitude),]
kud49_BlackR <- kud49_BlackR[, c("Transmitter", "Longitude", "Latitude")]
coordinates(kud49_BlackR) <- c("Longitude", "Latitude")
proj4string(kud49_BlackR) <- CRS("+init=epsg:4326")

kernel.49.BlackR <- kernelUD(kud49_BlackR, h = "href")
BlackR49.poly <- getverticeshr(kernel.49.BlackR, percent = 95)

BlackR49.sp <- fortify(BlackR49.poly)
BlackR49.df <- as.data.frame(BlackR49.poly)

kmlPolygons(obj = BlackR49.poly, kmlfile="BlackR.b49.kml", col = "blue")

#Black rockfish - 12050
BR.positions.b50 <- BlackR_accel %>% filter(survey.period == "June 3-10") %>% filter(Transmitter == "A69-9007-12050")
BR.positions.b50 <- BR.positions.b50 %>% dplyr::group_by(Transmitter)
kud50_BlackR <- BR.positions.b50[!is.na(BR.positions.b50$Longitude) & !is.na(BR.positions.b50$Latitude),]
kud50_BlackR <- kud50_BlackR[, c("Transmitter", "Longitude", "Latitude")]
coordinates(kud50_BlackR) <- c("Longitude", "Latitude")
proj4string(kud50_BlackR) <- CRS("+init=epsg:4326")

kernel.50.BlackR <- kernelUD(kud50_BlackR, h = "href")
BlackR50.poly <- getverticeshr(kernel.50.BlackR, percent = 95)

BlackR50.sp <- fortify(BlackR50.poly)
BlackR50.df <- as.data.frame(BlackR50.poly)

kmlPolygons(obj = BlackR50.poly, kmlfile="BlackR.b50.kml", col = "blue")
 
#Black rockfish - 12051 #############################
BR.positions.b51 <- BlackR_accel %>% filter(survey.period == "June 3-10") %>% filter(Transmitter == "A69-9007-12051")
BR.positions.b51 <- BR.positions.b51 %>% dplyr::group_by(Transmitter)
kud51_BlackR <- BR.positions.b51[!is.na(BR.positions.b51$Longitude) & !is.na(BR.positions.b51$Latitude),]
kud51_BlackR <- kud51_BlackR[, c("Transmitter", "Longitude", "Latitude")]
coordinates(kud51_BlackR) <- c("Longitude", "Latitude")
proj4string(kud51_BlackR) <- CRS("+init=epsg:4326")

kernel.51.BlackR <- kernelUD(kud51_BlackR, h = "href")
BlackR51.poly <- getverticeshr(kernel.51.BlackR, percent = 95)

BlackR51.sp <- fortify(BlackR51.poly)
BlackR51.df <- as.data.frame(BlackR51.poly)

#Black rockfish - 12052
BR.positions.b52 <- BlackR_accel %>% filter(survey.period == "June 3-10") %>% filter(Transmitter == "A69-9007-12052")
BR.positions.b52 <- BR.positions.b52 %>% dplyr::group_by(Transmitter)
kud52_BlackR <- BR.positions.b52[!is.na(BR.positions.b52$Longitude) & !is.na(BR.positions.b52$Latitude),]
kud52_BlackR <- kud52_BlackR[, c("Transmitter", "Longitude", "Latitude")]
coordinates(kud52_BlackR) <- c("Longitude", "Latitude")
proj4string(kud52_BlackR) <- CRS("+init=epsg:4326")

kernel.52.BlackR <- kernelUD(kud52_BlackR, h = "href")
BlackR52.poly <- getverticeshr(kernel.52.BlackR, percent = 95)

BlackR52.sp <- fortify(BlackR52.poly)
BlackR52.df <- as.data.frame(BlackR52.poly)

kmlPolygons(obj = BlackR52.poly, kmlfile="BlackR.b52.kml", col = "blue")

#Black rockfish - 12053 ############################
BR.positions.b53 <- BlackR_accel %>% filter(survey.period == "June 3-10") %>% filter(Transmitter == "A69-9007-12053")
BR.positions.b53 <- BR.positions.b53 %>% dplyr::group_by(Transmitter)
kud53_BlackR <- BR.positions.b53[!is.na(BR.positions.b53$Longitude) & !is.na(BR.positions.b53$Latitude),]
kud53_BlackR <- kud53_BlackR[, c("Transmitter", "Longitude", "Latitude")]
coordinates(kud53_BlackR) <- c("Longitude", "Latitude")
proj4string(kud53_BlackR) <- CRS("+init=epsg:4326")

kernel.53.BlackR <- kernelUD(kud53_BlackR, h = "href")
BlackR53.poly <- getverticeshr(kernel.53.BlackR, percent = 95)

BlackR53.sp <- fortify(BlackR53.poly)
BlackR53.df <- as.data.frame(BlackR53.poly)

kmlPolygons(obj = BlackR53.poly, kmlfile="BlackR.b53.kml", col = "blue")

#Black rockfish - 12054
BR.positions.b54 <- BlackR_accel %>% filter(survey.period == "June 3-10") %>% filter(Transmitter == "A69-9007-12054")
BR.positions.b54 <- BR.positions.b54 %>% dplyr::group_by(Transmitter)
kud54_BlackR <- BR.positions.b54[!is.na(BR.positions.b54$Longitude) & !is.na(BR.positions.b54$Latitude),]
kud54_BlackR <- kud54_BlackR[, c("Transmitter", "Longitude", "Latitude")]
coordinates(kud54_BlackR) <- c("Longitude", "Latitude")
proj4string(kud54_BlackR) <- CRS("+init=epsg:4326")

kernel.54.BlackR <- kernelUD(kud54_BlackR, h = "href")
BlackR54.poly <- getverticeshr(kernel.54.BlackR, percent = 95)

BlackR54.sp <- fortify(BlackR54.poly)
BlackR54.df <- as.data.frame(BlackR54.poly)

kmlPolygons(obj = BlackR54.poly, kmlfile="BlackR.b54.kml", col = "blue")

#Black rockfish - 12055########################################
BR.positions.b55 <- BlackR_accel %>% filter(survey.period == "June 3-10") %>% filter(Transmitter == "A69-9007-12055")
BR.positions.b55 <- BR.positions.b55 %>% dplyr::group_by(Transmitter)
kud55_BlackR <- BR.positions.b55[!is.na(BR.positions.b55$Longitude) & !is.na(BR.positions.b55$Latitude),]
kud55_BlackR <- kud55_BlackR[, c("Transmitter", "Longitude", "Latitude")]
coordinates(kud55_BlackR) <- c("Longitude", "Latitude")
proj4string(kud55_BlackR) <- CRS("+init=epsg:4326")

kernel.55.BlackR <- kernelUD(kud55_BlackR, h = "href")
BlackR55.poly <- getverticeshr(kernel.55.BlackR, percent = 95)

BlackR55.sp <- fortify(BlackR55.poly)
BlackR55.df <- as.data.frame(BlackR55.poly)

#Black rockfish - 12056########################################
BR.positions.b56 <- BlackR_accel %>% filter(survey.period == "June 3-10") %>% filter(Transmitter == "A69-9007-12056")
BR.positions.b56 <- BR.positions.b56 %>% dplyr::group_by(Transmitter)
kud56_BlackR <- BR.positions.b56[!is.na(BR.positions.b56$Longitude) & !is.na(BR.positions.b56$Latitude),]
kud56_BlackR <- kud56_BlackR[, c("Transmitter", "Longitude", "Latitude")]
coordinates(kud56_BlackR) <- c("Longitude", "Latitude")
proj4string(kud56_BlackR) <- CRS("+init=epsg:4326")

kernel.56.BlackR <- kernelUD(kud56_BlackR, h = "href")
BlackR56.poly <- getverticeshr(kernel.56.BlackR, percent = 95)

BlackR56.sp <- fortify(BlackR56.poly)
BlackR56.df <- as.data.frame(BlackR56.poly)

kmlPolygons(obj = BlackR56.poly, kmlfile="BlackR.b56.kml", col = "blue")

#Black rockfish - 12057########################################
BR.positions.b57 <- BlackR_accel %>% filter(survey.period == "June 3-10") %>% filter(Transmitter == "A69-9007-12057")
BR.positions.b57 <- BR.positions.b57 %>% dplyr::group_by(Transmitter)
kud57_BlackR <- BR.positions.b57[!is.na(BR.positions.b57$Longitude) & !is.na(BR.positions.b57$Latitude),]
kud57_BlackR <- kud57_BlackR[, c("Transmitter", "Longitude", "Latitude")]
coordinates(kud57_BlackR) <- c("Longitude", "Latitude")
proj4string(kud57_BlackR) <- CRS("+init=epsg:4326")

kernel.57.BlackR <- kernelUD(kud57_BlackR, h = "href")
BlackR57.poly <- getverticeshr(kernel.57.BlackR, percent = 95)

BlackR57.sp <- fortify(BlackR57.poly)
BlackR57.df <- as.data.frame(BlackR57.poly)

#Black rockfish - 12058
BR.positions.b58 <- BlackR_accel %>% filter(survey.period == "June 3-10") %>% filter(Transmitter == "A69-9007-12058")
BR.positions.b58 <- BR.positions.b58 %>% dplyr::group_by(Transmitter)
kud58_BlackR <- BR.positions.b58[!is.na(BR.positions.b58$Longitude) & !is.na(BR.positions.b58$Latitude),]
kud58_BlackR <- kud58_BlackR[, c("Transmitter", "Longitude", "Latitude")]
coordinates(kud58_BlackR) <- c("Longitude", "Latitude")
proj4string(kud58_BlackR) <- CRS("+init=epsg:4326")

kernel.58.BlackR <- kernelUD(kud58_BlackR, h = "href")
BlackR58.poly <- getverticeshr(kernel.58.BlackR, percent = 95)

BlackR58.sp <- fortify(BlackR58.poly)
BlackR58.df <- as.data.frame(BlackR58.poly)

kmlPolygons(obj = BlackR58.poly, kmlfile="BlackR.b58.kml", col = "blue")

#Black rockfish - 12059 ##########################
BR.positions.b59 <- BlackR_accel %>% filter(survey.period == "June 3-10") %>% filter(Transmitter == "A69-9007-12059")
BR.positions.b59 <- BR.positions.b59 %>% dplyr::group_by(Transmitter)
kud59_BlackR <- BR.positions.b59[!is.na(BR.positions.b59$Longitude) & !is.na(BR.positions.b59$Latitude),]
kud59_BlackR <- kud59_BlackR[, c("Transmitter", "Longitude", "Latitude")]
coordinates(kud59_BlackR) <- c("Longitude", "Latitude")
proj4string(kud59_BlackR) <- CRS("+init=epsg:4326")

kernel.59.BlackR <- kernelUD(kud59_BlackR, h = "href")
BlackR59.poly <- getverticeshr(kernel.59.BlackR, percent = 95)

BlackR59.sp <- fortify(BlackR59.poly)
BlackR59.df <- as.data.frame(BlackR59.poly)

kmlPolygons(obj = BlackR59.poly, kmlfile="BlackR.b59.kml", col = "blue")

#Black rockfish - 12060
BR.positions.b60 <- BlackR_accel %>% filter(survey.period == "June 3-10") %>% filter(Transmitter == "A69-9007-12060")
BR.positions.b60 <- BR.positions.b60 %>% dplyr::group_by(Transmitter)
kud60_BlackR <- BR.positions.b60[!is.na(BR.positions.b60$Longitude) & !is.na(BR.positions.b60$Latitude),]
kud60_BlackR <- kud60_BlackR[, c("Transmitter", "Longitude", "Latitude")]
coordinates(kud60_BlackR) <- c("Longitude", "Latitude")
proj4string(kud60_BlackR) <- CRS("+init=epsg:4326")

kernel.60.BlackR <- kernelUD(kud60_BlackR, h = "href")
BlackR60.poly <- getverticeshr(kernel.60.BlackR, percent = 95)

BlackR60.sp <- fortify(BlackR60.poly)
BlackR60.df <- as.data.frame(BlackR60.poly)

kmlPolygons(obj = BlackR60.poly, kmlfile="BlackR.b60.kml", col = "blue")

#Black rockfish - 12061 ##############################3
BR.positions.b61 <- BlackR_accel %>% filter(survey.period == "June 3-10") %>% filter(Transmitter == "A69-9007-12061")
BR.positions.b61 <- BR.positions.b61 %>% dplyr::group_by(Transmitter)
kud61_BlackR <- BR.positions.b61[!is.na(BR.positions.b61$Longitude) & !is.na(BR.positions.b61$Latitude),]
kud61_BlackR <- kud61_BlackR[, c("Transmitter", "Longitude", "Latitude")]
coordinates(kud61_BlackR) <- c("Longitude", "Latitude")
proj4string(kud61_BlackR) <- CRS("+init=epsg:4326")

kernel.61.BlackR <- kernelUD(kud61_BlackR, h = "href")
BlackR61.poly <- getverticeshr(kernel.61.BlackR, percent = 95)

BlackR61.sp <- fortify(BlackR61.poly)
BlackR61.df <- as.data.frame(BlackR61.poly)

kmlPolygons(obj = BlackR61.poly, kmlfile="BlackR.b61.kml", col = "blue")

#Black rockfish - 12062
BR.positions.b62 <- BlackR_accel %>% filter(survey.period == "June 3-10") %>% filter(Transmitter == "A69-9007-12062")
BR.positions.b62 <- BR.positions.b62 %>% dplyr::group_by(Transmitter)
kud62_BlackR <- BR.positions.b62[!is.na(BR.positions.b62$Longitude) & !is.na(BR.positions.b62$Latitude),]
kud62_BlackR <- kud62_BlackR[, c("Transmitter", "Longitude", "Latitude")]
coordinates(kud62_BlackR) <- c("Longitude", "Latitude")
proj4string(kud62_BlackR) <- CRS("+init=epsg:4326")

kernel.62.BlackR <- kernelUD(kud62_BlackR, h = "href")
BlackR62.poly <- getverticeshr(kernel.62.BlackR, percent = 95)

BlackR62.sp <- fortify(BlackR62.poly)
BlackR62.df <- as.data.frame(BlackR62.poly)

kmlPolygons(obj = BlackR62.poly, kmlfile="BlackR.b62.kml", col = "blue")

#Black rockfish - 12063 ##############################
BR.positions.b63 <- BlackR_accel %>% filter(survey.period == "June 3-10") %>% filter(Transmitter == "A69-9007-12063")
BR.positions.b63 <- BR.positions.b63 %>% dplyr::group_by(Transmitter)
kud63_BlackR <- BR.positions.b63[!is.na(BR.positions.b63$Longitude) & !is.na(BR.positions.b63$Latitude),]
kud63_BlackR <- kud63_BlackR[, c("Transmitter", "Longitude", "Latitude")]
coordinates(kud63_BlackR) <- c("Longitude", "Latitude")
proj4string(kud63_BlackR) <- CRS("+init=epsg:4326")

kernel.63.BlackR <- kernelUD(kud63_BlackR, h = "href")
BlackR63.poly <- getverticeshr(kernel.63.BlackR, percent = 95)

BlackR63.sp <- fortify(BlackR63.poly)
BlackR63.df <- as.data.frame(BlackR63.poly)

kmlPolygons(obj = BlackR63.poly, kmlfile="BlackR.b63.kml", col = "blue")

#Black rockfish - 12064
BR.positions.b64 <- BlackR_accel %>% filter(survey.period == "June 3-10") %>% filter(Transmitter == "A69-9007-12064")
BR.positions.b64 <- BR.positions.b64 %>% dplyr::group_by(Transmitter)
kud64_BlackR <- BR.positions.b64[!is.na(BR.positions.b64$Longitude) & !is.na(BR.positions.b64$Latitude),]
kud64_BlackR <- kud64_BlackR[, c("Transmitter", "Longitude", "Latitude")]
coordinates(kud64_BlackR) <- c("Longitude", "Latitude")
proj4string(kud64_BlackR) <- CRS("+init=epsg:4326")

kernel.64.BlackR <- kernelUD(kud64_BlackR, h = "href")
BlackR64.poly <- getverticeshr(kernel.64.BlackR, percent = 95)

BlackR64.sp <- fortify(BlackR64.poly)
BlackR64.df <- as.data.frame(BlackR64.poly)

kmlPolygons(obj = BlackR64.poly, kmlfile="BlackR.b64.kml", col = "blue")

#Black rockfish - 12065 #################################
BR.positions.b65 <- BlackR_accel %>% filter(survey.period == "June 3-10") %>% filter(Transmitter == "A69-9007-12065")
BR.positions.b65 <- BR.positions.b65 %>% dplyr::group_by(Transmitter)
kud65_BlackR <- BR.positions.b65[!is.na(BR.positions.b65$Longitude) & !is.na(BR.positions.b65$Latitude),]
kud65_BlackR <- kud65_BlackR[, c("Transmitter", "Longitude", "Latitude")]
coordinates(kud65_BlackR) <- c("Longitude", "Latitude")
proj4string(kud65_BlackR) <- CRS("+init=epsg:4326")

kernel.65.BlackR <- kernelUD(kud65_BlackR, h = "href")
BlackR65.poly <- getverticeshr(kernel.65.BlackR, percent = 95)

BlackR65.sp <- fortify(BlackR65.poly)
BlackR65.df <- as.data.frame(BlackR65.poly)

#Black rockfish - 12066
BR.positions.b66 <- BlackR_accel %>% filter(survey.period == "June 3-10") %>% filter(Transmitter == "A69-9007-12066")
BR.positions.b66 <- BR.positions.b66 %>% dplyr::group_by(Transmitter)
kud66_BlackR <- BR.positions.b66[!is.na(BR.positions.b66$Longitude) & !is.na(BR.positions.b66$Latitude),]
kud66_BlackR <- kud66_BlackR[, c("Transmitter", "Longitude", "Latitude")]
coordinates(kud66_BlackR) <- c("Longitude", "Latitude")
proj4string(kud66_BlackR) <- CRS("+init=epsg:4326")

kernel.66.BlackR <- kernelUD(kud66_BlackR, h = "href")
BlackR66.poly <- getverticeshr(kernel.66.BlackR, percent = 95)

BlackR66.sp <- fortify(BlackR66.poly)
BlackR66.df <- as.data.frame(BlackR66.poly)

kmlPolygons(obj = BlackR66.poly, kmlfile="BlackR.b66.kml", col = "blue")

#Black rockfish - 12067 ################################3
BR.positions.b67 <- BlackR_accel %>% filter(survey.period == "June 3-10") %>% filter(Transmitter == "A69-9007-12067")
BR.positions.b67 <- BR.positions.b67 %>% dplyr::group_by(Transmitter)
kud67_BlackR <- BR.positions.b67[!is.na(BR.positions.b67$Longitude) & !is.na(BR.positions.b67$Latitude),]
kud67_BlackR <- kud67_BlackR[, c("Transmitter", "Longitude", "Latitude")]
coordinates(kud67_BlackR) <- c("Longitude", "Latitude")
proj4string(kud67_BlackR) <- CRS("+init=epsg:4326")

kernel.67.BlackR <- kernelUD(kud67_BlackR, h = "href")
BlackR67.poly <- getverticeshr(kernel.67.BlackR, percent = 95)

BlackR67.sp <- fortify(BlackR67.poly)
BlackR67.df <- as.data.frame(BlackR67.poly)


#Black rockfish - 12068
BR.positions.b68 <- BlackR_accel %>% filter(survey.period == "June 3-10") %>% filter(Transmitter == "A69-9007-12068")
BR.positions.b68 <- BR.positions.b68 %>% dplyr::group_by(Transmitter)
kud68_BlackR <- BR.positions.b68[!is.na(BR.positions.b68$Longitude) & !is.na(BR.positions.b68$Latitude),]
kud68_BlackR <- kud68_BlackR[, c("Transmitter", "Longitude", "Latitude")]
coordinates(kud68_BlackR) <- c("Longitude", "Latitude")
proj4string(kud68_BlackR) <- CRS("+init=epsg:4326")

kernel.68.BlackR <- kernelUD(kud68_BlackR, h = "href")
BlackR68.poly <- getverticeshr(kernel.68.BlackR, percent = 95)

BlackR68.sp <- fortify(BlackR68.poly)
BlackR68.df <- as.data.frame(BlackR68.poly)

kmlPolygons(obj = BlackR68.poly, kmlfile="BlackR.b68.kml", col = "blue")

#Black rockfish - 12069 ##############################
BR.positions.b69 <- BlackR_accel %>% filter(survey.period == "June 3-10") %>% filter(Transmitter == "A69-9007-12069")
BR.positions.b69 <- BR.positions.b69 %>% dplyr::group_by(Transmitter)
kud69_BlackR <- BR.positions.b69[!is.na(BR.positions.b69$Longitude) & !is.na(BR.positions.b69$Latitude),]
kud69_BlackR <- kud69_BlackR[, c("Transmitter", "Longitude", "Latitude")]
coordinates(kud69_BlackR) <- c("Longitude", "Latitude")
proj4string(kud69_BlackR) <- CRS("+init=epsg:4326")

kernel.69.BlackR <- kernelUD(kud69_BlackR, h = "href")
BlackR69.poly <- getverticeshr(kernel.69.BlackR, percent = 95)

BlackR69.sp <- fortify(BlackR69.poly)
BlackR69.df <- as.data.frame(BlackR69.poly)

#Black rockfish - 12070
BR.positions.b70 <- BlackR_accel %>% filter(survey.period == "June 3-10") %>% filter(Transmitter == "A69-9007-12070")
BR.positions.b70 <- BR.positions.b70 %>% dplyr::group_by(Transmitter)
kud70_BlackR <- BR.positions.b70[!is.na(BR.positions.b70$Longitude) & !is.na(BR.positions.b70$Latitude),]
kud70_BlackR <- kud70_BlackR[, c("Transmitter", "Longitude", "Latitude")]
coordinates(kud70_BlackR) <- c("Longitude", "Latitude")
proj4string(kud70_BlackR) <- CRS("+init=epsg:4326")

kernel.70.BlackR <- kernelUD(kud70_BlackR, h = "href")
BlackR70.poly <- getverticeshr(kernel.70.BlackR, percent = 95)

BlackR70.sp <- fortify(BlackR70.poly)
BlackR70.df <- as.data.frame(BlackR70.poly)

kmlPolygons(obj = BlackR70.poly, kmlfile="BlackR.b70.kml", col = "blue")

#Black rockfish - 12072 #################################33
BR.positions.b72 <- BlackR_accel %>% filter(survey.period == "June 3-10") %>% filter(Transmitter == "A69-9007-12072")
BR.positions.b72 <- BR.positions.b72 %>% dplyr::group_by(Transmitter)
kud72_BlackR <- BR.positions.b72[!is.na(BR.positions.b72$Longitude) & !is.na(BR.positions.b72$Latitude),]
kud72_BlackR <- kud72_BlackR[, c("Transmitter", "Longitude", "Latitude")]
coordinates(kud72_BlackR) <- c("Longitude", "Latitude")
proj4string(kud72_BlackR) <- CRS("+init=epsg:4326")

kernel.72.BlackR <- kernelUD(kud72_BlackR, h = "href")
BlackR72.poly <- getverticeshr(kernel.72.BlackR, percent = 95)

BlackR72.sp <- fortify(BlackR72.poly)
BlackR72.df <- as.data.frame(BlackR72.poly)

#Black rockfish - 12074
BR.positions.b74 <- BlackR_accel %>% filter(survey.period == "June 3-10") %>% filter(Transmitter == "A69-9007-12074")
BR.positions.b74 <- BR.positions.b74 %>% dplyr::group_by(Transmitter)
kud74_BlackR <- BR.positions.b74[!is.na(BR.positions.b74$Longitude) & !is.na(BR.positions.b74$Latitude),]
kud74_BlackR <- kud74_BlackR[, c("Transmitter", "Longitude", "Latitude")]
coordinates(kud74_BlackR) <- c("Longitude", "Latitude")
proj4string(kud74_BlackR) <- CRS("+init=epsg:4326")

kernel.74.BlackR <- kernelUD(kud74_BlackR, h = "href")
BlackR74.poly <- getverticeshr(kernel.74.BlackR, percent = 95)

BlackR74.sp <- fortify(BlackR74.poly)
BlackR74.df <- as.data.frame(BlackR74.poly)

kmlPolygons(obj = BlackR74.poly, kmlfile="BlackR.b74.kml", col = "blue")

#Black rockfish - 12076
BR.positions.b76 <- BlackR_accel %>% filter(survey.period == "June 3-10") %>% filter(Transmitter == "A69-9007-12076")
BR.positions.b76 <- BR.positions.b76 %>% dplyr::group_by(Transmitter)
kud76_BlackR <- BR.positions.b76[!is.na(BR.positions.b76$Longitude) & !is.na(BR.positions.b76$Latitude),]
kud76_BlackR <- kud76_BlackR[, c("Transmitter", "Longitude", "Latitude")]
coordinates(kud76_BlackR) <- c("Longitude", "Latitude")
proj4string(kud76_BlackR) <- CRS("+init=epsg:4326")

kernel.76.BlackR <- kernelUD(kud76_BlackR, h = "href")
BlackR76.poly <- getverticeshr(kernel.76.BlackR, percent = 95)

BlackR76.sp <- fortify(BlackR76.poly)
BlackR76.df <- as.data.frame(BlackR76.poly)

kmlPolygons(obj = BlackR76.poly, kmlfile="BlackR.b76.kml", col = "blue")

BlackR.before.df <- bind_rows(BlackR48.df, BlackR50.df, BlackR52.df, 
                              BlackR54.df, BlackR58.df,
                              BlackR60.df, BlackR62.df, BlackR64.df,
                              BlackR66.df, BlackR68.df, BlackR70.df,
                              BlackR74.df, BlackR76.df)
BlackR.before.df <- BlackR.before.df %>% rename(area.before = area)

BlackR.before.poly <- bind(BlackR48.poly, BlackR50.poly, BlackR52.poly, BlackR54.poly,
                           BlackR58.poly, BlackR60.poly, BlackR62.poly, BlackR64.poly, BlackR66.poly,
                           BlackR68.poly, BlackR70.poly, BlackR74.poly, BlackR76.poly)

kmlPolygons(obj = BlackR.before.poly, kmlfile="BlackR.before.kml", col = "blue")


## 

## During
BR.positions.during <- BlackR_accel %>% filter(periods == "During")
BR.positions.during <- BR.positions.during %>% dplyr::group_by(Transmitter)
dur_BlackR <- BR.positions.during[!is.na(BR.positions.during$Longitude) & !is.na(BR.positions.during$Latitude),]
dur_BlackR.sp <- dur_BlackR[, c("Transmitter", "Longitude", "Latitude")]
coordinates(dur_BlackR.sp) <- c("Longitude", "Latitude")
proj4string(dur_BlackR.sp) <- CRS("+init=epsg:4326")

kernel.dur.BlackR <- kernelUD(dur_BlackR.sp, h = "href")
BlackR.dur.poly <- getverticeshr(kernel.dur.BlackR, percent = 95)

BlackR.dur.sp <- fortify(BlackR.dur.poly)
BlackR.dur.df <- as.data.frame(BlackR.dur.poly)
BlackR.dur.df <- BlackR.dur.df %>% rename(area.dur = area)

kmlPolygons(obj = BlackR.dur.poly, kmlfile="BlackR.dur.kml", col = "blue")


## After
BR.positions.after <- BlackR_accel %>% filter(periods == "After")
BR.positions.after <- BR.positions.after %>% dplyr::group_by(Transmitter)
after_BlackR <- BR.positions.after[!is.na(BR.positions.after$Longitude) & !is.na(BR.positions.after$Latitude),]
after_BlackR.sp <- after_BlackR[, c("Transmitter", "Longitude", "Latitude")]
coordinates(after_BlackR.sp) <- c("Longitude", "Latitude")
proj4string(after_BlackR.sp) <- CRS("+init=epsg:4326")

kernel.after.BlackR <- kernelUD(after_BlackR.sp, h = "href")
BlackR.after.poly <- getverticeshr(kernel.after.BlackR, percent = 95)

BlackR.after.sp <- fortify(BlackR.after.poly)
BlackR.after.df <- as.data.frame(BlackR.after.poly)
BlackR.after.df <- BlackR.after.df %>% rename(area.after = area)

kmlPolygons(obj = BlackR.after.poly, kmlfile="BlackR.after.kml", col = "blue")


#dataframe of areas of individuals by three periods

BlackR.all.df <- BlackR.before.df %>% 
  inner_join(BlackR.dur.df, by='id') %>% 
  inner_join(BlackR.after.df, by='id')


BR_ANOVA <- BlackR.all.df %>%
  gather(key = "period", value = "area", area.before, area.dur, area.after) %>%
  convert_as_factor(id, period)
head(BR_ANOVA, 3)

BR_ANOVA %>%
  group_by(period) %>%
  get_summary_stats(area, type = "mean_sd")

bxp <- ggplot(BR_ANOVA, aes(x = period, y = area), add = "point") + geom_boxplot()
bxp




coarse.period.lvl <- c("area.before", "area.dur", "area.after")
BR_ANOVA$period <- factor(BR_ANOVA$period, levels = coarse.period.lvl[c(1:3)])

br.aov <- anova_test(data = BR_ANOVA, dv = area, wid = id, within = period)
get_anova_table(br.aov)

pwc <- BR_ANOVA %>%
  pairwise_t_test(
    area ~ period, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
pwc

#descriptive stats
which(BlackR.before.df$area.before %in% c(max(BlackR.before.df$area.before))) #12054
which(BlackR.dur.df$area.dur %in% c(max(BlackR.dur.df$area.dur))) #12058
which(BlackR.after.df$area.after %in% c(max(BlackR.after.df$area.after))) #12056

max(BlackR.before.df$area.before)
max(BlackR.dur.df$area.dur)
max(BlackR.after.df$area.after)




## find centroid of all KUDs, all periods
library(sf)
library(ggplot2)
library(rgeos)

# using sf
cent_example <- st_as_sf(BlackR.p0.12048.poly)
sf_cent <- st_centroid(cent_example)

# using rgeos
sp_cent <- gCentroid(BlackR.p0.12048.poly, byid = TRUE)
sp_cent <- sp_cent %>% st_as_sf()

# plot both together to confirm that they are equivalent
ggplot() + 
  geom_sf(data = cent_example, fill = "cyan") +
  geom_sf(data = sf_cent, color = "magenta") +
  geom_sf(data = sp_cent, color = "blue") +
  coord_sf()


#each individual
fishpoly_listp0 <- list()
fishpoly_listp0 <- c(BlackR.p0.12048.poly, BlackR.p0.12050.poly, BlackR.p0.12052.poly, BlackR.p0.12058.poly, 
                     BlackR.p0.12060.poly, BlackR.p0.12062.poly, BlackR.p0.12064.poly, BlackR.p0.12066.poly,
                     BlackR.p0.12068.poly, BlackR.p0.12070.poly, BlackR.p0.12074.poly, BlackR.p0.12076.poly)

fishpoly_listp1 <- list()
fishpoly_listp1 <- c(BlackR.p1.12048.poly, BlackR.p1.12050.poly, BlackR.p1.12052.poly, BlackR.p1.12054.poly,
                     BlackR.p1.12056.poly, BlackR.p1.12058.poly, BlackR.p1.12060.poly, BlackR.p1.12062.poly, 
                     BlackR.p1.12064.poly, BlackR.p1.12066.poly, BlackR.p1.12068.poly, BlackR.p1.12070.poly, 
                     BlackR.p1.12074.poly, BlackR.p1.12076.poly)

fishpoly_listp2 <- list()
fishpoly_listp2 <- c(BlackR.p2.12048.poly, BlackR.p2.12050.poly, BlackR.p2.12052.poly, BlackR.p2.12054.poly,
                     BlackR.p2.12056.poly, BlackR.p2.12058.poly, BlackR.p2.12060.poly, BlackR.p2.12062.poly, 
                     BlackR.p2.12064.poly, BlackR.p2.12066.poly, BlackR.p2.12068.poly, BlackR.p2.12070.poly, 
                     BlackR.p2.12074.poly, BlackR.p2.12076.poly)

fishpoly_listp3 <- list()
fishpoly_listp3 <- c(BlackR.p3.12048.poly, BlackR.p3.12050.poly, BlackR.p3.12052.poly, BlackR.p3.12054.poly,
                     BlackR.p3.12056.poly, BlackR.p3.12058.poly, BlackR.p3.12060.poly, BlackR.p3.12062.poly, 
                     BlackR.p3.12064.poly, BlackR.p3.12066.poly, BlackR.p3.12068.poly, BlackR.p3.12070.poly, 
                     BlackR.p3.12074.poly, BlackR.p3.12076.poly)

fishpoly_list11 <- list()
fishpoly_list11 <- c(BlackR.11.12048.poly, BlackR.11.12050.poly, BlackR.11.12052.poly,
                     BlackR.11.12060.poly, BlackR.11.12062.poly, 
                     BlackR.11.12064.poly, BlackR.11.12066.poly, BlackR.11.12070.poly, 
                     BlackR.11.12074.poly, BlackR.11.12076.poly)

fishpoly_listp5 <- list()
fishpoly_listp5 <- c(BlackR.p5.12048.poly, BlackR.p5.12050.poly, BlackR.p5.12052.poly, BlackR.p5.12054.poly,
                     BlackR.p5.12058.poly, BlackR.p5.12060.poly, BlackR.p5.12062.poly, 
                     BlackR.p5.12064.poly, BlackR.p5.12066.poly, BlackR.p5.12068.poly, BlackR.p5.12070.poly, 
                     BlackR.p5.12074.poly, BlackR.p5.12076.poly)

fishpoly_list18 <- list()
fishpoly_list18 <- c(BlackR.18.12048.poly, BlackR.18.12050.poly, BlackR.18.12052.poly, BlackR.18.12060.poly, BlackR.18.12062.poly, 
                     BlackR.18.12064.poly, BlackR.18.12066.poly, BlackR.18.12068.poly, BlackR.18.12070.poly, 
                     BlackR.18.12074.poly, BlackR.18.12076.poly)

fishpoly_listp7 <- list()
fishpoly_listp7 <- c(BlackR.p7.12048.poly, BlackR.p7.12050.poly, BlackR.p7.12052.poly, BlackR.p7.12054.poly,
                     BlackR.p7.12056.poly, BlackR.p7.12058.poly, BlackR.p7.12060.poly, BlackR.p7.12062.poly, 
                     BlackR.p7.12064.poly, BlackR.p7.12066.poly, BlackR.p7.12068.poly, BlackR.p7.12070.poly, 
                     BlackR.p7.12074.poly, BlackR.p7.12076.poly)

fishpoly_listp8 <- list()
fishpoly_listp8 <- c(BlackR.p8.12048.poly, BlackR.p8.12050.poly, BlackR.p8.12052.poly, BlackR.p8.12054.poly,
                     BlackR.p8.12056.poly, BlackR.p8.12058.poly, BlackR.p8.12060.poly, BlackR.p8.12062.poly, 
                     BlackR.p8.12064.poly, BlackR.p8.12066.poly, BlackR.p8.12068.poly, BlackR.p8.12070.poly, 
                     BlackR.p8.12074.poly, BlackR.p8.12076.poly)

fishpoly_listp9 <- list()
fishpoly_listp9 <- c(BlackR.p9.12048.poly, BlackR.p9.12050.poly, BlackR.p9.12052.poly, BlackR.p9.12054.poly,
                     BlackR.p9.12058.poly, BlackR.p9.12060.poly, BlackR.p9.12062.poly, 
                     BlackR.p9.12064.poly, BlackR.p9.12066.poly, BlackR.p9.12068.poly, BlackR.p9.12070.poly, 
                     BlackR.p9.12074.poly, BlackR.p9.12076.poly)



for (i in seq_along(fishpoly_listp0)){
  fishind <- fishpoly_listp0[[i]]
  
  dat <- fishind
  fishname <- dat$id
  
  sp_cent <- gCentroid(dat, byid = TRUE)
  sp_cent <- sp_cent %>% st_as_sf()
  
  filename1 <- paste("BlackR", fishname, "p0", "centroid", sep = ".")
  assign(filename1, sp_cent)
  
}

for (i in seq_along(fishpoly_listp1)){
  fishind <- fishpoly_listp1[[i]]
  
  dat <- fishind
  fishname <- dat$id
  
  sp_cent <- gCentroid(dat, byid = TRUE)
  sp_cent <- sp_cent %>% st_as_sf()
  
  filename1 <- paste("BlackR", fishname, "p1", "centroid", sep = ".")
  assign(filename1, sp_cent)
  
}

for (i in seq_along(fishpoly_listp2)){
  fishind <- fishpoly_listp2[[i]]
  
  dat <- fishind
  fishname <- dat$id
  
  sp_cent <- gCentroid(dat, byid = TRUE)
  sp_cent <- sp_cent %>% st_as_sf()
  
  filename1 <- paste("BlackR", fishname, "p2", "centroid", sep = ".")
  assign(filename1, sp_cent)
  
}

for (i in seq_along(fishpoly_listp3)){
  fishind <- fishpoly_listp3[[i]]
  
  dat <- fishind
  fishname <- dat$id
  
  sp_cent <- gCentroid(dat, byid = TRUE)
  sp_cent <- sp_cent %>% st_as_sf()
  
  filename1 <- paste("BlackR", fishname, "p3", "centroid", sep = ".")
  assign(filename1, sp_cent)
  
}

for (i in seq_along(fishpoly_list11)){
  fishind <- fishpoly_list11[[i]]
  
  dat <- fishind
  fishname <- dat$id
  
  sp_cent <- gCentroid(dat, byid = TRUE)
  sp_cent <- sp_cent %>% st_as_sf()
  
  filename1 <- paste("BlackR", fishname, "11", "centroid", sep = ".")
  assign(filename1, sp_cent)
  
}

for (i in seq_along(fishpoly_listp5)){
  fishind <- fishpoly_listp5[[i]]
  
  dat <- fishind
  fishname <- dat$id
  
  sp_cent <- gCentroid(dat, byid = TRUE)
  sp_cent <- sp_cent %>% st_as_sf()
  
  filename1 <- paste("BlackR", fishname, "p5", "centroid", sep = ".")
  assign(filename1, sp_cent)
  
}

for (i in seq_along(fishpoly_list18)){
  fishind <- fishpoly_list18[[i]]
  
  dat <- fishind
  fishname <- dat$id
  
  sp_cent <- gCentroid(dat, byid = TRUE)
  sp_cent <- sp_cent %>% st_as_sf()
  
  filename1 <- paste("BlackR", fishname, "18", "centroid", sep = ".")
  assign(filename1, sp_cent)
  
}

for (i in seq_along(fishpoly_listp7)){
  fishind <- fishpoly_listp7[[i]]
  
  dat <- fishind
  fishname <- dat$id
  
  sp_cent <- gCentroid(dat, byid = TRUE)
  sp_cent <- sp_cent %>% st_as_sf()
  
  filename1 <- paste("BlackR", fishname, "p7", "centroid", sep = ".")
  assign(filename1, sp_cent)
  
}

for (i in seq_along(fishpoly_listp8)){
  fishind <- fishpoly_listp8[[i]]
  
  dat <- fishind
  fishname <- dat$id
  
  sp_cent <- gCentroid(dat, byid = TRUE)
  sp_cent <- sp_cent %>% st_as_sf()
  
  filename1 <- paste("BlackR", fishname, "p8", "centroid", sep = ".")
  assign(filename1, sp_cent)
  
}

for (i in seq_along(fishpoly_listp9)){
  fishind <- fishpoly_listp9[[i]]
  
  dat <- fishind
  fishname <- dat$id
  
  sp_cent <- gCentroid(dat, byid = TRUE)
  sp_cent <- sp_cent %>% st_as_sf()
  
  filename1 <- paste("BlackR", fishname, "p9", "centroid", sep = ".")
  assign(filename1, sp_cent)
  
}

cent_12048 <- sf_to_df(`BlackR.A69-9007-12048.p0.centroid`)
cent_12048 <- rbind(cent_12048, sf_to_df(`BlackR.A69-9007-12048.p1.centroid`), sf_to_df(`BlackR.A69-9007-12048.p2.centroid`),
                sf_to_df(`BlackR.A69-9007-12048.p3.centroid`), sf_to_df(`BlackR.A69-9007-12048.11.centroid`), sf_to_df(`BlackR.A69-9007-12048.p5.centroid`),
                sf_to_df(`BlackR.A69-9007-12048.18.centroid`), sf_to_df(`BlackR.A69-9007-12048.p7.centroid`), sf_to_df(`BlackR.A69-9007-12048.p8.centroid`),
                sf_to_df(`BlackR.A69-9007-12048.p9.centroid`))
cent_12048 <- cent_12048 %>% mutate(point_id = "12048")
area_12048 <- rbind(BlackR.p0.12048, BlackR.p1.12048, BlackR.p2.12048, BlackR.p3.12048, BlackR.11.12048,
                    BlackR.p5.12048, BlackR.18.12048, BlackR.p7.12048, BlackR.p8.12048, BlackR.p9.12048)
area_12048 <- area_12048 %>% mutate(id = "12048")

cent_12050 <- sf_to_df(`BlackR.A69-9007-12050.p0.centroid`)
cent_12050 <- rbind(cent_12050, sf_to_df(`BlackR.A69-9007-12050.p1.centroid`), sf_to_df(`BlackR.A69-9007-12050.p2.centroid`),
                sf_to_df(`BlackR.A69-9007-12050.p3.centroid`), sf_to_df(`BlackR.A69-9007-12050.11.centroid`), sf_to_df(`BlackR.A69-9007-12050.p5.centroid`),
                sf_to_df(`BlackR.A69-9007-12050.18.centroid`), sf_to_df(`BlackR.A69-9007-12050.p7.centroid`), sf_to_df(`BlackR.A69-9007-12050.p8.centroid`),
                sf_to_df(`BlackR.A69-9007-12050.p9.centroid`))
cent_12050 <- cent_12050 %>% mutate(point_id = "12050")
area_12050 <- rbind(BlackR.p0.12050, BlackR.p1.12050, BlackR.p2.12050, BlackR.p3.12050, BlackR.11.12050,
                    BlackR.p5.12050, BlackR.18.12050, BlackR.p7.12050, BlackR.p8.12050, BlackR.p9.12050)
area_12050 <- area_12050 %>% mutate(id = "12050")

cent_12052 <- sf_to_df(`BlackR.A69-9007-12052.p0.centroid`)
cent_12052 <- rbind(cent_12052, sf_to_df(`BlackR.A69-9007-12052.p1.centroid`), sf_to_df(`BlackR.A69-9007-12052.p2.centroid`),
                sf_to_df(`BlackR.A69-9007-12052.p3.centroid`), sf_to_df(`BlackR.A69-9007-12052.11.centroid`), sf_to_df(`BlackR.A69-9007-12052.p5.centroid`),
                sf_to_df(`BlackR.A69-9007-12052.18.centroid`), sf_to_df(`BlackR.A69-9007-12052.p7.centroid`), sf_to_df(`BlackR.A69-9007-12052.p8.centroid`),
                sf_to_df(`BlackR.A69-9007-12052.p9.centroid`))
cent_12052 <- cent_12052 %>% mutate(point_id = "12052")
area_12052 <- rbind(BlackR.p0.12052, BlackR.p1.12052, BlackR.p2.12052, BlackR.p3.12052, BlackR.11.12052,
                    BlackR.p5.12052, BlackR.18.12052, BlackR.p7.12052, BlackR.p8.12052, BlackR.p9.12052)
area_12052 <- area_12052 %>% mutate(id = "12052")

cent_12054 <- sf_to_df(`BlackR.A69-9007-12054.p1.centroid`)
cent_12054 <- rbind(cent_12054, sf_to_df(`BlackR.A69-9007-12054.p2.centroid`),
                sf_to_df(`BlackR.A69-9007-12054.p3.centroid`), sf_to_df(`BlackR.A69-9007-12054.p5.centroid`),
                sf_to_df(`BlackR.A69-9007-12054.p7.centroid`), sf_to_df(`BlackR.A69-9007-12054.p8.centroid`),
                sf_to_df(`BlackR.A69-9007-12054.p9.centroid`))
cent_12054 <- cent_12054 %>% mutate(point_id = "12054")
area_12054 <- rbind(BlackR.p1.12054, BlackR.p2.12054, BlackR.p3.12054,
                    BlackR.p5.12054, BlackR.p7.12054, BlackR.p8.12054, BlackR.p9.12054)
area_12054 <- area_12054 %>% mutate(id = "12054")

cent_12056 <- sf_to_df(`BlackR.A69-9007-12056.p1.centroid`)
cent_12056 <- rbind(cent_12056, sf_to_df(`BlackR.A69-9007-12056.p2.centroid`),
                sf_to_df(`BlackR.A69-9007-12056.p3.centroid`), 
                sf_to_df(`BlackR.A69-9007-12056.p7.centroid`), sf_to_df(`BlackR.A69-9007-12056.p8.centroid`))
cent_12056 <- cent_12056 %>% mutate(point_id = "12056")
area_12056 <- rbind(BlackR.p1.12056, BlackR.p2.12056, BlackR.p3.12056,
                    BlackR.p7.12056, BlackR.p8.12056)
area_12056 <- area_12056 %>% mutate(id = "12056")

cent_12058 <- sf_to_df(`BlackR.A69-9007-12058.p0.centroid`)
cent_12058 <- rbind(cent_12058, sf_to_df(`BlackR.A69-9007-12058.p1.centroid`), sf_to_df(`BlackR.A69-9007-12058.p2.centroid`),
                sf_to_df(`BlackR.A69-9007-12058.p3.centroid`), sf_to_df(`BlackR.A69-9007-12058.p5.centroid`),
                sf_to_df(`BlackR.A69-9007-12058.p7.centroid`), sf_to_df(`BlackR.A69-9007-12058.p8.centroid`),
                sf_to_df(`BlackR.A69-9007-12058.p9.centroid`))
cent_12058 <- cent_12058 %>% mutate(point_id = "12058")
area_12058 <- rbind(BlackR.p0.12058, BlackR.p1.12058, BlackR.p2.12058, BlackR.p3.12058,
                    BlackR.p5.12058, BlackR.p7.12058, BlackR.p8.12058, BlackR.p9.12058)
area_12058 <- area_12058 %>% mutate(id = "12058")

cent_12060 <- sf_to_df(`BlackR.A69-9007-12060.p0.centroid`)
cent_12060 <- rbind(cent_12060, sf_to_df(`BlackR.A69-9007-12060.p1.centroid`), sf_to_df(`BlackR.A69-9007-12060.p2.centroid`),
                sf_to_df(`BlackR.A69-9007-12060.p3.centroid`), sf_to_df(`BlackR.A69-9007-12060.11.centroid`), sf_to_df(`BlackR.A69-9007-12060.p5.centroid`),
                sf_to_df(`BlackR.A69-9007-12060.18.centroid`), sf_to_df(`BlackR.A69-9007-12060.p7.centroid`), sf_to_df(`BlackR.A69-9007-12060.p8.centroid`),
                sf_to_df(`BlackR.A69-9007-12060.p9.centroid`))
cent_12060 <- cent_12060 %>% mutate(point_id = "12060")
area_12060 <- rbind(BlackR.p0.12060, BlackR.p1.12060, BlackR.p2.12060, BlackR.p3.12060, BlackR.11.12060,
                    BlackR.p5.12060, BlackR.18.12060, BlackR.p7.12060, BlackR.p8.12060, BlackR.p9.12060)
area_12060 <- area_12060 %>% mutate(id = "12060")

cent_12062 <- sf_to_df(`BlackR.A69-9007-12062.p0.centroid`)
cent_12062 <- rbind(cent_12062, sf_to_df(`BlackR.A69-9007-12062.p1.centroid`), sf_to_df(`BlackR.A69-9007-12062.p2.centroid`),
                sf_to_df(`BlackR.A69-9007-12062.p3.centroid`), sf_to_df(`BlackR.A69-9007-12062.11.centroid`), sf_to_df(`BlackR.A69-9007-12062.p5.centroid`),
                sf_to_df(`BlackR.A69-9007-12062.18.centroid`), sf_to_df(`BlackR.A69-9007-12062.p7.centroid`), sf_to_df(`BlackR.A69-9007-12062.p8.centroid`),
                sf_to_df(`BlackR.A69-9007-12062.p9.centroid`))
cent_12062 <- cent_12062 %>% mutate(point_id = "12062")
area_12062 <- rbind(BlackR.p0.12062, BlackR.p1.12062, BlackR.p2.12062, BlackR.p3.12062, BlackR.11.12062,
                    BlackR.p5.12062, BlackR.18.12062, BlackR.p7.12062, BlackR.p8.12062, BlackR.p9.12062)
area_12062 <- area_12062 %>% mutate(id = "12062")

cent_12064 <- sf_to_df(`BlackR.A69-9007-12064.p0.centroid`)
cent_12064 <- rbind(cent_12064, sf_to_df(`BlackR.A69-9007-12064.p1.centroid`), sf_to_df(`BlackR.A69-9007-12064.p2.centroid`),
                sf_to_df(`BlackR.A69-9007-12064.p3.centroid`), sf_to_df(`BlackR.A69-9007-12064.11.centroid`), sf_to_df(`BlackR.A69-9007-12064.p5.centroid`),
                sf_to_df(`BlackR.A69-9007-12064.18.centroid`), sf_to_df(`BlackR.A69-9007-12064.p7.centroid`), sf_to_df(`BlackR.A69-9007-12064.p8.centroid`),
                sf_to_df(`BlackR.A69-9007-12064.p9.centroid`))
cent_12064 <- cent_12064 %>% mutate(point_id = "12064")
area_12064 <- rbind(BlackR.p0.12064, BlackR.p1.12064, BlackR.p2.12064, BlackR.p3.12064, BlackR.11.12064,
                    BlackR.p5.12064, BlackR.18.12064, BlackR.p7.12064, BlackR.p8.12064, BlackR.p9.12064)
area_12064 <- area_12064 %>% mutate(id = "12064")

cent_12066 <- sf_to_df(`BlackR.A69-9007-12066.p0.centroid`)
cent_12066 <- rbind(cent_12066, sf_to_df(`BlackR.A69-9007-12066.p1.centroid`), sf_to_df(`BlackR.A69-9007-12066.p2.centroid`),
                sf_to_df(`BlackR.A69-9007-12066.p3.centroid`), sf_to_df(`BlackR.A69-9007-12066.11.centroid`), sf_to_df(`BlackR.A69-9007-12066.p5.centroid`),
                sf_to_df(`BlackR.A69-9007-12066.18.centroid`), sf_to_df(`BlackR.A69-9007-12066.p7.centroid`), sf_to_df(`BlackR.A69-9007-12066.p8.centroid`),
                sf_to_df(`BlackR.A69-9007-12066.p9.centroid`))
cent_12066 <- cent_12066 %>% mutate(point_id = "12066")
area_12066 <- rbind(BlackR.p0.12066, BlackR.p1.12066, BlackR.p2.12066, BlackR.p3.12066, BlackR.11.12066,
                    BlackR.p5.12066, BlackR.18.12066, BlackR.p7.12066, BlackR.p8.12066, BlackR.p9.12066)
area_12066 <- area_12066 %>% mutate(id = "12066")

cent_12068 <- sf_to_df(`BlackR.A69-9007-12068.p0.centroid`)
cent_12068 <- rbind(cent_12068, sf_to_df(`BlackR.A69-9007-12068.p1.centroid`), sf_to_df(`BlackR.A69-9007-12068.p2.centroid`),
                sf_to_df(`BlackR.A69-9007-12068.p3.centroid`), sf_to_df(`BlackR.A69-9007-12068.p5.centroid`),
                sf_to_df(`BlackR.A69-9007-12068.18.centroid`), sf_to_df(`BlackR.A69-9007-12068.p7.centroid`), sf_to_df(`BlackR.A69-9007-12048.p8.centroid`),
                sf_to_df(`BlackR.A69-9007-12068.p9.centroid`))
cent_12068 <- cent_12068 %>% mutate(point_id = "12068")
area_12068 <- rbind(BlackR.p0.12068, BlackR.p1.12068, BlackR.p2.12068, BlackR.p3.12068,
                    BlackR.p5.12068, BlackR.18.12068, BlackR.p7.12068, BlackR.p8.12068, BlackR.p9.12068)
area_12068 <- area_12068 %>% mutate(id = "12068")


cent_12070 <- sf_to_df(`BlackR.A69-9007-12070.p0.centroid`)
cent_12070 <- rbind(cent_12070, sf_to_df(`BlackR.A69-9007-12070.p1.centroid`), sf_to_df(`BlackR.A69-9007-12070.p2.centroid`),
                sf_to_df(`BlackR.A69-9007-12070.p3.centroid`), sf_to_df(`BlackR.A69-9007-12070.11.centroid`), sf_to_df(`BlackR.A69-9007-12070.p5.centroid`),
                sf_to_df(`BlackR.A69-9007-12070.18.centroid`), sf_to_df(`BlackR.A69-9007-12070.p7.centroid`), sf_to_df(`BlackR.A69-9007-12070.p8.centroid`),
                sf_to_df(`BlackR.A69-9007-12070.p9.centroid`))
cent_12070 <- cent_12070 %>% mutate(point_id = "12070")
area_12070 <- rbind(BlackR.p0.12070, BlackR.p1.12070, BlackR.p2.12070, BlackR.p3.12070, BlackR.11.12070,
                    BlackR.p5.12070, BlackR.18.12070, BlackR.p7.12070, BlackR.p8.12070, BlackR.p9.12070)
area_12070 <- area_12070 %>% mutate(id = "12070")

cent_12074 <- sf_to_df(`BlackR.A69-9007-12074.p0.centroid`)
cent_12074 <- rbind(cent_12074, sf_to_df(`BlackR.A69-9007-12074.p1.centroid`), sf_to_df(`BlackR.A69-9007-12074.p2.centroid`),
                sf_to_df(`BlackR.A69-9007-12074.p3.centroid`), sf_to_df(`BlackR.A69-9007-12074.11.centroid`), sf_to_df(`BlackR.A69-9007-12074.p5.centroid`),
                sf_to_df(`BlackR.A69-9007-12074.18.centroid`), sf_to_df(`BlackR.A69-9007-12074.p7.centroid`), sf_to_df(`BlackR.A69-9007-12074.p8.centroid`),
                sf_to_df(`BlackR.A69-9007-12074.p9.centroid`))
cent_12074 <- cent_12074 %>% mutate(point_id = "12074")
area_12074 <- rbind(BlackR.p0.12074, BlackR.p1.12074, BlackR.p2.12074, BlackR.p3.12074, BlackR.11.12074,
                    BlackR.p5.12074, BlackR.18.12074, BlackR.p7.12074, BlackR.p8.12074, BlackR.p9.12074)
area_12074 <- area_12074 %>% mutate(id = "12074")

cent_12076 <- sf_to_df(`BlackR.A69-9007-12076.p0.centroid`)
cent_12076 <- rbind(cent_12076, sf_to_df(`BlackR.A69-9007-12076.p1.centroid`), sf_to_df(`BlackR.A69-9007-12076.p2.centroid`),
                sf_to_df(`BlackR.A69-9007-12076.p3.centroid`), sf_to_df(`BlackR.A69-9007-12076.11.centroid`), sf_to_df(`BlackR.A69-9007-12076.p5.centroid`),
                sf_to_df(`BlackR.A69-9007-12076.18.centroid`), sf_to_df(`BlackR.A69-9007-12076.p7.centroid`), sf_to_df(`BlackR.A69-9007-12076.p8.centroid`),
                sf_to_df(`BlackR.A69-9007-12076.p9.centroid`))
cent_12076 <- cent_12076 %>% mutate(point_id = "12076")
area_12076 <- rbind(BlackR.p0.12076, BlackR.p1.12076, BlackR.p2.12076, BlackR.p3.12076, BlackR.11.12076,
                    BlackR.p5.12076, BlackR.18.12076, BlackR.p7.12076, BlackR.p8.12076, BlackR.p9.12076)
area_12076 <- area_12076 %>% mutate(id = "12076")

#centroid_list <- list()



### Distance to center of reef
library(raster)
library(sfheaders)

reef_cent <- data.frame(long = -124.47589, lat = 42.69939)
reef_cent <- st_as_sf(reef_cent, coords = c('long', 'lat'), crs = st_crs(4326))


#cent_pts <- data.frame(ID = ,Centr = ,Period = ,Reef = reef_cent)

dist12048 <- pointDistance(cent_12048[,3:4], reef_cent, lonlat = TRUE)
dist12048 <- data.frame(distance = dist12048, id = rep("12048", 10), per = c("p0", "p1", "p2", "p3", "11", "p5", "18", "p7", "p8", "p9"))
area12048 <- data.frame(area = area_12048$area, id = area_12048$id, per = c("p0", "p1", "p2", "p3", "11", "p5", "18", "p7", "p8", "p9"))

dist12050 <- pointDistance(cent_12050[,3:4], reef_cent, lonlat = TRUE)
dist12050 <- data.frame(distance = dist12050, id = rep("12050", 10), per = c("p0", "p1", "p2", "p3", "11", "p5", "18", "p7", "p8", "p9"))
area12050 <- data.frame(area = area_12050$area, id = area_12050$id, per = c("p0", "p1", "p2", "p3", "11", "p5", "18", "p7", "p8", "p9"))

dist12052 <- pointDistance(cent_12052[,3:4], reef_cent, lonlat = TRUE)
dist12052 <- data.frame(distance = dist12052, id = rep("12052", 10), per = c("p0", "p1", "p2", "p3", "11", "p5", "18", "p7", "p8", "p9"))
area12052 <- data.frame(area = area_12052$area, id = area_12052$id, per = c("p0", "p1", "p2", "p3", "11", "p5", "18", "p7", "p8", "p9"))

dist12054 <- pointDistance(cent_12054[,3:4], reef_cent, lonlat = TRUE)
dist12054 <- data.frame(distance = dist12054, id = rep("12054", 7), per = c("p1", "p2", "p3", "p5", "p7", "p8", "p9"))
area12054 <- data.frame(area = area_12054$area, id = area_12054$id, per = c("p1", "p2", "p3", "p5", "p7", "p8", "p9"))

dist12056 <- pointDistance(cent_12056[,3:4], reef_cent, lonlat = TRUE)
dist12056 <- data.frame(distance = dist12056, id = rep("12056", 5), per = c("p1", "p2", "p3", "p7", "p8"))
area12056 <- data.frame(area = area_12056$area, id = area_12056$id, per = c("p1", "p2", "p3", "p7", "p8"))


dist12058 <- pointDistance(cent_12058[,3:4], reef_cent, lonlat = TRUE)
dist12058 <- data.frame(distance = dist12058, id = rep("12058", 8), per = c("p0", "p1", "p2", "p3", "p5", "p7", "p8", "p9"))
area12058 <- data.frame(area = area_12058$area, id = area_12058$id, per = c("p0", "p1", "p2", "p3", "p5", "p7", "p8", "p9"))

dist12060 <- pointDistance(cent_12060[,3:4], reef_cent, lonlat = TRUE)
dist12060 <- data.frame(distance = dist12060, id = rep("12060", 10), per = c("p0", "p1", "p2", "p3", "11", "p5", "18", "p7", "p8", "p9"))
area12060 <- data.frame(area = area_12060$area, id = area_12060$id, per = c("p0", "p1", "p2", "p3", "11", "p5", "18", "p7", "p8", "p9"))

dist12062 <- pointDistance(cent_12062[,3:4], reef_cent, lonlat = TRUE)
dist12062 <- data.frame(distance = dist12062, id = rep("12062", 10), per = c("p0", "p1", "p2", "p3", "11", "p5", "18", "p7", "p8", "p9"))
area12062 <- data.frame(area = area_12062$area, id = area_12062$id, per = c("p0", "p1", "p2", "p3", "11", "p5", "18", "p7", "p8", "p9"))


dist12064 <- pointDistance(cent_12064[,3:4], reef_cent, lonlat = TRUE)
dist12064 <- data.frame(distance = dist12064, id = rep("12064", 10), per = c("p0", "p1", "p2", "p3", "11", "p5", "18", "p7", "p8", "p9"))
area12064 <- data.frame(area = area_12064$area, id = area_12064$id, per = c("p0", "p1", "p2", "p3", "11", "p5", "18", "p7", "p8", "p9"))


dist12066 <- pointDistance(cent_12066[,3:4], reef_cent, lonlat = TRUE)
dist12066 <- data.frame(distance = dist12066, id = rep("12066", 10), per = c("p0", "p1", "p2", "p3", "11", "p5", "18", "p7", "p8", "p9"))
area12066 <- data.frame(area = area_12066$area, id = area_12066$id, per = c("p0", "p1", "p2", "p3", "11", "p5", "18", "p7", "p8", "p9"))


dist12068 <- pointDistance(cent_12068[,3:4], reef_cent, lonlat = TRUE)
dist12068 <- data.frame(distance = dist12068, id = rep("12068", 9), per = c("p0", "p1", "p2", "p3", "p5", "18", "p7", "p8", "p9"))
area12068 <- data.frame(area = area_12068$area, id = area_12068$id, per = c("p0", "p1", "p2", "p3", "p5", "18", "p7", "p8", "p9"))


dist12070 <- pointDistance(cent_12070[,3:4], reef_cent, lonlat = TRUE)
dist12070 <- data.frame(distance = dist12070, id = rep("12070", 10), per = c("p0", "p1", "p2", "p3", "11", "p5", "18", "p7", "p8", "p9"))
area12070 <- data.frame(area = area_12070$area, id = area_12070$id, per = c("p0", "p1", "p2", "p3", "11", "p5", "18", "p7", "p8", "p9"))

dist12074 <- pointDistance(cent_12074[,3:4], reef_cent, lonlat = TRUE)
dist12074 <- data.frame(distance = dist12074, id = rep("12074", 10), per = c("p0", "p1", "p2", "p3", "11", "p5", "18", "p7", "p8", "p9"))
area12074 <- data.frame(area = area_12074$area, id = area_12074$id, per = c("p0", "p1", "p2", "p3", "11", "p5", "18", "p7", "p8", "p9"))

### Repeated Measures ANOVA for distance from center of reef ("safe habitat") -
####### next time also try distance to that fish's center for total use? for 3 months?
###### 
library(tidyverse)
library(ggpubr)
library(rstatix)

BlackR.all.dist <- dist12048 %>% rbind(dist12050, dist12052, dist12054, dist12056, dist12058, 
                                       dist12060, dist12062, dist12064, dist12066, dist12068, 
                                       dist12070, dist12074)

BlackR.dist.ANOVA <- BlackR.all.dist %>% convert_as_factor(id, per)

BlackR.all.area <- area12048 %>% rbind(area12050, area12052, area12054, area12056, area12058, 
                                       area12060, area12062, area12064, area12066, area12068, 
                                       area12070, area12074)
BlackR.area.ANOVA <- BlackR.all.area %>% convert_as_factor(id, per)

  #gather(key = "period", value = "area", area.before, area.dur, area.after) %>%
head(BlackR.dist.ANOVA, 3)
head(BlackR.area.ANOVA, 3)

BlackR.dist.ANOVA %>%
  group_by(per) %>%
  get_summary_stats(distance, type = "mean_sd")


period.levels <- c("p0", "p1", "p2", "p3", "11", "p5", "18", "p7", "p8", "p9")
BlackR.dist.ANOVA$per <- factor(BlackR.dist.ANOVA$per, levels = period.levels[c(1:10)])
BlackR.dist.ANOVA <- BlackR.dist.ANOVA %>% na.omit()

BlackR.area.ANOVA$per <- factor(BlackR.area.ANOVA$per, levels = period.levels[c(1:10)])
BlackR.area.ANOVA <- BlackR.area.ANOVA %>% na.omit()
#BlackR.area.ANOVA <- BlackR.area.ANOVA %>% filter(!(id == "12068" | id == "12058" | id == "12056" | id == "12054"))

#br.dist.aov <- anova_test(data = BlackR.dist.ANOVA, dv = distance, wid = id, within = per)
#get_anova_table(br.dist.aov)

#br.area.aov <- anova_test(data = BlackR.area.ANOVA, dv = area, wid = id, within = per)
#get_anova_table(br.area.aov) # p < 0.0005


bxp <- ggboxplot(BlackR.area.ANOVA, x = "per", y = "area")
bxp

ggboxplot(BlackR.dist.ANOVA, x = "per", y = "distance")

BlackR.area.ANOVA %>%
  group_by(per) %>%
  identify_outliers(area) # test for extreme outliers - 12054 p2, 12056 p2, 12058 p5 . 12070 is highly mobile individual (/greater area)

BlackR.dist.ANOVA %>%
  group_by(per) %>%
  shapiro_test(distance) #test for normality - some fail (below 0.05)

ggqqplot(BlackR.area.ANOVA, "area", facet.by = "per") # all points fall within line



pwc_BR_area <- aov(area ~ factor(per) + Error(factor(id)), data = BlackR.area.ANOVA)
summary(pwc_BR_area) #### use this one - p = 00.0118 stat diff in mean area between the periods; # of individ, examine effec of periods on area - period lead to statistically sign. differences ni areas 
emm_BR_area <- emmeans(pwc_BR_area, ~ per)
emm_BR_area
contrast(emm_BR_area, method = "pairwise", adjust = "bonferroni")

##########################################################################
pwc <- pw.BlackR %>%
  pairwise_t_test(
    distance ~ per, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
pwc

pwc_BR_dist <- aov(distance ~ factor(per) + Error(factor(id)), data = BlackR.dist.ANOVA)
summary(pwc_BR_dist) ## p = 0.688, not significant difference
emm_BR_dist <- emmeans(pwc_BR_dist, ~ per)
contrast(emm_BR_dist, method = "pairwise", adjust = "bonferroni")

library(emmeans)
#emm_BR_dist <- emmeans(pwc_BR_dist, ~ per)
#pairs(emm_BR_dist, )

#br.dist.aov <- anova_test(data = pw.BlackR, dv = distance, wid = id, within = per) #p =0.951, no significant pairwise differences to center of reef, no difference (size of reef is about 800 m ), 
#don't put in box plot (range of values between x and x, reef size 800m (within reef))
#get_anova_table(br.dist.aov)



#area by june 11, 12-16, 18
smaller_area <- BlackR.area.ANOVA %>% filter(per == "p3" | per == "11" | per == "p5" | per == "18" | per == "p7")
br.smlarea.aov <- aov(area ~ factor(per) + Error(factor(id)), data = smaller_area)
summary(br.smlarea.aov) #### p = 0.0813 stat diff in mean area between the periods; # of individ, examine effec of periods on area - period lead to statistically sign. differences ni areas 
emm_BR_smlarea <- emmeans(br.smlarea.aov, ~ per)
contrast(emm_BR_smlarea, method = "pairwise", adjust = "bonferroni")


## same but no extreme outliers !!!!!!!!!!!!!!!!!!!!!!!!!
noout.BlackR <- BlackR.area.ANOVA %>% filter(!(id == "12058"))
smaller_area_noout <- noout.BlackR %>% filter(per == "p3" | per == "11" | per == "p5" | per == "18" | per == "p7")
br.smlarea.aov.no <- aov(area ~ factor(per) + Error(factor(id)), data = smaller_area_noout)
summary(br.smlarea.aov.no) #### p = 0.0695 stat diff in mean area between the periods; # of individ, examine effec of periods on area - period lead to statistically sign. differences ni areas 
emm_BR_smlarea_no <- emmeans(br.smlarea.aov.no, ~ per)
contrast(emm_BR_smlarea_no, method = "pairwise", adjust = "bonferroni")

### Distance to center of total use ?
###### HERE 
## calculate centroid for total study.....
library(raster)
library(sfheaders)

reef_cent <- data.frame(long = -124.47589, lat = 42.69939)
reef_cent <- st_as_sf(reef_cent, coords = c('long', 'lat'), crs = st_crs(4326))


cent_pts <- data.frame(ID = ,Centr = ,Period = ,Reef = reef_cent)

dist12048 <- pointDistance(cent_12048[,3:4], reef_cent, lonlat = TRUE)
dist12048 <- data.frame(distance = dist12048, id = rep("12048", 10), per = c("p0", "p1", "p2", "p3", "11", "p5", "18", "p7", "p8", "p9"))

dist12050 <- pointDistance(cent_12050[,3:4], reef_cent, lonlat = TRUE)
dist12050 <- data.frame(distance = dist12050, id = rep("12050", 10), per = c("p0", "p1", "p2", "p3", "11", "p5", "18", "p7", "p8", "p9"))

dist12052 <- pointDistance(cent_12052[,3:4], reef_cent, lonlat = TRUE)
dist12052 <- data.frame(distance = dist12052, id = rep("12052", 10), per = c("p0", "p1", "p2", "p3", "11", "p5", "18", "p7", "p8", "p9"))

dist12054 <- pointDistance(cent_12054[,3:4], reef_cent, lonlat = TRUE)
dist12054 <- data.frame(distance = dist12054, id = rep("12054", 7), per = c("p1", "p2", "p3", "p5", "p7", "p8", "p9"))


dist12056 <- pointDistance(cent_12056[,3:4], reef_cent, lonlat = TRUE)
dist12056 <- data.frame(distance = dist12056, id = rep("12056", 5), per = c("p1", "p2", "p3", "p7", "p8"))


dist12058 <- pointDistance(cent_12058[,3:4], reef_cent, lonlat = TRUE)
dist12058 <- data.frame(distance = dist12058, id = rep("12058", 8), per = c("p0", "p1", "p2", "p3", "p5", "p7", "p8", "p9"))


dist12060 <- pointDistance(cent_12060[,3:4], reef_cent, lonlat = TRUE)
dist12060 <- data.frame(distance = dist12060, id = rep("12060", 10), per = c("p0", "p1", "p2", "p3", "11", "p5", "18", "p7", "p8", "p9"))


dist12062 <- pointDistance(cent_12062[,3:4], reef_cent, lonlat = TRUE)
dist12062 <- data.frame(distance = dist12062, id = rep("12062", 10), per = c("p0", "p1", "p2", "p3", "11", "p5", "18", "p7", "p8", "p9"))


dist12064 <- pointDistance(cent_12064[,3:4], reef_cent, lonlat = TRUE)
dist12064 <- data.frame(distance = dist12064, id = rep("12064", 10), per = c("p0", "p1", "p2", "p3", "11", "p5", "18", "p7", "p8", "p9"))


dist12066 <- pointDistance(cent_12066[,3:4], reef_cent, lonlat = TRUE)
dist12066 <- data.frame(distance = dist12066, id = rep("12066", 10), per = c("p0", "p1", "p2", "p3", "11", "p5", "18", "p7", "p8", "p9"))


dist12068 <- pointDistance(cent_12068[,3:4], reef_cent, lonlat = TRUE)
dist12068 <- data.frame(distance = dist12068, id = rep("12068", 9), per = c("p0", "p1", "p2", "p3", "p5", "18", "p7", "p8", "p9"))


dist12070 <- pointDistance(cent_12070[,3:4], reef_cent, lonlat = TRUE)
dist12070 <- data.frame(distance = dist12070, id = rep("12070", 10), per = c("p0", "p1", "p2", "p3", "11", "p5", "18", "p7", "p8", "p9"))


dist12074 <- pointDistance(cent_12074[,3:4], reef_cent, lonlat = TRUE)
dist12074 <- data.frame(distance = dist12074, id = rep("12074", 10), per = c("p0", "p1", "p2", "p3", "11", "p5", "18", "p7", "p8", "p9"))

### Repeated Measures ANOVA for distance from center of reef ("safe habitat") -
####### next time also try distance to that fish's center for total use? for 3 months?
###### 
library(tidyverse)
library(ggpubr)
library(rstatix)

BlackR.all.dist <- dist12048 %>% rbind(dist12050, dist12052, dist12054, dist12056, dist12058, 
                                       dist12060, dist12062, dist12064, dist12066, dist12068, 
                                       dist12070, dist12074)





skew_BR <- BlackR_acceleration %>% drop_na(Sensor.Value)

skew_BRp3 <- skew_BR %>% filter(survey.period == "June 3-10")
skew_BR11 <- skew_BR %>% filter(survey.period == "June 11")
skew_BRp5 <- skew_BR %>% filter(survey.period == "June 12-16")
#skew_BR18 <- skew_BR %>% filter(survey.period == "June 18")
#skew_BRp7 <- skew_BR %>% filter(survey.period == "June 19-26")

skew_BR_periods <- skewness(skew_BRp3$Sensor.Value)
skew_BR_periods <- skew_BR_periods %>% rbind(skewness(skew_BR11$Sensor.Value), skewness(skew_BRp5$Sensor.Value),
                                             skewness(skew_BR18$Sensor.Value), skewness(skew_BRp7$Sensor.Value))

kurt_BR_periods <- kurtosis(skew_BRp3$Sensor.Value)
kurt_BR_periods <- kurt_BR_periods %>% rbind(kurtosis(skew_BR11$Sensor.Value), kurtosis(skew_BRp5$Sensor.Value))#,
                                             #kurtosis(skew_BR18$Sensor.Value, kurtosis(skew_BRp7$Sensor.Value)))

# measures of skewness and mean changes?