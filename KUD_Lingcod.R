# Lingcod
## Before
### 13249
LC.positions.b49 <- Lingcod_accel %>% filter(periods == "Before") %>% filter(Transmitter == "A69-9007-13249")
LC.positions.b49 <- LC.positions.b49 %>% dplyr::group_by(Transmitter)
b49_LC <- LC.positions.b49[!is.na(LC.positions.b49$Longitude) & !is.na(LC.positions.b49$Latitude),]
b49_LC.sp <- b49_LC[, c("Transmitter", "Longitude", "Latitude")]
coordinates(b49_LC.sp) <- c("Longitude", "Latitude")
proj4string(b49_LC.sp) <- CRS("+init=epsg:4326")

kernel.b49.LC <- kernelUD(b49_LC.sp, h = "href")
LC.b49.poly <- getverticeshr(kernel.b49.LC, percent = 95)

LC.b49.sp <- fortify(LC.b49.poly)
LC.b49.df <- as.data.frame(LC.b49.poly)
LC.b49.df <- LC.b49.df %>% rename(area.before = area)


### 13250 #################################
LC.positions.b50 <- Lingcod_accel %>% filter(periods == "Before") %>% filter(Transmitter == "A69-9007-13250")
LC.positions.b50 <- LC.positions.b50 %>% dplyr::group_by(Transmitter)
b50_LC <- LC.positions.b50[!is.na(LC.positions.b50$Longitude) & !is.na(LC.positions.b50$Latitude),]
b50_LC.sp <- b50_LC[, c("Transmitter", "Longitude", "Latitude")]
coordinates(b50_LC.sp) <- c("Longitude", "Latitude")
proj4string(b50_LC.sp) <- CRS("+init=epsg:4326")

kernel.b50.LC <- kernelUD(b50_LC.sp, h = "href")
LC.b50.poly <- getverticeshr(kernel.b50.LC, percent = 95)

LC.b50.sp <- fortify(LC.b50.poly)
LC.b50.df <- as.data.frame(LC.b50.poly)
LC.b50.df <- LC.b50.df %>% rename(area.before = area)

### 13251 #################################
LC.positions.b51 <- Lingcod_accel %>% filter(periods == "Before") %>% filter(Transmitter == "A69-9007-13251")
LC.positions.b51 <- LC.positions.b51 %>% dplyr::group_by(Transmitter)
b51_LC <- LC.positions.b51[!is.na(LC.positions.b51$Longitude) & !is.na(LC.positions.b51$Latitude),]
b51_LC.sp <- b51_LC[, c("Transmitter", "Longitude", "Latitude")]
coordinates(b51_LC.sp) <- c("Longitude", "Latitude")
proj4string(b51_LC.sp) <- CRS("+init=epsg:4326")

kernel.b51.LC <- kernelUD(b51_LC.sp, h = "href")
LC.b51.poly <- getverticeshr(kernel.b51.LC, percent = 95)

LC.b51.sp <- fortify(LC.b51.poly)
LC.b51.df <- as.data.frame(LC.b51.poly)
LC.b51.df <- LC.b51.df %>% rename(area.before = area)

### 13252 #######################
LC.positions.b52 <- Lingcod_accel %>% filter(periods == "Before") %>% filter(Transmitter == "A69-9007-13252")
LC.positions.b52 <- LC.positions.b52 %>% dplyr::group_by(Transmitter)
b52_LC <- LC.positions.b52[!is.na(LC.positions.b52$Longitude) & !is.na(LC.positions.b52$Latitude),]
b52_LC.sp <- b52_LC[, c("Transmitter", "Longitude", "Latitude")]
coordinates(b52_LC.sp) <- c("Longitude", "Latitude")
proj4string(b52_LC.sp) <- CRS("+init=epsg:4326")

kernel.b52.LC <- kernelUD(b52_LC.sp, h = "href")
LC.b52.poly <- getverticeshr(kernel.b52.LC, percent = 95)

LC.b52.sp <- fortify(LC.b52.poly)
LC.b52.df <- as.data.frame(LC.b52.poly)
LC.b52.df <- LC.b52.df %>% rename(area.before = area)

### 13253 ##################
LC.positions.b53 <- Lingcod_accel %>% filter(periods == "Before") %>% filter(Transmitter == "A69-9007-13253")
LC.positions.b53 <- LC.positions.b53 %>% dplyr::group_by(Transmitter)
b53_LC <- LC.positions.b53[!is.na(LC.positions.b53$Longitude) & !is.na(LC.positions.b53$Latitude),]
b53_LC.sp <- b53_LC[, c("Transmitter", "Longitude", "Latitude")]
coordinates(b53_LC.sp) <- c("Longitude", "Latitude")
proj4string(b53_LC.sp) <- CRS("+init=epsg:4326")

kernel.b53.LC <- kernelUD(b53_LC.sp, h = "href")
LC.b53.poly <- getverticeshr(kernel.b53.LC, percent = 95)

LC.b53.sp <- fortify(LC.b53.poly)
LC.b53.df <- as.data.frame(LC.b53.poly)
LC.b53.df <- LC.b53.df %>% rename(area.before = area)


### 13254 ##############
LC.positions.b54 <- Lingcod_accel %>% filter(periods == "Before") %>% filter(Transmitter == "A69-9007-13254")
LC.positions.b54 <- LC.positions.b54 %>% dplyr::group_by(Transmitter)
b54_LC <- LC.positions.b54[!is.na(LC.positions.b54$Longitude) & !is.na(LC.positions.b54$Latitude),]
b54_LC.sp <- b54_LC[, c("Transmitter", "Longitude", "Latitude")]
coordinates(b54_LC.sp) <- c("Longitude", "Latitude")
proj4string(b54_LC.sp) <- CRS("+init=epsg:4326")

kernel.b54.LC <- kernelUD(b54_LC.sp, h = "href")
LC.b54.poly <- getverticeshr(kernel.b54.LC, percent = 95)

LC.b54.sp <- fortify(LC.b54.poly)
LC.b54.df <- as.data.frame(LC.b54.poly)
LC.b54.df <- LC.b54.df %>% rename(area.before = area)

### 13255 ############################
LC.positions.b55 <- Lingcod_accel %>% filter(periods == "Before") %>% filter(Transmitter == "A69-9007-13255")
LC.positions.b55 <- LC.positions.b55 %>% dplyr::group_by(Transmitter)
b55_LC <- LC.positions.b55[!is.na(LC.positions.b55$Longitude) & !is.na(LC.positions.b55$Latitude),]
b55_LC.sp <- b55_LC[, c("Transmitter", "Longitude", "Latitude")]
coordinates(b55_LC.sp) <- c("Longitude", "Latitude")
proj4string(b55_LC.sp) <- CRS("+init=epsg:4326")

kernel.b55.LC <- kernelUD(b55_LC.sp, h = "href")
LC.b55.poly <- getverticeshr(kernel.b55.LC, percent = 95)

LC.b55.sp <- fortify(LC.b55.poly)
LC.b55.df <- as.data.frame(LC.b55.poly)
LC.b55.df <- LC.b55.df %>% rename(area.before = area)

### 13256
LC.positions.b56 <- Lingcod_accel %>% filter(periods == "Before") %>% filter(Transmitter == "A69-9007-13256")
LC.positions.b56 <- LC.positions.b56 %>% dplyr::group_by(Transmitter)
b56_LC <- LC.positions.b56[!is.na(LC.positions.b56$Longitude) & !is.na(LC.positions.b56$Latitude),]
b56_LC.sp <- b56_LC[, c("Transmitter", "Longitude", "Latitude")]
coordinates(b56_LC.sp) <- c("Longitude", "Latitude")
proj4string(b56_LC.sp) <- CRS("+init=epsg:4326")

kernel.b56.LC <- kernelUD(b56_LC.sp, h = "href")
LC.b56.poly <- getverticeshr(kernel.b56.LC, percent = 95)

LC.b56.sp <- fortify(LC.b56.poly)
LC.b56.df <- as.data.frame(LC.b56.poly)
LC.b56.df <- LC.b56.df %>% rename(area.before = area)

### 13257 ##################################
LC.positions.b57 <- Lingcod_accel %>% filter(periods == "Before") %>% filter(Transmitter == "A69-9007-13257")
LC.positions.b57 <- LC.positions.b57 %>% dplyr::group_by(Transmitter)
b57_LC <- LC.positions.b57[!is.na(LC.positions.b57$Longitude) & !is.na(LC.positions.b57$Latitude),]
b57_LC.sp <- b57_LC[, c("Transmitter", "Longitude", "Latitude")]
coordinates(b57_LC.sp) <- c("Longitude", "Latitude")
proj4string(b57_LC.sp) <- CRS("+init=epsg:4326")

kernel.b57.LC <- kernelUD(b57_LC.sp, h = "href")
LC.b57.poly <- getverticeshr(kernel.b57.LC, percent = 95)

LC.b57.sp <- fortify(LC.b57.poly)
LC.b57.df <- as.data.frame(LC.b57.poly)
LC.b57.df <- LC.b57.df %>% rename(area.before = area)

### 13258 ##########################
LC.positions.b58 <- Lingcod_accel %>% filter(periods == "Before") %>% filter(Transmitter == "A69-9007-13258")
LC.positions.b58 <- LC.positions.b58 %>% dplyr::group_by(Transmitter)
b58_LC <- LC.positions.b58[!is.na(LC.positions.b58$Longitude) & !is.na(LC.positions.b58$Latitude),]
b58_LC.sp <- b58_LC[, c("Transmitter", "Longitude", "Latitude")]
coordinates(b58_LC.sp) <- c("Longitude", "Latitude")
proj4string(b58_LC.sp) <- CRS("+init=epsg:4326")

kernel.b58.LC <- kernelUD(b58_LC.sp, h = "href")
LC.b58.poly <- getverticeshr(kernel.b58.LC, percent = 95)

LC.b58.sp <- fortify(LC.b58.poly)
LC.b58.df <- as.data.frame(LC.b58.poly)
LC.b58.df <- LC.b58.df %>% rename(area.before = area)

### 13259 ################################
LC.positions.b59 <- Lingcod_accel %>% filter(periods == "Before") %>% filter(Transmitter == "A69-9007-13259")
LC.positions.b59 <- LC.positions.b59 %>% dplyr::group_by(Transmitter)
b59_LC <- LC.positions.b59[!is.na(LC.positions.b59$Longitude) & !is.na(LC.positions.b59$Latitude),]
b59_LC.sp <- b59_LC[, c("Transmitter", "Longitude", "Latitude")]
coordinates(b59_LC.sp) <- c("Longitude", "Latitude")
proj4string(b59_LC.sp) <- CRS("+init=epsg:4326")

kernel.b59.LC <- kernelUD(b59_LC.sp, h = "href")
LC.b59.poly <- getverticeshr(kernel.b59.LC, percent = 95)

LC.b59.sp <- fortify(LC.b59.poly)
LC.b59.df <- as.data.frame(LC.b59.poly)
LC.b59.df <- LC.b59.df %>% rename(area.before = area)

### 13260
LC.positions.b60 <- Lingcod_accel %>% filter(periods == "Before") %>% filter(Transmitter == "A69-9007-13260")
LC.positions.b60 <- LC.positions.b60 %>% dplyr::group_by(Transmitter)
b60_LC <- LC.positions.b60[!is.na(LC.positions.b60$Longitude) & !is.na(LC.positions.b60$Latitude),]
b60_LC.sp <- b60_LC[, c("Transmitter", "Longitude", "Latitude")]
coordinates(b60_LC.sp) <- c("Longitude", "Latitude")
proj4string(b60_LC.sp) <- CRS("+init=epsg:4326")

kernel.b60.LC <- kernelUD(b60_LC.sp, h = "href")
LC.b60.poly <- getverticeshr(kernel.b60.LC, percent = 95)

LC.b60.sp <- fortify(LC.b60.poly)
LC.b60.df <- as.data.frame(LC.b60.poly)
LC.b60.df <- LC.b60.df %>% rename(area.before = area)


### 13261 #################################
LC.positions.b61 <- Lingcod_accel %>% filter(periods == "Before") %>% filter(Transmitter == "A69-9007-13261")
LC.positions.b61 <- LC.positions.b61 %>% dplyr::group_by(Transmitter)
b61_LC <- LC.positions.b61[!is.na(LC.positions.b61$Longitude) & !is.na(LC.positions.b61$Latitude),]
b61_LC.sp <- b61_LC[, c("Transmitter", "Longitude", "Latitude")]
coordinates(b61_LC.sp) <- c("Longitude", "Latitude")
proj4string(b61_LC.sp) <- CRS("+init=epsg:4326")

kernel.b61.LC <- kernelUD(b61_LC.sp, h = "href")
LC.b61.poly <- getverticeshr(kernel.b61.LC, percent = 95)

LC.b61.sp <- fortify(LC.b61.poly)
LC.b61.df <- as.data.frame(LC.b61.poly)
LC.b61.df <- LC.b61.df %>% rename(area.before = area)

### 13262 ##########################
LC.positions.b62 <- Lingcod_accel %>% filter(periods == "Before") %>% filter(Transmitter == "A69-9007-13262")
LC.positions.b62 <- LC.positions.b62 %>% dplyr::group_by(Transmitter)
b62_LC <- LC.positions.b62[!is.na(LC.positions.b62$Longitude) & !is.na(LC.positions.b62$Latitude),]
b62_LC.sp <- b62_LC[, c("Transmitter", "Longitude", "Latitude")]
coordinates(b62_LC.sp) <- c("Longitude", "Latitude")
proj4string(b62_LC.sp) <- CRS("+init=epsg:4326")

kernel.b62.LC <- kernelUD(b62_LC.sp, h = "href")
LC.b62.poly <- getverticeshr(kernel.b62.LC, percent = 95)

LC.b62.sp <- fortify(LC.b62.poly)
LC.b62.df <- as.data.frame(LC.b62.poly)
LC.b62.df <- LC.b62.df %>% rename(area.before = area)

### 13263
LC.positions.b63 <- Lingcod_accel %>% filter(periods == "Before") %>% filter(Transmitter == "A69-9007-13249")
LC.positions.b63 <- LC.positions.b63 %>% dplyr::group_by(Transmitter)
b63_LC <- LC.positions.b63[!is.na(LC.positions.b63$Longitude) & !is.na(LC.positions.b63$Latitude),]
b63_LC.sp <- b63_LC[, c("Transmitter", "Longitude", "Latitude")]
coordinates(b63_LC.sp) <- c("Longitude", "Latitude")
proj4string(b63_LC.sp) <- CRS("+init=epsg:4326")

kernel.b63.LC <- kernelUD(b63_LC.sp, h = "href")
LC.b63.poly <- getverticeshr(kernel.b63.LC, percent = 95)

LC.b63.sp <- fortify(LC.b63.poly)
LC.b63.df <- as.data.frame(LC.b63.poly)
LC.b63.df <- LC.b63.df %>% rename(area.before = area)








## During
LC.positions.dur <- Lingcod_accel %>% filter(periods == "During")
LC.positions.dur <- LC.positions.dur %>% dplyr::group_by(Transmitter)
dur_LC <- LC.positions.dur[!is.na(LC.positions.dur$Longitude) & !is.na(LC.positions.dur$Latitude),]
dur_LC.sp <- dur_LC[, c("Transmitter", "Longitude", "Latitude")]
coordinates(dur_LC.sp) <- c("Longitude", "Latitude")
proj4string(dur_LC.sp) <- CRS("+init=epsg:4326")

kernel.dur.LC <- kernelUD(dur_LC.sp, h = "href")
LC.dur.poly <- getverticeshr(kernel.dur.LC, percent = 95)

LC.dur.sp <- fortify(LC.dur.poly)
LC.dur.df <- as.data.frame(LC.dur.poly)
LC.dur.df <- LC.dur.df %>% rename(area.dur = area)



## After
LC.positions.after <- Lingcod_accel %>% filter(periods == "After")
LC.positions.after <- LC.positions.after %>% dplyr::group_by(Transmitter)
after_LC <- LC.positions.after[!is.na(LC.positions.after$Longitude) & !is.na(LC.positions.after$Latitude),]
after_LC.sp <- after_LC[, c("Transmitter", "Longitude", "Latitude")]
coordinates(after_LC.sp) <- c("Longitude", "Latitude")
proj4string(after_LC.sp) <- CRS("+init=epsg:4326")

kernel.after.LC <- kernelUD(after_LC.sp, h = "href")
LC.after.poly <- getverticeshr(kernel.after.LC, percent = 95)

LC.after.sp <- fortify(LC.after.poly)
LC.after.df <- as.data.frame(LC.after.poly)
LC.after.df <- LC.after.df %>% rename(area.after = area)
