# Lingcod
## Before
### 13249
LC.positions.b49 <- periods_Lingcod %>% filter(coarse.period == "Before") %>% filter(Transmitter == "A69-9007-13249")
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


### 13250 
LC.positions.b50 <- periods_Lingcod %>% filter(coarse.period == "Before") %>% filter(Transmitter == "A69-9007-13250")
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

### 13251 
LC.positions.b51 <- periods_Lingcod %>% filter(coarse.period == "Before") %>% filter(Transmitter == "A69-9007-13251")
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
LC.positions.b52 <- periods_Lingcod %>% filter(coarse.period == "Before") %>% filter(Transmitter == "A69-9007-13252")
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
LC.positions.b53 <- periods_Lingcod %>% filter(coarse.period == "Before") %>% filter(Transmitter == "A69-9007-13253")
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


### 13254 
LC.positions.b54 <- periods_Lingcod %>% filter(coarse.period == "Before") %>% filter(Transmitter == "A69-9007-13254")
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

### 13255 
LC.positions.b55 <- periods_Lingcod %>% filter(coarse.period == "Before") %>% filter(Transmitter == "A69-9007-13255")
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
LC.positions.b56 <- periods_Lingcod %>% filter(coarse.period == "Before") %>% filter(Transmitter == "A69-9007-13256")
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
LC.positions.b57 <- periods_Lingcod %>% filter(coarse.period == "Before") %>% filter(Transmitter == "A69-9007-13257")
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

### 13258
LC.positions.b58 <- periods_Lingcod %>% filter(coarse.period == "Before") %>% filter(Transmitter == "A69-9007-13258")
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

### 13259
LC.positions.b59 <- periods_Lingcod %>% filter(coarse.period == "Before") %>% filter(Transmitter == "A69-9007-13259")
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
LC.positions.b60 <- periods_Lingcod %>% filter(coarse.period == "Before") %>% filter(Transmitter == "A69-9007-13260")
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
LC.positions.b61 <- periods_Lingcod %>% filter(coarse.period == "Before") %>% filter(Transmitter == "A69-9007-13261")
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
LC.positions.b62 <- periods_Lingcod %>% filter(coarse.period == "Before") %>% filter(Transmitter == "A69-9007-13262")
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
LC.positions.b63 <- periods_Lingcod %>% filter(coarse.period == "Before") %>% filter(Transmitter == "A69-9007-13249")
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



Lingcod.before.df <- bind_rows(LC.b49.df, LC.b50.df, LC.b51.df, LC.b54.df, 
                               LC.b55.df, LC.b56.df, LC.b58.df, LC.b59.df,
                               LC.b60.df, LC.b63.df)







## During
### 13249
LC.positions.d49 <- periods_Lingcod %>% filter(coarse.period == "During") %>% filter(Transmitter == "A69-9007-13249")
LC.positions.d49 <- LC.positions.d49 %>% dplyr::group_by(Transmitter)
d49_LC <- LC.positions.d49[!is.na(LC.positions.d49$Longitude) & !is.na(LC.positions.d49$Latitude),]
d49_LC.sp <- d49_LC[, c("Transmitter", "Longitude", "Latitude")]
coordinates(d49_LC.sp) <- c("Longitude", "Latitude")
proj4string(d49_LC.sp) <- CRS("+init=epsg:4326")

kernel.d49.LC <- kernelUD(d49_LC.sp, h = "href")
LC.d49.poly <- getverticeshr(kernel.d49.LC, percent = 95)

LC.d49.sp <- fortify(LC.d49.poly)
LC.d49.df <- as.data.frame(LC.d49.poly)
LC.d49.df <- LC.d49.df %>% rename(area.dur = area)


### 13250 
LC.positions.d50 <- periods_Lingcod %>% filter(coarse.period == "During") %>% filter(Transmitter == "A69-9007-13250")
LC.positions.d50 <- LC.positions.d50 %>% dplyr::group_by(Transmitter)
d50_LC <- LC.positions.d50[!is.na(LC.positions.d50$Longitude) & !is.na(LC.positions.d50$Latitude),]
d50_LC.sp <- d50_LC[, c("Transmitter", "Longitude", "Latitude")]
coordinates(d50_LC.sp) <- c("Longitude", "Latitude")
proj4string(d50_LC.sp) <- CRS("+init=epsg:4326")

kernel.d50.LC <- kernelUD(d50_LC.sp, h = "href")
LC.d50.poly <- getverticeshr(kernel.d50.LC, percent = 95)

LC.d50.sp <- fortify(LC.d50.poly)
LC.d50.df <- as.data.frame(LC.d50.poly)
LC.d50.df <- LC.d50.df %>% rename(area.dur = area)

### 13251 
LC.positions.d51 <- periods_Lingcod %>% filter(coarse.period == "During") %>% filter(Transmitter == "A69-9007-13251")
LC.positions.d51 <- LC.positions.d51 %>% dplyr::group_by(Transmitter)
d51_LC <- LC.positions.d51[!is.na(LC.positions.d51$Longitude) & !is.na(LC.positions.d51$Latitude),]
d51_LC.sp <- d51_LC[, c("Transmitter", "Longitude", "Latitude")]
coordinates(d51_LC.sp) <- c("Longitude", "Latitude")
proj4string(d51_LC.sp) <- CRS("+init=epsg:4326")

kernel.d51.LC <- kernelUD(d51_LC.sp, h = "href")
LC.d51.poly <- getverticeshr(kernel.d51.LC, percent = 95)

LC.d51.sp <- fortify(LC.d51.poly)
LC.d51.df <- as.data.frame(LC.d51.poly)
LC.d51.df <- LC.d51.df %>% rename(area.dur = area)

### 13252 #######################
LC.positions.d52 <- periods_Lingcod %>% filter(coarse.period == "During") %>% filter(Transmitter == "A69-9007-13252")
LC.positions.d52 <- LC.positions.d52 %>% dplyr::group_by(Transmitter)
d52_LC <- LC.positions.d52[!is.na(LC.positions.d52$Longitude) & !is.na(LC.positions.d52$Latitude),]
d52_LC.sp <- d52_LC[, c("Transmitter", "Longitude", "Latitude")]
coordinates(d52_LC.sp) <- c("Longitude", "Latitude")
proj4string(d52_LC.sp) <- CRS("+init=epsg:4326")

kernel.d52.LC <- kernelUD(d52_LC.sp, h = "href")
LC.d52.poly <- getverticeshr(kernel.d52.LC, percent = 95)

LC.d52.sp <- fortify(LC.d52.poly)
LC.d52.df <- as.data.frame(LC.d52.poly)
LC.d52.df <- LC.d52.df %>% rename(area.dur = area)

### 13253 ##################
LC.positions.d53 <- periods_Lingcod %>% filter(coarse.period == "During") %>% filter(Transmitter == "A69-9007-13253")
LC.positions.d53 <- LC.positions.d53 %>% dplyr::group_by(Transmitter)
d53_LC <- LC.positions.d53[!is.na(LC.positions.d53$Longitude) & !is.na(LC.positions.d53$Latitude),]
d53_LC.sp <- d53_LC[, c("Transmitter", "Longitude", "Latitude")]
coordinates(d53_LC.sp) <- c("Longitude", "Latitude")
proj4string(d53_LC.sp) <- CRS("+init=epsg:4326")

kernel.d53.LC <- kernelUD(d53_LC.sp, h = "href")
LC.d53.poly <- getverticeshr(kernel.d53.LC, percent = 95)

LC.d53.sp <- fortify(LC.d53.poly)
LC.d53.df <- as.data.frame(LC.d53.poly)
LC.d53.df <- LC.d53.df %>% rename(area.dur = area)


### 13254 ###########################
LC.positions.d54 <- periods_Lingcod %>% filter(coarse.period == "During") %>% filter(Transmitter == "A69-9007-13254")
LC.positions.d54 <- LC.positions.d54 %>% dplyr::group_by(Transmitter)
d54_LC <- LC.positions.d54[!is.na(LC.positions.d54$Longitude) & !is.na(LC.positions.d54$Latitude),]
d54_LC.sp <- d54_LC[, c("Transmitter", "Longitude", "Latitude")]
coordinates(d54_LC.sp) <- c("Longitude", "Latitude")
proj4string(d54_LC.sp) <- CRS("+init=epsg:4326")

kernel.d54.LC <- kernelUD(d54_LC.sp, h = "href")
LC.d54.poly <- getverticeshr(kernel.d54.LC, percent = 95)

LC.d54.sp <- fortify(LC.d54.poly)
LC.d54.df <- as.data.frame(LC.d54.poly)
LC.d54.df <- LC.d54.df %>% rename(area.dur = area)

### 13255 
LC.positions.d55 <- periods_Lingcod %>% filter(coarse.period == "During") %>% filter(Transmitter == "A69-9007-13255")
LC.positions.d55 <- LC.positions.d55 %>% dplyr::group_by(Transmitter)
d55_LC <- LC.positions.d55[!is.na(LC.positions.d55$Longitude) & !is.na(LC.positions.d55$Latitude),]
d55_LC.sp <- d55_LC[, c("Transmitter", "Longitude", "Latitude")]
coordinates(d55_LC.sp) <- c("Longitude", "Latitude")
proj4string(d55_LC.sp) <- CRS("+init=epsg:4326")

kernel.d55.LC <- kernelUD(d55_LC.sp, h = "href")
LC.d55.poly <- getverticeshr(kernel.d55.LC, percent = 95)

LC.d55.sp <- fortify(LC.d55.poly)
LC.d55.df <- as.data.frame(LC.d55.poly)
LC.d55.df <- LC.d55.df %>% rename(area.dur = area)

### 13256 #######################################
LC.positions.d56 <- periods_Lingcod %>% filter(coarse.period == "During") %>% filter(Transmitter == "A69-9007-13256")
LC.positions.d56 <- LC.positions.d56 %>% dplyr::group_by(Transmitter)
d56_LC <- LC.positions.d56[!is.na(LC.positions.d56$Longitude) & !is.na(LC.positions.d56$Latitude),]
d56_LC.sp <- d56_LC[, c("Transmitter", "Longitude", "Latitude")]
coordinates(d56_LC.sp) <- c("Longitude", "Latitude")
proj4string(d56_LC.sp) <- CRS("+init=epsg:4326")

kernel.d56.LC <- kernelUD(d56_LC.sp, h = "href")
LC.d56.poly <- getverticeshr(kernel.d56.LC, percent = 95)

LC.d56.sp <- fortify(LC.d56.poly)
LC.d56.df <- as.data.frame(LC.d56.poly)
LC.d56.df <- LC.d56.df %>% rename(area.dur = area)

### 13257 ##################################
LC.positions.d57 <- periods_Lingcod %>% filter(coarse.period == "During") %>% filter(Transmitter == "A69-9007-13257")
LC.positions.d57 <- LC.positions.d57 %>% dplyr::group_by(Transmitter)
d57_LC <- LC.positions.d57[!is.na(LC.positions.d57$Longitude) & !is.na(LC.positions.d57$Latitude),]
d57_LC.sp <- d57_LC[, c("Transmitter", "Longitude", "Latitude")]
coordinates(d57_LC.sp) <- c("Longitude", "Latitude")
proj4string(d57_LC.sp) <- CRS("+init=epsg:4326")

kernel.d57.LC <- kernelUD(d57_LC.sp, h = "href")
LC.d57.poly <- getverticeshr(kernel.d57.LC, percent = 95)

LC.d57.sp <- fortify(LC.d57.poly)
LC.d57.df <- as.data.frame(LC.d57.poly)
LC.d57.df <- LC.d57.df %>% rename(area.dur = area)

### 13258 ###########################
LC.positions.d58 <- periods_Lingcod %>% filter(coarse.period == "During") %>% filter(Transmitter == "A69-9007-13258")
LC.positions.d58 <- LC.positions.d58 %>% dplyr::group_by(Transmitter)
d58_LC <- LC.positions.d58[!is.na(LC.positions.d58$Longitude) & !is.na(LC.positions.d58$Latitude),]
d58_LC.sp <- d58_LC[, c("Transmitter", "Longitude", "Latitude")]
coordinates(d58_LC.sp) <- c("Longitude", "Latitude")
proj4string(d58_LC.sp) <- CRS("+init=epsg:4326")

kernel.d58.LC <- kernelUD(d58_LC.sp, h = "href")
LC.d58.poly <- getverticeshr(kernel.d58.LC, percent = 95)

LC.d58.sp <- fortify(LC.d58.poly)
LC.d58.df <- as.data.frame(LC.d58.poly)
LC.d58.df <- LC.d58.df %>% rename(area.dur = area)

### 13259 ###########################
LC.positions.d59 <- periods_Lingcod %>% filter(coarse.period == "During") %>% filter(Transmitter == "A69-9007-13259")
LC.positions.d59 <- LC.positions.d59 %>% dplyr::group_by(Transmitter)
d59_LC <- LC.positions.d59[!is.na(LC.positions.d59$Longitude) & !is.na(LC.positions.d59$Latitude),]
d59_LC.sp <- d59_LC[, c("Transmitter", "Longitude", "Latitude")]
coordinates(d59_LC.sp) <- c("Longitude", "Latitude")
proj4string(d59_LC.sp) <- CRS("+init=epsg:4326")

kernel.d59.LC <- kernelUD(d59_LC.sp, h = "href")
LC.d59.poly <- getverticeshr(kernel.d59.LC, percent = 95)

LC.d59.sp <- fortify(LC.d59.poly)
LC.d59.df <- as.data.frame(LC.d59.poly)
LC.d59.df <- LC.d59.df %>% rename(area.dur = area)

### 13260 #######################
LC.positions.d60 <- periods_Lingcod %>% filter(coarse.period == "During") %>% filter(Transmitter == "A69-9007-13260")
LC.positions.d60 <- LC.positions.d60 %>% dplyr::group_by(Transmitter)
d60_LC <- LC.positions.d60[!is.na(LC.positions.d60$Longitude) & !is.na(LC.positions.d60$Latitude),]
d60_LC.sp <- d60_LC[, c("Transmitter", "Longitude", "Latitude")]
coordinates(d60_LC.sp) <- c("Longitude", "Latitude")
proj4string(d60_LC.sp) <- CRS("+init=epsg:4326")

kernel.d60.LC <- kernelUD(d60_LC.sp, h = "href")
LC.d60.poly <- getverticeshr(kernel.d60.LC, percent = 95)

LC.d60.sp <- fortify(LC.d60.poly)
LC.d60.df <- as.data.frame(LC.d60.poly)
LC.d60.df <- LC.d60.df %>% rename(area.dur = area)


### 13261 #################################
LC.positions.d61 <- periods_Lingcod %>% filter(coarse.period == "During") %>% filter(Transmitter == "A69-9007-13261")
LC.positions.d61 <- LC.positions.d61 %>% dplyr::group_by(Transmitter)
d61_LC <- LC.positions.d61[!is.na(LC.positions.d61$Longitude) & !is.na(LC.positions.d61$Latitude),]
d61_LC.sp <- d61_LC[, c("Transmitter", "Longitude", "Latitude")]
coordinates(d61_LC.sp) <- c("Longitude", "Latitude")
proj4string(d61_LC.sp) <- CRS("+init=epsg:4326")

kernel.d61.LC <- kernelUD(d61_LC.sp, h = "href")
LC.d61.poly <- getverticeshr(kernel.d61.LC, percent = 95)

LC.d61.sp <- fortify(LC.d61.poly)
LC.d61.df <- as.data.frame(LC.d61.poly)
LC.d61.df <- LC.d61.df %>% rename(area.dur = area)

### 13262 ##########################
LC.positions.d62 <- periods_Lingcod %>% filter(coarse.period == "During") %>% filter(Transmitter == "A69-9007-13262")
LC.positions.d62 <- LC.positions.d62 %>% dplyr::group_by(Transmitter)
d62_LC <- LC.positions.d62[!is.na(LC.positions.d62$Longitude) & !is.na(LC.positions.d62$Latitude),]
d62_LC.sp <- d62_LC[, c("Transmitter", "Longitude", "Latitude")]
coordinates(d62_LC.sp) <- c("Longitude", "Latitude")
proj4string(d62_LC.sp) <- CRS("+init=epsg:4326")

kernel.d62.LC <- kernelUD(d62_LC.sp, h = "href")
LC.d62.poly <- getverticeshr(kernel.d62.LC, percent = 95)

LC.d62.sp <- fortify(LC.d62.poly)
LC.d62.df <- as.data.frame(LC.d62.poly)
LC.d62.df <- LC.d62.df %>% rename(area.dur = area)

### 13263
LC.positions.d63 <- periods_Lingcod %>% filter(coarse.period == "During") %>% filter(Transmitter == "A69-9007-13249")
LC.positions.d63 <- LC.positions.d63 %>% dplyr::group_by(Transmitter)
d63_LC <- LC.positions.d63[!is.na(LC.positions.d63$Longitude) & !is.na(LC.positions.d63$Latitude),]
d63_LC.sp <- d63_LC[, c("Transmitter", "Longitude", "Latitude")]
coordinates(d63_LC.sp) <- c("Longitude", "Latitude")
proj4string(d63_LC.sp) <- CRS("+init=epsg:4326")

kernel.d63.LC <- kernelUD(d63_LC.sp, h = "href")
LC.d63.poly <- getverticeshr(kernel.d63.LC, percent = 95)

LC.d63.sp <- fortify(LC.d63.poly)
LC.d63.df <- as.data.frame(LC.d63.poly)
LC.d63.df <- LC.d63.df %>% rename(area.dur = area)



Lingcod.dur.df <- bind_rows(LC.d49.df, LC.d50.df, LC.d51.df, 
                               LC.d55.df, LC.d63.df)






## After
### 13249
LC.positions.a49 <- periods_Lingcod %>% filter(coarse.period == "After") %>% filter(Transmitter == "A69-9007-13249")
LC.positions.a49 <- LC.positions.a49 %>% dplyr::group_by(Transmitter)
a49_LC <- LC.positions.a49[!is.na(LC.positions.a49$Longitude) & !is.na(LC.positions.a49$Latitude),]
a49_LC.sp <- a49_LC[, c("Transmitter", "Longitude", "Latitude")]
coordinates(a49_LC.sp) <- c("Longitude", "Latitude")
proj4string(a49_LC.sp) <- CRS("+init=epsg:4326")

kernel.a49.LC <- kernelUD(a49_LC.sp, h = "href")
LC.a49.poly <- getverticeshr(kernel.a49.LC, percent = 95)

LC.a49.sp <- fortify(LC.a49.poly)
LC.a49.df <- as.data.frame(LC.a49.poly)
LC.a49.df <- LC.a49.df %>% rename(area.after = area)


### 13250  #########################
LC.positions.a50 <- periods_Lingcod %>% filter(coarse.period == "After") %>% filter(Transmitter == "A69-9007-13250")
LC.positions.a50 <- LC.positions.a50 %>% dplyr::group_by(Transmitter)
a50_LC <- LC.positions.a50[!is.na(LC.positions.a50$Longitude) & !is.na(LC.positions.a50$Latitude),]
a50_LC.sp <- a50_LC[, c("Transmitter", "Longitude", "Latitude")]
coordinates(a50_LC.sp) <- c("Longitude", "Latitude")
proj4string(a50_LC.sp) <- CRS("+init=epsg:4326")

kernel.a50.LC <- kernelUD(a50_LC.sp, h = "href")
LC.a50.poly <- getverticeshr(kernel.a50.LC, percent = 95)

LC.a50.sp <- fortify(LC.a50.poly)
LC.a50.df <- as.data.frame(LC.a50.poly)
LC.a50.df <- LC.a50.df %>% rename(area.after = area)

### 13251 ########################
LC.positions.a51 <- periods_Lingcod %>% filter(coarse.period == "After") %>% filter(Transmitter == "A69-9007-13251")
LC.positions.a51 <- LC.positions.a51 %>% dplyr::group_by(Transmitter)
a51_LC <- LC.positions.a51[!is.na(LC.positions.a51$Longitude) & !is.na(LC.positions.a51$Latitude),]
a51_LC.sp <- a51_LC[, c("Transmitter", "Longitude", "Latitude")]
coordinates(a51_LC.sp) <- c("Longitude", "Latitude")
proj4string(a51_LC.sp) <- CRS("+init=epsg:4326")

kernel.a51.LC <- kernelUD(a51_LC.sp, h = "href")
LC.a51.poly <- getverticeshr(kernel.a51.LC, percent = 95)

LC.a51.sp <- fortify(LC.a51.poly)
LC.a51.df <- as.data.frame(LC.a51.poly)
LC.a51.df <- LC.a51.df %>% rename(area.after = area)

### 13252 #######################
LC.positions.a52 <- periods_Lingcod %>% filter(coarse.period == "After") %>% filter(Transmitter == "A69-9007-13252")
LC.positions.a52 <- LC.positions.a52 %>% dplyr::group_by(Transmitter)
a52_LC <- LC.positions.a52[!is.na(LC.positions.a52$Longitude) & !is.na(LC.positions.a52$Latitude),]
a52_LC.sp <- a52_LC[, c("Transmitter", "Longitude", "Latitude")]
coordinates(a52_LC.sp) <- c("Longitude", "Latitude")
proj4string(a52_LC.sp) <- CRS("+init=epsg:4326")

kernel.a52.LC <- kernelUD(a52_LC.sp, h = "href")
LC.a52.poly <- getverticeshr(kernel.a52.LC, percent = 95)

LC.a52.sp <- fortify(LC.a52.poly)
LC.a52.df <- as.data.frame(LC.a52.poly)
LC.a52.df <- LC.a52.df %>% rename(area.after = area)

### 13253
LC.positions.a53 <- periods_Lingcod %>% filter(coarse.period == "After") %>% filter(Transmitter == "A69-9007-13253")
LC.positions.a53 <- LC.positions.a53 %>% dplyr::group_by(Transmitter)
a53_LC <- LC.positions.a53[!is.na(LC.positions.a53$Longitude) & !is.na(LC.positions.a53$Latitude),]
a53_LC.sp <- a53_LC[, c("Transmitter", "Longitude", "Latitude")]
coordinates(a53_LC.sp) <- c("Longitude", "Latitude")
proj4string(a53_LC.sp) <- CRS("+init=epsg:4326")

kernel.a53.LC <- kernelUD(a53_LC.sp, h = "href")
LC.a53.poly <- getverticeshr(kernel.a53.LC, percent = 95)

LC.a53.sp <- fortify(LC.a53.poly)
LC.a53.df <- as.data.frame(LC.a53.poly)
LC.a53.df <- LC.a53.df %>% rename(area.after = area)


### 13254 
LC.positions.a54 <- periods_Lingcod %>% filter(coarse.period == "After") %>% filter(Transmitter == "A69-9007-13254")
LC.positions.a54 <- LC.positions.a54 %>% dplyr::group_by(Transmitter)
a54_LC <- LC.positions.a54[!is.na(LC.positions.a54$Longitude) & !is.na(LC.positions.a54$Latitude),]
a54_LC.sp <- a54_LC[, c("Transmitter", "Longitude", "Latitude")]
coordinates(a54_LC.sp) <- c("Longitude", "Latitude")
proj4string(a54_LC.sp) <- CRS("+init=epsg:4326")

kernel.a54.LC <- kernelUD(a54_LC.sp, h = "href")
LC.a54.poly <- getverticeshr(kernel.a54.LC, percent = 95)

LC.a54.sp <- fortify(LC.a54.poly)
LC.a54.df <- as.data.frame(LC.a54.poly)
LC.a54.df <- LC.a54.df %>% rename(area.after = area)

### 13255 ##########################
LC.positions.a55 <- periods_Lingcod %>% filter(coarse.period == "After") %>% filter(Transmitter == "A69-9007-13255")
LC.positions.a55 <- LC.positions.a55 %>% dplyr::group_by(Transmitter)
a55_LC <- LC.positions.a55[!is.na(LC.positions.a55$Longitude) & !is.na(LC.positions.a55$Latitude),]
a55_LC.sp <- a55_LC[, c("Transmitter", "Longitude", "Latitude")]
coordinates(a55_LC.sp) <- c("Longitude", "Latitude")
proj4string(a55_LC.sp) <- CRS("+init=epsg:4326")

kernel.a55.LC <- kernelUD(a55_LC.sp, h = "href")
LC.a55.poly <- getverticeshr(kernel.a55.LC, percent = 95)

LC.a55.sp <- fortify(LC.a55.poly)
LC.a55.df <- as.data.frame(LC.a55.poly)
LC.a55.df <- LC.a55.df %>% rename(area.after = area)

### 13256
LC.positions.a56 <- periods_Lingcod %>% filter(coarse.period == "After") %>% filter(Transmitter == "A69-9007-13256")
LC.positions.a56 <- LC.positions.a56 %>% dplyr::group_by(Transmitter)
a56_LC <- LC.positions.a56[!is.na(LC.positions.a56$Longitude) & !is.na(LC.positions.a56$Latitude),]
a56_LC.sp <- a56_LC[, c("Transmitter", "Longitude", "Latitude")]
coordinates(a56_LC.sp) <- c("Longitude", "Latitude")
proj4string(a56_LC.sp) <- CRS("+init=epsg:4326")

kernel.a56.LC <- kernelUD(a56_LC.sp, h = "href")
LC.a56.poly <- getverticeshr(kernel.a56.LC, percent = 95)

LC.a56.sp <- fortify(LC.a56.poly)
LC.a56.df <- as.data.frame(LC.a56.poly)
LC.a56.df <- LC.a56.df %>% rename(area.after = area)

### 13257 ##################################
LC.positions.a57 <- periods_Lingcod %>% filter(coarse.period == "After") %>% filter(Transmitter == "A69-9007-13257")
LC.positions.a57 <- LC.positions.a57 %>% dplyr::group_by(Transmitter)
a57_LC <- LC.positions.a57[!is.na(LC.positions.a57$Longitude) & !is.na(LC.positions.a57$Latitude),]
a57_LC.sp <- a57_LC[, c("Transmitter", "Longitude", "Latitude")]
coordinates(a57_LC.sp) <- c("Longitude", "Latitude")
proj4string(a57_LC.sp) <- CRS("+init=epsg:4326")

kernel.a57.LC <- kernelUD(a57_LC.sp, h = "href")
LC.a57.poly <- getverticeshr(kernel.a57.LC, percent = 95)

LC.a57.sp <- fortify(LC.a57.poly)
LC.a57.df <- as.data.frame(LC.a57.poly)
LC.a57.df <- LC.a57.df %>% rename(area.after = area)

### 13258
LC.positions.a58 <- periods_Lingcod %>% filter(coarse.period == "After") %>% filter(Transmitter == "A69-9007-13258")
LC.positions.a58 <- LC.positions.a58 %>% dplyr::group_by(Transmitter)
a58_LC <- LC.positions.a58[!is.na(LC.positions.a58$Longitude) & !is.na(LC.positions.a58$Latitude),]
a58_LC.sp <- a58_LC[, c("Transmitter", "Longitude", "Latitude")]
coordinates(a58_LC.sp) <- c("Longitude", "Latitude")
proj4string(a58_LC.sp) <- CRS("+init=epsg:4326")

kernel.a58.LC <- kernelUD(a58_LC.sp, h = "href")
LC.a58.poly <- getverticeshr(kernel.a58.LC, percent = 95)

LC.a58.sp <- fortify(LC.a58.poly)
LC.a58.df <- as.data.frame(LC.a58.poly)
LC.a58.df <- LC.a58.df %>% rename(area.after = area)

### 13259 ###########################
LC.positions.a59 <- periods_Lingcod %>% filter(coarse.period == "After") %>% filter(Transmitter == "A69-9007-13259")
LC.positions.a59 <- LC.positions.a59 %>% dplyr::group_by(Transmitter)
a59_LC <- LC.positions.a59[!is.na(LC.positions.a59$Longitude) & !is.na(LC.positions.a59$Latitude),]
a59_LC.sp <- a59_LC[, c("Transmitter", "Longitude", "Latitude")]
coordinates(a59_LC.sp) <- c("Longitude", "Latitude")
proj4string(a59_LC.sp) <- CRS("+init=epsg:4326")

kernel.a59.LC <- kernelUD(a59_LC.sp, h = "href")
LC.a59.poly <- getverticeshr(kernel.a59.LC, percent = 95)

LC.a59.sp <- fortify(LC.a59.poly)
LC.a59.df <- as.data.frame(LC.a59.poly)
LC.a59.df <- LC.a59.df %>% rename(area.after = area)

### 13260 #######################
LC.positions.a60 <- periods_Lingcod %>% filter(coarse.period == "After") %>% filter(Transmitter == "A69-9007-13260")
LC.positions.a60 <- LC.positions.a60 %>% dplyr::group_by(Transmitter)
a60_LC <- LC.positions.a60[!is.na(LC.positions.a60$Longitude) & !is.na(LC.positions.a60$Latitude),]
a60_LC.sp <- a60_LC[, c("Transmitter", "Longitude", "Latitude")]
coordinates(a60_LC.sp) <- c("Longitude", "Latitude")
proj4string(a60_LC.sp) <- CRS("+init=epsg:4326")

kernel.a60.LC <- kernelUD(a60_LC.sp, h = "href")
LC.a60.poly <- getverticeshr(kernel.a60.LC, percent = 95)

LC.a60.sp <- fortify(LC.a60.poly)
LC.a60.df <- as.data.frame(LC.a60.poly)
LC.a60.df <- LC.a60.df %>% rename(area.after = area)


### 13261
LC.positions.a61 <- periods_Lingcod %>% filter(coarse.period == "After") %>% filter(Transmitter == "A69-9007-13261")
LC.positions.a61 <- LC.positions.a61 %>% dplyr::group_by(Transmitter)
a61_LC <- LC.positions.a61[!is.na(LC.positions.a61$Longitude) & !is.na(LC.positions.a61$Latitude),]
a61_LC.sp <- a61_LC[, c("Transmitter", "Longitude", "Latitude")]
coordinates(a61_LC.sp) <- c("Longitude", "Latitude")
proj4string(a61_LC.sp) <- CRS("+init=epsg:4326")

kernel.a61.LC <- kernelUD(a61_LC.sp, h = "href")
LC.a61.poly <- getverticeshr(kernel.a61.LC, percent = 95)

LC.a61.sp <- fortify(LC.a61.poly)
LC.a61.df <- as.data.frame(LC.a61.poly)
LC.a61.df <- LC.a61.df %>% rename(area.after = area)

### 13262 ##########################
LC.positions.a62 <- periods_Lingcod %>% filter(coarse.period == "After") %>% filter(Transmitter == "A69-9007-13262")
LC.positions.a62 <- LC.positions.a62 %>% dplyr::group_by(Transmitter)
a62_LC <- LC.positions.a62[!is.na(LC.positions.a62$Longitude) & !is.na(LC.positions.a62$Latitude),]
a62_LC.sp <- a62_LC[, c("Transmitter", "Longitude", "Latitude")]
coordinates(a62_LC.sp) <- c("Longitude", "Latitude")
proj4string(a62_LC.sp) <- CRS("+init=epsg:4326")

kernel.a62.LC <- kernelUD(a62_LC.sp, h = "href")
LC.a62.poly <- getverticeshr(kernel.a62.LC, percent = 95)

LC.a62.sp <- fortify(LC.a62.poly)
LC.a62.df <- as.data.frame(LC.a62.poly)
LC.a62.df <- LC.a62.df %>% rename(area.after = area)

### 13263
LC.positions.a63 <- periods_Lingcod %>% filter(coarse.period == "After") %>% filter(Transmitter == "A69-9007-13249")
LC.positions.a63 <- LC.positions.a63 %>% dplyr::group_by(Transmitter)
a63_LC <- LC.positions.a63[!is.na(LC.positions.a63$Longitude) & !is.na(LC.positions.a63$Latitude),]
a63_LC.sp <- a63_LC[, c("Transmitter", "Longitude", "Latitude")]
coordinates(a63_LC.sp) <- c("Longitude", "Latitude")
proj4string(a63_LC.sp) <- CRS("+init=epsg:4326")

kernel.a63.LC <- kernelUD(a63_LC.sp, h = "href")
LC.a63.poly <- getverticeshr(kernel.a63.LC, percent = 95)

LC.a63.sp <- fortify(LC.a63.poly)
LC.a63.df <- as.data.frame(LC.a63.poly)
LC.a63.df <- LC.a63.df %>% rename(area.after = area)


Lingcod.after.df <- bind_rows(LC.a49.df, LC.a53.df, LC.a54.df, 
                               LC.a56.df, LC.a58.df, LC.a61.df,
                               LC.a63.df)

##All together now!
###dataframe of areas of individuals by three periods
Lingcod.all.df <- Lingcod.before.df %>% 
  full_join(Lingcod.dur.df, by='id') %>% 
  full_join(Lingcod.after.df, by='id')

###repeated measures anova
coarse.period.lvl <- c("area.before", "area.dur", "area.after")
LC_ANOVA <- Lingcod.all.df %>%
  gather(key = "period", value = "area", area.before, area.dur, area.after) %>%
  convert_as_factor(id, period)
LC_ANOVA$period <- factor(LC_ANOVA$period, levels = coarse.period.lvl[c(1:3)])

LC_ANOVA %>%
  group_by(period) %>%
  get_summary_stats(area, type = "mean_sd")

bxp <- ggplot(LC_ANOVA, aes(x = period, y = area), add = "point") + geom_boxplot()
bxp


#gotta remove NAs for the repeated measures ANOVA
lc.aov <- anova_test(data = LC_ANOVA, dv = area, wid = id, within = period)
get_anova_table(lc.aov)

pwc <- LC_ANOVA %>%
  pairwise_t_test(
    area ~ period, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
pwc

#regular ole ANOVA
summary(aov(area ~ period, data = LC_ANOVA))


## During
LC.positions.dur <- periods_Lingcod %>% filter(coarse.period == "During")
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
LC.positions.after <- periods_Lingcod %>% filter(coarse.period == "After")
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
