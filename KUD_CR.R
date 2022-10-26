
## Before
#China rockfish

##66 #####################
CR.positions.b66 <- ChinaR_accel %>% filter(periods == "Before")%>% filter(Transmitter == "A69-9007-13266")
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
CR.positions.b67 <- ChinaR_accel %>% filter(periods == "Before")%>% filter(Transmitter == "A69-9007-13267")
CR.positions.b67 <- CR.positions.b67 %>% dplyr::group_by(Transmitter)
b67_ChinaR <- CR.positions.b67[!is.na(CR.positions.b67$Longitude) & !is.na(CR.positions.b67$Latitude),]
b67_ChinaR.sp <- b67_ChinaR[, c("Transmitter", "Longitude", "Latitude")]
coordinates(b67_ChinaR.sp) <- c("Longitude", "Latitude")
proj4string(b67_ChinaR.sp) <- CRS("+init=epsg:4326")

kernel.b67.ChinaR <- kernelUD(b67_ChinaR.sp, h = "href")
ChinaR.b67.poly <- getverticeshr(kernel.b67.ChinaR, percent = 95)

ChinaR.b67.sp <- fortify(ChinaR.b67.poly)
ChinaR.b67.df <- as.data.frame(ChinaR.b67.poly)


##72
CR.positions.b72 <- ChinaR_accel %>% filter(periods == "Before")%>% filter(Transmitter == "A69-9007-13272")
CR.positions.b72 <- CR.positions.b72 %>% dplyr::group_by(Transmitter)
b72_ChinaR <- CR.positions.b72[!is.na(CR.positions.b72$Longitude) & !is.na(CR.positions.b72$Latitude),]
b72_ChinaR.sp <- b72_ChinaR[, c("Transmitter", "Longitude", "Latitude")]
coordinates(b72_ChinaR.sp) <- c("Longitude", "Latitude")
proj4string(b72_ChinaR.sp) <- CRS("+init=epsg:4326")

kernel.b72.ChinaR <- kernelUD(b72_ChinaR.sp, h = "href")
ChinaR.b72.poly <- getverticeshr(kernel.b72.ChinaR, percent = 95)

ChinaR.b72.sp <- fortify(ChinaR.b72.poly)
ChinaR.b72.df <- as.data.frame(ChinaR.b72.poly)


##75 ###################################
CR.positions.b75 <- ChinaR_accel %>% filter(periods == "Before")%>% filter(Transmitter == "A69-9007-13275")
CR.positions.b75 <- CR.positions.b75 %>% dplyr::group_by(Transmitter)
b75_ChinaR <- CR.positions.b75[!is.na(CR.positions.b75$Longitude) & !is.na(CR.positions.b75$Latitude),]
b75_ChinaR.sp <- b75_ChinaR[, c("Transmitter", "Longitude", "Latitude")]
coordinates(b75_ChinaR.sp) <- c("Longitude", "Latitude")
proj4string(b75_ChinaR.sp) <- CRS("+init=epsg:4326")

kernel.b75.ChinaR <- kernelUD(b75_ChinaR.sp, h = "href")
ChinaR.b75.poly <- getverticeshr(kernel.b75.ChinaR, percent = 95)

ChinaR.b75.sp <- fortify(ChinaR.b75.poly)
ChinaR.b75.df <- as.data.frame(ChinaR.b75.poly)


##77
CR.positions.b77 <- ChinaR_accel %>% filter(periods == "Before")%>% filter(Transmitter == "A69-9007-13277")
CR.positions.b77 <- CR.positions.b77 %>% dplyr::group_by(Transmitter)
b77_ChinaR <- CR.positions.b77[!is.na(CR.positions.b77$Longitude) & !is.na(CR.positions.b77$Latitude),]
b77_ChinaR.sp <- b77_ChinaR[, c("Transmitter", "Longitude", "Latitude")]
coordinates(b77_ChinaR.sp) <- c("Longitude", "Latitude")
proj4string(b77_ChinaR.sp) <- CRS("+init=epsg:4326")

kernel.b77.ChinaR <- kernelUD(b77_ChinaR.sp, h = "href")
ChinaR.b77.poly <- getverticeshr(kernel.b77.ChinaR, percent = 95)

ChinaR.b77.sp <- fortify(ChinaR.b77.poly)
ChinaR.b77.df <- as.data.frame(ChinaR.b77.poly)

##78
CR.positions.b78 <- ChinaR_accel %>% filter(periods == "Before")%>% filter(Transmitter == "A69-9007-13278")
CR.positions.b78 <- CR.positions.b78 %>% dplyr::group_by(Transmitter)
b78_ChinaR <- CR.positions.b78[!is.na(CR.positions.b78$Longitude) & !is.na(CR.positions.b78$Latitude),]
b78_ChinaR.sp <- b78_ChinaR[, c("Transmitter", "Longitude", "Latitude")]
coordinates(b78_ChinaR.sp) <- c("Longitude", "Latitude")
proj4string(b78_ChinaR.sp) <- CRS("+init=epsg:4326")

kernel.b78.ChinaR <- kernelUD(b78_ChinaR.sp, h = "href")
ChinaR.b78.poly <- getverticeshr(kernel.b78.ChinaR, percent = 95)

ChinaR.b78.sp <- fortify(ChinaR.b78.poly)
ChinaR.b78.df <- as.data.frame(ChinaR.b78.poly)



ChinaR.before.df <- bind_rows(ChinaR.b67.df, ChinaR.b72.df, 
                              ChinaR.b77.df,
                              ChinaR.b78.df)
ChinaR.before.df <- ChinaR.before.df %>% rename(area.before = area)


## During
### 67
CR.positions.d67 <- ChinaR_accel %>% filter(periods == "During")%>% filter(Transmitter == "A69-9007-13267")
CR.positions.d67 <- CR.positions.d67 %>% dplyr::group_by(Transmitter)
d67_ChinaR <- CR.positions.d67[!is.na(CR.positions.d67$Longitude) & !is.na(CR.positions.d67$Latitude),]
d67_ChinaR.sp <- d67_ChinaR[, c("Transmitter", "Longitude", "Latitude")]
coordinates(d67_ChinaR.sp) <- c("Longitude", "Latitude")
proj4string(d67_ChinaR.sp) <- CRS("+init=epsg:4326")

kernel.d67.ChinaR <- kernelUD(d67_ChinaR.sp, h = "href")
ChinaR.d67.poly <- getverticeshr(kernel.d67.ChinaR, percent = 95)

ChinaR.d67.sp <- fortify(ChinaR.d67.poly)
ChinaR.d67.df <- as.data.frame(ChinaR.d67.poly)


#69
CR.positions.d69 <- ChinaR_accel %>% filter(periods == "During")%>% filter(Transmitter == "A69-9007-13269")
CR.positions.d69 <- CR.positions.d69 %>% dplyr::group_by(Transmitter)
d69_ChinaR <- CR.positions.d69[!is.na(CR.positions.d69$Longitude) & !is.na(CR.positions.d69$Latitude),]
d69_ChinaR.sp <- d69_ChinaR[, c("Transmitter", "Longitude", "Latitude")]
coordinates(d69_ChinaR.sp) <- c("Longitude", "Latitude")
proj4string(d69_ChinaR.sp) <- CRS("+init=epsg:4326")

kernel.d69.ChinaR <- kernelUD(d69_ChinaR.sp, h = "href")
ChinaR.d69.poly <- getverticeshr(kernel.d69.ChinaR, percent = 95)

ChinaR.d69.sp <- fortify(ChinaR.d69.poly)
ChinaR.d69.df <- as.data.frame(ChinaR.d69.poly)

##72
CR.positions.d72 <- ChinaR_accel %>% filter(periods == "During")%>% filter(Transmitter == "A69-9007-13272")
CR.positions.d72 <- CR.positions.d72 %>% dplyr::group_by(Transmitter)
d72_ChinaR <- CR.positions.d72[!is.na(CR.positions.d72$Longitude) & !is.na(CR.positions.d72$Latitude),]
d72_ChinaR.sp <- d72_ChinaR[, c("Transmitter", "Longitude", "Latitude")]
coordinates(d72_ChinaR.sp) <- c("Longitude", "Latitude")
proj4string(d72_ChinaR.sp) <- CRS("+init=epsg:4326")

kernel.d72.ChinaR <- kernelUD(d72_ChinaR.sp, h = "href")
ChinaR.d72.poly <- getverticeshr(kernel.d72.ChinaR, percent = 95)

ChinaR.d72.sp <- fortify(ChinaR.d72.poly)
ChinaR.d72.df <- as.data.frame(ChinaR.d72.poly)

##73 ##################
CR.positions.d73 <- ChinaR_accel %>% filter(periods == "During")%>% filter(Transmitter == "A69-9007-13273")
CR.positions.d73 <- CR.positions.d73 %>% dplyr::group_by(Transmitter)
d73_ChinaR <- CR.positions.d73[!is.na(CR.positions.d73$Longitude) & !is.na(CR.positions.d73$Latitude),]
d73_ChinaR.sp <- d73_ChinaR[, c("Transmitter", "Longitude", "Latitude")]
coordinates(d73_ChinaR.sp) <- c("Longitude", "Latitude")
proj4string(d73_ChinaR.sp) <- CRS("+init=epsg:4326")

kernel.d73.ChinaR <- kernelUD(d73_ChinaR.sp, h = "href")
ChinaR.d73.poly <- getverticeshr(kernel.d73.ChinaR, percent = 95)

ChinaR.d73.sp <- fortify(ChinaR.d73.poly)
ChinaR.d73.df <- as.data.frame(ChinaR.d73.poly)

##74 ##################33
CR.positions.d74 <- ChinaR_accel %>% filter(periods == "During")%>% filter(Transmitter == "A69-9007-13274")
CR.positions.d74 <- CR.positions.d74 %>% dplyr::group_by(Transmitter)
d74_ChinaR <- CR.positions.d74[!is.na(CR.positions.d74$Longitude) & !is.na(CR.positions.d74$Latitude),]
d74_ChinaR.sp <- d74_ChinaR[, c("Transmitter", "Longitude", "Latitude")]
coordinates(d74_ChinaR.sp) <- c("Longitude", "Latitude")
proj4string(d74_ChinaR.sp) <- CRS("+init=epsg:4326")

kernel.d74.ChinaR <- kernelUD(d74_ChinaR.sp, h = "href")
ChinaR.d74.poly <- getverticeshr(kernel.d74.ChinaR, percent = 95)

ChinaR.d74.sp <- fortify(ChinaR.d74.poly)
ChinaR.d74.df <- as.data.frame(ChinaR.d74.poly)

##75 ####################
CR.positions.d75 <- ChinaR_accel %>% filter(periods == "During")%>% filter(Transmitter == "A69-9007-13275")
CR.positions.d75 <- CR.positions.d75 %>% dplyr::group_by(Transmitter)
d75_ChinaR <- CR.positions.d75[!is.na(CR.positions.d75$Longitude) & !is.na(CR.positions.d75$Latitude),]
d75_ChinaR.sp <- d75_ChinaR[, c("Transmitter", "Longitude", "Latitude")]
coordinates(d75_ChinaR.sp) <- c("Longitude", "Latitude")
proj4string(d75_ChinaR.sp) <- CRS("+init=epsg:4326")

kernel.d75.ChinaR <- kernelUD(d75_ChinaR.sp, h = "href")
ChinaR.d75.poly <- getverticeshr(kernel.d75.ChinaR, percent = 95)

ChinaR.d75.sp <- fortify(ChinaR.d75.poly)
ChinaR.d75.df <- as.data.frame(ChinaR.d75.poly)

##77
CR.positions.d77 <- ChinaR_accel %>% filter(periods == "During")%>% filter(Transmitter == "A69-9007-13277")
CR.positions.d77 <- CR.positions.d77 %>% dplyr::group_by(Transmitter)
d77_ChinaR <- CR.positions.d77[!is.na(CR.positions.d77$Longitude) & !is.na(CR.positions.d77$Latitude),]
d77_ChinaR.sp <- d77_ChinaR[, c("Transmitter", "Longitude", "Latitude")]
coordinates(d77_ChinaR.sp) <- c("Longitude", "Latitude")
proj4string(d77_ChinaR.sp) <- CRS("+init=epsg:4326")

kernel.d77.ChinaR <- kernelUD(d77_ChinaR.sp, h = "href")
ChinaR.d77.poly <- getverticeshr(kernel.d77.ChinaR, percent = 95)

ChinaR.d77.sp <- fortify(ChinaR.d77.poly)
ChinaR.d77.df <- as.data.frame(ChinaR.d77.poly)

##78
CR.positions.d78 <- ChinaR_accel %>% filter(periods == "During")%>% filter(Transmitter == "A69-9007-13278")
CR.positions.d78 <- CR.positions.d78 %>% dplyr::group_by(Transmitter)
d78_ChinaR <- CR.positions.d78[!is.na(CR.positions.d78$Longitude) & !is.na(CR.positions.d78$Latitude),]
d78_ChinaR.sp <- d78_ChinaR[, c("Transmitter", "Longitude", "Latitude")]
coordinates(d78_ChinaR.sp) <- c("Longitude", "Latitude")
proj4string(d78_ChinaR.sp) <- CRS("+init=epsg:4326")

kernel.d78.ChinaR <- kernelUD(d78_ChinaR.sp, h = "href")
ChinaR.d78.poly <- getverticeshr(kernel.d78.ChinaR, percent = 95)

ChinaR.d78.sp <- fortify(ChinaR.d78.poly)
ChinaR.d78.df <- as.data.frame(ChinaR.d78.poly)

ChinaR.dur.df <- bind_rows(ChinaR.d67.df, ChinaR.d72.df, 
                           ChinaR.d77.df, ChinaR.d78.df,
                              ChinaR.d69.df)
ChinaR.dur.df <- ChinaR.dur.df %>% rename(area.dur = area)



## After
### 66 #####################
CR.positions.a66 <- ChinaR_accel %>% filter(periods == "After")%>% filter(Transmitter == "A69-9007-13266")
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
CR.positions.a67 <- ChinaR_accel %>% filter(periods == "After")%>% filter(Transmitter == "A69-9007-13267")
CR.positions.a67 <- CR.positions.a67 %>% dplyr::group_by(Transmitter)
a67_ChinaR <- CR.positions.a67[!is.na(CR.positions.a67$Longitude) & !is.na(CR.positions.a67$Latitude),]
a67_ChinaR.sp <- a67_ChinaR[, c("Transmitter", "Longitude", "Latitude")]
coordinates(a67_ChinaR.sp) <- c("Longitude", "Latitude")
proj4string(a67_ChinaR.sp) <- CRS("+init=epsg:4326")

kernel.a67.ChinaR <- kernelUD(a67_ChinaR.sp, h = "href")
ChinaR.a67.poly <- getverticeshr(kernel.a67.ChinaR, percent = 95)

ChinaR.a67.sp <- fortify(ChinaR.a67.poly)
ChinaR.a67.df <- as.data.frame(ChinaR.a67.poly)

### 69
CR.positions.a69 <- ChinaR_accel %>% filter(periods == "After")%>% filter(Transmitter == "A69-9007-13269")
CR.positions.a69 <- CR.positions.a69 %>% dplyr::group_by(Transmitter)
a69_ChinaR <- CR.positions.a69[!is.na(CR.positions.a69$Longitude) & !is.na(CR.positions.a69$Latitude),]
a69_ChinaR.sp <- a69_ChinaR[, c("Transmitter", "Longitude", "Latitude")]
coordinates(a69_ChinaR.sp) <- c("Longitude", "Latitude")
proj4string(a69_ChinaR.sp) <- CRS("+init=epsg:4326")

kernel.a69.ChinaR <- kernelUD(a69_ChinaR.sp, h = "href")
ChinaR.a69.poly <- getverticeshr(kernel.a69.ChinaR, percent = 95)

ChinaR.a69.sp <- fortify(ChinaR.a69.poly)
ChinaR.a69.df <- as.data.frame(ChinaR.a69.poly)


##72
CR.positions.a72 <- ChinaR_accel %>% filter(periods == "After")%>% filter(Transmitter == "A69-9007-13272")
CR.positions.a72 <- CR.positions.a72 %>% dplyr::group_by(Transmitter)
a72_ChinaR <- CR.positions.a72[!is.na(CR.positions.a72$Longitude) & !is.na(CR.positions.a72$Latitude),]
a72_ChinaR.sp <- a72_ChinaR[, c("Transmitter", "Longitude", "Latitude")]
coordinates(a72_ChinaR.sp) <- c("Longitude", "Latitude")
proj4string(a72_ChinaR.sp) <- CRS("+init=epsg:4326")

kernel.a72.ChinaR <- kernelUD(a72_ChinaR.sp, h = "href")
ChinaR.a72.poly <- getverticeshr(kernel.a72.ChinaR, percent = 95)

ChinaR.a72.sp <- fortify(ChinaR.a72.poly)
ChinaR.a72.df <- as.data.frame(ChinaR.a72.poly)

##73 ########################
CR.positions.a73 <- ChinaR_accel %>% filter(periods == "After")%>% filter(Transmitter == "A69-9007-13273")
CR.positions.a73 <- CR.positions.a73 %>% dplyr::group_by(Transmitter)
a73_ChinaR <- CR.positions.a73[!is.na(CR.positions.a73$Longitude) & !is.na(CR.positions.a73$Latitude),]
a73_ChinaR.sp <- a73_ChinaR[, c("Transmitter", "Longitude", "Latitude")]
coordinates(a73_ChinaR.sp) <- c("Longitude", "Latitude")
proj4string(a73_ChinaR.sp) <- CRS("+init=epsg:4326")

kernel.a73.ChinaR <- kernelUD(a73_ChinaR.sp, h = "href")
ChinaR.a73.poly <- getverticeshr(kernel.a73.ChinaR, percent = 95)

ChinaR.a73.sp <- fortify(ChinaR.a73.poly)
ChinaR.a73.df <- as.data.frame(ChinaR.a73.poly)

##74 #####################################
CR.positions.a74 <- ChinaR_accel %>% filter(periods == "After")%>% filter(Transmitter == "A69-9007-13274")
CR.positions.a74 <- CR.positions.a74 %>% dplyr::group_by(Transmitter)
a74_ChinaR <- CR.positions.a74[!is.na(CR.positions.a74$Longitude) & !is.na(CR.positions.a74$Latitude),]
a74_ChinaR.sp <- a74_ChinaR[, c("Transmitter", "Longitude", "Latitude")]
coordinates(a74_ChinaR.sp) <- c("Longitude", "Latitude")
proj4string(a74_ChinaR.sp) <- CRS("+init=epsg:4326")

kernel.a74.ChinaR <- kernelUD(a74_ChinaR.sp, h = "href")
ChinaR.a74.poly <- getverticeshr(kernel.a74.ChinaR, percent = 95)

ChinaR.a74.sp <- fortify(ChinaR.a74.poly)
ChinaR.a74.df <- as.data.frame(ChinaR.a74.poly)

##75
CR.positions.a75 <- ChinaR_accel %>% filter(periods == "After")%>% filter(Transmitter == "A69-9007-13275")
CR.positions.a75 <- CR.positions.a75 %>% dplyr::group_by(Transmitter)
a75_ChinaR <- CR.positions.a75[!is.na(CR.positions.a75$Longitude) & !is.na(CR.positions.a75$Latitude),]
a75_ChinaR.sp <- a75_ChinaR[, c("Transmitter", "Longitude", "Latitude")]
coordinates(a75_ChinaR.sp) <- c("Longitude", "Latitude")
proj4string(a75_ChinaR.sp) <- CRS("+init=epsg:4326")

kernel.a75.ChinaR <- kernelUD(a75_ChinaR.sp, h = "href")
ChinaR.a75.poly <- getverticeshr(kernel.a75.ChinaR, percent = 95)

ChinaR.a75.sp <- fortify(ChinaR.a75.poly)
ChinaR.a75.df <- as.data.frame(ChinaR.a75.poly)

##77
CR.positions.a77 <- ChinaR_accel %>% filter(periods == "After")%>% filter(Transmitter == "A69-9007-13277")
CR.positions.a77 <- CR.positions.a77 %>% dplyr::group_by(Transmitter)
a77_ChinaR <- CR.positions.a77[!is.na(CR.positions.a77$Longitude) & !is.na(CR.positions.a77$Latitude),]
a77_ChinaR.sp <- a77_ChinaR[, c("Transmitter", "Longitude", "Latitude")]
coordinates(a77_ChinaR.sp) <- c("Longitude", "Latitude")
proj4string(a77_ChinaR.sp) <- CRS("+init=epsg:4326")

kernel.a77.ChinaR <- kernelUD(a77_ChinaR.sp, h = "href")
ChinaR.a77.poly <- getverticeshr(kernel.a77.ChinaR, percent = 95)

ChinaR.a77.sp <- fortify(ChinaR.a77.poly)
ChinaR.a77.df <- as.data.frame(ChinaR.a77.poly)

##78
CR.positions.a78 <- ChinaR_accel %>% filter(periods == "After")%>% filter(Transmitter == "A69-9007-13278")
CR.positions.a78 <- CR.positions.a78 %>% dplyr::group_by(Transmitter)
a78_ChinaR <- CR.positions.a78[!is.na(CR.positions.a78$Longitude) & !is.na(CR.positions.a78$Latitude),]
a78_ChinaR.sp <- a78_ChinaR[, c("Transmitter", "Longitude", "Latitude")]
coordinates(a78_ChinaR.sp) <- c("Longitude", "Latitude")
proj4string(a78_ChinaR.sp) <- CRS("+init=epsg:4326")

kernel.a78.ChinaR <- kernelUD(a78_ChinaR.sp, h = "href")
ChinaR.a78.poly <- getverticeshr(kernel.a78.ChinaR, percent = 95)

ChinaR.a78.sp <- fortify(ChinaR.a78.poly)
ChinaR.a78.df <- as.data.frame(ChinaR.a78.poly)

ChinaR.after.df <- bind_rows(ChinaR.a67.df, ChinaR.a72.df,
                           ChinaR.a75.df, ChinaR.a77.df, ChinaR.a78.df,
                           ChinaR.a69.df)
ChinaR.after.df <- ChinaR.after.df %>% rename(area.after = area)


#dataframe of areas of individuals by three periods
ChinaR.all.df <- ChinaR.before.df %>% 
  inner_join(ChinaR.dur.df, by='id') %>% 
  inner_join(ChinaR.after.df, by='id')

#repeated measures anova
CR_ANOVA <- ChinaR.all.df %>%
  gather(key = "period", value = "area", area.before, area.dur, area.after) %>%
  convert_as_factor(id, period)
coarse.period.lvl <- c("area.before", "area.dur", "area.after")
#head(BR_ANOVA, 3)

CR_ANOVA %>%
  group_by(period) %>%
  get_summary_stats(area, type = "mean_sd")

bxp <- ggplot(CR_ANOVA, aes(x = period, y = area), add = "point") + geom_boxplot()
bxp

CR_ANOVA$period <- factor(CR_ANOVA$period, levels = coarse.period.lvl[c(1:3)])

cr.aov <- anova_test(data = CR_ANOVA, dv = area, wid = id, within = period)
get_anova_table(cr.aov)

pwc <- CR_ANOVA %>%
  pairwise_t_test(
    area ~ period, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
pwc