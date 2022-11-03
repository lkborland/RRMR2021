### spatial UD for animals 

## Before
#Black rockfish - 12048
BR.positions.b48 <- BlackR_accel %>% filter(periods == "Before") %>% filter(Transmitter == "A69-9007-12048")
BR.positions.b48 <- BR.positions.b48 %>% dplyr::group_by(Transmitter)
kud48_BlackR <- BR.positions.b48[!is.na(BR.positions.b48$Longitude) & !is.na(BR.positions.b48$Latitude),]
kud48_BlackR <- kud48_BlackR[, c("Transmitter", "Longitude", "Latitude")]
coordinates(kud48_BlackR) <- c("Longitude", "Latitude")
proj4string(kud48_BlackR) <- CRS("+init=epsg:4326")

kernel.48.BlackR <- kernelUD(kud48_BlackR, h = "href")
BlackR48.poly <- getverticeshr(kernel.48.BlackR, percent = 95)

BlackR48.sp <- fortify(BlackR48.poly)
BlackR48.df <- as.data.frame(BlackR48.poly)

#g <- ggplot(BlackR76.sp, aes(x = long, y = lat, fill = id)) +
#  geom_polygon(alpha = .4) +
#  ggthemes::scale_fill_gdocs() +
#  coord_equal() +
#  theme_void()
#g

#Black rockfish - 12050
BR.positions.b50 <- BlackR_accel %>% filter(periods == "Before") %>% filter(Transmitter == "A69-9007-12050")
BR.positions.b50 <- BR.positions.b50 %>% dplyr::group_by(Transmitter)
kud50_BlackR <- BR.positions.b50[!is.na(BR.positions.b50$Longitude) & !is.na(BR.positions.b50$Latitude),]
kud50_BlackR <- kud50_BlackR[, c("Transmitter", "Longitude", "Latitude")]
coordinates(kud50_BlackR) <- c("Longitude", "Latitude")
proj4string(kud50_BlackR) <- CRS("+init=epsg:4326")

kernel.50.BlackR <- kernelUD(kud50_BlackR, h = "href")
BlackR50.poly <- getverticeshr(kernel.50.BlackR, percent = 95)

BlackR50.sp <- fortify(BlackR50.poly)
BlackR50.df <- as.data.frame(BlackR50.poly)

#Black rockfish - 12052
BR.positions.b52 <- BlackR_accel %>% filter(periods == "Before") %>% filter(Transmitter == "A69-9007-12052")
BR.positions.b52 <- BR.positions.b52 %>% dplyr::group_by(Transmitter)
kud52_BlackR <- BR.positions.b52[!is.na(BR.positions.b52$Longitude) & !is.na(BR.positions.b52$Latitude),]
kud52_BlackR <- kud52_BlackR[, c("Transmitter", "Longitude", "Latitude")]
coordinates(kud52_BlackR) <- c("Longitude", "Latitude")
proj4string(kud52_BlackR) <- CRS("+init=epsg:4326")

kernel.52.BlackR <- kernelUD(kud52_BlackR, h = "href")
BlackR52.poly <- getverticeshr(kernel.52.BlackR, percent = 95)

BlackR52.sp <- fortify(BlackR52.poly)
BlackR52.df <- as.data.frame(BlackR52.poly)

#Black rockfish - 12054
BR.positions.b54 <- BlackR_accel %>% filter(periods == "Before") %>% filter(Transmitter == "A69-9007-12054")
BR.positions.b54 <- BR.positions.b54 %>% dplyr::group_by(Transmitter)
kud54_BlackR <- BR.positions.b54[!is.na(BR.positions.b54$Longitude) & !is.na(BR.positions.b54$Latitude),]
kud54_BlackR <- kud54_BlackR[, c("Transmitter", "Longitude", "Latitude")]
coordinates(kud54_BlackR) <- c("Longitude", "Latitude")
proj4string(kud54_BlackR) <- CRS("+init=epsg:4326")

kernel.54.BlackR <- kernelUD(kud54_BlackR, h = "href")
BlackR54.poly <- getverticeshr(kernel.54.BlackR, percent = 95)

BlackR54.sp <- fortify(BlackR54.poly)
BlackR54.df <- as.data.frame(BlackR54.poly)

#Black rockfish - 12056########################################
BR.positions.b56 <- BlackR_accel %>% filter(periods == "Before") %>% filter(Transmitter == "A69-9007-12056")
BR.positions.b56 <- BR.positions.b56 %>% dplyr::group_by(Transmitter)
kud56_BlackR <- BR.positions.b56[!is.na(BR.positions.b56$Longitude) & !is.na(BR.positions.b56$Latitude),]
kud56_BlackR <- kud56_BlackR[, c("Transmitter", "Longitude", "Latitude")]
coordinates(kud56_BlackR) <- c("Longitude", "Latitude")
proj4string(kud56_BlackR) <- CRS("+init=epsg:4326")

kernel.56.BlackR <- kernelUD(kud56_BlackR, h = "href")
BlackR56.poly <- getverticeshr(kernel.56.BlackR, percent = 95)

BlackR56.sp <- fortify(BlackR56.poly)
BlackR56.df <- as.data.frame(BlackR56.poly)

#Black rockfish - 12058
BR.positions.b58 <- BlackR_accel %>% filter(periods == "Before") %>% filter(Transmitter == "A69-9007-12058")
BR.positions.b58 <- BR.positions.b58 %>% dplyr::group_by(Transmitter)
kud58_BlackR <- BR.positions.b58[!is.na(BR.positions.b58$Longitude) & !is.na(BR.positions.b58$Latitude),]
kud58_BlackR <- kud58_BlackR[, c("Transmitter", "Longitude", "Latitude")]
coordinates(kud58_BlackR) <- c("Longitude", "Latitude")
proj4string(kud58_BlackR) <- CRS("+init=epsg:4326")

kernel.58.BlackR <- kernelUD(kud58_BlackR, h = "href")
BlackR58.poly <- getverticeshr(kernel.58.BlackR, percent = 95)

BlackR58.sp <- fortify(BlackR58.poly)
BlackR58.df <- as.data.frame(BlackR58.poly)

#Black rockfish - 12060
BR.positions.b60 <- BlackR_accel %>% filter(periods == "Before") %>% filter(Transmitter == "A69-9007-12060")
BR.positions.b60 <- BR.positions.b60 %>% dplyr::group_by(Transmitter)
kud60_BlackR <- BR.positions.b60[!is.na(BR.positions.b60$Longitude) & !is.na(BR.positions.b60$Latitude),]
kud60_BlackR <- kud60_BlackR[, c("Transmitter", "Longitude", "Latitude")]
coordinates(kud60_BlackR) <- c("Longitude", "Latitude")
proj4string(kud60_BlackR) <- CRS("+init=epsg:4326")

kernel.60.BlackR <- kernelUD(kud60_BlackR, h = "href")
BlackR60.poly <- getverticeshr(kernel.60.BlackR, percent = 95)

BlackR60.sp <- fortify(BlackR60.poly)
BlackR60.df <- as.data.frame(BlackR60.poly)

#Black rockfish - 12062
BR.positions.b62 <- BlackR_accel %>% filter(periods == "Before") %>% filter(Transmitter == "A69-9007-12062")
BR.positions.b62 <- BR.positions.b62 %>% dplyr::group_by(Transmitter)
kud62_BlackR <- BR.positions.b62[!is.na(BR.positions.b62$Longitude) & !is.na(BR.positions.b62$Latitude),]
kud62_BlackR <- kud62_BlackR[, c("Transmitter", "Longitude", "Latitude")]
coordinates(kud62_BlackR) <- c("Longitude", "Latitude")
proj4string(kud62_BlackR) <- CRS("+init=epsg:4326")

kernel.62.BlackR <- kernelUD(kud62_BlackR, h = "href")
BlackR62.poly <- getverticeshr(kernel.62.BlackR, percent = 95)

BlackR62.sp <- fortify(BlackR62.poly)
BlackR62.df <- as.data.frame(BlackR62.poly)

#Black rockfish - 12064
BR.positions.b64 <- BlackR_accel %>% filter(periods == "Before") %>% filter(Transmitter == "A69-9007-12064")
BR.positions.b64 <- BR.positions.b64 %>% dplyr::group_by(Transmitter)
kud64_BlackR <- BR.positions.b64[!is.na(BR.positions.b64$Longitude) & !is.na(BR.positions.b64$Latitude),]
kud64_BlackR <- kud64_BlackR[, c("Transmitter", "Longitude", "Latitude")]
coordinates(kud64_BlackR) <- c("Longitude", "Latitude")
proj4string(kud64_BlackR) <- CRS("+init=epsg:4326")

kernel.64.BlackR <- kernelUD(kud64_BlackR, h = "href")
BlackR64.poly <- getverticeshr(kernel.64.BlackR, percent = 95)

BlackR64.sp <- fortify(BlackR64.poly)
BlackR64.df <- as.data.frame(BlackR64.poly)

#Black rockfish - 12066
BR.positions.b66 <- BlackR_accel %>% filter(periods == "Before") %>% filter(Transmitter == "A69-9007-12066")
BR.positions.b66 <- BR.positions.b66 %>% dplyr::group_by(Transmitter)
kud66_BlackR <- BR.positions.b66[!is.na(BR.positions.b66$Longitude) & !is.na(BR.positions.b66$Latitude),]
kud66_BlackR <- kud66_BlackR[, c("Transmitter", "Longitude", "Latitude")]
coordinates(kud66_BlackR) <- c("Longitude", "Latitude")
proj4string(kud66_BlackR) <- CRS("+init=epsg:4326")

kernel.66.BlackR <- kernelUD(kud66_BlackR, h = "href")
BlackR66.poly <- getverticeshr(kernel.66.BlackR, percent = 95)

BlackR66.sp <- fortify(BlackR66.poly)
BlackR66.df <- as.data.frame(BlackR66.poly)

#Black rockfish - 12068
BR.positions.b68 <- BlackR_accel %>% filter(periods == "Before") %>% filter(Transmitter == "A69-9007-12068")
BR.positions.b68 <- BR.positions.b68 %>% dplyr::group_by(Transmitter)
kud68_BlackR <- BR.positions.b68[!is.na(BR.positions.b68$Longitude) & !is.na(BR.positions.b68$Latitude),]
kud68_BlackR <- kud68_BlackR[, c("Transmitter", "Longitude", "Latitude")]
coordinates(kud68_BlackR) <- c("Longitude", "Latitude")
proj4string(kud68_BlackR) <- CRS("+init=epsg:4326")

kernel.68.BlackR <- kernelUD(kud68_BlackR, h = "href")
BlackR68.poly <- getverticeshr(kernel.68.BlackR, percent = 95)

BlackR68.sp <- fortify(BlackR68.poly)
BlackR68.df <- as.data.frame(BlackR68.poly)

#Black rockfish - 12070
BR.positions.b70 <- BlackR_accel %>% filter(periods == "Before") %>% filter(Transmitter == "A69-9007-12070")
BR.positions.b70 <- BR.positions.b70 %>% dplyr::group_by(Transmitter)
kud70_BlackR <- BR.positions.b70[!is.na(BR.positions.b70$Longitude) & !is.na(BR.positions.b70$Latitude),]
kud70_BlackR <- kud70_BlackR[, c("Transmitter", "Longitude", "Latitude")]
coordinates(kud70_BlackR) <- c("Longitude", "Latitude")
proj4string(kud70_BlackR) <- CRS("+init=epsg:4326")

kernel.70.BlackR <- kernelUD(kud70_BlackR, h = "href")
BlackR70.poly <- getverticeshr(kernel.70.BlackR, percent = 95)

BlackR70.sp <- fortify(BlackR70.poly)
BlackR70.df <- as.data.frame(BlackR70.poly)

#Black rockfish - 12074
BR.positions.b74 <- BlackR_accel %>% filter(periods == "Before") %>% filter(Transmitter == "A69-9007-12074")
BR.positions.b74 <- BR.positions.b74 %>% dplyr::group_by(Transmitter)
kud74_BlackR <- BR.positions.b74[!is.na(BR.positions.b74$Longitude) & !is.na(BR.positions.b74$Latitude),]
kud74_BlackR <- kud74_BlackR[, c("Transmitter", "Longitude", "Latitude")]
coordinates(kud74_BlackR) <- c("Longitude", "Latitude")
proj4string(kud74_BlackR) <- CRS("+init=epsg:4326")

kernel.74.BlackR <- kernelUD(kud74_BlackR, h = "href")
BlackR74.poly <- getverticeshr(kernel.74.BlackR, percent = 95)

BlackR74.sp <- fortify(BlackR74.poly)
BlackR74.df <- as.data.frame(BlackR74.poly)

#Black rockfish - 12076
BR.positions.b76 <- BlackR_accel %>% filter(periods == "Before") %>% filter(Transmitter == "A69-9007-12076")
BR.positions.b76 <- BR.positions.b76 %>% dplyr::group_by(Transmitter)
kud76_BlackR <- BR.positions.b76[!is.na(BR.positions.b76$Longitude) & !is.na(BR.positions.b76$Latitude),]
kud76_BlackR <- kud76_BlackR[, c("Transmitter", "Longitude", "Latitude")]
coordinates(kud76_BlackR) <- c("Longitude", "Latitude")
proj4string(kud76_BlackR) <- CRS("+init=epsg:4326")

kernel.76.BlackR <- kernelUD(kud76_BlackR, h = "href")
BlackR76.poly <- getverticeshr(kernel.76.BlackR, percent = 95)

BlackR76.sp <- fortify(BlackR76.poly)
BlackR76.df <- as.data.frame(BlackR76.poly)

BlackR.before.df <- bind_rows(BlackR48.df, BlackR50.df, BlackR52.df, 
                              BlackR54.df, BlackR58.df,
                              BlackR60.df, BlackR62.df, BlackR64.df,
                              BlackR66.df, BlackR68.df, BlackR70.df,
                              BlackR74.df, BlackR76.df)
BlackR.before.df <- BlackR.before.df %>% rename(area.before = area)


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
max(BlackR.before.df$area.before)
max(BlackR.dur.df$area.dur)
max(BlackR.after.df$area.after)