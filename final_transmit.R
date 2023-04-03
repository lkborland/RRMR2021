library(tidyverse)
#library(glatos)
library(lubridate)
library(gghighlight)
library(rstatix)
library(lme4)
library(gamm4)
library(car)
library(hms)
library(tibbletime)
library(moments)

#Upload tagsheet
tagsheet <- as_tibble(read.csv("E:\\MS research\\RRMR2021ReceiverLogs\\RRMRTagSheet.csv")) %>%
  rename(Transmitter = VUE.Tag.ID)

#new variable containing transmitter number and corresponding type of animal
animal_transmit <- dplyr::select(tagsheet, Transmitter, Tag.Destination)

#Assign prelim transmitter (animal) logs to variables in the environment
A_12048 <- as_tibble(read.csv("E:\\MS research\\RRMR2021ReceiverLogs\\VUE_export_Sept\\A69-9007-12048.csv", 
                              na.strings = c("Sensor Fault"))) %>%
  rename(Date.time.UTC = Date.and.Time..UTC.) %>%
  mutate(tag.type = "acceleration")

A_12049 <- as_tibble(read.csv("E:\\MS research\\RRMR2021ReceiverLogs\\VUE_export_Sept\\A69-9007-12049.csv", 
                              na.strings = c("Sensor Fault"))) %>%
  rename(Date.time.UTC = Date.and.Time..UTC.) %>%
  mutate(tag.type = "depth")

A_12050 <- as_tibble(read.csv("E:\\MS research\\RRMR2021ReceiverLogs\\VUE_export_Sept\\A69-9007-12050.csv", 
                              na.strings = c("Sensor Fault"))) %>%
  rename(Date.time.UTC = Date.and.Time..UTC.) %>%
  mutate(tag.type = "acceleration")

A_12051 <- as_tibble(read.csv("E:\\MS research\\RRMR2021ReceiverLogs\\VUE_export_Sept\\A69-9007-12051.csv", 
                              na.strings = c("Sensor Fault"))) %>% 
  rename(Date.time.UTC = Date.and.Time..UTC.) %>%
  mutate(tag.type = "depth")

A_12052 <- as_tibble(read.csv("E:\\MS research\\RRMR2021ReceiverLogs\\VUE_export_Sept\\A69-9007-12052.csv", 
                              na.strings = c("Sensor Fault"))) %>% 
  rename(Date.time.UTC = Date.and.Time..UTC.) %>%
  mutate(tag.type = "acceleration")

A_12053 <- as_tibble(read.csv("E:\\MS research\\RRMR2021ReceiverLogs\\VUE_export_Sept\\A69-9007-12053.csv", 
                              na.strings = c("Sensor Fault"))) %>% 
  rename(Date.time.UTC = Date.and.Time..UTC.) %>%
  mutate(tag.type = "depth")

A_12054 <- as_tibble(read.csv("E:\\MS research\\RRMR2021ReceiverLogs\\VUE_export_Sept\\A69-9007-12054.csv", 
                              na.strings = c("Sensor Fault"))) %>% 
  rename(Date.time.UTC = Date.and.Time..UTC.) %>%
  mutate(tag.type = "acceleration")

A_12055 <- as_tibble(read.csv("E:\\MS research\\RRMR2021ReceiverLogs\\VUE_export_Sept\\A69-9007-12055.csv", 
                              na.strings = c("Sensor Fault"))) %>% 
  rename(Date.time.UTC = Date.and.Time..UTC.) %>%
  mutate(tag.type = "depth")

A_12056 <- as_tibble(read.csv("E:\\MS research\\RRMR2021ReceiverLogs\\VUE_export_Sept\\A69-9007-12056.csv", 
                              na.strings = c("Sensor Fault"))) %>% 
  rename(Date.time.UTC = Date.and.Time..UTC.) %>%
  mutate(tag.type = "acceleration")

A_12057 <- as_tibble(read.csv("E:\\MS research\\RRMR2021ReceiverLogs\\VUE_export_Sept\\A69-9007-12057.csv", 
                              na.strings = c("Sensor Fault"))) %>% 
  rename(Date.time.UTC = Date.and.Time..UTC.) %>%
  mutate(tag.type = "depth")

A_12058<- as_tibble(read.csv("E:\\MS research\\RRMR2021ReceiverLogs\\VUE_export_Sept\\A69-9007-12058.csv", 
                             na.strings = c("Sensor Fault"))) %>% 
  rename(Date.time.UTC = Date.and.Time..UTC.) %>%
  mutate(tag.type = "acceleration")

A_12059 <- as_tibble(read.csv("E:\\MS research\\RRMR2021ReceiverLogs\\VUE_export_Sept\\A69-9007-12059.csv", 
                              na.strings = c("Sensor Fault"))) %>% 
  rename(Date.time.UTC = Date.and.Time..UTC.) %>%
  mutate(tag.type = "depth")

A_12060 <- as_tibble(read.csv("E:\\MS research\\RRMR2021ReceiverLogs\\VUE_export_Sept\\A69-9007-12060.csv", 
                              na.strings = c("Sensor Fault"))) %>% 
  rename(Date.time.UTC = Date.and.Time..UTC.) %>%
  mutate(tag.type = "acceleration")

A_12061 <- as_tibble(read.csv("E:\\MS research\\RRMR2021ReceiverLogs\\VUE_export_Sept\\A69-9007-12061.csv", 
                              na.strings = c("Sensor Fault"))) %>% 
  rename(Date.time.UTC = Date.and.Time..UTC.) %>%
  mutate(tag.type = "depth")

A_12062 <- as_tibble(read.csv("E:\\MS research\\RRMR2021ReceiverLogs\\VUE_export_Sept\\A69-9007-12062.csv", 
                              na.strings = c("Sensor Fault"))) %>% 
  rename(Date.time.UTC = Date.and.Time..UTC.) %>%
  mutate(tag.type = "acceleration")

A_12063 <- as_tibble(read.csv("E:\\MS research\\RRMR2021ReceiverLogs\\VUE_export_Sept\\A69-9007-12063.csv", 
                              na.strings = c("Sensor Fault"))) %>% 
  rename(Date.time.UTC = Date.and.Time..UTC.) %>%
  mutate(tag.type = "depth")

A_12064 <- as_tibble(read.csv("E:\\MS research\\RRMR2021ReceiverLogs\\VUE_export_Sept\\A69-9007-12064.csv", 
                              na.strings = c("Sensor Fault"))) %>% 
  rename(Date.time.UTC = Date.and.Time..UTC.) %>%
  mutate(tag.type = "acceleration")

A_12065 <- as_tibble(read.csv("E:\\MS research\\RRMR2021ReceiverLogs\\VUE_export_Sept\\A69-9007-12065.csv", 
                              na.strings = c("Sensor Fault"))) %>% 
  rename(Date.time.UTC = Date.and.Time..UTC.) %>%
  mutate(tag.type = "depth")

A_12066 <- as_tibble(read.csv("E:\\MS research\\RRMR2021ReceiverLogs\\VUE_export_Sept\\A69-9007-12066.csv", 
                              na.strings = c("Sensor Fault"))) %>% 
  rename(Date.time.UTC = Date.and.Time..UTC.) %>%
  mutate(tag.type = "acceleration")

A_12067 <- as_tibble(read.csv("E:\\MS research\\RRMR2021ReceiverLogs\\VUE_export_Sept\\A69-9007-12067.csv", 
                              na.strings = c("Sensor Fault"))) %>% 
  rename(Date.time.UTC = Date.and.Time..UTC.) %>%
  mutate(tag.type = "depth")

A_12068 <- as_tibble(read.csv("E:\\MS research\\RRMR2021ReceiverLogs\\VUE_export_Sept\\A69-9007-12068.csv", 
                              na.strings = c("Sensor Fault"))) %>% 
  rename(Date.time.UTC = Date.and.Time..UTC.) %>%
  mutate(tag.type = "acceleration")

A_12069 <- as_tibble(read.csv("E:\\MS research\\RRMR2021ReceiverLogs\\VUE_export_Sept\\A69-9007-12069.csv", 
                              na.strings = c("Sensor Fault"))) %>% 
  rename(Date.time.UTC = Date.and.Time..UTC.) %>%
  mutate(tag.type = "depth")

A_12070 <- as_tibble(read.csv("E:\\MS research\\RRMR2021ReceiverLogs\\VUE_export_Sept\\A69-9007-12070.csv", 
                              na.strings = c("Sensor Fault"))) %>% 
  rename(Date.time.UTC = Date.and.Time..UTC.) %>%
  mutate(tag.type = "acceleration")

A_12071 <- as_tibble(read.csv("E:\\MS research\\RRMR2021ReceiverLogs\\VUE_export_Sept\\A69-9007-12071.csv", 
                              na.strings = c("Sensor Fault"))) %>% 
  rename(Date.time.UTC = Date.and.Time..UTC.) %>%
  mutate(tag.type = "depth")

A_12072 <- as_tibble(read.csv("E:\\MS research\\RRMR2021ReceiverLogs\\VUE_export_Sept\\A69-9007-12072.csv", 
                              na.strings = c("Sensor Fault"))) %>% 
  rename(Date.time.UTC = Date.and.Time..UTC.) %>%
  mutate(tag.type = "acceleration")

A_12073 <- as_tibble(read.csv("E:\\MS research\\RRMR2021ReceiverLogs\\VUE_export_Sept\\A69-9007-12073.csv", 
                              na.strings = c("Sensor Fault"))) %>% 
  rename(Date.time.UTC = Date.and.Time..UTC.) %>%
  mutate(tag.type = "depth")

A_12074 <- as_tibble(read.csv("E:\\MS research\\RRMR2021ReceiverLogs\\VUE_export_Sept\\A69-9007-12074.csv", 
                              na.strings = c("Sensor Fault"))) %>% 
  rename(Date.time.UTC = Date.and.Time..UTC.) %>%
  mutate(tag.type = "acceleration")

A_12075 <- as_tibble(read.csv("E:\\MS research\\RRMR2021ReceiverLogs\\VUE_export_Sept\\A69-9007-12075.csv", 
                              na.strings = c("Sensor Fault"))) %>% 
  rename(Date.time.UTC = Date.and.Time..UTC.) %>%
  mutate(tag.type = "depth")

A_12076 <- as_tibble(read.csv("E:\\MS research\\RRMR2021ReceiverLogs\\VUE_export_Sept\\A69-9007-12076.csv", 
                              na.strings = c("Sensor Fault"))) %>% 
  rename(Date.time.UTC = Date.and.Time..UTC.) %>%
  mutate(tag.type = "acceleration")

A_12077 <- as_tibble(read.csv("E:\\MS research\\RRMR2021ReceiverLogs\\VUE_export_Sept\\A69-9007-12077.csv", 
                              na.strings = c("Sensor Fault"))) %>% 
  rename(Date.time.UTC = Date.and.Time..UTC.) %>%
  mutate(tag.type = "depth")



A_13249 <- as_tibble(read.csv("E:\\MS research\\RRMR2021ReceiverLogs\\VUE_export_Sept\\A69-9007-13249.csv", 
                              na.strings = c("Sensor Fault"))) %>% 
  rename(Date.time.UTC = Date.and.Time..UTC.)

A_13250 <- as_tibble(read.csv("E:\\MS research\\RRMR2021ReceiverLogs\\VUE_export_Sept\\A69-9007-13250.csv", 
                              na.strings = c("Sensor Fault"))) %>% 
  rename(Date.time.UTC = Date.and.Time..UTC.)

A_13251 <- as_tibble(read.csv("E:\\MS research\\RRMR2021ReceiverLogs\\VUE_export_Sept\\A69-9007-13251.csv", 
                              na.strings = c("Sensor Fault"))) %>% 
  rename(Date.time.UTC = Date.and.Time..UTC.)

A_13252 <- as_tibble(read.csv("E:\\MS research\\RRMR2021ReceiverLogs\\VUE_export_Sept\\A69-9007-13252.csv", 
                              na.strings = c("Sensor Fault"))) %>% 
  rename(Date.time.UTC = Date.and.Time..UTC.)

A_13253 <- as_tibble(read.csv("E:\\MS research\\RRMR2021ReceiverLogs\\VUE_export_Sept\\A69-9007-13253.csv", 
                              na.strings = c("Sensor Fault"))) %>% 
  rename(Date.time.UTC = Date.and.Time..UTC.)

A_13254 <- as_tibble(read.csv("E:\\MS research\\RRMR2021ReceiverLogs\\VUE_export_Sept\\A69-9007-13254.csv", 
                              na.strings = c("Sensor Fault"))) %>% 
  rename(Date.time.UTC = Date.and.Time..UTC.)

A_13255 <- as_tibble(read.csv("E:\\MS research\\RRMR2021ReceiverLogs\\VUE_export_Sept\\A69-9007-13255.csv", 
                              na.strings = c("Sensor Fault"))) %>% 
  rename(Date.time.UTC = Date.and.Time..UTC.)

A_13256 <- as_tibble(read.csv("E:\\MS research\\RRMR2021ReceiverLogs\\VUE_export_Sept\\A69-9007-13256.csv", 
                              na.strings = c("Sensor Fault"))) %>% 
  rename(Date.time.UTC = Date.and.Time..UTC.)

A_13257 <- as_tibble(read.csv("E:\\MS research\\RRMR2021ReceiverLogs\\VUE_export_Sept\\A69-9007-13257.csv", 
                              na.strings = c("Sensor Fault"))) %>% 
  rename(Date.time.UTC = Date.and.Time..UTC.)

A_13258 <- as_tibble(read.csv("E:\\MS research\\RRMR2021ReceiverLogs\\VUE_export_Sept\\A69-9007-13258.csv", 
                              na.strings = c("Sensor Fault"))) %>% 
  rename(Date.time.UTC = Date.and.Time..UTC.)

A_13259 <- as_tibble(read.csv("E:\\MS research\\RRMR2021ReceiverLogs\\VUE_export_Sept\\A69-9007-13259.csv", 
                              na.strings = c("Sensor Fault"))) %>% 
  rename(Date.time.UTC = Date.and.Time..UTC.)

A_13260 <- as_tibble(read.csv("E:\\MS research\\RRMR2021ReceiverLogs\\VUE_export_Sept\\A69-9007-13260.csv", 
                              na.strings = c("Sensor Fault"))) %>% 
  rename(Date.time.UTC = Date.and.Time..UTC.)

A_13261 <- as_tibble(read.csv("E:\\MS research\\RRMR2021ReceiverLogs\\VUE_export_Sept\\A69-9007-13261.csv", 
                              na.strings = c("Sensor Fault"))) %>% 
  rename(Date.time.UTC = Date.and.Time..UTC.)

A_13262 <- as_tibble(read.csv("E:\\MS research\\RRMR2021ReceiverLogs\\VUE_export_Sept\\A69-9007-13262.csv", 
                              na.strings = c("Sensor Fault"))) %>% 
  rename(Date.time.UTC = Date.and.Time..UTC.)

A_13263 <- as_tibble(read.csv("E:\\MS research\\RRMR2021ReceiverLogs\\VUE_export_Sept\\A69-9007-13263.csv", 
                              na.strings = c("Sensor Fault"))) %>% 
  rename(Date.time.UTC = Date.and.Time..UTC.)

A_13264 <- as_tibble(read.csv("E:\\MS research\\RRMR2021ReceiverLogs\\VUE_export_Sept\\A69-9007-13264.csv", 
                              na.strings = c("Sensor Fault"))) %>% 
  rename(Date.time.UTC = Date.and.Time..UTC.)

A_13265 <- as_tibble(read.csv("E:\\MS research\\RRMR2021ReceiverLogs\\VUE_export_Sept\\A69-9007-13265.csv", 
                              na.strings = c("Sensor Fault"))) %>% 
  rename(Date.time.UTC = Date.and.Time..UTC.)

A_13266 <- as_tibble(read.csv("E:\\MS research\\RRMR2021ReceiverLogs\\VUE_export_Sept\\A69-9007-13266.csv", 
                              na.strings = c("Sensor Fault"))) %>% 
  rename(Date.time.UTC = Date.and.Time..UTC.)

A_13267 <- as_tibble(read.csv("E:\\MS research\\RRMR2021ReceiverLogs\\VUE_export_Sept\\A69-9007-13267.csv", 
                              na.strings = c("Sensor Fault"))) %>% 
  rename(Date.time.UTC = Date.and.Time..UTC.)

A_13268 <- as_tibble(read.csv("E:\\MS research\\RRMR2021ReceiverLogs\\VUE_export_Sept\\A69-9007-13268.csv", 
                              na.strings = c("Sensor Fault"))) %>% 
  rename(Date.time.UTC = Date.and.Time..UTC.)

A_13269 <- as_tibble(read.csv("E:\\MS research\\RRMR2021ReceiverLogs\\VUE_export_Sept\\A69-9007-13269.csv", 
                              na.strings = c("Sensor Fault"))) %>% 
  rename(Date.time.UTC = Date.and.Time..UTC.)

A_13270 <- as_tibble(read.csv("E:\\MS research\\RRMR2021ReceiverLogs\\VUE_export_Sept\\A69-9007-13270.csv", 
                              na.strings = c("Sensor Fault"))) %>% 
  rename(Date.time.UTC = Date.and.Time..UTC.)

A_13271 <- as_tibble(read.csv("E:\\MS research\\RRMR2021ReceiverLogs\\VUE_export_Sept\\A69-9007-13271.csv", 
                              na.strings = c("Sensor Fault"))) %>% 
  rename(Date.time.UTC = Date.and.Time..UTC.)

A_13272 <- as_tibble(read.csv("E:\\MS research\\RRMR2021ReceiverLogs\\VUE_export_Sept\\A69-9007-13272.csv", 
                              na.strings = c("Sensor Fault"))) %>% 
  rename(Date.time.UTC = Date.and.Time..UTC.)

A_13273 <- as_tibble(read.csv("E:\\MS research\\RRMR2021ReceiverLogs\\VUE_export_Sept\\A69-9007-13273.csv", 
                              na.strings = c("Sensor Fault"))) %>% 
  rename(Date.time.UTC = Date.and.Time..UTC.)

A_13274 <- as_tibble(read.csv("E:\\MS research\\RRMR2021ReceiverLogs\\VUE_export_Sept\\A69-9007-13274.csv", 
                              na.strings = c("Sensor Fault"))) %>% 
  rename(Date.time.UTC = Date.and.Time..UTC.)

A_13275 <- as_tibble(read.csv("E:\\MS research\\RRMR2021ReceiverLogs\\VUE_export_Sept\\A69-9007-13275.csv", 
                              na.strings = c("Sensor Fault"))) %>% 
  rename(Date.time.UTC = Date.and.Time..UTC.)

A_13276 <- as_tibble(read.csv("E:\\MS research\\RRMR2021ReceiverLogs\\VUE_export_Sept\\A69-9007-13276.csv", 
                              na.strings = c("Sensor Fault"))) %>% 
  rename(Date.time.UTC = Date.and.Time..UTC.)

A_13277 <- as_tibble(read.csv("E:\\MS research\\RRMR2021ReceiverLogs\\VUE_export_Sept\\A69-9007-13277.csv", 
                              na.strings = c("Sensor Fault"))) %>% 
  rename(Date.time.UTC = Date.and.Time..UTC.)

A_13278 <- as_tibble(read.csv("E:\\MS research\\RRMR2021ReceiverLogs\\VUE_export_Sept\\A69-9007-13278.csv", 
                              na.strings = c("Sensor Fault"))) %>% 
  rename(Date.time.UTC = Date.and.Time..UTC.)

A_13279 <- as_tibble(read.csv("E:\\MS research\\RRMR2021ReceiverLogs\\VUE_export_Sept\\A69-9007-13279.csv", 
                              na.strings = c("Sensor Fault"))) %>% 
  rename(Date.time.UTC = Date.and.Time..UTC.)

A_13280 <- as_tibble(read.csv("E:\\MS research\\RRMR2021ReceiverLogs\\VUE_export_Sept\\A69-9007-13280.csv", 
                              na.strings = c("Sensor Fault"))) %>% 
  rename(Date.time.UTC = Date.and.Time..UTC.)

A_13281 <- as_tibble(read.csv("E:\\MS research\\RRMR2021ReceiverLogs\\VUE_export_Sept\\A69-9007-13281.csv", 
                              na.strings = c("Sensor Fault"))) %>% 
  rename(Date.time.UTC = Date.and.Time..UTC.)

A_13282 <- as_tibble(read.csv("E:\\MS research\\RRMR2021ReceiverLogs\\VUE_export_Sept\\A69-9007-13282.csv", 
                              na.strings = c("Sensor Fault"))) %>% 
  rename(Date.time.UTC = Date.and.Time..UTC.)

A_13283 <- as_tibble(read.csv("E:\\MS research\\RRMR2021ReceiverLogs\\VUE_export_Sept\\A69-9007-13283.csv", 
                              na.strings = c("Sensor Fault"))) %>% 
  rename(Date.time.UTC = Date.and.Time..UTC.)

A_13284 <- as_tibble(read.csv("E:\\MS research\\RRMR2021ReceiverLogs\\VUE_export_Sept\\A69-9007-13284.csv", 
                              na.strings = c("Sensor Fault"))) %>% 
  rename(Date.time.UTC = Date.and.Time..UTC.)

A_13285 <- as_tibble(read.csv("E:\\MS research\\RRMR2021ReceiverLogs\\VUE_export_Sept\\A69-9007-13285.csv", 
                              na.strings = c("Sensor Fault"))) %>% 
  rename(Date.time.UTC = Date.and.Time..UTC.)

A_13286 <- as_tibble(read.csv("E:\\MS research\\RRMR2021ReceiverLogs\\VUE_export_Sept\\A69-9007-13286.csv", 
                              na.strings = c("Sensor Fault"))) %>% 
  rename(Date.time.UTC = Date.and.Time..UTC.)

A_13287 <- as_tibble(read.csv("E:\\MS research\\RRMR2021ReceiverLogs\\VUE_export_Sept\\A69-9007-13287.csv", 
                              na.strings = c("Sensor Fault"))) %>% 
  rename(Date.time.UTC = Date.and.Time..UTC.)

A_13289 <- as_tibble(read.csv("E:\\MS research\\RRMR2021ReceiverLogs\\VUE_export_Sept\\A69-9007-13289.csv", 
                              na.strings = c("Sensor Fault"))) %>% 
  rename(Date.time.UTC = Date.and.Time..UTC.)

A_13290 <- as_tibble(read.csv("E:\\MS research\\RRMR2021ReceiverLogs\\VUE_export_Sept\\A69-9007-13290.csv", 
                              na.strings = c("Sensor Fault"))) %>% 
  rename(Date.time.UTC = Date.and.Time..UTC.)

A_13291 <- as_tibble(read.csv("E:\\MS research\\RRMR2021ReceiverLogs\\VUE_export_Sept\\A69-9007-13291.csv", 
                              na.strings = c("Sensor Fault"))) %>% 
  rename(Date.time.UTC = Date.and.Time..UTC.)

A_13292 <- as_tibble(read.csv("E:\\MS research\\RRMR2021ReceiverLogs\\VUE_export_Sept\\A69-9007-13292.csv", 
                              na.strings = c("Sensor Fault"))) %>% 
  rename(Date.time.UTC = Date.and.Time..UTC.)

A_13293 <- as_tibble(read.csv("E:\\MS research\\RRMR2021ReceiverLogs\\VUE_export_Sept\\A69-9007-13293.csv", 
                              na.strings = c("Sensor Fault"))) %>% 
  rename(Date.time.UTC = Date.and.Time..UTC.)


#combine detections by transmitter tag to plot accelerometer data over time
#STILL NEED TO ADD CONFIRMATION OF SENSOR UNIT AGREEMENT
A_comb_1 <- bind_rows(A_12048, A_12049, A_12050, A_12051, A_12052, A_12053, A_12054, A_12055, A_12056, A_12057, A_12058, A_12059, 
                      A_12060, A_12061, A_12062, A_12063, A_12064, A_12065, A_12066, A_12067, A_12068, A_12069, A_12070, A_12071,
                      A_12072, A_12073, A_12074, A_12075, A_12076, A_12077, A_13249, A_13250, A_13251, A_13252, A_13253, A_13254, 
                      A_13255, A_13256, A_13257, A_13258, A_13259, A_13260, A_13261, A_13262, A_13263, A_13264, A_13265, A_13266, 
                      A_13267, A_13269, A_13271, A_13272, A_13273, A_13274, A_13275, A_13276, A_13277, A_13278, A_13279, A_13280, 
                      A_13281, A_13282, A_13283, A_13284, A_13285, A_13286, A_13287, A_13289, A_13290, A_13291, A_13292, A_13293)

A_comb_2 <- left_join(A_comb_1, animal_transmit, by = "Transmitter")


#Select out species and create separate tibbles for visualization
dat_BlackR_accel <- A_comb_2 %>% filter(str_detect(Tag.Destination, "Black")) %>% filter(str_detect(tag.type, "acceleration"))
dat_BlackR_depth <- A_comb_2 %>% filter(str_detect(Tag.Destination, "Black")) %>% filter(str_detect(tag.type, "depth"))
dat_ChinaR <- A_comb_2 %>% filter(str_detect(Tag.Destination, "China"))
dat_Dungeness <- A_comb_2 %>% filter(str_detect(Tag.Destination, "Dungeness"))
dat_Lingcod <- A_comb_2 %>% filter(str_detect(Tag.Destination, "Lingcod"))

#convert to posixct time type, convert observation times to US Pacific time zone
dat_Dungeness$Date.time.UTC <- as.POSIXct(strptime(dat_Dungeness$Date.time.UTC, "%Y-%m-%d %H:%M:%S"))
dat_Dungeness$Date.time.UTC <- with_tz(dat_Dungeness$Date.time.UTC, tzone = "US/Pacific")

dat_Lingcod$Date.time.UTC <- as.POSIXct(strptime(dat_Lingcod$Date.time.UTC, "%Y-%m-%d %H:%M:%S"))
dat_Lingcod$Date.time.UTC <- with_tz(dat_Lingcod$Date.time.UTC, tzone = "US/Pacific")

dat_BlackR_accel$Date.time.UTC <- as.POSIXct(strptime(dat_BlackR_accel$Date.time.UTC, "%Y-%m-%d %H:%M:%S"))
dat_BlackR_accel$Date.time.UTC <- with_tz(dat_BlackR_accel$Date.time.UTC, tzone = "US/Pacific")

dat_BlackR_depth$Date.time.UTC <- as.POSIXct(strptime(dat_BlackR_depth$Date.time.UTC, "%Y-%m-%d %H:%M:%S"))
dat_BlackR_depth$Date.time.UTC <- with_tz(dat_BlackR_depth$Date.time.UTC, tzone = "US/Pacific")

dat_ChinaR$Date.time.UTC <- as.POSIXct(strptime(dat_ChinaR$Date.time.UTC, "%Y-%m-%d %H:%M:%S"))
dat_ChinaR$Date.time.UTC <- with_tz(dat_ChinaR$Date.time.UTC, tzone = "US/Pacific")


#PRELIMINARY - tell R the time range of seismic survey
surveydates <- read.csv("E:\\MS research\\RRMR2021ReceiverLogs\\periods_time.csv")
surveydates$Date.time.UTC <- as.POSIXct(strptime(surveydates$Date.time.UTC, "%Y-%m-%d %H:%M:%S"))
surveydates$Date.time.UTC <- with_tz(surveydates$Date.time.UTC, tzone = "US/Pacific")

May10_start <- ymd_hms("2021-05-10 00:00:00", tz = "US/Pacific")
May17_end <- ymd_hms("2021-05-17 23:59:59", tz = "US/Pacific")

May18_start <- ymd_hms("2021-05-18 00:00:00", tz = "US/Pacific")
May25_end <- ymd_hms("2021-05-25 23:59:59", tz = "US/Pacific")

May26_start <- ymd_hms("2021-05-26 00:00:00", tz = "US/Pacific")
June2_end <- ymd_hms("2021-06-02 23:59:59", tz = "US/Pacific")

June3_start <- ymd_hms("2021-06-03 00:00:00", tz = "US/Pacific")
June10_end <- ymd_hms("2021-06-10 23:59:59", tz = "US/Pacific")

June11_start <- ymd_hms("2021-06-11 00:00:00", tz = "US/Pacific")
June11_end <- ymd_hms("2021-06-11 23:59:59", tz = "US/Pacific")

June12_start <- ymd_hms("2021-06-12 00:00:00", tz = "US/Pacific")
June16_end <- ymd_hms("2021-06-16 23:59:59", tz = "US/Pacific")

#example plotting looking at other estimated timeframes of seismic survey booms
#variables containing days of preliminary seismic boom detections

June17_start <- ymd_hms("2021-06-17 00:00:00", tz = "US/Pacific")
June17_end <- ymd_hms("2021-06-17 23:59:59", tz = "US/Pacific")

June18_start <- ymd_hms("2021-06-18 00:00:00", tz = "US/Pacific")
June18_end <- ymd_hms("2021-06-18 23:59:59", tz = "US/Pacific")

June19_start <- ymd_hms("2021-06-19 00:00:00", tz = "US/Pacific")
June26_end <- ymd_hms("2021-06-26 23:59:59", tz = "US/Pacific")

June27_start <- ymd_hms("2021-06-27 00:00:00", tz = "US/Pacific")
July4_end <- ymd_hms("2021-07-04 23:59:59", tz = "US/Pacific")

July5_start <- ymd_hms("2021-07-05 00:00:00", tz = "US/Pacific")
July12_end <- ymd_hms("2021-07-12 23:59:59", tz = "US/Pacific")

##nighttime periods


June10_PM <- ymd_hms("2021-06-10 23:00:00", tz = "US/Pacific")
June11_PM <- ymd_hms("2021-06-11 05:39:00", tz = "US/Pacific")

nighttime <- hms::as_hms("23:00:00")
nighttime <- as.POSIXct(nighttime, format = "%H:%M:%S")
date(nighttime) <- today(tzone = "US/Pacific")

morningtime <- hms::as_hms("05:39:00")
morningtime <- as.POSIXct(morningtime, format = "%H:%M:%S")
date(morningtime) <- today(tzone = "US/Pacific")


periodsbr.ex <- dat_BlackR_accel %>% mutate(survey.period = case_when(Date.time.UTC < May10_start ~ "Early",
                                                                      Date.time.UTC >= May10_start & Date.time.UTC <= May17_end ~ "May 10-17",
                                                                      Date.time.UTC >= May18_start & Date.time.UTC <= May25_end ~ "May 18-25",
                                                                      Date.time.UTC >= May26_start & Date.time.UTC <= June2_end ~ "May 26-June 2",
                                                                      Date.time.UTC >= June3_start & Date.time.UTC <= June10_end ~ "June 3-10",
                                                                      Date.time.UTC >= June11_start & Date.time.UTC <= June11_end ~ "June 11",
                                                                      Date.time.UTC >= June12_start & Date.time.UTC <= June16_end ~ "June 12-16",
                                                                      Date.time.UTC >= June17_start & Date.time.UTC <= June17_end ~ "June 17",
                                                                      Date.time.UTC >= June18_start & Date.time.UTC <= June18_end ~ "June 18",
                                                                      Date.time.UTC >= June19_start & Date.time.UTC <= June26_end ~ "June 19-26",
                                                                      Date.time.UTC >= June27_start & Date.time.UTC <= July4_end ~ "June 27-July 4",
                                                                      Date.time.UTC >= July5_start & Date.time.UTC <= July12_end ~ "July 5-12"))


###################################################################################################
#adding "period" categorizations to data (need to bin as the resolution is too fine to be visible on a plot)
periods_BlackR_accel <- dat_BlackR_accel %>% mutate(survey.period = case_when(Date.time.UTC < May10_start ~ "Early",
                                                                              Date.time.UTC >= May10_start & Date.time.UTC <= May17_end ~ "May 10-17",
                                                                              Date.time.UTC >= May18_start & Date.time.UTC <= May25_end ~ "May 18-25",
                                                                              Date.time.UTC >= May26_start & Date.time.UTC <= June2_end ~ "May 26-June 2",
                                                                              Date.time.UTC >= June3_start & Date.time.UTC <= June10_end ~ "June 3-10",
                                                                              Date.time.UTC >= June11_start & Date.time.UTC <= June11_end ~ "June 11",
                                                                              Date.time.UTC >= June12_start & Date.time.UTC <= June16_end ~ "June 12-16",
                                                                              Date.time.UTC >= June17_start & Date.time.UTC <= June17_end ~ "June 17",
                                                                              Date.time.UTC >= June18_start & Date.time.UTC <= June18_end ~ "June 18",
                                                                              Date.time.UTC >= June19_start & Date.time.UTC <= June26_end ~ "June 19-26",
                                                                              Date.time.UTC >= June27_start & Date.time.UTC <= July4_end ~ "June 27-July 4",
                                                                              Date.time.UTC >= July5_start & Date.time.UTC <= July12_end ~ "July 5-12"))

periods_BlackR_depth <- dat_BlackR_depth %>% mutate(survey.period = case_when(Date.time.UTC < May10_start ~ "Early",
                                                                              Date.time.UTC >= May10_start & Date.time.UTC <= May17_end ~ "May 10-17",
                                                                              Date.time.UTC >= May18_start & Date.time.UTC <= May25_end ~ "May 18-25",
                                                                              Date.time.UTC >= May26_start & Date.time.UTC <= June2_end ~ "May 26-June 2",
                                                                              Date.time.UTC >= June3_start & Date.time.UTC <= June10_end ~ "June 3-10",
                                                                              Date.time.UTC >= June11_start & Date.time.UTC <= June11_end ~ "June 11",
                                                                              Date.time.UTC >= June12_start & Date.time.UTC <= June16_end ~ "June 12-16",
                                                                              Date.time.UTC >= June17_start & Date.time.UTC <= June17_end ~ "June 17",
                                                                              Date.time.UTC >= June18_start & Date.time.UTC <= June18_end ~ "June 18",
                                                                              Date.time.UTC >= June19_start & Date.time.UTC <= June26_end ~ "June 19-26",
                                                                              Date.time.UTC >= June27_start & Date.time.UTC <= July4_end ~ "June 27-July 4",
                                                                              Date.time.UTC >= July5_start & Date.time.UTC <= July12_end ~ "July 5-12"))

periods_ChinaR <- dat_ChinaR %>% mutate(survey.period = case_when(Date.time.UTC < May10_start ~ "Early",
                                                                  Date.time.UTC >= May10_start & Date.time.UTC <= May17_end ~ "May 10-17",
                                                                  Date.time.UTC >= May18_start & Date.time.UTC <= May25_end ~ "May 18-25",
                                                                  Date.time.UTC >= May26_start & Date.time.UTC <= June2_end ~ "May 26-June 2",
                                                                  Date.time.UTC >= June3_start & Date.time.UTC <= June10_end ~ "June 3-10",
                                                                  Date.time.UTC >= June11_start & Date.time.UTC <= June11_end ~ "June 11",
                                                                  Date.time.UTC >= June12_start & Date.time.UTC <= June16_end ~ "June 12-16",
                                                                  Date.time.UTC >= June17_start & Date.time.UTC <= June17_end ~ "June 17",
                                                                  Date.time.UTC >= June18_start & Date.time.UTC <= June18_end ~ "June 18",
                                                                  Date.time.UTC >= June19_start & Date.time.UTC <= June26_end ~ "June 19-26",
                                                                  Date.time.UTC >= June27_start & Date.time.UTC <= July4_end ~ "June 27-July 4",
                                                                  Date.time.UTC >= July5_start & Date.time.UTC <= July12_end ~ "July 5-12"))

periods_Dungeness <- dat_Dungeness %>% mutate(survey.period = case_when(Date.time.UTC < June11_start ~ "May 20-June 10",
                                                                        Date.time.UTC >= June11_start & Date.time.UTC <= June11_end ~ "June 11",
                                                                        Date.time.UTC > June11_end & Date.time.UTC < June16_start ~ "June 12-16",
                                                                        Date.time.UTC >= June16_start & Date.time.UTC <= June16_end ~ "June 16",
                                                                        Date.time.UTC >= June17_start & Date.time.UTC <= June17_end ~ "June 17",
                                                                        Date.time.UTC >= June18_start & Date.time.UTC <= June18_end ~ "June 18",
                                                                        Date.time.UTC > June18_end & Date.time.UTC <= July11_end ~ "June 19-July 11"))

periods_Lingcod <- dat_Lingcod %>% mutate(survey.period = case_when(Date.time.UTC < June11_start ~ "May 20-June 10",
                                                                    Date.time.UTC >= June11_start & Date.time.UTC <= June11_end ~ "June 11",
                                                                    Date.time.UTC > June11_end & Date.time.UTC < June16_start ~ "June 12-16",
                                                                    Date.time.UTC >= June16_start & Date.time.UTC <= June16_end ~ "June 16",
                                                                    Date.time.UTC >= June17_start & Date.time.UTC <= June17_end ~ "June 17",
                                                                    Date.time.UTC >= June18_start & Date.time.UTC <= June18_end ~ "June 18",
                                                                    Date.time.UTC > June18_end & Date.time.UTC <= July11_end ~ "June 19-July 11"))




#add column of minimum time based on detections, by individual
#periods_Dungeness <- periods_Dungeness %>% group_by(Transmitter) %>% mutate(time.min = min(Date.time.UTC))
#periods_Lingcod <- periods_Lingcod %>% group_by(Transmitter) %>% mutate(time.min = min(Date.time.UTC))


#find minimum (earliest) time for each transmitter to feed to time since release
#add first time to each row by individual/tag
#use difftime for time after release
#periods_Dungeness$TSR <- difftime(periods_Dungeness$Date.time.UTC, periods_Dungeness$time.min, units = "mins")
#periods_Lingcod$TSR <- difftime(periods_Lingcod$Date.time.UTC, periods_Lingcod$time.min, units = "mins")




#create vector of coarse survey times
#survey_times <- c(June11_start, June11_end, June16_start, June16_end, June17_start, June17_end, June18_start, June18_end, July11_end)

#survey_times #######################



##Evaluate potential mortality events / dropped tags
ggplot(periods_Dungeness, aes(Date.time.UTC, Sensor.Value)) + 
  geom_point() + facet_grid(rows = vars(Transmitter)) +
  labs(x = "Time", y = "Acceleration values", title = "Dungeness acceleration over time", 
       caption = "Preliminary analyses")

ggplot(periods_Lingcod, aes(Date.time.UTC, Sensor.Value)) + 
  geom_point() + facet_grid(rows = vars(Transmitter)) +
  labs(x = "Time", y = "Acceleration values", title = "Lingcod acceleration over time", 
       caption = "Preliminary analyses")

ggplot(periods_BlackR_accel, aes(Date.time.UTC, Sensor.Value)) + 
  geom_point() + facet_grid(rows = vars(Transmitter)) +
  labs(x = "Time", y = "Acceleration values", title = "Black rockfish acceleration over time", 
       caption = "Preliminary analyses")

ggplot(periods_BlackR_depth, aes(Date.time.UTC, Sensor.Value)) + 
  geom_point() + facet_grid(rows = vars(Transmitter))

ggplot(periods_ChinaR, aes(Date.time.UTC, Sensor.Value)) + 
  geom_point() + facet_grid(rows = vars(Transmitter)) +
  labs(x = "Time", y = "Acceleration values", title = "China rockfish acceleration over time", 
       caption = "Preliminary analyses")

#remove observations of transmitters with too low sample size/observations
##China R: 13265
periods_ChinaR <- periods_ChinaR %>% filter(!(Transmitter == "A69-9007-13265" | Transmitter == "A69-9007-13268"))

##Black R accel: 12072
periods_BlackR_accel <- periods_BlackR_accel %>% filter(!(Transmitter == "A69-9007-12072"))
#periodsbr.ex <- periodsbr.ex %>% filter(!(Transmitter == "A69-9007-12072"))
periods_BlackR_depth <- periods_BlackR_depth %>% filter(!(Transmitter == "A69-9007-12073"))

##Lingcod: 13250, 13262
periods_Lingcod <- periods_Lingcod %>% filter(!(Transmitter == "A69-9007-13262"))

##Dungeness: 13293
periods_Dungeness <- periods_Dungeness %>% filter(!(Transmitter == "A69-9007-13293"))


#ex_fish <- periods_Dungeness %>% filter(Transmitter == "A69-9007-13293")
#plot(ex_fish$Date.time.UTC, ex_fish$Sensor.Value)

##vector of generic sunrise to sunset
#Port_O_Sun <- read.csv("E:\\MS research\\RRMR2021ReceiverLogs\\NOAA Port Orford tides\\NOAA_PortO_NightDay2021.csv")
#Port_O_Sun <- Port_O_Sun %>% rename(Date = ï..Date)

## HERE __________________________________________________________________
#Port_O_Sun$Sunrise.Time <- as.POSIXct(Port_O_Sun$Sunrise.Time, format = "%H:%M")
#Port_O_Sun$Sunset.Time <- as.POSIXct(Port_O_Sun$Sunset.Time, format = "%H:%M")
#Port_O_Sun$Sunrise.Time <- with_tz(Port_O_Sun$Sunrise.Time, tzone = "US/Pacific")
#Port_O_Sun$Sunset.Time <- with_tz(Port_O_Sun$Sunset.Time, tzone = "US/Pacific")

#Port_O_Sun$Date <- mdy(Port_O_Sun$Date)
#Port_O_Sun <- as_tibble(Port_O_Sun)

#Port_O_Sun$Sunrise.Time <- as.POSIXct(Port_O_Sun$Sunrise.Time, format = "%H:%M")
#Port_O_Sun$Sunset.Time <- as.POSIXct(Port_O_Sun$Sunset.Time, format = "%H:%M")
#Port_O_Sun$Sunrise.Time <- with_tz(Port_O_Sun$Sunrise.Time, tzone = "US/Pacific")
#Port_O_Sun$Sunset.Time <- with_tz(Port_O_Sun$Sunset.Time, tzone = "US/Pacific")
#__________________________________________________________________________

##plot average acceleration over time, overlay shaded periods as "night" and dotted line as beginning of seismic survey period

periods_Dungeness <- periods_Dungeness %>% mutate(coarse.period = case_when(survey.period == "May 20-June 10" ~ "Before",
                                                                            survey.period == "June 11" | survey.period == "June 12-16" 
                                                                            | survey.period == "June 16" | survey.period == "June 17" 
                                                                            | survey.period == "June 18" ~ "During",
                                                                            survey.period == "June 19-July 11" ~ "After"))

periods_Lingcod <- periods_Lingcod %>% mutate(coarse.period = case_when(survey.period == "May 20-June 10" ~ "Before",
                                                                            survey.period == "June 11" | survey.period == "June 12-16" 
                                                                            | survey.period == "June 16" | survey.period == "June 17" 
                                                                            | survey.period == "June 18" ~ "During",
                                                                            survey.period == "June 19-July 11" ~ "After"))
periods_BlackR_accel <- periods_BlackR_accel %>% mutate(coarse.period = case_when(survey.period == "May 20-June 10" ~ "Before",
                                                                            survey.period == "June 11" | survey.period == "June 12-16" 
                                                                            | survey.period == "June 16" | survey.period == "June 17" 
                                                                            | survey.period == "June 18" ~ "During",
                                                                            survey.period == "June 19-July 11" ~ "After"))

periods_BlackR_depth <- periods_BlackR_depth %>% mutate(coarse.period = case_when(survey.period == "May 20-June 10" ~ "Before",
                                                                                  survey.period == "June 11" | survey.period == "June 12-16" 
                                                                                  | survey.period == "June 16" | survey.period == "June 17" 
                                                                                  | survey.period == "June 18" ~ "During",
                                                                                  survey.period == "June 19-July 11" ~ "After"))

periods_ChinaR <- periods_ChinaR %>% mutate(coarse.period = case_when(survey.period == "May 20-June 10" ~ "Before",
                                                                            survey.period == "June 11" | survey.period == "June 12-16" 
                                                                            | survey.period == "June 16" | survey.period == "June 17" 
                                                                            | survey.period == "June 18" ~ "During",
                                                                            survey.period == "June 19-July 11" ~ "After"))




##example to evaluate function 
ex_date <- periods_Dungeness %>% select(detect_day) %>% slice(1)
ex_time <- periods_Dungeness %>% select(detect_time) %>% slice(1)
subset(Port_O_Sun, as.Date(Port_O_Sun$Date) %in% ex_date)


#case_day_night <- function(fishdate, fishtime) {
  
  exdate <- subset(Port_O_Sun, as.Date(Port_O_Sun$Date) %in% fishdate)
  sunrise <- exdate$Sunrise.Time
  sunset <- exdate$Sunset.Time
  daynight <- case_when(
                   fishtime < sunrise | fishtime >= sunset ~ "Night",
                   fishtime >= sunrise & fishtime < sunset ~ "Day"
               )
  return(daynight)


#case_day_night <- function(fishdate, fishtime) {
  
  exdate <- subset(Port_O_Sun, as.Date(Port_O_Sun$Date) %in% fishdate)
  sunrise <- exdate$Sunrise.Time
  sunset <- exdate$Sunset.Time
  daynight <- case_when(
    fishtime >= sunrise & fishtime < sunset ~ "Day",
    fishtime < sunrise | fishtime >= sunset ~ "Night")
  return(daynight)

  }

#add day/night info to datasets
#periods_Dungeness <- periods_Dungeness %>% dplyr::mutate(day.night = case_day_night(detect_day, detect_time)) #doesn't work

#periods_Dungeness$day.night <- mapply(FUN = case_day_night, periods_Dungeness$detect_day, periods_Dungeness$detect_time, SIMPLIFY = FALSE)

#periods_Dungeness$day.night <- do.call( function(detect_day,detect_time,...) case_day_night(detect_day, detect_time), periods_Dungeness )



#add in fine-scale noise data from Integral NoiseSpotter
#data on rms sound pressure levels, peak pressure in 30 s windows, 
#and the cumulative exposure levels in 30 s windows. The three files correspond to the three sensors located 35 cm, 
# 50 cm and 70 cm above the sea bed.
NS35cm <- read.csv("E:\\MS research\\Integral NoiseSpotter Data\\Jan 23\\Acoustic\\file40B_35cmAB.csv")
NS35cm$Time <- as.POSIXct(NS35cm$Time, format = "%m/%d/%Y %H:%M:%S")
NS35cm$Time <- with_tz(NS35cm$Time, tzone = "US/Pacific")
NS35cm <- NS35cm %>% rename(Date.time.UTC = Time)
NS35cm$Date.time.UTC <- round_date(NS35cm$Date.time.UTC, "30 seconds")

NS35cm_vel <- read.csv("E:\\MS research\\Integral NoiseSpotter Data\\Jan 23\\Acoustic\\file40B_vel_35cmAB.csv")
NS35cm_vel$Time <- as.POSIXct(NS35cm_vel$Time, format = "%m/%d/%Y %H:%M:%S")
NS35cm_vel$Time <- with_tz(NS35cm_vel$Time, tzone = "US/Pacific")
NS35cm_vel <- NS35cm_vel %>% rename(Date.time.UTC = Time)
NS35cm_vel$Date.time.UTC <- round_date(NS35cm_vel$Date.time.UTC, "30 seconds")

NS50cm <- read.csv("E:\\MS research\\Integral NoiseSpotter Data\\Jan 23\\Acoustic\\file100_50cmAB.csv")
NS50cm$Time <- as.POSIXct(NS50cm$Time, format = "%m/%d/%Y %H:%M:%S")
NS50cm$Time <- with_tz(NS50cm$Time, tzone = "US/Pacific")
NS50cm <- NS50cm %>% rename(Date.time.UTC = Time)
NS50cm$Date.time.UTC <- round_date(NS50cm$Date.time.UTC, "30 seconds")

NS50cm_vel <- read.csv("E:\\MS research\\Integral NoiseSpotter Data\\Jan 23\\Acoustic\\file100_vel_50cmAB.csv")
NS50cm_vel$Time <- as.POSIXct(NS50cm_vel$Time, format = "%m/%d/%Y %H:%M:%S")
NS50cm_vel$Time <- with_tz(NS50cm_vel$Time, tzone = "US/Pacific")
NS50cm_vel <- NS50cm_vel %>% rename(Date.time.UTC = Time)
NS50cm_vel$Date.time.UTC <- round_date(NS50cm_vel$Date.time.UTC, "30 seconds")


NS70cm <- read.csv("E:\\MS research\\Integral NoiseSpotter Data\\Jan 23\\Acoustic\\file40A_70cmAB.csv")
NS70cm$Time <- as.POSIXct(NS70cm$Time, format = "%m/%d/%Y %H:%M:%S")
NS70cm$Time <- with_tz(NS70cm$Time, tzone = "US/Pacific")
NS70cm <- NS70cm %>% rename(Date.time.UTC = Time)
NS70cm$Date.time.UTC <- round_date(NS70cm$Date.time.UTC, "30 seconds")

NS70cm_vel <- read.csv("E:\\MS research\\Integral NoiseSpotter Data\\Jan 23\\Acoustic\\file40A_vel_70cmAB.csv")
NS70cm_vel$Time <- as.POSIXct(NS70cm_vel$Time, format = "%m/%d/%Y %H:%M:%S")
NS70cm_vel$Time <- with_tz(NS70cm_vel$Time, tzone = "US/Pacific")
NS70cm_vel <- NS70cm_vel %>% rename(Date.time.UTC = Time)
NS70cm_vel$Date.time.UTC <- round_date(NS70cm_vel$Date.time.UTC, "30 seconds")

#combine noise data sets
NSall_press <- full_join(NS35cm, NS50cm, by = "Date.time.UTC", multiple = "all")
NSall_press <- full_join(NSall_press, NS70cm, by = "Date.time.UTC", multiple = "all")

NSall_pm <- full_join(NS35cm_vel, NS50cm_vel, by = "Date.time.UTC")
NSall_pm <- full_join(NSall_pm, NS70cm_vel, by = "Date.time.UTC")

NS35all <- full_join(NS35cm, NS35cm_vel, by = "Date.time.UTC")
NS50all <- full_join(NS50cm, NS50cm_vel, by = "Date.time.UTC")
NS70all <- full_join(NS70cm, NS70cm_vel, by = "Date.time.UTC")


ggplot(NS35all, aes(Peak.SPL.x, Peak.SPL.y)) + geom_point()

ggplot(NS35cm_vel, aes(Date.time.UTC, SEL)) + 
  geom_point() +
  labs(x = "Time", y = "Sound Exposure Level (35cm sensor)", title = "Received sound exposure levels")


#round times in acceleration data sets to combine with Noise Spotter data
#periods_Dungeness$Date.time.UTC <- round_date(periods_Dungeness$Date.time.UTC, "30 seconds")
#periods_Lingcod$Date.time.UTC <- round_date(periods_Lingcod$Date.time.UTC, "30 seconds")
periods_BlackR_accel$Date.time.UTC <- round_date(periods_BlackR_accel$Date.time.UTC, "30 seconds")
periods_BlackR_depth$Date.time.UTC <- round_date(periods_BlackR_depth$Date.time.UTC, "30 seconds")
periods_ChinaR$Date.time.UTC <- round_date(periods_ChinaR$Date.time.UTC, "30 seconds")

periodsbr.ex$Date.time.UTC <- round_date(periodsbr.ex$Date.time.UTC, "30 seconds")


#### splitting out date and times of each row (observation) separately for function
periods_Dungeness <- periods_Dungeness %>% dplyr::mutate(detect_day = date(Date.time.UTC)) %>% 
  dplyr::mutate(detect_time = hms::as_hms(Date.time.UTC))
periods_Dungeness$detect_time <- as.POSIXct(periods_Dungeness$detect_time, format = "%H:%M:%S")
date(periods_Dungeness$detect_time) <- today(tzone = "US/Pacific")

periods_Lingcod <- periods_Lingcod %>% dplyr::mutate(detect_day = date(Date.time.UTC)) %>% 
  dplyr::mutate(detect_time = hms::as_hms(Date.time.UTC))
periods_Lingcod$detect_time <- as.POSIXct(periods_Lingcod$detect_time, format = "%H:%M:%S")
date(periods_Lingcod$detect_time) <- today(tzone = "US/Pacific")

periods_BlackR_accel <- periods_BlackR_accel %>% dplyr::mutate(detect_day = date(Date.time.UTC)) %>% 
  dplyr::mutate(detect_time = hms::as_hms(Date.time.UTC))
periods_BlackR_accel$detect_time <- as.POSIXct(periods_BlackR_accel$detect_time, format = "%H:%M:%S")
date(periods_BlackR_accel$detect_time) <- today(tzone = "US/Pacific")

periods_BlackR_depth <- periods_BlackR_depth %>% dplyr::mutate(detect_day = date(Date.time.UTC)) %>% 
  dplyr::mutate(detect_time = hms::as_hms(Date.time.UTC))
periods_BlackR_depth$detect_time <- as.POSIXct(periods_BlackR_depth$detect_time, format = "%H:%M:%S")
date(periods_BlackR_depth$detect_time) <- today(tzone = "US/Pacific")

periods_ChinaR <- periods_ChinaR %>% dplyr::mutate(detect_day = date(Date.time.UTC)) %>% 
  dplyr::mutate(detect_time = hms::as_hms(Date.time.UTC))
periods_ChinaR$detect_time <- as.POSIXct(periods_ChinaR$detect_time, format = "%H:%M:%S")
date(periods_ChinaR$detect_time) <- today(tzone = "US/Pacific")

periodsbr.ex <- periodsbr.ex %>% dplyr::mutate(detect_day = date(Date.time.UTC)) %>% 
  dplyr::mutate(detect_time = hms::as_hms(Date.time.UTC))
periodsbr.ex$detect_time <- as.POSIXct(periodsbr.ex$detect_time, format = "%H:%M:%S")
date(periodsbr.ex$detect_time) <- today(tzone = "US/Pacific")


################################
##############################
###########################
#### run HOB script here #####
################################
################################


#add noise levels to data sets of movement metrics, remove observations without noise data
###############################################################
#periods_Dungeness_noise <- inner_join(NS35cm_vel, periods_Dungeness, by = "Date.time.UTC")
#periods_Lingcod_noise <- inner_join(NS35cm, periods_Lingcod, by = "Date.time.UTC")
periods_BlackR_accel_70noise <- inner_join(NS70all, periods_BlackR_accel, by = "Date.time.UTC", multiple = "all")
periods_BlackR_depth_70noise <- inner_join(NS70all, periods_BlackR_depth, by = "Date.time.UTC", multiple = "all")
periods_ChinaR_50noise <- inner_join(NS50all, periods_ChinaR, by = "Date.time.UTC", multiple = "all")
periods_BlackR_HOB_70noise <- inner_join(NS70all, periods_BlackR_HOB, by = "Date.time.UTC", multiple = "all")

#add Port Orford SBE data to data sets with sensor values
Port_O_SBE <- read.csv("E:\\MS research\\RRMR2021ReceiverLogs\\Port_Orford_SBE.csv")
##use data points at depth only - filter out those with depths smaller than 30m
Port_O_SBE <- Port_O_SBE %>% filter(DepthM > 30)
##rename columns for ease of use
#Port_O_SBE <- Port_O_SBE %>% rename(TempC = Tv290C) %>% rename(Salinity = Sal00) %>% rename(DepthM = DepSM)
##change timezone of data (same point in time, convert to UTC from PDT to merge with transmitter data)
Port_O_SBE$Time <- as.POSIXct(Port_O_SBE$Time, format = "%m/%d/%Y %H:%M")
Port_O_SBE$Time <- with_tz(Port_O_SBE$Time, tzone = "US/Pacific")
Port_O_SBE <- Port_O_SBE %>% rename(Date.time.UTC = Time)


#add Port Orford temp data to data sets - do I need this even??
Port_O_earlytemp <- read.csv("E:\\MS research\\RRMR2021ReceiverLogs\\port_o_temp_early.csv")
Port_O_earlytemp <- Port_O_earlytemp %>% dplyr::select(-X) %>% drop_na()
Port_O_earlytemp$Time <- as.POSIXct(Port_O_earlytemp$Time, format = "%m/%d/%Y %H:%M")
Port_O_earlytemp$Time <- with_tz(Port_O_earlytemp$Time, tzone = "US/Pacific")
Port_O_earlytemp <- Port_O_earlytemp %>% rename(Date.time.UTC = Time)

# Port O wind data
Port_O_wind <- read.csv("E:\\MS research\\RRMR2021ReceiverLogs\\Wind_PortO\\PORO3_2021_wind.csv")
Port_O_wind <- Port_O_wind %>% unite("Date.time.UTC", Date:Time, remove = FALSE, sep = " ")
Port_O_wind$Date.time.UTC <- as.POSIXct(Port_O_wind$Date.time.UTC, format = "%m/%d/%Y %H:%M:%S", tz = "US/Pacific")
Port_O_wind$Date.time.UTC <- with_tz(Port_O_wind$Date.time.UTC, tzone = "US/Pacific")


# RV Langseth data
Langseth_dist <- read.csv("E:\\MS research\\RRMR2021ReceiverLogs\\Langseth_data\\Langseth_distNS.csv")
Langseth_dist <- Langseth_dist %>% unite("Date.time.UTC", Date:Time.UTC., remove = FALSE, sep = " ")
Langseth_dist$Date.time.UTC <- as.POSIXct(Langseth_dist$Date.time.UTC, format = "%m/%d/%Y %H:%M:%S", tz = "UTC")
Langseth_dist$Date.time.UTC <- with_tz(Langseth_dist$Date.time.UTC, tzone = "US/Pacific")
Langseth_dist <- Langseth_dist %>% dplyr::select(-X, -Date, -Time.UTC.)

#add SBE data to data sets of movement metrics
#periods_Dungeness_SBE <- full_join(Port_O_SBE, periods_Dungeness, by = "Date.time.UTC")
#periods_Lingcod_SBE <- full_join(Port_O_SBE, periods_Lingcod, by = "Date.time.UTC")
periods_BlackR_accel_SBE <- full_join(Port_O_SBE, periods_BlackR_accel, by = "Date.time.UTC", multiple = "all")
periods_BlackR_depth_SBE <- full_join(Port_O_SBE, periods_BlackR_depth, by = "Date.time.UTC", multiple = "all")
periods_ChinaR_SBE <- full_join(Port_O_SBE, periods_ChinaR, by = "Date.time.UTC", multiple = "all")
periods_BlackR_HOB_SBE <- full_join(Port_O_SBE, periods_BlackR_HOB, by = "Date.time.UTC", multiple = "all")

periodsbr.ex.SBE <- full_join(Port_O_SBE, periodsbr.ex, by = "Date.time.UTC")

############# 
# RUN IMPUTATION / read in CSV files
###############
################

BlackR_accelall_imputed <- read.csv("E:\\MS research\\RRMR2021ReceiverLogs\\BlackR_accelall_imputed.csv")
BlackR_accelall_imputed$Date.time.UTC <- as.POSIXct(BlackR_accelall_imputed$Date.time.UTC, format = "%Y-%m-%d %H:%M:%S", tz = "US/Pacific")

BlackR_HOBall_imputed <- read.csv("E:\\MS research\\RRMR2021ReceiverLogs\\BlackR_HOBall_imputed.csv")
BlackR_HOBall_imputed$Date.time.UTC <- as.POSIXct(BlackR_HOBall_imputed$Date.time.UTC, format = "%Y-%m-%d %H:%M:%S", tz = "US/Pacific")

ChinaR_accelall_imputed <- read.csv("E:\\MS research\\RRMR2021ReceiverLogs\\ChinaR_accelall_imputed.csv")
ChinaR_accelall_imputed$Date.time.UTC <- as.POSIXct(ChinaR_accelall_imputed$Date.time.UTC, format = "%Y-%m-%d %H:%M:%S", tz = "US/Pacific")

BlackR_accel_imputedwind <- read.csv("E:\\MS research\\RRMR2021ReceiverLogs\\BlackR_accel_imputedwind.csv")
BlackR_accel_imputedwind$Date.time.UTC <- as.POSIXct(BlackR_accel_imputedwind$Date.time.UTC, format = "%Y-%m-%d %H:%M:%S", tz = "US/Pacific")




####### combine all datasets into "Big Dataset" for GAMM purposes
############# HERE ##################333
BlackR_acceleration <- dplyr::inner_join(NS70all, BlackR_accelall_imputed, by = "Date.time.UTC", multiple = "all")

ChinaR_acceleration <- dplyr::inner_join(NS50all, ChinaR_accelall_imputed, by = "Date.time.UTC", multiple = "all")

#BlackR_acceleration_wind <- dplyr::inner_join(NS70all, BlackR_accel_imputedwind, by = "Date.time.UTC", multiple = "all")


#### do this
BlackR_acceleration_imputedwind <- dplyr::inner_join(BlackR_accel_imputedwind, BlackR_acceleration, by = "Date.time.UTC", multiple = "all")
BlackR_acceleration_imputedwind$hourtime <- hour(BlackR_acceleration_imputedwind$detect_time)



#upper 25th quartiles
acc75_D <- periods_Dungeness_SBE[,.(median = quantile(Sensor.Value, probs = c(0.5), na.rm = TRUE), 
                                      u75 = quantile(Sensor.Value,probs = c(0.75), na.rm = TRUE)),
                                  by = .(Sensor.Value, Transmitter, TempC, C0mS.cm, Salinity, survey.period,Date.time.UTC)]









NS35cm$Time <- as.POSIXct(NS35cm$Time, format = "%m/%d/%Y %H:%M:%S")
NS35cm$Time <- with_tz(NS35cm$Time, tzone = "US/Pacific")
NS35cm <- NS35cm %>% rename(Date.time.UTC = Time)
NS35cm$Date.time.UTC <- round_date(NS35cm$Date.time.UTC, "30 seconds")


#convert all times to same day to evaluate daily movement
#timeperiod_sameday <- as.POSIXct(Dungeness_before$Date.time.UTC, format="%H:%M:%S")


ggplot(periods_Dungeness, aes(x = Date.time.UTC, y = Sensor.Value)) + 
  geom_area(colour = "light blue") + geom_vline(xintercept = survey_times, linetype = 2) +
  annotate("rect", xmin = diurnal_night[1], xmax = diurnal_night[2], ymin = 0, alpha = .2) +
  labs(x = "Time", y = "Acceleration values", title = "Dungeness acceleration over time", 
       caption = "Preliminary analyses")

#gghighlight(max(Temp) > 93, label_key = Month)
# facet_grid(survey.period ~ .) + 
#gghighlight(hms(Date.time.UTC) >= "20:30:00" & hms(Date.time.UTC) <= "05:45:00")
#annotate("rect", xmin = "20:30:00", xmax = "05:45:00", ymin = 0, ymax = 4.5, alpha = .1, fill = "gray")
#annotate("rect", xmin = 1950, xmax = 1980, ymin = -1, ymax = 1, alpha = .1,fill = "blue")

##assess number of receivers each species detected on 
length(unique(periods_Lingcod$Receiver))
length(unique(periods_Dungeness$Receiver))
length(unique(periods_ChinaR$Receiver))
length(unique(periods_BlackR_accel$Receiver))

########################------------------------------
n_Lingcod <- periods_Lingcod %>% dplyr::count(survey.period, Transmitter)
n_Lingcod <- n_Lingcod %>% dplyr::group_by(survey.period) %>% dplyr::summarise(n()) %>% dplyr::rename(number = 'n()')                                                  

Lingcodn_overperiods <- ggplot(n_Lingcod, aes(x=survey.period, fill = survey.period)) + 
  geom_bar() +
  scale_x_discrete(limits=c("May 20-June 10", "June 11", "June 12-16", "June 16", "June 17", "June 18", "June 19-July 11")) +
  labs(x = "Period of survey", y = "Number of individuals", title = "Lingcod Individuals by Period")+  
  scale_fill_manual(values = noise_colors_repeat, breaks = period_lvl, labels = noise_labels, name = "Survey period")

Lingcodn_overperiods
###----------------------------------------------------


#ex_lingcod <- periods_Lingcod %>% filter(Transmitter == "A69-9007-13255")
#length(unique(ex_lingcod$Receiver))

length(unique(subset(periods_Lingcod$Transmitter, periods_Lingcod$survey.period == "May 20-June 10")))
length(unique(subset(periods_Lingcod$Transmitter, periods_Lingcod$survey.period == "June 11")))
length(unique(subset(periods_Lingcod$Transmitter, periods_Lingcod$survey.period == "June 12-16")))
length(unique(subset(periods_Lingcod$Transmitter, periods_Lingcod$survey.period == "June 16")))
length(unique(subset(periods_Lingcod$Transmitter, periods_Lingcod$survey.period == "June 17")))
length(unique(subset(periods_Lingcod$Transmitter, periods_Lingcod$survey.period == "June 18")))
length(unique(subset(periods_Lingcod$Transmitter, periods_Lingcod$survey.period == "June 19-July 11")))


#Dungeness unique receivers
D1 <- periods_Dungeness %>% filter(detect_day == as.Date("2021-06-01")) %>% group_by(Transmitter) %>% summarise(n_distinct(Receiver)) %>% mutate(periods = "Before") %>% mutate(Date = "2021-06-01")
D2 <- periods_Dungeness  %>% filter(detect_day == as.Date("2021-06-03")) %>% group_by(Transmitter) %>% summarise(n_distinct(Receiver)) %>% mutate(periods = "Before") %>% mutate(Date = "2021-06-03")
D3 <- periods_Dungeness  %>% filter(detect_day == as.Date("2021-06-08")) %>% group_by(Transmitter) %>% summarise(n_distinct(Receiver)) %>% mutate(periods = "Before") %>% mutate(Date = "2021-06-08")

D4 <- periods_Dungeness  %>% filter(detect_day == as.Date("2021-06-11")) %>% group_by(Transmitter) %>% summarise(n_distinct(Receiver)) %>% mutate(periods = "During") %>% mutate(Date = "2021-06-11")
D5 <- periods_Dungeness  %>% filter(detect_day == as.Date("2021-06-17")) %>% group_by(Transmitter) %>% summarise(n_distinct(Receiver)) %>% mutate(periods = "During") %>% mutate(Date = "2021-06-17")
D6 <- periods_Dungeness  %>% filter(detect_day == as.Date("2021-06-18")) %>% group_by(Transmitter) %>% summarise(n_distinct(Receiver)) %>% mutate(periods = "During") %>% mutate(Date = "2021-06-18")

D7 <- periods_Dungeness  %>% filter(detect_day == as.Date("2021-06-21")) %>% group_by(Transmitter) %>% summarise(n_distinct(Receiver)) %>% mutate(periods = "After") %>% mutate(Date = "2021-06-21")
D8 <- periods_Dungeness  %>% filter(detect_day == as.Date("2021-06-25")) %>% group_by(Transmitter) %>% summarise(n_distinct(Receiver)) %>% mutate(periods = "After") %>% mutate(Date = "2021-06-25")
D9 <- periods_Dungeness  %>% filter(detect_day == as.Date("2021-07-01")) %>% group_by(Transmitter) %>% summarise(n_distinct(Receiver)) %>% mutate(periods = "After") %>% mutate(Date = "2021-07-01")

Dungeness_receivers <- bind_rows(D1, D2, D3, D4, D5, D6, D7, D8, D9) %>% rename(n.receiver = 'n_distinct(Receiver)')

ggplot(Dungeness_receivers, aes(x=Date, y=n.receiver, fill=periods)) + 
  geom_boxplot() +  stat_summary(fun.data=mean_sdl, geom="pointrange", color="black") +
  labs(x = "Date of survey", y = "Average unique receivers detected on", title = "Dungeness Detection Range by Period", fill="periods",
       caption = "Preliminary analyses")


#Black R unique receivers
B1 <- periods_BlackR_accel %>% filter(detect_day == as.Date("2021-06-01")) %>% group_by(Transmitter) %>% summarise(n_distinct(Receiver)) %>% mutate(periods = "Before") %>% mutate(Date = "2021-06-01")
B2 <- periods_BlackR_accel  %>% filter(detect_day == as.Date("2021-06-03")) %>% group_by(Transmitter) %>% summarise(n_distinct(Receiver)) %>% mutate(periods = "Before") %>% mutate(Date = "2021-06-03")
B3 <- periods_BlackR_accel  %>% filter(detect_day == as.Date("2021-06-08")) %>% group_by(Transmitter) %>% summarise(n_distinct(Receiver)) %>% mutate(periods = "Before") %>% mutate(Date = "2021-06-08")

B4 <- periods_BlackR_accel  %>% filter(detect_day == as.Date("2021-06-11")) %>% group_by(Transmitter) %>% summarise(n_distinct(Receiver)) %>% mutate(periods = "During") %>% mutate(Date = "2021-06-11")
B5 <- periods_BlackR_accel  %>% filter(detect_day == as.Date("2021-06-17")) %>% group_by(Transmitter) %>% summarise(n_distinct(Receiver)) %>% mutate(periods = "During") %>% mutate(Date = "2021-06-17")
B6 <- periods_BlackR_accel  %>% filter(detect_day == as.Date("2021-06-18")) %>% group_by(Transmitter) %>% summarise(n_distinct(Receiver)) %>% mutate(periods = "During") %>% mutate(Date = "2021-06-18")

B7 <- periods_BlackR_accel  %>% filter(detect_day == as.Date("2021-06-21")) %>% group_by(Transmitter) %>% summarise(n_distinct(Receiver)) %>% mutate(periods = "After") %>% mutate(Date = "2021-06-21")
B8 <- periods_BlackR_accel  %>% filter(detect_day == as.Date("2021-06-25")) %>% group_by(Transmitter) %>% summarise(n_distinct(Receiver)) %>% mutate(periods = "After") %>% mutate(Date = "2021-06-25")
B9 <- periods_BlackR_accel  %>% filter(detect_day == as.Date("2021-07-01")) %>% group_by(Transmitter) %>% summarise(n_distinct(Receiver)) %>% mutate(periods = "After") %>% mutate(Date = "2021-07-01")

BlackR_receivers <- bind_rows(B1, B2, B3, B4, B5, B6, B7, B8, B9) %>% rename(n.receiver = 'n_distinct(Receiver)')

############ BOOK CHAPTER PLOT 2
n_BRR <- BlackR_receivers %>% dplyr::count(Date, Transmitter)
n_BRR <- n_BRR %>% dplyr::group_by(Date) %>% dplyr::summarise(n()) %>% dplyr::rename(number = 'n()')                                                
max_BRR <- BlackR_receivers %>% dplyr::group_by(Date) %>% summarise(max_val = max(density(n.receiver)[[1]]) + .2)
n_BRR <- full_join(n_BRR, max_BRR, by = "Date")

ggplot(BlackR_receivers, aes(x=Date, y=n.receiver, fill=Date)) + 
  geom_violin(trim=TRUE) +  stat_summary(fun.data=mean_sdl, geom="pointrange", color="black") +
  labs(x = "Survey period", y = expression(paste("Unique receivers")), title = "Black Rockfish Detection by Period", fill="Period")+  
  scale_fill_manual(values = noise_colors_9, breaks = noise_lvl_9, labels = noise_labels_9) + 
  scale_x_discrete(labels = noise_labels_9) +
  geom_text(data = n_BRR,
            aes(label = number,
                y = 15.5,
                x = Date,
                group = Date),
            position = position_dodge(width = 0.9),
            check_overlap = TRUE,
            show.legend = FALSE,
            inherit.aes = FALSE)

ggplot(BlackR_receivers, aes(x=Date, y=n.receiver, fill=periods)) + 
  geom_boxplot() +  stat_summary(fun.data=mean_sdl, geom="pointrange", color="black") +
  labs(x = "Date of survey", y = "Average unique receivers detected on", title = "Black rockfish Detection Range by Period", fill="periods",
       caption = "Preliminary analyses")


#China R unique receivers
C1 <- periods_ChinaR %>% filter(detect_day == as.Date("2021-06-01")) %>% group_by(Transmitter) %>% summarise(n_distinct(Receiver)) %>% mutate(periods = "Before") %>% mutate(Date = "2021-06-01")
C2 <- periods_ChinaR  %>% filter(detect_day == as.Date("2021-06-03")) %>% group_by(Transmitter) %>% summarise(n_distinct(Receiver)) %>% mutate(periods = "Before") %>% mutate(Date = "2021-06-03")
C3 <- periods_ChinaR  %>% filter(detect_day == as.Date("2021-06-08")) %>% group_by(Transmitter) %>% summarise(n_distinct(Receiver)) %>% mutate(periods = "Before") %>% mutate(Date = "2021-06-08")

C4 <- periods_ChinaR  %>% filter(detect_day == as.Date("2021-06-11")) %>% group_by(Transmitter) %>% summarise(n_distinct(Receiver)) %>% mutate(periods = "During") %>% mutate(Date = "2021-06-11")
C5 <- periods_ChinaR  %>% filter(detect_day == as.Date("2021-06-17")) %>% group_by(Transmitter) %>% summarise(n_distinct(Receiver)) %>% mutate(periods = "During") %>% mutate(Date = "2021-06-17")
C6 <- periods_ChinaR  %>% filter(detect_day == as.Date("2021-06-18")) %>% group_by(Transmitter) %>% summarise(n_distinct(Receiver)) %>% mutate(periods = "During") %>% mutate(Date = "2021-06-18")

C7 <- periods_ChinaR  %>% filter(detect_day == as.Date("2021-06-21")) %>% group_by(Transmitter) %>% summarise(n_distinct(Receiver)) %>% mutate(periods = "After") %>% mutate(Date = "2021-06-21")
C8 <- periods_ChinaR  %>% filter(detect_day == as.Date("2021-06-25")) %>% group_by(Transmitter) %>% summarise(n_distinct(Receiver)) %>% mutate(periods = "After") %>% mutate(Date = "2021-06-25")
C9 <- periods_ChinaR  %>% filter(detect_day == as.Date("2021-07-01")) %>% group_by(Transmitter) %>% summarise(n_distinct(Receiver)) %>% mutate(periods = "After") %>% mutate(Date = "2021-07-01")

ChinaR_receivers <- bind_rows(C1, C2, C3, C4, C5, C6, C7, C8, C9) %>% rename(n.receiver = 'n_distinct(Receiver)')

ggplot(ChinaR_receivers, aes(x=Date, y=n.receiver, fill=periods)) + 
  geom_boxplot() +  stat_summary(fun.data=mean_sdl, geom="pointrange", color="black") +
  labs(x = "Date of survey", y = "Average unique receivers detected on", title = "China rockfish Detection Range by Period", fill="periods",
       caption = "Preliminary analyses")

###### BOOK CHPT PLOT 4
n_CRR <- ChinaR_receivers %>% dplyr::count(Date, Transmitter)
n_CRR <- n_CRR %>% dplyr::group_by(Date) %>% dplyr::summarise(n()) %>% dplyr::rename(number = 'n()')                                                
max_CRR <- ChinaR_receivers %>% dplyr::group_by(Date) %>% summarise(max_val = max(density(n.receiver)[[1]]) + .2)
n_CRR <- full_join(n_CRR, max_CRR, by = "Date")

ggplot(ChinaR_receivers, aes(x=Date, y=n.receiver, fill=Date)) + 
  geom_violin(trim=TRUE) +  stat_summary(fun.data=mean_sdl, geom="pointrange", color="black") +
  labs(x = "Survey period", y = expression(paste("Unique receivers")), title = "China Rockfish Detection by Period", fill="Period")+  
  scale_fill_manual(values = noise_colors_9, breaks = noise_lvl_9, labels = noise_labels_9) + 
  scale_x_discrete(labels = noise_labels_9) +
  geom_text(data = n_CRR,
            aes(label = number,
                y = 7,
                x = Date,
                group = Date),
            position = position_dodge(width = 0.9),
            check_overlap = TRUE,
            show.legend = FALSE,
            inherit.aes = FALSE)


#Lingccod unique receivers
L1 <- periods_Lingcod %>% filter(detect_day == as.Date("2021-06-01")) %>% group_by(Transmitter) %>% summarise(n_distinct(Receiver)) %>% mutate(periods = "Before") %>% mutate(Date = "2021-06-01")
L2 <- periods_Lingcod  %>% filter(detect_day == as.Date("2021-06-03")) %>% group_by(Transmitter) %>% summarise(n_distinct(Receiver)) %>% mutate(periods = "Before") %>% mutate(Date = "2021-06-03")
L3 <- periods_Lingcod  %>% filter(detect_day == as.Date("2021-06-08")) %>% group_by(Transmitter) %>% summarise(n_distinct(Receiver)) %>% mutate(periods = "Before") %>% mutate(Date = "2021-06-08")

L4 <- periods_Lingcod  %>% filter(detect_day == as.Date("2021-06-11")) %>% group_by(Transmitter) %>% summarise(n_distinct(Receiver)) %>% mutate(periods = "During") %>% mutate(Date = "2021-06-11")
L5 <- periods_Lingcod  %>% filter(detect_day == as.Date("2021-06-17")) %>% group_by(Transmitter) %>% summarise(n_distinct(Receiver)) %>% mutate(periods = "During") %>% mutate(Date = "2021-06-17")
L6 <- periods_Lingcod  %>% filter(detect_day == as.Date("2021-06-18")) %>% group_by(Transmitter) %>% summarise(n_distinct(Receiver)) %>% mutate(periods = "During") %>% mutate(Date = "2021-06-18")

L7 <- periods_Lingcod  %>% filter(detect_day == as.Date("2021-06-21")) %>% group_by(Transmitter) %>% summarise(n_distinct(Receiver)) %>% mutate(periods = "After") %>% mutate(Date = "2021-06-21")
L8 <- periods_Lingcod  %>% filter(detect_day == as.Date("2021-06-25")) %>% group_by(Transmitter) %>% summarise(n_distinct(Receiver)) %>% mutate(periods = "After") %>% mutate(Date = "2021-06-25")
L9 <- periods_Lingcod  %>% filter(detect_day == as.Date("2021-07-01")) %>% group_by(Transmitter) %>% summarise(n_distinct(Receiver)) %>% mutate(periods = "After") %>% mutate(Date = "2021-07-01")

Lingcod_receivers <- bind_rows(L1, L2, L3, L4, L5, L6, L7, L8, L9) %>% rename(n.receiver = 'n_distinct(Receiver)')

ggplot(Lingcod_receivers, aes(x=Date, y=n.receiver, fill=periods)) + 
  geom_boxplot() +  stat_summary(fun.data=mean_sdl, geom="pointrange", color="black") +
  labs(x = "Date of survey", y = "Average unique receivers detected on", title = "Lingcod Detection Range by Period", fill="periods",
       caption = "Preliminary analyses")






#individual fish evaluate - 8, 9, 10, 13, 14
ggplot(A_12062, aes(Date.time.UTC, Sensor.Value)) + 
  geom_point() +
  labs(x = "Time", y = "Acceleration values", title = "Black rockfish acceleration over time", 
       caption = "Preliminary analyses")

#create violin plot of preliminary Dungeness crab data across periods for visualization of acceleration values
# Violin plot with trimmed tails and adding median points as data is skewed (mean inappropriate)

#set up labeling each plot with counts
#nlabels_D <- table(periods_Dungeness$survey.period)


#  To create the median labels, you can use by
#meds <- c(by(mtcars$mpg, mtcars$cyl, median))

#ggplot(mtcars, aes(factor(cyl), mpg, label=rownames(mtcars))) +
#  geom_boxplot(fill = "grey80", colour = "#3366FF") + 
#  geom_text(data = data.frame(), aes(x = names(meds) , y = meds, 
#                                     label = paste("n =", nlabels)))

n_Dungeness <- periods_Dungeness %>% dplyr::count(survey.period, Transmitter)
n_Dungeness <- n_Dungeness %>% dplyr::group_by(survey.period) %>% dplyr::summarise(n()) %>% dplyr::rename(number = 'n()')                                                
max_Dungeness <- periods_Dungeness %>% dplyr::group_by(survey.period) %>% summarise(max_val = max(density(Sensor.Value)[[1]]) + .2)
n_Dungeness <- full_join(n_Dungeness, max_Dungeness, by = "survey.period")

violin_Dungeness <- ggplot(periods_Dungeness, aes(x=survey.period, y=Sensor.Value, fill=survey.period)) + 
  geom_violin(trim=FALSE) +  stat_summary(fun.data=mean_sdl, geom="pointrange", color="black") +
  scale_x_discrete(limits=c("May 20-June 10", "June 11", "June 12-16", "June 16", "June 17", "June 18", "June 19-July 11")) +
  labs(x = "Period of survey", y = "Acceleration values", title = "Dungeness Crab Acceleration by Period", fill="Period",
       caption = "Preliminary analyses") +  scale_fill_manual(values = noise_colors_repeat, breaks = period_lvl, labels = noise_labels) +
  geom_text(data = n_Dungeness,
            aes(label = number,
                y = max_val,
                x = survey.period,
                group = survey.period),
            position = position_dodge(width = 0.9),
            check_overlap = TRUE,
            show.legend = FALSE,
            inherit.aes = FALSE)
violin_Dungeness



violin_Lingcod <- ggplot(periods_Lingcod, aes(x=survey.period, y=Sensor.Value, fill=survey.period)) + 
  geom_violin(trim=TRUE) +  stat_summary(fun.data=mean_sdl, geom="pointrange", color="black") +
  scale_x_discrete(limits=c("May 20-June 10", "June 11", "June 12-16", "June 16", "June 17", "June 18", "June 19-July 11")) +
  labs(x = "Period of survey", y = "Acceleration values", title = "Lingcod Acceleration by Period", fill="Period")+
  scale_fill_manual(values = noise_colors_repeat, breaks = period_lvl, labels = noise_labels)
violin_Lingcod



n_BRA <- periods_BlackR_accel %>% dplyr::count(survey.period, Transmitter)
n_BRA <- n_BRA %>% dplyr::group_by(survey.period) %>% dplyr::summarise(n()) %>% dplyr::rename(number = 'n()')                                                
max_BRA <- periods_BlackR_accel %>% dplyr::group_by(survey.period) %>% summarise(max_val = max(density(Sensor.Value)[[1]]) + .2)
n_BRA <- full_join(n_BRA, max_BRA, by = "survey.period")

violin_BlackR_accel <- ggplot(periods_BlackR_accel, aes(x=survey.period, y=Sensor.Value, fill=survey.period)) + 
  geom_violin(trim=TRUE) +  stat_summary(fun.data=mean_sdl, geom="pointrange", color="black") +
  scale_x_discrete(limits=c("May 20-June 10", "June 11", "June 12-16", "June 16", "June 17", "June 18", "June 19-July 11")) +
  labs(x = "Period of survey", y = expression(paste("Acceleration (m/s"^"2", ")")), title = "Black Rockfish Acceleration by Period", fill="Period")+  
  scale_fill_manual(values = noise_colors_repeat, breaks = period_lvl, labels = noise_labels) + 
  geom_text(data = n_BRA,
            aes(label = number,
                y = max_val,
                x = survey.period,
                group = survey.period),
            position = position_dodge(width = 0.9),
            check_overlap = TRUE,
            show.legend = FALSE,
            inherit.aes = FALSE)

violin_BlackR_accel

#annotate("text",
#x = 1:length(table(data$group)),
#y = aggregate(values ~ group, data, median)[ , 2],
#label = length(unique(periods_BlackR_accel$Transmitter)),
#col = "red",
#vjust = - 1)

violin_BlackR_depth <- ggplot(periods_BlackR_depth, aes(x=survey.period, y=Sensor.Value, fill=survey.period)) + 
  geom_violin(trim=TRUE) +  stat_summary(fun.data=mean_sdl, geom="pointrange", color="black") +
  scale_x_discrete(limits=c("May 20-June 10", "June 11", "June 12-16", "June 16", "June 17", "June 18", "June 19-July 11")) +
  labs(x = "Period of survey", y = "Depth (m)", title = "Black Rockfish Depth by Period", fill="Period") +
  scale_y_reverse()+  scale_fill_manual(values = noise_colors_repeat, breaks = period_lvl, labels = noise_labels)
violin_BlackR_depth

violin_BlackR_HOB <- ggplot(Black, aes(x=survey.period, y=HOB, fill=survey.period)) + 
  geom_violin(trim=TRUE) +  stat_summary(fun.data=mean_sdl, geom="pointrange", color="black") +
  scale_x_discrete(limits=c("May 20-June 10", "June 11", "June 12-16", "June 16", "June 17", "June 18", "June 19-July 11")) +
  labs(x = "Period of survey", y = "Height off bottom (m)", title = "Black Rockfish HOB by Period", fill="Period") +
  scale_fill_manual(values = noise_colors_repeat, breaks = period_lvl, labels = noise_labels)
violin_BlackR_HOB


violin_ChinaR <- ggplot(periods_ChinaR, aes(x=survey.period, y=Sensor.Value, fill=survey.period)) + 
  geom_violin(trim=FALSE) +  stat_summary(fun.data=mean_sdl, geom="pointrange", color="black") +
  scale_x_discrete(limits=c("May 20-June 10", "June 11", "June 12-16", "June 16", "June 17", "June 18", "June 19-July 11")) +
  labs(x = "Period of survey", y = "Acceleration values", title = "China Rockfish Acceleration by Period", fill="Period",
       caption = "Preliminary analyses")+  scale_fill_manual(values = noise_colors_repeat, breaks = period_lvl, labels = noise_labels)
violin_ChinaR


#Dungeness accel random days
DV1 <- periods_Dungeness %>% filter(detect_day == as.Date("2021-06-01")) %>% group_by(Transmitter) %>% mutate(periods = "Before") %>% mutate(Date = "2021-06-01")
DV2 <- periods_Dungeness  %>% filter(detect_day == as.Date("2021-06-03")) %>% group_by(Transmitter) %>% mutate(periods = "Before") %>% mutate(Date = "2021-06-03")
DV3 <- periods_Dungeness  %>% filter(detect_day == as.Date("2021-06-08")) %>% group_by(Transmitter) %>% mutate(periods = "Before") %>% mutate(Date = "2021-06-08")

DV4 <- periods_Dungeness  %>% filter(detect_day == as.Date("2021-06-11")) %>% group_by(Transmitter) %>% mutate(periods = "During") %>% mutate(Date = "2021-06-11")
DV5 <- periods_Dungeness  %>% filter(detect_day == as.Date("2021-06-17")) %>% group_by(Transmitter) %>% mutate(periods = "During") %>% mutate(Date = "2021-06-17")
DV6 <- periods_Dungeness  %>% filter(detect_day == as.Date("2021-06-18")) %>% group_by(Transmitter) %>% mutate(periods = "During") %>% mutate(Date = "2021-06-18")

DV7 <- periods_Dungeness  %>% filter(detect_day == as.Date("2021-06-21")) %>% group_by(Transmitter) %>% mutate(periods = "After") %>% mutate(Date = "2021-06-21")
DV8 <- periods_Dungeness  %>% filter(detect_day == as.Date("2021-06-25")) %>% group_by(Transmitter) %>% mutate(periods = "After") %>% mutate(Date = "2021-06-25")
DV9 <- periods_Dungeness  %>% filter(detect_day == as.Date("2021-07-01")) %>% group_by(Transmitter) %>% mutate(periods = "After") %>% mutate(Date = "2021-07-01")

Dungeness_accel <- bind_rows(DV1, DV2, DV3, DV4, DV5, DV6, DV7, DV8, DV9)


ggplot(Dungeness_accel, aes(x=Date, y=Sensor.Value, fill=periods)) + 
  geom_violin(trim=FALSE) +  stat_summary(fun.data=mean_sdl, geom="pointrange", color="black") +
  labs(x = "Date of survey", y = "Acceleration values", title = "Dungeness Crab Acceleration by Period", fill="Period",
       caption = "Preliminary analyses")+  scale_fill_manual(values = noise_colors_repeat, breaks = period_lvl, labels = noise_labels)





#BlackR accel random days
BV1 <- periods_BlackR_accel %>% filter(detect_day == as.Date("2021-06-01")) %>% group_by(Transmitter) %>% mutate(periods = "Before") %>% mutate(Date = "2021-06-01")
BV2 <- periods_BlackR_accel  %>% filter(detect_day == as.Date("2021-06-03")) %>% group_by(Transmitter) %>% mutate(periods = "Before") %>% mutate(Date = "2021-06-03")
BV3 <- periods_BlackR_accel  %>% filter(detect_day == as.Date("2021-06-08")) %>% group_by(Transmitter) %>% mutate(periods = "Before") %>% mutate(Date = "2021-06-08")

BV4 <- periods_BlackR_accel  %>% filter(detect_day == as.Date("2021-06-11")) %>% group_by(Transmitter) %>% mutate(periods = "During") %>% mutate(Date = "2021-06-11")
BV5 <- periods_BlackR_accel  %>% filter(detect_day == as.Date("2021-06-17")) %>% group_by(Transmitter) %>% mutate(periods = "During") %>% mutate(Date = "2021-06-17")
BV6 <- periods_BlackR_accel  %>% filter(detect_day == as.Date("2021-06-18")) %>% group_by(Transmitter) %>% mutate(periods = "During") %>% mutate(Date = "2021-06-18")

BV7 <- periods_BlackR_accel  %>% filter(detect_day == as.Date("2021-06-21")) %>% group_by(Transmitter) %>% mutate(periods = "After") %>% mutate(Date = "2021-06-21")
BV8 <- periods_BlackR_accel  %>% filter(detect_day == as.Date("2021-06-25")) %>% group_by(Transmitter) %>% mutate(periods = "After") %>% mutate(Date = "2021-06-25")
BV9 <- periods_BlackR_accel  %>% filter(detect_day == as.Date("2021-07-01")) %>% group_by(Transmitter) %>% mutate(periods = "After") %>% mutate(Date = "2021-07-01")

BlackR_accel <- bind_rows(BV1, BV2, BV3, BV4, BV5, BV6, BV7, BV8, BV9)


####### BOOK CHAPTER PLOT 1
n_BRA <- BlackR_accel %>% dplyr::count(Date, Transmitter)
n_BRA <- n_BRA %>% dplyr::group_by(Date) %>% dplyr::summarise(n()) %>% dplyr::rename(number = 'n()')                                                
max_BRA <- BlackR_accel %>% dplyr::group_by(Date) %>% summarise(max_val = max(density(Sensor.Value)[[1]]) + .2)
n_BRA <- full_join(n_BRA, max_BRA, by = "Date")

ggplot(BlackR_accel, aes(x=Date, y=Sensor.Value, fill=Date)) + 
  geom_violin(trim=TRUE) +  stat_summary(fun.data=mean_sdl, geom="pointrange", color="black") +
  labs(x = "Survey period", y = expression(paste("Acceleration (m/s"^"2", ")")), title = "Black Rockfish Acceleration by Period", fill="Period")+  
  scale_fill_manual(values = noise_colors_9, breaks = noise_lvl_9, labels = noise_labels_9) + 
  scale_x_discrete(labels = noise_labels_9) +
  geom_text(data = n_BRA,
            aes(label = number,
                y = 4,
                x = Date,
                group = Date),
            position = position_dodge(width = 0.9),
            check_overlap = TRUE,
            show.legend = FALSE,
            inherit.aes = FALSE)
  


violin_BlackR_accel_9 <- ggplot(BlackR_accel, aes(x=Date, y=Sensor.Value, fill=periods)) + 
  geom_violin(trim=TRUE) +  stat_summary(fun.data=mean_sdl, geom="pointrange", color="black") +
  scale_x_discrete(limits=noise_labels_9) +
  labs(x = "Period of survey", y = "Acceleration values", title = "Black Rockfish Acceleration by Period", fill="Period")+  
  scale_fill_manual(values = noise_colors_repeat, breaks = noise_labels_9, labels = noise_labels_9)
violin_BlackR_accel

#BlackR depth random days
BDV1 <- periods_BlackR_depth %>% filter(detect_day == as.Date("2021-06-01")) %>% group_by(Transmitter) %>% mutate(periods = "Before") %>% mutate(Date = "2021-06-01")
BDV2 <- periods_BlackR_depth  %>% filter(detect_day == as.Date("2021-06-03")) %>% group_by(Transmitter) %>% mutate(periods = "Before") %>% mutate(Date = "2021-06-03")
BDV3 <- periods_BlackR_depth  %>% filter(detect_day == as.Date("2021-06-08")) %>% group_by(Transmitter) %>% mutate(periods = "Before") %>% mutate(Date = "2021-06-08")

BDV4 <- periods_BlackR_depth  %>% filter(detect_day == as.Date("2021-06-11")) %>% group_by(Transmitter) %>% mutate(periods = "During") %>% mutate(Date = "2021-06-11")
BDV5 <- periods_BlackR_depth  %>% filter(detect_day == as.Date("2021-06-17")) %>% group_by(Transmitter) %>% mutate(periods = "During") %>% mutate(Date = "2021-06-17")
BDV6 <- periods_BlackR_depth  %>% filter(detect_day == as.Date("2021-06-18")) %>% group_by(Transmitter) %>% mutate(periods = "During") %>% mutate(Date = "2021-06-18")

BDV7 <- periods_BlackR_depth  %>% filter(detect_day == as.Date("2021-06-21")) %>% group_by(Transmitter) %>% mutate(periods = "After") %>% mutate(Date = "2021-06-21")
BDV8 <- periods_BlackR_depth  %>% filter(detect_day == as.Date("2021-06-25")) %>% group_by(Transmitter) %>% mutate(periods = "After") %>% mutate(Date = "2021-06-25")
BDV9 <- periods_BlackR_depth  %>% filter(detect_day == as.Date("2021-07-01")) %>% group_by(Transmitter) %>% mutate(periods = "After") %>% mutate(Date = "2021-07-01")

BlackR_depth <- bind_rows(BDV1, BDV2, BDV3, BDV4, BDV5, BDV6, BDV7, BDV8, BDV9)


ggplot(BlackR_depth, aes(x=Date, y=Sensor.Value, fill=Date)) + 
  geom_violin(trim=TRUE) +  stat_summary(fun.data=mean_sdl, geom="pointrange", color="black") +
  labs(x = "Survey period", y = "Depth (m)", title = "Black Rockfish Depth by Period", fill="Period")+  
  scale_fill_manual(values = noise_colors_9, breaks = noise_lvl_9, labels = noise_labels_9) + 
  scale_x_discrete(labels = noise_labels_9) + scale_y_reverse()

ggplot(BlackR_accel, aes(x=Date, y=Sensor.Value, fill=periods)) + 
  geom_violin(trim=FALSE) +  stat_summary(fun.data=mean_sdl, geom="pointrange", color="black") +
  labs(x = "Date of survey", y = "Acceleration values", title = "Black Rockfish Acceleration by Period", fill="Period",
       caption = "Preliminary analyses")

#BlackR HOB random days
BHV1 <- Black %>% filter(detect_day == as.Date("2021-06-01")) %>% group_by(Transmitter) %>% mutate(periods = "Before") %>% mutate(Date = "2021-06-01")
BHV2 <- Black  %>% filter(detect_day == as.Date("2021-06-03")) %>% group_by(Transmitter) %>% mutate(periods = "Before") %>% mutate(Date = "2021-06-03")
BHV3 <- Black  %>% filter(detect_day == as.Date("2021-06-08")) %>% group_by(Transmitter) %>% mutate(periods = "Before") %>% mutate(Date = "2021-06-08")

BHV4 <- Black  %>% filter(detect_day == as.Date("2021-06-11")) %>% group_by(Transmitter) %>% mutate(periods = "During") %>% mutate(Date = "2021-06-11")
BHV5 <- Black  %>% filter(detect_day == as.Date("2021-06-17")) %>% group_by(Transmitter) %>% mutate(periods = "During") %>% mutate(Date = "2021-06-17")
BHV6 <- Black  %>% filter(detect_day == as.Date("2021-06-18")) %>% group_by(Transmitter) %>% mutate(periods = "During") %>% mutate(Date = "2021-06-18")

BHV7 <- Black  %>% filter(detect_day == as.Date("2021-06-21")) %>% group_by(Transmitter) %>% mutate(periods = "After") %>% mutate(Date = "2021-06-21")
BHV8 <- Black  %>% filter(detect_day == as.Date("2021-06-25")) %>% group_by(Transmitter) %>% mutate(periods = "After") %>% mutate(Date = "2021-06-25")
BHV9 <- Black  %>% filter(detect_day == as.Date("2021-07-01")) %>% group_by(Transmitter) %>% mutate(periods = "After") %>% mutate(Date = "2021-07-01")

BlackR_HOB9 <- bind_rows(BHV1, BHV2, BHV3, BHV4, BHV5, BHV6, BHV7, BHV8, BHV9)


ggplot(BlackR_HOB9, aes(x=Date, y=HOB, fill=Date)) + 
  geom_violin(trim=TRUE) +  stat_summary(fun.data=mean_sdl, geom="pointrange", color="black") +
  labs(x = "Survey period", y = "Height off bottom (m)", title = "Black Rockfish HOB by Period", fill="Period")+  
  scale_fill_manual(values = noise_colors_9, breaks = noise_lvl_9, labels = noise_labels_9) + 
  scale_x_discrete(labels = noise_labels_9)



#China R random days
CV1 <- periods_ChinaR %>% filter(detect_day == as.Date("2021-06-01")) %>% group_by(Transmitter) %>% mutate(periods = "Before") %>% mutate(Date = "2021-06-01")
CV2 <- periods_ChinaR  %>% filter(detect_day == as.Date("2021-06-03")) %>% group_by(Transmitter) %>% mutate(periods = "Before") %>% mutate(Date = "2021-06-03")
CV3 <- periods_ChinaR  %>% filter(detect_day == as.Date("2021-06-08")) %>% group_by(Transmitter) %>% mutate(periods = "Before") %>% mutate(Date = "2021-06-08")

CV4 <- periods_ChinaR  %>% filter(detect_day == as.Date("2021-06-11")) %>% group_by(Transmitter) %>% mutate(periods = "During") %>% mutate(Date = "2021-06-11")
CV5 <- periods_ChinaR  %>% filter(detect_day == as.Date("2021-06-17")) %>% group_by(Transmitter) %>% mutate(periods = "During") %>% mutate(Date = "2021-06-17")
CV6 <- periods_ChinaR  %>% filter(detect_day == as.Date("2021-06-18")) %>% group_by(Transmitter) %>% mutate(periods = "During") %>% mutate(Date = "2021-06-18")

CV7 <- periods_ChinaR  %>% filter(detect_day == as.Date("2021-06-21")) %>% group_by(Transmitter) %>% mutate(periods = "After") %>% mutate(Date = "2021-06-21")
CV8 <- periods_ChinaR  %>% filter(detect_day == as.Date("2021-06-25")) %>% group_by(Transmitter) %>% mutate(periods = "After") %>% mutate(Date = "2021-06-25")
CV9 <- periods_ChinaR  %>% filter(detect_day == as.Date("2021-07-01")) %>% group_by(Transmitter) %>% mutate(periods = "After") %>% mutate(Date = "2021-07-01")

ChinaR_accel <- bind_rows(CV1, CV2, CV3, CV4, CV5, CV6, CV7, CV8, CV9)

################# BOOK CHPT PLOT 3
n_CRA <- ChinaR_accel %>% dplyr::count(Date, Transmitter)
n_CRA <- n_CRA %>% dplyr::group_by(Date) %>% dplyr::summarise(n()) %>% dplyr::rename(number = 'n()')                                                
max_CRA <- ChinaR_accel %>% dplyr::group_by(Date) %>% summarise(max_val = max(density(Sensor.Value)[[1]]) + .2)
n_CRA <- full_join(n_CRA, max_CRA, by = "Date")


ggplot(ChinaR_accel, aes(x=Date, y=Sensor.Value, fill=Date)) + 
  geom_violin(trim=TRUE) +  stat_summary(fun.data=mean_sdl, geom="pointrange", color="black") +
  labs(x = "Survey period", y = expression(paste("Acceleration (m/s"^"2", ")")), title = "China Rockfish Acceleration by Period", fill="Period")+  
  scale_fill_manual(values = noise_colors_9, breaks = noise_lvl_9, labels = noise_labels_9) + 
  scale_x_discrete(labels = noise_labels_9) +
  geom_text(data = n_CRA,
            aes(label = number,
                y = 4,
                x = Date,
                group = Date),
            position = position_dodge(width = 0.9),
            check_overlap = TRUE,
            show.legend = FALSE,
            inherit.aes = FALSE)



ggplot(ChinaR_accel, aes(x=Date, y=Sensor.Value, fill=periods)) + 
  geom_violin(trim=FALSE) +  stat_summary(fun.data=mean_sdl, geom="pointrange", color="black") +
  labs(x = "Date of survey", y = "Acceleration values", title = "China Rockfish Acceleration by Period", fill="Period",
       caption = "Preliminary analyses")


#Lingcod accel random days
LV1 <- periods_Lingcod %>% filter(detect_day == as.Date("2021-06-01")) %>% group_by(Transmitter) %>% mutate(periods = "Before") %>% mutate(Date = "2021-06-01")
LV2 <- periods_Lingcod  %>% filter(detect_day == as.Date("2021-06-03")) %>% group_by(Transmitter) %>% mutate(periods = "Before") %>% mutate(Date = "2021-06-03")
LV3 <- periods_Lingcod  %>% filter(detect_day == as.Date("2021-06-08")) %>% group_by(Transmitter) %>% mutate(periods = "Before") %>% mutate(Date = "2021-06-08")

LV4 <- periods_Lingcod  %>% filter(survey.period == "June 11") %>% group_by(Transmitter) %>% mutate(periods = "During") %>% mutate(Date = "2021-06-11")
LV5 <- periods_Lingcod  %>% filter(survey.period == "June 17") %>% group_by(Transmitter) %>% mutate(periods = "During") %>% mutate(Date = "2021-06-17")
LV6 <- periods_Lingcod  %>% filter(survey.period == "June 18") %>% group_by(Transmitter) %>% mutate(periods = "During") %>% mutate(Date = "2021-06-18")

LV7 <- periods_Lingcod  %>% filter(detect_day == as.Date("2021-06-21")) %>% group_by(Transmitter) %>% mutate(periods = "After") %>% mutate(Date = "2021-06-21")
LV8 <- periods_Lingcod  %>% filter(detect_day == as.Date("2021-06-25")) %>% group_by(Transmitter) %>% mutate(periods = "After") %>% mutate(Date = "2021-06-25")
LV9 <- periods_Lingcod  %>% filter(detect_day == as.Date("2021-07-01")) %>% group_by(Transmitter) %>% mutate(periods = "After") %>% mutate(Date = "2021-07-01")

Lingcod_accel <- bind_rows(LV1, LV2, LV3, LV4, LV5, LV6, LV7, LV8, LV9)


ggplot(Lingcod_accel, aes(x=Date, y=Sensor.Value, fill=periods)) + 
  geom_violin(trim=TRUE) +
  labs(x = "Date of survey", y = "Acceleration values", title = "Lingcod Acceleration by Period", fill="Period",
       caption = "Preliminary analyses")



#accel June 9 - 18########################################
BR_June <- periods_BlackR_accel_noise %>% filter(detect_day >= as.Date("2021-06-09") & detect_day <= as.Date("2021-06-18"))
CR_June <- periods_ChinaR_noise %>% filter(detect_day >= as.Date("2021-06-09") & detect_day <= as.Date("2021-06-18"))
Lingcod_June <- periods_Lingcod_noise %>% filter(detect_day >= as.Date("2021-06-09") & detect_day <= as.Date("2021-06-18"))
Dungeness_June <- periods_Dungeness_noise %>% filter(detect_day >= as.Date("2021-06-09") & detect_day <= as.Date("2021-06-18"))
#add sound data to this tibble
#BR_June <- inner_join(___________, BR_June, by = "Date.time.UTC") #join by 30 second rounding 
#CR_June <- inner_join(___________, CR_June, by = "Date.time.UTC") #join by 30 second rounding
#Lingcod_June <- inner_join(___________, Lingcod_June, by = "Date.time.UTC") #join by 30 second rounding
#Dungeness_June <- inner_join(___________, Dungeness_June, by = "Date.time.UTC") #join by 30 second rounding



#round times in acceleration data sets to combine with Noise Spotter data and SBE
Dungeness_accel$Date.time.UTC <- round_date(Dungeness_accel$Date.time.UTC, "30 seconds")
Lingcod_accel$Date.time.UTC <- round_date(Lingcod_accel$Date.time.UTC, "30 seconds")
BlackR_accel$Date.time.UTC <- round_date(BlackR_accel$Date.time.UTC, "30 seconds")
ChinaR_accel$Date.time.UTC <- round_date(ChinaR_accel$Date.time.UTC, "30 seconds")
BlackR_HOB9$Date.time.UTC <- round_date(BlackR_HOB9$Date.time.UTC, "30 seconds")

Dungeness_accel <- full_join(Port_O_SBE, Dungeness_accel, by = "Date.time.UTC")
Lingcod_accel <- full_join(Port_O_SBE, Lingcod_accel, by = "Date.time.UTC")
BlackR_accel <- full_join(Port_O_SBE, BlackR_accel, by = "Date.time.UTC")
ChinaR_accel <- full_join(Port_O_SBE, ChinaR_accel, by = "Date.time.UTC")
BlackR_HOB9 <- full_join(Port_O_SBE, BlackR_HOB9, by = "Date.time.UTC")



####################
#create example plotting for all accelerometer data for all species
ex_plot <- ggplot(plot_dat, aes(Date.time.UTC, Sensor.Value, color = Tag.Destination)) + 
  geom_point()
ex_plot + facet_grid( ~ .Tag.Destination)


#Lingcod quick statistics
t.test(Sensor.Value ~ survey.period, alternative = "two.sided", data = periods_Lingcod)