library(tidyverse)
library(glatos)
library(lubridate)
library(gghighlight)
library(rstatix)
library(lme4)
library(gamm4)
library(car)
library(hms)

#Upload tagsheet
tagsheet <- as_tibble(read.csv("D:\\MS research\\RRMR2021ReceiverLogs\\RRMRTagSheet.csv")) %>%
  rename(Transmitter = VUE.Tag.ID)

#new variable containing transmitter number and corresponding type of animal
animal_transmit <- dplyr::select(tagsheet, Transmitter, Tag.Destination)

#Assign prelim transmitter (animal) logs to variables in the environment
A_12048 <- as_tibble(read.csv("D:\\MS research\\RRMR2021ReceiverLogs\\VUE_export_final\\A69-9007-12048.csv", 
                              na.strings = c("Sensor Fault"))) %>%
  rename(Date.time.UTC = ï..Date.and.Time..UTC.) %>%
  mutate(tag.type = "acceleration")

A_12049 <- as_tibble(read.csv("D:\\MS research\\RRMR2021ReceiverLogs\\VUE_export_final\\A69-9007-12049.csv", 
                              na.strings = c("Sensor Fault"))) %>%
  rename(Date.time.UTC = ï..Date.and.Time..UTC.) %>%
  mutate(tag.type = "depth")

A_12050 <- as_tibble(read.csv("D:\\MS research\\RRMR2021ReceiverLogs\\VUE_export_final\\A69-9007-12050.csv", 
                              na.strings = c("Sensor Fault"))) %>%
  rename(Date.time.UTC = ï..Date.and.Time..UTC.) %>%
  mutate(tag.type = "acceleration")

A_12051 <- as_tibble(read.csv("D:\\MS research\\RRMR2021ReceiverLogs\\VUE_export_final\\A69-9007-12051.csv", 
                              na.strings = c("Sensor Fault"))) %>% 
  rename(Date.time.UTC = ï..Date.and.Time..UTC.) %>%
  mutate(tag.type = "depth")

A_12052 <- as_tibble(read.csv("D:\\MS research\\RRMR2021ReceiverLogs\\VUE_export_final\\A69-9007-12052.csv", 
                              na.strings = c("Sensor Fault"))) %>% 
  rename(Date.time.UTC = ï..Date.and.Time..UTC.) %>%
  mutate(tag.type = "acceleration")

A_12053 <- as_tibble(read.csv("D:\\MS research\\RRMR2021ReceiverLogs\\VUE_export_final\\A69-9007-12053.csv", 
                              na.strings = c("Sensor Fault"))) %>% 
  rename(Date.time.UTC = ï..Date.and.Time..UTC.) %>%
  mutate(tag.type = "depth")

A_12054 <- as_tibble(read.csv("D:\\MS research\\RRMR2021ReceiverLogs\\VUE_export_final\\A69-9007-12054.csv", 
                              na.strings = c("Sensor Fault"))) %>% 
  rename(Date.time.UTC = ï..Date.and.Time..UTC.) %>%
  mutate(tag.type = "acceleration")

A_12055 <- as_tibble(read.csv("D:\\MS research\\RRMR2021ReceiverLogs\\VUE_export_final\\A69-9007-12055.csv", 
                              na.strings = c("Sensor Fault"))) %>% 
  rename(Date.time.UTC = ï..Date.and.Time..UTC.) %>%
  mutate(tag.type = "depth")

A_12056 <- as_tibble(read.csv("D:\\MS research\\RRMR2021ReceiverLogs\\VUE_export_final\\A69-9007-12056.csv", 
                              na.strings = c("Sensor Fault"))) %>% 
  rename(Date.time.UTC = ï..Date.and.Time..UTC.) %>%
  mutate(tag.type = "acceleration")

A_12057 <- as_tibble(read.csv("D:\\MS research\\RRMR2021ReceiverLogs\\VUE_export_final\\A69-9007-12057.csv", 
                              na.strings = c("Sensor Fault"))) %>% 
  rename(Date.time.UTC = ï..Date.and.Time..UTC.) %>%
  mutate(tag.type = "depth")

A_12058<- as_tibble(read.csv("D:\\MS research\\RRMR2021ReceiverLogs\\VUE_export_final\\A69-9007-12058.csv", 
                             na.strings = c("Sensor Fault"))) %>% 
  rename(Date.time.UTC = ï..Date.and.Time..UTC.) %>%
  mutate(tag.type = "acceleration")

A_12059 <- as_tibble(read.csv("D:\\MS research\\RRMR2021ReceiverLogs\\VUE_export_final\\A69-9007-12059.csv", 
                              na.strings = c("Sensor Fault"))) %>% 
  rename(Date.time.UTC = ï..Date.and.Time..UTC.) %>%
  mutate(tag.type = "depth")

A_12060 <- as_tibble(read.csv("D:\\MS research\\RRMR2021ReceiverLogs\\VUE_export_final\\A69-9007-12060.csv", 
                              na.strings = c("Sensor Fault"))) %>% 
  rename(Date.time.UTC = ï..Date.and.Time..UTC.) %>%
  mutate(tag.type = "acceleration")

A_12061 <- as_tibble(read.csv("D:\\MS research\\RRMR2021ReceiverLogs\\VUE_export_final\\A69-9007-12061.csv", 
                              na.strings = c("Sensor Fault"))) %>% 
  rename(Date.time.UTC = ï..Date.and.Time..UTC.) %>%
  mutate(tag.type = "depth")

A_12062 <- as_tibble(read.csv("D:\\MS research\\RRMR2021ReceiverLogs\\VUE_export_final\\A69-9007-12062.csv", 
                              na.strings = c("Sensor Fault"))) %>% 
  rename(Date.time.UTC = ï..Date.and.Time..UTC.) %>%
  mutate(tag.type = "acceleration")

A_12063 <- as_tibble(read.csv("D:\\MS research\\RRMR2021ReceiverLogs\\VUE_export_final\\A69-9007-12063.csv", 
                              na.strings = c("Sensor Fault"))) %>% 
  rename(Date.time.UTC = ï..Date.and.Time..UTC.) %>%
  mutate(tag.type = "depth")

A_12064 <- as_tibble(read.csv("D:\\MS research\\RRMR2021ReceiverLogs\\VUE_export_final\\A69-9007-12064.csv", 
                              na.strings = c("Sensor Fault"))) %>% 
  rename(Date.time.UTC = ï..Date.and.Time..UTC.) %>%
  mutate(tag.type = "acceleration")

A_12065 <- as_tibble(read.csv("D:\\MS research\\RRMR2021ReceiverLogs\\VUE_export_final\\A69-9007-12065.csv", 
                              na.strings = c("Sensor Fault"))) %>% 
  rename(Date.time.UTC = ï..Date.and.Time..UTC.) %>%
  mutate(tag.type = "depth")

A_12066 <- as_tibble(read.csv("D:\\MS research\\RRMR2021ReceiverLogs\\VUE_export_final\\A69-9007-12066.csv", 
                              na.strings = c("Sensor Fault"))) %>% 
  rename(Date.time.UTC = ï..Date.and.Time..UTC.) %>%
  mutate(tag.type = "acceleration")

A_12067 <- as_tibble(read.csv("D:\\MS research\\RRMR2021ReceiverLogs\\VUE_export_final\\A69-9007-12067.csv", 
                              na.strings = c("Sensor Fault"))) %>% 
  rename(Date.time.UTC = ï..Date.and.Time..UTC.) %>%
  mutate(tag.type = "depth")

A_12068 <- as_tibble(read.csv("D:\\MS research\\RRMR2021ReceiverLogs\\VUE_export_final\\A69-9007-12068.csv", 
                              na.strings = c("Sensor Fault"))) %>% 
  rename(Date.time.UTC = ï..Date.and.Time..UTC.) %>%
  mutate(tag.type = "acceleration")

A_12069 <- as_tibble(read.csv("D:\\MS research\\RRMR2021ReceiverLogs\\VUE_export_final\\A69-9007-12069.csv", 
                              na.strings = c("Sensor Fault"))) %>% 
  rename(Date.time.UTC = ï..Date.and.Time..UTC.) %>%
  mutate(tag.type = "depth")

A_12070 <- as_tibble(read.csv("D:\\MS research\\RRMR2021ReceiverLogs\\VUE_export_final\\A69-9007-12070.csv", 
                              na.strings = c("Sensor Fault"))) %>% 
  rename(Date.time.UTC = ï..Date.and.Time..UTC.) %>%
  mutate(tag.type = "acceleration")

A_12071 <- as_tibble(read.csv("D:\\MS research\\RRMR2021ReceiverLogs\\VUE_export_final\\A69-9007-12071.csv", 
                              na.strings = c("Sensor Fault"))) %>% 
  rename(Date.time.UTC = ï..Date.and.Time..UTC.) %>%
  mutate(tag.type = "depth")

A_12072 <- as_tibble(read.csv("D:\\MS research\\RRMR2021ReceiverLogs\\VUE_export_final\\A69-9007-12072.csv", 
                              na.strings = c("Sensor Fault"))) %>% 
  rename(Date.time.UTC = ï..Date.and.Time..UTC.) %>%
  mutate(tag.type = "acceleration")

A_12073 <- as_tibble(read.csv("D:\\MS research\\RRMR2021ReceiverLogs\\VUE_export_final\\A69-9007-12073.csv", 
                              na.strings = c("Sensor Fault"))) %>% 
  rename(Date.time.UTC = ï..Date.and.Time..UTC.) %>%
  mutate(tag.type = "depth")

A_12074 <- as_tibble(read.csv("D:\\MS research\\RRMR2021ReceiverLogs\\VUE_export_final\\A69-9007-12074.csv", 
                              na.strings = c("Sensor Fault"))) %>% 
  rename(Date.time.UTC = ï..Date.and.Time..UTC.) %>%
  mutate(tag.type = "acceleration")

A_12075 <- as_tibble(read.csv("D:\\MS research\\RRMR2021ReceiverLogs\\VUE_export_final\\A69-9007-12075.csv", 
                              na.strings = c("Sensor Fault"))) %>% 
  rename(Date.time.UTC = ï..Date.and.Time..UTC.) %>%
  mutate(tag.type = "depth")

A_12076 <- as_tibble(read.csv("D:\\MS research\\RRMR2021ReceiverLogs\\VUE_export_final\\A69-9007-12076.csv", 
                              na.strings = c("Sensor Fault"))) %>% 
  rename(Date.time.UTC = ï..Date.and.Time..UTC.) %>%
  mutate(tag.type = "acceleration")

A_12077 <- as_tibble(read.csv("D:\\MS research\\RRMR2021ReceiverLogs\\VUE_export_final\\A69-9007-12077.csv", 
                              na.strings = c("Sensor Fault"))) %>% 
  rename(Date.time.UTC = ï..Date.and.Time..UTC.) %>%
  mutate(tag.type = "depth")



A_13249 <- as_tibble(read.csv("D:\\MS research\\RRMR2021ReceiverLogs\\VUE_export_final\\A69-9007-13249.csv", 
                              na.strings = c("Sensor Fault"))) %>% 
  rename(Date.time.UTC = ï..Date.and.Time..UTC.)

A_13250 <- as_tibble(read.csv("D:\\MS research\\RRMR2021ReceiverLogs\\VUE_export_final\\A69-9007-13250.csv", 
                              na.strings = c("Sensor Fault"))) %>% 
  rename(Date.time.UTC = ï..Date.and.Time..UTC.)

A_13251 <- as_tibble(read.csv("D:\\MS research\\RRMR2021ReceiverLogs\\VUE_export_final\\A69-9007-13251.csv", 
                              na.strings = c("Sensor Fault"))) %>% 
  rename(Date.time.UTC = ï..Date.and.Time..UTC.)

A_13252 <- as_tibble(read.csv("D:\\MS research\\RRMR2021ReceiverLogs\\VUE_export_final\\A69-9007-13252.csv", 
                              na.strings = c("Sensor Fault"))) %>% 
  rename(Date.time.UTC = ï..Date.and.Time..UTC.)

A_13253 <- as_tibble(read.csv("D:\\MS research\\RRMR2021ReceiverLogs\\VUE_export_final\\A69-9007-13253.csv", 
                              na.strings = c("Sensor Fault"))) %>% 
  rename(Date.time.UTC = ï..Date.and.Time..UTC.)

A_13254 <- as_tibble(read.csv("D:\\MS research\\RRMR2021ReceiverLogs\\VUE_export_final\\A69-9007-13254.csv", 
                              na.strings = c("Sensor Fault"))) %>% 
  rename(Date.time.UTC = ï..Date.and.Time..UTC.)

A_13255 <- as_tibble(read.csv("D:\\MS research\\RRMR2021ReceiverLogs\\VUE_export_final\\A69-9007-13255.csv", 
                              na.strings = c("Sensor Fault"))) %>% 
  rename(Date.time.UTC = ï..Date.and.Time..UTC.)

A_13256 <- as_tibble(read.csv("D:\\MS research\\RRMR2021ReceiverLogs\\VUE_export_final\\A69-9007-13256.csv", 
                              na.strings = c("Sensor Fault"))) %>% 
  rename(Date.time.UTC = ï..Date.and.Time..UTC.)

A_13257 <- as_tibble(read.csv("D:\\MS research\\RRMR2021ReceiverLogs\\VUE_export_final\\A69-9007-13257.csv", 
                              na.strings = c("Sensor Fault"))) %>% 
  rename(Date.time.UTC = ï..Date.and.Time..UTC.)

A_13258 <- as_tibble(read.csv("D:\\MS research\\RRMR2021ReceiverLogs\\VUE_export_final\\A69-9007-13258.csv", 
                              na.strings = c("Sensor Fault"))) %>% 
  rename(Date.time.UTC = ï..Date.and.Time..UTC.)

A_13259 <- as_tibble(read.csv("D:\\MS research\\RRMR2021ReceiverLogs\\VUE_export_final\\A69-9007-13259.csv", 
                              na.strings = c("Sensor Fault"))) %>% 
  rename(Date.time.UTC = ï..Date.and.Time..UTC.)

A_13260 <- as_tibble(read.csv("D:\\MS research\\RRMR2021ReceiverLogs\\VUE_export_final\\A69-9007-13260.csv", 
                              na.strings = c("Sensor Fault"))) %>% 
  rename(Date.time.UTC = ï..Date.and.Time..UTC.)

A_13261 <- as_tibble(read.csv("D:\\MS research\\RRMR2021ReceiverLogs\\VUE_export_final\\A69-9007-13261.csv", 
                              na.strings = c("Sensor Fault"))) %>% 
  rename(Date.time.UTC = ï..Date.and.Time..UTC.)

A_13262 <- as_tibble(read.csv("D:\\MS research\\RRMR2021ReceiverLogs\\VUE_export_final\\A69-9007-13262.csv", 
                              na.strings = c("Sensor Fault"))) %>% 
  rename(Date.time.UTC = ï..Date.and.Time..UTC.)

A_13263 <- as_tibble(read.csv("D:\\MS research\\RRMR2021ReceiverLogs\\VUE_export_final\\A69-9007-13263.csv", 
                              na.strings = c("Sensor Fault"))) %>% 
  rename(Date.time.UTC = ï..Date.and.Time..UTC.)

A_13264 <- as_tibble(read.csv("D:\\MS research\\RRMR2021ReceiverLogs\\VUE_export_final\\A69-9007-13264.csv", 
                              na.strings = c("Sensor Fault"))) %>% 
  rename(Date.time.UTC = ï..Date.and.Time..UTC.)

A_13265 <- as_tibble(read.csv("D:\\MS research\\RRMR2021ReceiverLogs\\VUE_export_final\\A69-9007-13265.csv", 
                              na.strings = c("Sensor Fault"))) %>% 
  rename(Date.time.UTC = ï..Date.and.Time..UTC.)

A_13266 <- as_tibble(read.csv("D:\\MS research\\RRMR2021ReceiverLogs\\VUE_export_final\\A69-9007-13266.csv", 
                              na.strings = c("Sensor Fault"))) %>% 
  rename(Date.time.UTC = ï..Date.and.Time..UTC.)

A_13267 <- as_tibble(read.csv("D:\\MS research\\RRMR2021ReceiverLogs\\VUE_export_final\\A69-9007-13267.csv", 
                              na.strings = c("Sensor Fault"))) %>% 
  rename(Date.time.UTC = ï..Date.and.Time..UTC.)

A_13268 <- as_tibble(read.csv("D:\\MS research\\RRMR2021ReceiverLogs\\VUE_export_final\\A69-9007-13268.csv", 
                              na.strings = c("Sensor Fault"))) %>% 
  rename(Date.time.UTC = ï..Date.and.Time..UTC.)

A_13269 <- as_tibble(read.csv("D:\\MS research\\RRMR2021ReceiverLogs\\VUE_export_final\\A69-9007-13269.csv", 
                              na.strings = c("Sensor Fault"))) %>% 
  rename(Date.time.UTC = ï..Date.and.Time..UTC.)

A_13270 <- as_tibble(read.csv("D:\\MS research\\RRMR2021ReceiverLogs\\VUE_export_final\\A69-9007-13270.csv", 
                              na.strings = c("Sensor Fault"))) %>% 
  rename(Date.time.UTC = ï..Date.and.Time..UTC.)

A_13271 <- as_tibble(read.csv("D:\\MS research\\RRMR2021ReceiverLogs\\VUE_export_final\\A69-9007-13271.csv", 
                              na.strings = c("Sensor Fault"))) %>% 
  rename(Date.time.UTC = ï..Date.and.Time..UTC.)

A_13272 <- as_tibble(read.csv("D:\\MS research\\RRMR2021ReceiverLogs\\VUE_export_final\\A69-9007-13272.csv", 
                              na.strings = c("Sensor Fault"))) %>% 
  rename(Date.time.UTC = ï..Date.and.Time..UTC.)

A_13273 <- as_tibble(read.csv("D:\\MS research\\RRMR2021ReceiverLogs\\VUE_export_final\\A69-9007-13273.csv", 
                              na.strings = c("Sensor Fault"))) %>% 
  rename(Date.time.UTC = ï..Date.and.Time..UTC.)

A_13274 <- as_tibble(read.csv("D:\\MS research\\RRMR2021ReceiverLogs\\VUE_export_final\\A69-9007-13274.csv", 
                              na.strings = c("Sensor Fault"))) %>% 
  rename(Date.time.UTC = ï..Date.and.Time..UTC.)

A_13275 <- as_tibble(read.csv("D:\\MS research\\RRMR2021ReceiverLogs\\VUE_export_final\\A69-9007-13275.csv", 
                              na.strings = c("Sensor Fault"))) %>% 
  rename(Date.time.UTC = ï..Date.and.Time..UTC.)

A_13276 <- as_tibble(read.csv("D:\\MS research\\RRMR2021ReceiverLogs\\VUE_export_final\\A69-9007-13276.csv", 
                              na.strings = c("Sensor Fault"))) %>% 
  rename(Date.time.UTC = ï..Date.and.Time..UTC.)

A_13277 <- as_tibble(read.csv("D:\\MS research\\RRMR2021ReceiverLogs\\VUE_export_final\\A69-9007-13277.csv", 
                              na.strings = c("Sensor Fault"))) %>% 
  rename(Date.time.UTC = ï..Date.and.Time..UTC.)

A_13278 <- as_tibble(read.csv("D:\\MS research\\RRMR2021ReceiverLogs\\VUE_export_final\\A69-9007-13278.csv", 
                              na.strings = c("Sensor Fault"))) %>% 
  rename(Date.time.UTC = ï..Date.and.Time..UTC.)

A_13279 <- as_tibble(read.csv("D:\\MS research\\RRMR2021ReceiverLogs\\VUE_export_final\\A69-9007-13279.csv", 
                              na.strings = c("Sensor Fault"))) %>% 
  rename(Date.time.UTC = ï..Date.and.Time..UTC.)

A_13280 <- as_tibble(read.csv("D:\\MS research\\RRMR2021ReceiverLogs\\VUE_export_final\\A69-9007-13280.csv", 
                              na.strings = c("Sensor Fault"))) %>% 
  rename(Date.time.UTC = ï..Date.and.Time..UTC.)

A_13281 <- as_tibble(read.csv("D:\\MS research\\RRMR2021ReceiverLogs\\VUE_export_final\\A69-9007-13281.csv", 
                              na.strings = c("Sensor Fault"))) %>% 
  rename(Date.time.UTC = ï..Date.and.Time..UTC.)

A_13282 <- as_tibble(read.csv("D:\\MS research\\RRMR2021ReceiverLogs\\VUE_export_final\\A69-9007-13282.csv", 
                              na.strings = c("Sensor Fault"))) %>% 
  rename(Date.time.UTC = ï..Date.and.Time..UTC.)

A_13283 <- as_tibble(read.csv("D:\\MS research\\RRMR2021ReceiverLogs\\VUE_export_final\\A69-9007-13283.csv", 
                              na.strings = c("Sensor Fault"))) %>% 
  rename(Date.time.UTC = ï..Date.and.Time..UTC.)

A_13284 <- as_tibble(read.csv("D:\\MS research\\RRMR2021ReceiverLogs\\VUE_export_final\\A69-9007-13284.csv", 
                              na.strings = c("Sensor Fault"))) %>% 
  rename(Date.time.UTC = ï..Date.and.Time..UTC.)

A_13285 <- as_tibble(read.csv("D:\\MS research\\RRMR2021ReceiverLogs\\VUE_export_final\\A69-9007-13285.csv", 
                              na.strings = c("Sensor Fault"))) %>% 
  rename(Date.time.UTC = ï..Date.and.Time..UTC.)

A_13286 <- as_tibble(read.csv("D:\\MS research\\RRMR2021ReceiverLogs\\VUE_export_final\\A69-9007-13286.csv", 
                              na.strings = c("Sensor Fault"))) %>% 
  rename(Date.time.UTC = ï..Date.and.Time..UTC.)

A_13287 <- as_tibble(read.csv("D:\\MS research\\RRMR2021ReceiverLogs\\VUE_export_final\\A69-9007-13287.csv", 
                              na.strings = c("Sensor Fault"))) %>% 
  rename(Date.time.UTC = ï..Date.and.Time..UTC.)

A_13289 <- as_tibble(read.csv("D:\\MS research\\RRMR2021ReceiverLogs\\VUE_export_final\\A69-9007-13289.csv", 
                              na.strings = c("Sensor Fault"))) %>% 
  rename(Date.time.UTC = ï..Date.and.Time..UTC.)

A_13290 <- as_tibble(read.csv("D:\\MS research\\RRMR2021ReceiverLogs\\VUE_export_final\\A69-9007-13290.csv", 
                              na.strings = c("Sensor Fault"))) %>% 
  rename(Date.time.UTC = ï..Date.and.Time..UTC.)

A_13291 <- as_tibble(read.csv("D:\\MS research\\RRMR2021ReceiverLogs\\VUE_export_final\\A69-9007-13291.csv", 
                              na.strings = c("Sensor Fault"))) %>% 
  rename(Date.time.UTC = ï..Date.and.Time..UTC.)

A_13292 <- as_tibble(read.csv("D:\\MS research\\RRMR2021ReceiverLogs\\VUE_export_final\\A69-9007-13292.csv", 
                              na.strings = c("Sensor Fault"))) %>% 
  rename(Date.time.UTC = ï..Date.and.Time..UTC.)

A_13293 <- as_tibble(read.csv("D:\\MS research\\RRMR2021ReceiverLogs\\VUE_export_final\\A69-9007-13293.csv", 
                              na.strings = c("Sensor Fault"))) %>% 
  rename(Date.time.UTC = ï..Date.and.Time..UTC.)


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


#ggploting example by species for accelerometer data: highlight preliminary dates for survey passes
#PRELIMINARY - tell R the time range of seismic survey
June11_start <- ymd_hms("2021-06-11 00:00:01", tz = "US/Pacific")
June11_end <- ymd_hms("2021-06-11 23:59:59", tz = "US/Pacific")



#example plotting looking at other estimated timeframes of seismic survey booms
#variables containing days of preliminary seismic boom detections
June16_start <- ymd_hms("2021-06-16 00:00:01", tz = "US/Pacific")
June16_end <- ymd_hms("2021-06-16 23:59:59", tz = "US/Pacific")

June17_start <- ymd_hms("2021-06-17 00:00:01", tz = "US/Pacific")
June17_end <- ymd_hms("2021-06-17 23:59:59", tz = "US/Pacific")

June18_start <- ymd_hms("2021-06-18 00:00:01", tz = "US/Pacific")
June18_end <- ymd_hms("2021-06-18 23:59:59", tz = "US/Pacific")

July11_end <- ymd_hms("2021-07-11 23:59:59", tz = "US/Pacific")

###################################################################################################
#adding "period" categorizations to data (need to bin as the resolution is too fine to be visible on a plot)
periods_BlackR_accel <- dat_BlackR_accel %>% mutate(survey.period = case_when(Date.time.UTC < June11_start ~ "May 20-June 10",
                                                                  Date.time.UTC >= June11_start & Date.time.UTC <= June11_end ~ "June 11",
                                                                  Date.time.UTC > June11_end & Date.time.UTC < June16_start ~ "June 12-16",
                                                                  Date.time.UTC >= June16_start & Date.time.UTC <= June16_end ~ "June 16",
                                                                  Date.time.UTC >= June17_start & Date.time.UTC <= June17_end ~ "June 17",
                                                                  Date.time.UTC >= June18_start & Date.time.UTC <= June18_end ~ "June 18",
                                                                  Date.time.UTC > June18_end & Date.time.UTC <= July11_end ~ "June 19-July 11"))

periods_BlackR_depth <- dat_BlackR_depth %>% mutate(survey.period = case_when(Date.time.UTC < June11_start ~ "May 20-June 10",
                                                                              Date.time.UTC >= June11_start & Date.time.UTC <= June11_end ~ "June 11",
                                                                              Date.time.UTC > June11_end & Date.time.UTC < June16_start ~ "June 12-16",
                                                                              Date.time.UTC >= June16_start & Date.time.UTC <= June16_end ~ "June 16",
                                                                              Date.time.UTC >= June17_start & Date.time.UTC <= June17_end ~ "June 17",
                                                                              Date.time.UTC >= June18_start & Date.time.UTC <= June18_end ~ "June 18",
                                                                              Date.time.UTC > June18_end & Date.time.UTC <= July11_end ~ "June 19-July 11"))


periods_ChinaR <- dat_ChinaR %>% mutate(survey.period = case_when(Date.time.UTC < June11_start ~ "May 20-June 10",
                                                                  Date.time.UTC >= June11_start & Date.time.UTC <= June11_end ~ "June 11",
                                                                  Date.time.UTC > June11_end & Date.time.UTC < June16_start ~ "June 12-16",
                                                                  Date.time.UTC >= June16_start & Date.time.UTC <= June16_end ~ "June 16",
                                                                  Date.time.UTC >= June17_start & Date.time.UTC <= June17_end ~ "June 17",
                                                                  Date.time.UTC >= June18_start & Date.time.UTC <= June18_end ~ "June 18",
                                                                  Date.time.UTC > June18_end & Date.time.UTC <= July11_end ~ "June 19-July 11"))

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


#convert to posixct time type, convert observation times to US Pacific time zone
periods_Dungeness$Date.time.UTC <- as.POSIXct(strptime(periods_Dungeness$Date.time.UTC, "%Y-%m-%d %H:%M:%S"))
periods_Dungeness$Date.time.UTC <- with_tz(periods_Dungeness$Date.time.UTC, tzone = "US/Pacific")

periods_Lingcod$Date.time.UTC <- as.POSIXct(strptime(periods_Lingcod$Date.time.UTC, "%Y-%m-%d %H:%M:%S"))
periods_Lingcod$Date.time.UTC <- with_tz(periods_Lingcod$Date.time.UTC, tzone = "US/Pacific")

periods_BlackR_accel$Date.time.UTC <- as.POSIXct(strptime(periods_BlackR_accel$Date.time.UTC, "%Y-%m-%d %H:%M:%S"))
periods_BlackR_accel$Date.time.UTC <- with_tz(periods_BlackR_accel$Date.time.UTC, tzone = "US/Pacific")

periods_BlackR_depth$Date.time.UTC <- as.POSIXct(strptime(periods_BlackR_depth$Date.time.UTC, "%Y-%m-%d %H:%M:%S"))
periods_BlackR_depth$Date.time.UTC <- with_tz(periods_BlackR_depth$Date.time.UTC, tzone = "US/Pacific")

periods_ChinaR$Date.time.UTC <- as.POSIXct(strptime(periods_ChinaR$Date.time.UTC, "%Y-%m-%d %H:%M:%S"))
periods_ChinaR$Date.time.UTC <- with_tz(periods_ChinaR$Date.time.UTC, tzone = "US/Pacific")

#add column of minimum time based on detections, by individual
#periods_Dungeness <- periods_Dungeness %>% group_by(Transmitter) %>% mutate(time.min = min(Date.time.UTC))
#periods_Lingcod <- periods_Lingcod %>% group_by(Transmitter) %>% mutate(time.min = min(Date.time.UTC))


#find minimum (earliest) time for each transmitter to feed to time since release
#add first time to each row by individual/tag
#use difftime for time after release
#periods_Dungeness$TSR <- difftime(periods_Dungeness$Date.time.UTC, periods_Dungeness$time.min, units = "mins")
#periods_Lingcod$TSR <- difftime(periods_Lingcod$Date.time.UTC, periods_Lingcod$time.min, units = "mins")




#create vector of coarse survey times
survey_times <- c(June11_start, June11_end, June16_start, June16_end, June17_start, June17_end, June18_start, June18_end, 
                  July11_end)

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

ggplot(periods_ChinaR, aes(Date.time.UTC, Sensor.Value)) + 
  geom_point() + facet_grid(rows = vars(Transmitter)) +
  labs(x = "Time", y = "Acceleration values", title = "China rockfish acceleration over time", 
       caption = "Preliminary analyses")

#remove observations of transmitters with too low sample size/observations
##China R: 13265
periods_ChinaR <- periods_ChinaR %>% filter(!(Transmitter == "A69-9007-13265"))

##Black R accel: 12072
periods_BlackR_accel <- periods_BlackR_accel %>% filter(!(Transmitter == "A69-9007-12072"))

##Lingcod: 13250, 13262
periods_Lingcod <- periods_Lingcod %>% filter(!(Transmitter == "A69-9007-13250" | Transmitter == "A69-9007-13262"))

##Dungeness: 13293
periods_Dungeness <- periods_Dungeness %>% filter(!(Transmitter == "A69-9007-13293"))




##vector of generic sunrise to sunset
Port_O_Sun <- read.csv("D:\\MS research\\RRMR2021ReceiverLogs\\NOAA Port Orford tides\\NOAA_PortO_NightDay2021.csv")
Port_O_Sun <- Port_O_Sun %>% rename(Date = ï..Date)

## HERE __________________________________________________________________
Port_O_Sun$Sunrise.Time <- as.POSIXct(Port_O_Sun$Sunrise.Time, format = "%H:%M")
Port_O_Sun$Sunset.Time <- as.POSIXct(Port_O_Sun$Sunset.Time, format = "%H:%M")
Port_O_Sun$Sunrise.Time <- with_tz(Port_O_Sun$Sunrise.Time, tzone = "US/Pacific")
Port_O_Sun$Sunset.Time <- with_tz(Port_O_Sun$Sunset.Time, tzone = "US/Pacific")

Port_O_Sun$Date <- mdy(Port_O_Sun$Date)
Port_O_Sun <- as_tibble(Port_O_Sun)

Port_O_Sun$Sunrise.Time <- as.POSIXct(Port_O_Sun$Sunrise.Time, format = "%H:%M")
Port_O_Sun$Sunset.Time <- as.POSIXct(Port_O_Sun$Sunset.Time, format = "%H:%M")
Port_O_Sun$Sunrise.Time <- with_tz(Port_O_Sun$Sunrise.Time, tzone = "US/Pacific")
Port_O_Sun$Sunset.Time <- with_tz(Port_O_Sun$Sunset.Time, tzone = "US/Pacific")
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
periods_ChinaR <- periods_ChinaR %>% mutate(coarse.period = case_when(survey.period == "May 20-June 10" ~ "Before",
                                                                            survey.period == "June 11" | survey.period == "June 12-16" 
                                                                            | survey.period == "June 16" | survey.period == "June 17" 
                                                                            | survey.period == "June 18" ~ "During",
                                                                            survey.period == "June 19-July 11" ~ "After"))


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


##example to evaluate function 
ex_date <- periods_Dungeness %>% select(detect_day) %>% slice(1)
ex_time <- periods_Dungeness %>% select(detect_time) %>% slice(1)
subset(Port_O_Sun, as.Date(Port_O_Sun$Date) %in% ex_date)


case_day_night <- function(fishdate, fishtime) {
  
  exdate <- subset(Port_O_Sun, as.Date(Port_O_Sun$Date) %in% fishdate)
  sunrise <- exdate$Sunrise.Time
  sunset <- exdate$Sunset.Time
  daynight <- case_when(
                   fishtime < sunrise | fishtime >= sunset ~ "Night",
                   fishtime >= sunrise & fishtime < sunset ~ "Day"
               )
  return(daynight)
}


case_day_night <- function(fishdate, fishtime) {
  
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

periods_Dungeness$day.night <- mapply(FUN = case_day_night, periods_Dungeness$detect_day, periods_Dungeness$detect_time, SIMPLIFY = FALSE)

#periods_Dungeness$day.night <- do.call( function(detect_day,detect_time,...) case_day_night(detect_day, detect_time), periods_Dungeness )



#add in fine-scale noise data from Integral NoiseSpotter
#data on rms sound pressure levels, peak pressure in 30 s windows, 
#and the cumulative exposure levels in 30 s windows. The three files correspond to the three sensors located 35 cm, 
#50 cm and 70 cm above the sea bed.
NS35cm <- read.csv("D:\\MS research\\Integral NoiseSpotter Data\\updated August 22\\file40B_35cmAB.csv")
NS35cm$Time <- as.POSIXct(NS35cm$Time, format = "%m/%d/%Y %H:%M:%S")
NS35cm$Time <- with_tz(NS35cm$Time, tzone = "US/Pacific")
NS35cm <- NS35cm %>% rename(Date.time.UTC = Time)
NS35cm$Date.time.UTC <- round_date(NS35cm$Date.time.UTC, "30 seconds")

NS35cm_vel <- read.csv("D:\\MS research\\Integral NoiseSpotter Data\\updated August 22\\file40B_vel_35cmAB.csv")
NS35cm_vel$Time <- as.POSIXct(NS35cm_vel$Time, format = "%m/%d/%Y %H:%M:%S")
NS35cm_vel$Time <- with_tz(NS35cm_vel$Time, tzone = "US/Pacific")
NS35cm_vel <- NS35cm_vel %>% rename(Date.time.UTC = Time)
NS35cm_vel$Date.time.UTC <- round_date(NS35cm_vel$Date.time.UTC, "30 seconds")

NS70cm <- read.csv("D:\\MS research\\Integral NoiseSpotter Data\\updated August 22\\file40A_70cmAB.csv") %>% rename(Time = ï..Time)
NS70cm$Time <- as.POSIXct(NS70cm$Time, format = "%m/%d/%Y %H:%M:%S")
NS70cm$Time <- with_tz(NS70cm$Time, tzone = "US/Pacific")
NS70cm <- NS70cm %>% rename(Date.time.UTC = Time)
NS70cm$Date.time.UTC <- round_date(NS70cm$Date.time.UTC, "30 seconds")

NS70cm_vel <- read.csv("D:\\MS research\\Integral NoiseSpotter Data\\updated August 22\\file40A_vel_70cmAB.csv") %>% rename(Time = ï..Time)
NS70cm_vel$Time <- as.POSIXct(NS70cm_vel$Time, format = "%m/%d/%Y %H:%M:%S")
NS70cm_vel$Time <- with_tz(NS70cm_vel$Time, tzone = "US/Pacific")
NS70cm_vel <- NS70cm_vel %>% rename(Date.time.UTC = Time)
NS70cm_vel$Date.time.UTC <- round_date(NS70cm_vel$Date.time.UTC, "30 seconds")

#combine noise data sets
NSall <- full_join(NS35cm, NS50cm, by = "Date.time.UTC")
NSall <- full_join(NSall, NS70cm, by = "Date.time.UTC")


#round times in acceleration data sets to combine with Noise Spotter data
periods_Dungeness$Date.time.UTC <- round_date(periods_Dungeness$Date.time.UTC, "30 seconds")
periods_Lingcod$Date.time.UTC <- round_date(periods_Lingcod$Date.time.UTC, "30 seconds")
periods_BlackR_accel$Date.time.UTC <- round_date(periods_BlackR_accel$Date.time.UTC, "30 seconds")
periods_BlackR_depth$Date.time.UTC <- round_date(periods_BlackR_depth$Date.time.UTC, "30 seconds")
periods_ChinaR$Date.time.UTC <- round_date(periods_ChinaR$Date.time.UTC, "30 seconds")


#add noise levels to data sets of movement metrics, remove observations without noise data
periods_Dungeness_noise <- inner_join(NSall, periods_Dungeness, by = "Date.time.UTC")
periods_Lingcod_noise <- inner_join(NSall, periods_Lingcod, by = "Date.time.UTC")
periods_BlackR_accel_noise <- inner_join(NSall, periods_BlackR_accel, by = "Date.time.UTC")
periods_BlackR_depth_noise <- inner_join(NSall, periods_BlackR_depth, by = "Date.time.UTC")
periods_ChinaR_noise <- inner_join(NSall, periods_ChinaR, by = "Date.time.UTC")

#add Port Orford SBE data to data sets with sensor values
Port_O_SBE <- read.csv("D:\\MS research\\RRMR2021ReceiverLogs\\Port_Orford_SBE.csv")
##use data points at depth only - filter out those with depths smaller than 30m
Port_O_SBE <- Port_O_SBE %>% filter(DepSM > 30)
##rename columns for ease of use
Port_O_SBE <- Port_O_SBE %>% rename(TempC = Tv290C) %>% rename(Salinity = Sal00) %>% rename(DepthM = DepSM)
##change timezone of data (same point in time, convert to UTC from PDT to merge with transmitter data)
Port_O_SBE$Time <- as.POSIXct(Port_O_SBE$Time, format = "%m/%d/%Y %H:%M")
Port_O_SBE$Time <- with_tz(Port_O_SBE$Time, tzone = "US/Pacific")
Port_O_SBE <- Port_O_SBE %>% rename(Date.time.UTC = Time)

#add SBE data to data sets of movement metrics
periods_Dungeness_SBE <- full_join(Port_O_SBE, periods_Dungeness, by = "Date.time.UTC")
periods_Lingcod_SBE <- full_join(Port_O_SBE, periods_Lingcod, by = "Date.time.UTC")
periods_BlackR_accel_SBE <- full_join(Port_O_SBE, periods_BlackR_accel, by = "Date.time.UTC")
periods_BlackR_depth_SBE <- full_join(Port_O_SBE, periods_BlackR_depth, by = "Date.time.UTC")
periods_ChinaR_SBE <- full_join(Port_O_SBE, periods_ChinaR, by = "Date.time.UTC")


#upper 75th quartiles
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

violin_Dungeness <- ggplot(periods_Dungeness, aes(x=survey.period, y=Sensor.Value, fill=survey.period)) + 
  geom_violin(trim=FALSE) +  stat_summary(fun.data=mean_sdl, geom="pointrange", color="black") +
  scale_x_discrete(limits=c("May 20-June 10", "June 11", "June 12-16", "June 16", "June 17", "June 18", "June 19-July 11")) +
  labs(x = "Period of survey", y = "Acceleration values", title = "Dungeness Crab Acceleration by Period", fill="Period",
       caption = "Preliminary analyses")
violin_Dungeness

#data = periods_Dungeness %>% group_by(survey.period) %>% count()

violin_Lingcod <- ggplot(periods_Lingcod, aes(x=survey.period, y=Sensor.Value, fill=survey.period)) + 
  geom_violin(trim=FALSE) +  stat_summary(fun.data=mean_sdl, geom="pointrange", color="black") +
  scale_x_discrete(limits=c("May 20-June 10", "June 11", "June 12-16", "June 16", "June 17", "June 18", "June 19-July 11")) +
  labs(x = "Period of survey", y = "Acceleration values", title = "Lingcod Acceleration by Period", fill="Period",
       caption = "Preliminary analyses")
violin_Lingcod

violin_BlackR_accel <- ggplot(periods_BlackR_accel, aes(x=survey.period, y=Sensor.Value, fill=survey.period)) + 
  geom_violin(trim=FALSE) +  stat_summary(fun.data=mean_sdl, geom="pointrange", color="black") +
  scale_x_discrete(limits=c("May 20-June 10", "June 11", "June 12-16", "June 16", "June 17", "June 18", "June 19-July 11")) +
  labs(x = "Period of survey", y = "Acceleration values", title = "Black Rockfish Acceleration by Period", fill="Period",
       caption = "Preliminary analyses")
violin_BlackR_accel

violin_BlackR_depth <- ggplot(periods_BlackR_depth, aes(x=survey.period, y=Sensor.Value, fill=survey.period)) + 
  geom_violin(trim=FALSE) +  stat_summary(fun.data=mean_sdl, geom="pointrange", color="black") +
  scale_x_discrete(limits=c("May 20-June 10", "June 11", "June 12-16", "June 16", "June 17", "June 18", "June 19-July 11")) +
  labs(x = "Period of survey", y = "Depth (m)", title = "Black Rockfish Depth by Period", fill="Period",
       caption = "Preliminary analyses") + scale_y_reverse()
violin_BlackR_depth


violin_ChinaR <- ggplot(periods_ChinaR, aes(x=survey.period, y=Sensor.Value, fill=survey.period)) + 
  geom_violin(trim=FALSE) +  stat_summary(fun.data=mean_sdl, geom="pointrange", color="black") +
  scale_x_discrete(limits=c("May 20-June 10", "June 11", "June 12-16", "June 16", "June 17", "June 18", "June 19-July 11")) +
  labs(x = "Period of survey", y = "Acceleration values", title = "China Rockfish Acceleration by Period", fill="Period",
       caption = "Preliminary analyses")
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
       caption = "Preliminary analyses")


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


ggplot(BlackR_accel, aes(x=Date, y=Sensor.Value, fill=periods)) + 
  geom_violin(trim=FALSE) +  stat_summary(fun.data=mean_sdl, geom="pointrange", color="black") +
  labs(x = "Date of survey", y = "Acceleration values", title = "Black Rockfish Acceleration by Period", fill="Period",
       caption = "Preliminary analyses")


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



#round times in acceleration data sets to combine with Noise Spotter data and SBE
Dungeness_accel$Date.time.UTC <- round_date(Dungeness_accel$Date.time.UTC, "30 seconds")
Lingcod_accel$Date.time.UTC <- round_date(Lingcod_accel$Date.time.UTC, "30 seconds")
BlackR_accel$Date.time.UTC <- round_date(BlackR_accel$Date.time.UTC, "30 seconds")
ChinaR_accel$Date.time.UTC <- round_date(ChinaR_accel$Date.time.UTC, "30 seconds")

Dungeness_accel <- full_join(Port_O_SBE, Dungeness_accel, by = "Date.time.UTC")
Lingcod_accel <- full_join(Port_O_SBE, Lingcod_accel, by = "Date.time.UTC")
BlackR_accel <- full_join(Port_O_SBE, BlackR_accel, by = "Date.time.UTC")
ChinaR_accel <- full_join(Port_O_SBE, ChinaR_accel, by = "Date.time.UTC")





####################
#create example plotting for all accelerometer data for all species
ex_plot <- ggplot(plot_dat, aes(Date.time.UTC, Sensor.Value, color = Tag.Destination)) + 
  geom_point()
ex_plot + facet_grid( ~ .Tag.Destination)



#Lingcod quick statistics
t.test(Sensor.Value ~ survey.period, alternative = "two.sided", data = periods_Lingcod)


