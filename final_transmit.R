library(tidyverse)
library(glatos)
library(lubridate)
library(gghighlight)
library(rstatix)
library(lme4)
library(gamm4)
library(car)

#Upload tagsheet
tagsheet <- as_tibble(read.csv("D:\\MS research\\RRMR2021ReceiverLogs\\RRMRTagSheet.csv")) %>%
  rename(Transmitter = VUE.Tag.ID)

#new variable containing transmitter number and corresponding type of animal
animal_transmit <- select(tagsheet, Transmitter, Tag.Destination)

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

ex_plot_BlackR <- ggplot(dat_BlackR_accel, aes(Date.time.UTC, Sensor.Value)) + 
  geom_point() + gghighlight((Date.time.UTC >= June11_start) & (Date.time.UTC <= June11_end)) + 
  labs(x = "Time", y = "Acceleration values", title = "Black Rockfish Acceleration Over Time", color="Transmitter",
       caption = "Preliminary analyses")

ex_plot_ChinaR <- ggplot(dat_ChinaR, aes(Date.time.UTC, Sensor.Value, color = Transmitter)) + 
  geom_point() + gghighlight((Date.time.UTC >= June11_start) & (Date.time.UTC <= June11_end)) + 
  labs(x = "Time", y = "Acceleration values", title = "China Rockfish Acceleration Over Time", color="Transmitter",
       caption = "Preliminary analyses")

ex_plot_Dungeness <- ggplot(dat_Dungeness, aes(Date.time.UTC, Sensor.Value, color = Transmitter)) + 
  geom_point() + gghighlight((Date.time.UTC >= June11_start) & (Date.time.UTC <= June11_end)) + 
  labs(x = "Time", y = "Acceleration values", title = "Dungeness Crab Acceleration Over Time", color="Transmitter",
       caption = "Preliminary analyses")

ex_plot_Lingcod <- ggplot(dat_Lingcod, aes(Date.time.UTC, Sensor.Value, color = Transmitter)) + 
  geom_point() + gghighlight((Date.time.UTC >= June11_start) & (Date.time.UTC <= June11_end)) +
  labs(x = "Time", y = "Acceleration values", title = "Lingcod Acceleration Over Time", color="Transmitter",
       caption = "Preliminary analyses")

#example plotting looking at other estimated timeframes of seismic survey booms
#variables containing days of preliminary seismic boom detections
June16_start <- ymd_hms("2021-06-16 00:00:01", tz = "US/Pacific")
June16_end <- ymd_hms("2021-06-16 23:59:59", tz = "US/Pacific")

June17_start <- ymd_hms("2021-06-17 00:00:01", tz = "US/Pacific")
June17_end <- ymd_hms("2021-06-17 23:59:59", tz = "US/Pacific")

June18_start <- ymd_hms("2021-06-18 00:00:01", tz = "US/Pacific")
June18_end <- ymd_hms("2021-06-18 23:59:59", tz = "US/Pacific")

July11_end <- ymd_hms("2021-07-11 23:59:59", tz = "US/Pacific")


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


#convert to posixct time type
periods_Dungeness$Date.time.UTC <- as.POSIXct(strptime(periods_Dungeness$Date.time.UTC, "%Y-%m-%d %H:%M:%S"))
periods_Lingcod$Date.time.UTC <- as.POSIXct(strptime(periods_Lingcod$Date.time.UTC, "%Y-%m-%d %H:%M:%S"))
#add column of minimum time based on detections, by individual
periods_Dungeness <- periods_Dungeness %>% group_by(Transmitter) %>% mutate(time.min = min(Date.time.UTC))
periods_Lingcod <- periods_Lingcod %>% group_by(Transmitter) %>% mutate(time.min = min(Date.time.UTC))


#find minimum (earliest) time for each transmitter to feed to time since release
#add first time to each row by individual/tag
#use difftime for time after release
periods_Dungeness$TSR <- difftime(periods_Dungeness$Date.time.UTC, periods_Dungeness$time.min, units = "mins")
periods_Lingcod$TSR <- difftime(periods_Lingcod$Date.time.UTC, periods_Lingcod$time.min, units = "mins")


#observe # of times and which individuals were detected during each periods
table(periods_Dungeness$Transmitter, periods_Dungeness$survey.period)
table(periods_Lingcod$Transmitter, periods_Lingcod$survey.period)
table(periods_BlackR_accel$Transmitter, periods_BlackR_accel$survey.period)
table(periods_ChinaR$Transmitter, periods_ChinaR$survey.period)


#count number of individuals detected for each period
#Dungeness
periods_Dungeness %>% filter(survey.period == "May 20-June 10") %>% count(Transmitter) #14
periods_Dungeness %>% filter(survey.period == "June 11") %>% count(Transmitter) #4
periods_Dungeness %>% filter(survey.period == "June 12-16") %>% count(Transmitter) #5
periods_Dungeness %>% filter(survey.period == "June 16") %>% count(Transmitter) #3
periods_Dungeness %>% filter(survey.period == "June 17") %>% count(Transmitter) #3
periods_Dungeness %>% filter(survey.period == "June 18") %>% count(Transmitter) #3
periods_Dungeness %>% filter(survey.period == "June 19-July 11") %>% count(Transmitter) #3

#Lingcod
periods_Lingcod %>% filter(survey.period == "May 20-June 10") %>% count(Transmitter) #14 - look at position data - shallow water or leave to north/south?? add covariates to model
periods_Lingcod %>% filter(survey.period == "June 11") %>% count(Transmitter) #3
periods_Lingcod %>% filter(survey.period == "June 12-16") %>% count(Transmitter) #6
periods_Lingcod %>% filter(survey.period == "June 16") %>% count(Transmitter) #5
periods_Lingcod %>% filter(survey.period == "June 17") %>% count(Transmitter) #5
periods_Lingcod %>% filter(survey.period == "June 18") %>% count(Transmitter)#4
periods_Lingcod %>% filter(survey.period == "June 19-July 11") %>% count(Transmitter) #10 -

#Black Rockfish accel
periods_BlackR_accel %>% filter(survey.period == "May 20-June 10") %>% count(Transmitter) #15
periods_BlackR_accel %>% filter(survey.period == "June 11") %>% count(Transmitter) #14
periods_BlackR_accel %>% filter(survey.period == "June 12-16") %>% count(Transmitter) #14
periods_BlackR_accel %>% filter(survey.period == "June 16") %>% count(Transmitter) #13
periods_BlackR_accel %>% filter(survey.period == "June 17") %>% count(Transmitter) #14
periods_BlackR_accel %>% filter(survey.period == "June 18") %>% count(Transmitter) #14
periods_BlackR_accel %>% filter(survey.period == "June 19-July 11") %>% count(Transmitter) #14
#Black Rockfish depth
periods_BlackR_depth %>% filter(survey.period == "May 20-June 10") %>% count(Transmitter) #15
periods_BlackR_depth %>% filter(survey.period == "June 11") %>% count(Transmitter) #13
periods_BlackR_depth %>% filter(survey.period == "June 12-16") %>% count(Transmitter) #14
periods_BlackR_depth %>% filter(survey.period == "June 16") %>% count(Transmitter) #13
periods_BlackR_depth %>% filter(survey.period == "June 17") %>% count(Transmitter) #14
periods_BlackR_depth %>% filter(survey.period == "June 18") %>% count(Transmitter) #14
periods_BlackR_depth %>% filter(survey.period == "June 19-July 11") %>% count(Transmitter) #14

#China Rockfish
periods_ChinaR %>% filter(survey.period == "May 20-June 10") %>% count(Transmitter) #13
periods_ChinaR %>% filter(survey.period == "June 11") %>% count(Transmitter) #8
periods_ChinaR %>% filter(survey.period == "June 12-16") %>% count(Transmitter) #10
periods_ChinaR %>% filter(survey.period == "June 16") %>% count(Transmitter) #10
periods_ChinaR %>% filter(survey.period == "June 17") %>% count(Transmitter) #11
periods_ChinaR %>% filter(survey.period == "June 18") %>% count(Transmitter) #9
periods_ChinaR %>% filter(survey.period == "June 19-July 11") %>% count(Transmitter) #10


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

##Dungeness: ?????
periods_Dungeness <- periods_Dungeness %>% filter(Date.time.UTC <= July11_end)



##vector of generic sunrise to sunset
Port_O_Sun <- read.csv("D:\\MS research\\RRMR2021ReceiverLogs\\NOAA Port Orford tides\\NOAA_PortO_NightDay2021.csv")
Port_O_Sun <- Port_O_Sun %>% rename(Date = ï..Date)
Port_O_Sun <- Port_O_Sun %>% unite("Sunrise.datetime", Date:Sunrise.Time, sep = " ", remove = FALSE) %>% 
                              unite("Sunset.datetime", c(Date, Sunset.Time), sep = " ", remove = FALSE)
Port_O_Sun$Sunrise.datetime <- as.POSIXct(Port_O_Sun$Sunrise.datetime, format = "%m/%d/%Y %H:%M")
Port_O_Sun$Sunset.datetime <- as.POSIXct(Port_O_Sun$Sunset.datetime, format = "%m/%d/%Y %H:%M")
Port_O_Sun$Sunrise.datetime <- with_tz(Port_O_Sun$Sunrise.datetime, tzone = "US/Pacific")
Port_O_Sun$Sunset.datetime <- with_tz(Port_O_Sun$Sunset.datetime, tzone = "US/Pacific")



##plot average acceleration over time, overlay shaded periods as "night" and dotted line as beginning of seismic survey period
Dungeness_before <- periods_Dungeness %>% filter(survey.period == "May 20-June 10")
Dungeness_during <- periods_Dungeness %>% filter(survey.period == "June 11" | survey.period == "June 12-16" 
                                                           | survey.period == "June 16" | survey.period == "June 17" 
                                                           | survey.period == "June 18")
Dungeness_after <- periods_Dungeness %>% filter(survey.period == "June 19-July 11")


periods_Dungeness <- periods_Dungeness %>% mutate(coarse.period = case_when(survey.period == "May 20-June 10" ~ "Before",
                                                                            survey.period == "June 11" | survey.period == "June 12-16" 
                                                                            | survey.period == "June 16" | survey.period == "June 17" 
                                                                            | survey.period == "June 18" ~ "During",
                                                                            survey.period == "June 19-July 11" ~ "After"))


#### FIX THIS
periods_Dungeness <- periods_Dungeness %>% mutate(day.night = 
                                                    case_when(Date.time.UTC >= Port_O_Sun$Sunrise.datetime & Date.time.UTC < Port_O_Sun$Sunset.datetime ~ "Day",
                                                              TRUE ~ "Night"))

ex_date <- "5/20/2021"
subset(Port_O_Sun, Date == ex_date)
Port_O_Sun[Port_O_Sun$Date == ex_date, ]

case_day_night <- function(date, time) {
  case_when(
    height > 200 | mass > 200 ~ "large",
    species == "Droid"        ~ "robot",
    TRUE                      ~ "other"
  )
}


#add in fine-scale noise data from Integral NoiseSpotter
#data on rms sound pressure levels, peak pressure in 30 s windows, 
#and the cumulative exposure levels in 30 s windows. The three files correspond to the three sensors located 35 cm, 
#50 cm and 70 cm above the sea bed.
NS35cm <- read.csv("D:\\MS research\\Integral NoiseSpotter Data\\file40B_35cmAB.csv")
NS35cm$Time <- as.POSIXct(NS35cm$Time, format = "%Y-%m-%d %H:%M:%S")
NS35cm$Time <- with_tz(NS35cm$Time, tzone = "US/Pacific")

NS50cm <- read.csv("D:\\MS research\\Integral NoiseSpotter Data\\file100_50cmAB.csv")
NS50cm$Time <- as.POSIXct(NS50cm$Time, format = "%Y-%m-%d %H:%M:%S")
NS50cm$Time <- with_tz(NS50cm$Time, tzone = "US/Pacific")

NS70cm <- read.csv("D:\\MS research\\Integral NoiseSpotter Data\\file40A_70cmAB.csv") %>% rename(Time = ï..Time)
NS70cm$Time <- as.POSIXct(NS70cm$Time, format = "%Y-%m-%d %H:%M:%S")
NS70cm$Time <- with_tz(NS70cm$Time, tzone = "US/Pacific")

#convert all times to same day to evaluate daily movement
timeperiod_sameday <- as.POSIXct(Dungeness_before$Date.time.UTC, format="%H:%M:%S")


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


#Adding Port Orford CTD data
## import file
Port_O_SBE <- read.csv("D:\\MS research\\RRMR2021ReceiverLogs\\Port_Orford_SBE_data.csv")

##use data points at depth only - filter out those with depths smaller than 30m
Port_O_SBE <- Port_O_SBE %>% filter(DepSM > 30)

##rename columns for ease of use
Port_O_SBE <- Port_O_SBE %>% rename(TempC = Tv290C) %>% rename(Salinity = Sal00) %>% rename(DepthM = DepSM)

##convert time to time object to tell R data is time in PDT
Port_O_SBE <- Port_O_SBE %>% unite("SBE_time", mm.dd.yyyy:hh.mm.ss, sep = " ", remove = FALSE)

##change timezone of data (same point in time, convert to UTC from PDT to merge with transmitter data)
Port_O_SBE <- Port_O_SBE %>% strptime(format = "%m/%d/%Y %H:%M:%S", tz = "PDT")

##join SBE data to transmitter sensor dataset 
#### NEED TIME ZONE FOR DAY.TIME INFO







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

####################
#create example plotting for all accelerometer data for all species
ex_plot <- ggplot(plot_dat, aes(Date.time.UTC, Sensor.Value, color = Tag.Destination)) + 
  geom_point()
ex_plot + facet_grid( ~ .Tag.Destination)



#Lingcod quick statistics
t.test(Sensor.Value ~ survey.period, alternative = "two.sided", data = periods_Lingcod)


