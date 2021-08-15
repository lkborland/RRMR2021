library(tidyverse)
library(glatos)
library(lubridate)


#Assign prelim receiver logs to variables in the environment
VR2AR_549764 <- read.csv("D:\\MS research\\Prelim_RRMR2021ReceiverLogs\\RRMR2021ReceiverLogs\\VR2AR_549764_20210721_1.csv")
VR2AR_549765 <- read.csv("D:\\MS research\\Prelim_RRMR2021ReceiverLogs\\RRMR2021ReceiverLogs\\VR2AR_549765_20210721_1.csv")
VR2AR_549766 <- read.csv("D:\\MS research\\Prelim_RRMR2021ReceiverLogs\\RRMR2021ReceiverLogs\\VR2AR_549766_20210721_1.csv")
VR2AR_549767 <- read.csv("D:\\MS research\\Prelim_RRMR2021ReceiverLogs\\RRMR2021ReceiverLogs\\VR2AR_549767_20210721_1.csv")
VR2AR_549768 <- read.csv("D:\\MS research\\Prelim_RRMR2021ReceiverLogs\\RRMR2021ReceiverLogs\\VR2AR_549768_20210721_1.csv")
VR2AR_549769 <- read.csv("D:\\MS research\\Prelim_RRMR2021ReceiverLogs\\RRMR2021ReceiverLogs\\VR2AR_549769_20210721_1.csv")
VR2AR_549770 <- read.csv("D:\\MS research\\Prelim_RRMR2021ReceiverLogs\\RRMR2021ReceiverLogs\\VR2AR_549770_20210721_1.csv")
VR2AR_549771 <- read.csv("D:\\MS research\\Prelim_RRMR2021ReceiverLogs\\RRMR2021ReceiverLogs\\VR2AR_549771_20210720_1.csv")
VR2AR_549772 <- read.csv("D:\\MS research\\Prelim_RRMR2021ReceiverLogs\\RRMR2021ReceiverLogs\\VR2AR_549772_20210721_1.csv")
VR2AR_549773 <- read.csv("D:\\MS research\\Prelim_RRMR2021ReceiverLogs\\RRMR2021ReceiverLogs\\VR2AR_549773_20210721_1.csv")
VR2W_103159 <- read.csv("D:\\MS research\\Prelim_RRMR2021ReceiverLogs\\RRMR2021ReceiverLogs\\VR2W_103159_20210608_1.csv")
VR2W_105472 <- read.csv("D:\\MS research\\Prelim_RRMR2021ReceiverLogs\\RRMR2021ReceiverLogs\\VR2W_105472_20210608_1.csv")
VR2W_105476 <- read.csv("D:\\MS research\\Prelim_RRMR2021ReceiverLogs\\RRMR2021ReceiverLogs\\VR2W_105476_20210608_1.csv")
VR2W_106004 <- read.csv("D:\\MS research\\Prelim_RRMR2021ReceiverLogs\\RRMR2021ReceiverLogs\\VR2W_106004_20210608_1.csv")
VR2W_110687 <- read.csv("D:\\MS research\\Prelim_RRMR2021ReceiverLogs\\RRMR2021ReceiverLogs\\VR2W_110687_20210608_1.csv")
VR2W_110687_2 <- read.csv("D:\\MS research\\Prelim_RRMR2021ReceiverLogs\\RRMR2021ReceiverLogs\\VR2W_110687_20210608_2.csv")
VR2W_110695 <- read.csv("D:\\MS research\\Prelim_RRMR2021ReceiverLogs\\RRMR2021ReceiverLogs\\VR2W_110695_20210608_1.csv")

#Assign prelim transmitter (animal) logs to variables in the environment
A_12048 <- as_tibble(read.csv("D:\\MS research\\Prelim_RRMR2021ReceiverLogs\\VUE Export\\A69-9007-12048.csv", 
              na.strings = c("Sensor Fault"))) %>%
              rename(Date.time.UTC = ï..Date.and.Time..UTC.)
  
A_12049 <- as_tibble(read.csv("D:\\MS research\\Prelim_RRMR2021ReceiverLogs\\VUE Export\\A69-9007-12049.csv", 
              na.strings = c("Sensor Fault"))) %>%
              rename(Date.time.UTC = ï..Date.and.Time..UTC.)

A_12050 <- as_tibble(read.csv("D:\\MS research\\Prelim_RRMR2021ReceiverLogs\\VUE Export\\A69-9007-12050.csv", 
              na.strings = c("Sensor Fault"))) %>%
              rename(Date.time.UTC = ï..Date.and.Time..UTC.)

A_12051 <- as_tibble(read.csv("D:\\MS research\\Prelim_RRMR2021ReceiverLogs\\VUE Export\\A69-9007-12051.csv", 
              na.strings = c("Sensor Fault"))) %>% 
              rename(Date.time.UTC = ï..Date.and.Time..UTC.)

A_12052 <- as_tibble(read.csv("D:\\MS research\\Prelim_RRMR2021ReceiverLogs\\VUE Export\\A69-9007-12052.csv", 
              na.strings = c("Sensor Fault"))) %>% 
              rename(Date.time.UTC = ï..Date.and.Time..UTC.)

A_12053 <- as_tibble(read.csv("D:\\MS research\\Prelim_RRMR2021ReceiverLogs\\VUE Export\\A69-9007-12053.csv", 
              na.strings = c("Sensor Fault"))) %>% 
              rename(Date.time.UTC = ï..Date.and.Time..UTC.)

A_12054 <- as_tibble(read.csv("D:\\MS research\\Prelim_RRMR2021ReceiverLogs\\VUE Export\\A69-9007-12054.csv", 
              na.strings = c("Sensor Fault"))) %>% 
              rename(Date.time.UTC = ï..Date.and.Time..UTC.)

A_12055 <- as_tibble(read.csv("D:\\MS research\\Prelim_RRMR2021ReceiverLogs\\VUE Export\\A69-9007-12055.csv", 
              na.strings = c("Sensor Fault"))) %>% 
              rename(Date.time.UTC = ï..Date.and.Time..UTC.)

A_12056 <- as_tibble(read.csv("D:\\MS research\\Prelim_RRMR2021ReceiverLogs\\VUE Export\\A69-9007-12056.csv", 
              na.strings = c("Sensor Fault"))) %>% 
              rename(Date.time.UTC = ï..Date.and.Time..UTC.)

A_12057 <- as_tibble(read.csv("D:\\MS research\\Prelim_RRMR2021ReceiverLogs\\VUE Export\\A69-9007-12057.csv", 
              na.strings = c("Sensor Fault"))) %>% 
              rename(Date.time.UTC = ï..Date.and.Time..UTC.)

A_12058<- as_tibble(read.csv("D:\\MS research\\Prelim_RRMR2021ReceiverLogs\\VUE Export\\A69-9007-12058.csv", 
              na.strings = c("Sensor Fault"))) %>% 
              rename(Date.time.UTC = ï..Date.and.Time..UTC.)

A_12059 <- as_tibble(read.csv("D:\\MS research\\Prelim_RRMR2021ReceiverLogs\\VUE Export\\A69-9007-12059.csv", 
              na.strings = c("Sensor Fault"))) %>% 
              rename(Date.time.UTC = ï..Date.and.Time..UTC.)

A_12060 <- as_tibble(read.csv("D:\\MS research\\Prelim_RRMR2021ReceiverLogs\\VUE Export\\A69-9007-12060.csv", 
              na.strings = c("Sensor Fault"))) %>% 
              rename(Date.time.UTC = ï..Date.and.Time..UTC.)

A_12061 <- as_tibble(read.csv("D:\\MS research\\Prelim_RRMR2021ReceiverLogs\\VUE Export\\A69-9007-12061.csv", 
              na.strings = c("Sensor Fault"))) %>% 
              rename(Date.time.UTC = ï..Date.and.Time..UTC.)

A_12062 <- as_tibble(read.csv("D:\\MS research\\Prelim_RRMR2021ReceiverLogs\\VUE Export\\A69-9007-12062.csv", 
              na.strings = c("Sensor Fault"))) %>% 
              rename(Date.time.UTC = ï..Date.and.Time..UTC.)

A_12063 <- as_tibble(read.csv("D:\\MS research\\Prelim_RRMR2021ReceiverLogs\\VUE Export\\A69-9007-12063.csv", 
              na.strings = c("Sensor Fault"))) %>% 
              rename(Date.time.UTC = ï..Date.and.Time..UTC.)

A_12064 <- as_tibble(read.csv("D:\\MS research\\Prelim_RRMR2021ReceiverLogs\\VUE Export\\A69-9007-12064.csv", 
              na.strings = c("Sensor Fault"))) %>% 
              rename(Date.time.UTC = ï..Date.and.Time..UTC.)

A_12065 <- as_tibble(read.csv("D:\\MS research\\Prelim_RRMR2021ReceiverLogs\\VUE Export\\A69-9007-12065.csv", 
              na.strings = c("Sensor Fault"))) %>% 
              rename(Date.time.UTC = ï..Date.and.Time..UTC.)

A_12066 <- as_tibble(read.csv("D:\\MS research\\Prelim_RRMR2021ReceiverLogs\\VUE Export\\A69-9007-12066.csv", 
              na.strings = c("Sensor Fault"))) %>% 
              rename(Date.time.UTC = ï..Date.and.Time..UTC.)

A_12067 <- as_tibble(read.csv("D:\\MS research\\Prelim_RRMR2021ReceiverLogs\\VUE Export\\A69-9007-12067.csv", 
              na.strings = c("Sensor Fault"))) %>% 
              rename(Date.time.UTC = ï..Date.and.Time..UTC.)

A_12068 <- as_tibble(read.csv("D:\\MS research\\Prelim_RRMR2021ReceiverLogs\\VUE Export\\A69-9007-12068.csv", 
              na.strings = c("Sensor Fault"))) %>% 
              rename(Date.time.UTC = ï..Date.and.Time..UTC.)

A_12069 <- as_tibble(read.csv("D:\\MS research\\Prelim_RRMR2021ReceiverLogs\\VUE Export\\A69-9007-12069.csv", 
              na.strings = c("Sensor Fault"))) %>% 
              rename(Date.time.UTC = ï..Date.and.Time..UTC.)

A_12070 <- as_tibble(read.csv("D:\\MS research\\Prelim_RRMR2021ReceiverLogs\\VUE Export\\A69-9007-12070.csv", 
              na.strings = c("Sensor Fault"))) %>% 
              rename(Date.time.UTC = ï..Date.and.Time..UTC.)

A_12071 <- as_tibble(read.csv("D:\\MS research\\Prelim_RRMR2021ReceiverLogs\\VUE Export\\A69-9007-12071.csv", 
              na.strings = c("Sensor Fault"))) %>% 
              rename(Date.time.UTC = ï..Date.and.Time..UTC.)

A_12074 <- as_tibble(read.csv("D:\\MS research\\Prelim_RRMR2021ReceiverLogs\\VUE Export\\A69-9007-12074.csv", 
              na.strings = c("Sensor Fault"))) %>% 
              rename(Date.time.UTC = ï..Date.and.Time..UTC.)

A_12075 <- as_tibble(read.csv("D:\\MS research\\Prelim_RRMR2021ReceiverLogs\\VUE Export\\A69-9007-12075.csv", 
              na.strings = c("Sensor Fault"))) %>% 
              rename(Date.time.UTC = ï..Date.and.Time..UTC.)

A_13249 <- as_tibble(read.csv("D:\\MS research\\Prelim_RRMR2021ReceiverLogs\\VUE Export\\A69-9007-13249.csv", 
              na.strings = c("Sensor Fault"))) %>% 
              rename(Date.time.UTC = ï..Date.and.Time..UTC.)

A_13250 <- as_tibble(read.csv("D:\\MS research\\Prelim_RRMR2021ReceiverLogs\\VUE Export\\A69-9007-13250.csv", 
              na.strings = c("Sensor Fault"))) %>% 
              rename(Date.time.UTC = ï..Date.and.Time..UTC.)

A_13251 <- as_tibble(read.csv("D:\\MS research\\Prelim_RRMR2021ReceiverLogs\\VUE Export\\A69-9007-13251.csv", 
              na.strings = c("Sensor Fault"))) %>% 
              rename(Date.time.UTC = ï..Date.and.Time..UTC.)

A_13254 <- as_tibble(read.csv("D:\\MS research\\Prelim_RRMR2021ReceiverLogs\\VUE Export\\A69-9007-13254.csv", 
              na.strings = c("Sensor Fault"))) %>% 
              rename(Date.time.UTC = ï..Date.and.Time..UTC.)

A_13258 <- as_tibble(read.csv("D:\\MS research\\Prelim_RRMR2021ReceiverLogs\\VUE Export\\A69-9007-13258.csv", 
              na.strings = c("Sensor Fault"))) %>% 
              rename(Date.time.UTC = ï..Date.and.Time..UTC.)

A_13259 <- as_tibble(read.csv("D:\\MS research\\Prelim_RRMR2021ReceiverLogs\\VUE Export\\A69-9007-13259.csv", 
              na.strings = c("Sensor Fault"))) %>% 
              rename(Date.time.UTC = ï..Date.and.Time..UTC.)

A_13260 <- as_tibble(read.csv("D:\\MS research\\Prelim_RRMR2021ReceiverLogs\\VUE Export\\A69-9007-13260.csv", 
              na.strings = c("Sensor Fault"))) %>% 
              rename(Date.time.UTC = ï..Date.and.Time..UTC.)

A_13267 <- as_tibble(read.csv("D:\\MS research\\Prelim_RRMR2021ReceiverLogs\\VUE Export\\A69-9007-13267.csv", 
              na.strings = c("Sensor Fault"))) %>% 
              rename(Date.time.UTC = ï..Date.and.Time..UTC.)

A_13269 <- as_tibble(read.csv("D:\\MS research\\Prelim_RRMR2021ReceiverLogs\\VUE Export\\A69-9007-13269.csv", 
              na.strings = c("Sensor Fault"))) %>% 
              rename(Date.time.UTC = ï..Date.and.Time..UTC.)

A_13271 <- as_tibble(read.csv("D:\\MS research\\Prelim_RRMR2021ReceiverLogs\\VUE Export\\A69-9007-13271.csv", 
              na.strings = c("Sensor Fault"))) %>% 
              rename(Date.time.UTC = ï..Date.and.Time..UTC.)

A_13276 <- as_tibble(read.csv("D:\\MS research\\Prelim_RRMR2021ReceiverLogs\\VUE Export\\A69-9007-13276.csv", 
              na.strings = c("Sensor Fault"))) %>% 
              rename(Date.time.UTC = ï..Date.and.Time..UTC.)

A_13279 <- as_tibble(read.csv("D:\\MS research\\Prelim_RRMR2021ReceiverLogs\\VUE Export\\A69-9007-13279.csv", 
              na.strings = c("Sensor Fault"))) %>% 
              rename(Date.time.UTC = ï..Date.and.Time..UTC.)

A_13280 <- as_tibble(read.csv("D:\\MS research\\Prelim_RRMR2021ReceiverLogs\\VUE Export\\A69-9007-13280.csv", 
              na.strings = c("Sensor Fault"))) %>% 
              rename(Date.time.UTC = ï..Date.and.Time..UTC.)

A_13281 <- as_tibble(read.csv("D:\\MS research\\Prelim_RRMR2021ReceiverLogs\\VUE Export\\A69-9007-13281.csv", 
              na.strings = c("Sensor Fault"))) %>% 
              rename(Date.time.UTC = ï..Date.and.Time..UTC.)

A_13282 <- as_tibble(read.csv("D:\\MS research\\Prelim_RRMR2021ReceiverLogs\\VUE Export\\A69-9007-13282.csv", 
              na.strings = c("Sensor Fault"))) %>% 
              rename(Date.time.UTC = ï..Date.and.Time..UTC.)

A_13283 <- as_tibble(read.csv("D:\\MS research\\Prelim_RRMR2021ReceiverLogs\\VUE Export\\A69-9007-13283.csv", 
              na.strings = c("Sensor Fault"))) %>% 
              rename(Date.time.UTC = ï..Date.and.Time..UTC.)

A_13284 <- as_tibble(read.csv("D:\\MS research\\Prelim_RRMR2021ReceiverLogs\\VUE Export\\A69-9007-13284.csv", 
              na.strings = c("Sensor Fault"))) %>% 
              rename(Date.time.UTC = ï..Date.and.Time..UTC.)

A_13285 <- as_tibble(read.csv("D:\\MS research\\Prelim_RRMR2021ReceiverLogs\\VUE Export\\A69-9007-13285.csv", 
              na.strings = c("Sensor Fault"))) %>% 
              rename(Date.time.UTC = ï..Date.and.Time..UTC.)

A_13286 <- as_tibble(read.csv("D:\\MS research\\Prelim_RRMR2021ReceiverLogs\\VUE Export\\A69-9007-13286.csv", 
              na.strings = c("Sensor Fault"))) %>% 
              rename(Date.time.UTC = ï..Date.and.Time..UTC.)

A_13287 <- as_tibble(read.csv("D:\\MS research\\Prelim_RRMR2021ReceiverLogs\\VUE Export\\A69-9007-13287.csv", 
              na.strings = c("Sensor Fault"))) %>% 
              rename(Date.time.UTC = ï..Date.and.Time..UTC.)

A_13290 <- as_tibble(read.csv("D:\\MS research\\Prelim_RRMR2021ReceiverLogs\\VUE Export\\A69-9007-13290.csv", 
              na.strings = c("Sensor Fault"))) %>% 
              rename(Date.time.UTC = ï..Date.and.Time..UTC.)

A_13291 <- as_tibble(read.csv("D:\\MS research\\Prelim_RRMR2021ReceiverLogs\\VUE Export\\A69-9007-13291.csv", 
              na.strings = c("Sensor Fault"))) %>% 
              rename(Date.time.UTC = ï..Date.and.Time..UTC.)

A_13292 <- as_tibble(read.csv("D:\\MS research\\Prelim_RRMR2021ReceiverLogs\\VUE Export\\A69-9007-13292.csv", 
              na.strings = c("Sensor Fault"))) %>% 
              rename(Date.time.UTC = ï..Date.and.Time..UTC.)

A_13293 <- as_tibble(read.csv("D:\\MS research\\Prelim_RRMR2021ReceiverLogs\\VUE Export\\A69-9007-13293.csv", 
              na.strings = c("Sensor Fault"))) %>% 
              rename(Date.time.UTC = ï..Date.and.Time..UTC.)

#Upload tagsheet
tagsheet <- read.csv("D:\\MS research\\Prelim_RRMR2021ReceiverLogs\\RRMRTagSheet.csv")

#combine detections by transmitter tag as example to plot accelerometer data over time
#STILL NEED TO ADD CONFIRMATION OF SENSOR UNIT AGREEMENT
ex_comb_1 <- bind_rows(A_12048,A_12049)

#PRELIMINARY - tell R the time range of seismic survey
prelim_start <- ymd_hms("2021-06-10 00:00:01")
prelim_end <- ymd_hms("2021-06-12 23:59:59")

#for ggplot - highlight data timeperiod 
plot_dat <- ex_comb_1
ex_plot <- ggplot(plot_dat, aes(Date.time.UTC, Sensor.Value, color = Transmitter)) + 
            geom_point()
ex_plot + geom_rect(data=rects, inherit.aes=FALSE, aes(xmin=2021-06-10, xmax=2021-06-13, ymin=min(A_12053$Sensor.Value),
                                                       ymax=max(A_12053$Sensor.Value), group=group), color="transparent", fill="orange", alpha=0.3)

rects <- data.frame(start=prelim_start, end=prelim_end, group=seq_along(prelim_start))