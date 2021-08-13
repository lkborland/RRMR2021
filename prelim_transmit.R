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



#Upload tagsheet
tagsheet <- read.csv("D:\\MS research\\Prelim_RRMR2021ReceiverLogs\\RRMRTagSheet.csv")


#PRELIMINARY - tell R the time range of seismic survey
prelim_start <- ymd_hms("2021-06-10 00:00:01")
prelim_end <- ymd_hms("2021-06-12 23:59:59")

#for ggplot - highlight data timeperiod 
rects <- data.frame(start=prelim_start, end=prelim_end, group=seq_along(prelim_start))