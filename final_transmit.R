library(tidyverse)
library(glatos)
library(lubridate)
library(gghighlight)
library(rstatix)
library(lme4)
library(gamm4)

#Upload tagsheet
tagsheet <- as_tibble(read.csv("D:\\MS research\\RRMR2021ReceiverLogs\\RRMRTagSheet.csv")) %>%
            rename(Transmitter = VUE.Tag.ID)

#new variable containing transmitter number and corresponding type of animal
animal_transmit <- select(tagsheet, Transmitter, Tag.Destination)

#Assign prelim transmitter (animal) logs to variables in the environment
A_12048 <- as_tibble(read.csv("D:\\MS research\\RRMR2021ReceiverLogs\\VUE_export_final\\A69-9007-12048.csv", 
            na.strings = c("Sensor Fault"))) %>%
            rename(Date.time.UTC = ï..Date.and.Time..UTC.)

A_12049 <- as_tibble(read.csv("D:\\MS research\\RRMR2021ReceiverLogs\\VUE_export_final\\A69-9007-12049.csv", 
            na.strings = c("Sensor Fault"))) %>%
            rename(Date.time.UTC = ï..Date.and.Time..UTC.)

A_12050 <- as_tibble(read.csv("D:\\MS research\\RRMR2021ReceiverLogs\\VUE_export_final\\A69-9007-12050.csv", 
            na.strings = c("Sensor Fault"))) %>%
            rename(Date.time.UTC = ï..Date.and.Time..UTC.)

A_12051 <- as_tibble(read.csv("D:\\MS research\\RRMR2021ReceiverLogs\\VUE_export_final\\A69-9007-12051.csv", 
            na.strings = c("Sensor Fault"))) %>% 
            rename(Date.time.UTC = ï..Date.and.Time..UTC.)

A_12052 <- as_tibble(read.csv("D:\\MS research\\RRMR2021ReceiverLogs\\VUE_export_final\\A69-9007-12052.csv", 
            na.strings = c("Sensor Fault"))) %>% 
            rename(Date.time.UTC = ï..Date.and.Time..UTC.)

A_12053 <- as_tibble(read.csv("D:\\MS research\\RRMR2021ReceiverLogs\\VUE_export_final\\A69-9007-12053.csv", 
            na.strings = c("Sensor Fault"))) %>% 
            rename(Date.time.UTC = ï..Date.and.Time..UTC.)

A_12054 <- as_tibble(read.csv("D:\\MS research\\RRMR2021ReceiverLogs\\VUE_export_final\\A69-9007-12054.csv", 
            na.strings = c("Sensor Fault"))) %>% 
            rename(Date.time.UTC = ï..Date.and.Time..UTC.)

A_12055 <- as_tibble(read.csv("D:\\MS research\\RRMR2021ReceiverLogs\\VUE_export_final\\A69-9007-12055.csv", 
            na.strings = c("Sensor Fault"))) %>% 
            rename(Date.time.UTC = ï..Date.and.Time..UTC.)

A_12056 <- as_tibble(read.csv("D:\\MS research\\RRMR2021ReceiverLogs\\VUE_export_final\\A69-9007-12056.csv", 
            na.strings = c("Sensor Fault"))) %>% 
            rename(Date.time.UTC = ï..Date.and.Time..UTC.)

A_12057 <- as_tibble(read.csv("D:\\MS research\\RRMR2021ReceiverLogs\\VUE_export_final\\A69-9007-12057.csv", 
            na.strings = c("Sensor Fault"))) %>% 
            rename(Date.time.UTC = ï..Date.and.Time..UTC.)

A_12058<- as_tibble(read.csv("D:\\MS research\\RRMR2021ReceiverLogs\\VUE_export_final\\A69-9007-12058.csv", 
            na.strings = c("Sensor Fault"))) %>% 
            rename(Date.time.UTC = ï..Date.and.Time..UTC.)

A_12059 <- as_tibble(read.csv("D:\\MS research\\RRMR2021ReceiverLogs\\VUE_export_final\\A69-9007-12059.csv", 
                              na.strings = c("Sensor Fault"))) %>% 
  rename(Date.time.UTC = ï..Date.and.Time..UTC.)

A_12060 <- as_tibble(read.csv("D:\\MS research\\RRMR2021ReceiverLogs\\VUE_export_final\\A69-9007-12060.csv", 
                              na.strings = c("Sensor Fault"))) %>% 
  rename(Date.time.UTC = ï..Date.and.Time..UTC.)

A_12061 <- as_tibble(read.csv("D:\\MS research\\RRMR2021ReceiverLogs\\VUE_export_final\\A69-9007-12061.csv", 
                              na.strings = c("Sensor Fault"))) %>% 
  rename(Date.time.UTC = ï..Date.and.Time..UTC.)

A_12062 <- as_tibble(read.csv("D:\\MS research\\RRMR2021ReceiverLogs\\VUE_export_final\\A69-9007-12062.csv", 
                              na.strings = c("Sensor Fault"))) %>% 
  rename(Date.time.UTC = ï..Date.and.Time..UTC.)

A_12063 <- as_tibble(read.csv("D:\\MS research\\RRMR2021ReceiverLogs\\VUE_export_final\\A69-9007-12063.csv", 
                              na.strings = c("Sensor Fault"))) %>% 
  rename(Date.time.UTC = ï..Date.and.Time..UTC.)

A_12064 <- as_tibble(read.csv("D:\\MS research\\RRMR2021ReceiverLogs\\VUE_export_final\\A69-9007-12064.csv", 
                              na.strings = c("Sensor Fault"))) %>% 
  rename(Date.time.UTC = ï..Date.and.Time..UTC.)

A_12065 <- as_tibble(read.csv("D:\\MS research\\RRMR2021ReceiverLogs\\VUE_export_final\\A69-9007-12065.csv", 
                              na.strings = c("Sensor Fault"))) %>% 
  rename(Date.time.UTC = ï..Date.and.Time..UTC.)

A_12066 <- as_tibble(read.csv("D:\\MS research\\RRMR2021ReceiverLogs\\VUE_export_final\\A69-9007-12066.csv", 
                              na.strings = c("Sensor Fault"))) %>% 
  rename(Date.time.UTC = ï..Date.and.Time..UTC.)

A_12067 <- as_tibble(read.csv("D:\\MS research\\RRMR2021ReceiverLogs\\VUE_export_final\\A69-9007-12067.csv", 
                              na.strings = c("Sensor Fault"))) %>% 
  rename(Date.time.UTC = ï..Date.and.Time..UTC.)

A_12068 <- as_tibble(read.csv("D:\\MS research\\RRMR2021ReceiverLogs\\VUE_export_final\\A69-9007-12068.csv", 
                              na.strings = c("Sensor Fault"))) %>% 
  rename(Date.time.UTC = ï..Date.and.Time..UTC.)

A_12069 <- as_tibble(read.csv("D:\\MS research\\RRMR2021ReceiverLogs\\VUE_export_final\\A69-9007-12069.csv", 
                              na.strings = c("Sensor Fault"))) %>% 
  rename(Date.time.UTC = ï..Date.and.Time..UTC.)

A_12070 <- as_tibble(read.csv("D:\\MS research\\RRMR2021ReceiverLogs\\VUE_export_final\\A69-9007-12070.csv", 
                              na.strings = c("Sensor Fault"))) %>% 
  rename(Date.time.UTC = ï..Date.and.Time..UTC.)

A_12071 <- as_tibble(read.csv("D:\\MS research\\RRMR2021ReceiverLogs\\VUE_export_final\\A69-9007-12071.csv", 
                              na.strings = c("Sensor Fault"))) %>% 
  rename(Date.time.UTC = ï..Date.and.Time..UTC.)

A_12072 <- as_tibble(read.csv("D:\\MS research\\RRMR2021ReceiverLogs\\VUE_export_final\\A69-9007-12072.csv", 
                              na.strings = c("Sensor Fault"))) %>% 
  rename(Date.time.UTC = ï..Date.and.Time..UTC.)

A_12073 <- as_tibble(read.csv("D:\\MS research\\RRMR2021ReceiverLogs\\VUE_export_final\\A69-9007-12073.csv", 
                              na.strings = c("Sensor Fault"))) %>% 
  rename(Date.time.UTC = ï..Date.and.Time..UTC.)

A_12074 <- as_tibble(read.csv("D:\\MS research\\RRMR2021ReceiverLogs\\VUE_export_final\\A69-9007-12074.csv", 
                              na.strings = c("Sensor Fault"))) %>% 
  rename(Date.time.UTC = ï..Date.and.Time..UTC.)

A_12075 <- as_tibble(read.csv("D:\\MS research\\RRMR2021ReceiverLogs\\VUE_export_final\\A69-9007-12075.csv", 
                              na.strings = c("Sensor Fault"))) %>% 
  rename(Date.time.UTC = ï..Date.and.Time..UTC.)

A_12076 <- as_tibble(read.csv("D:\\MS research\\RRMR2021ReceiverLogs\\VUE_export_final\\A69-9007-12076.csv", 
                              na.strings = c("Sensor Fault"))) %>% 
  rename(Date.time.UTC = ï..Date.and.Time..UTC.)

A_12077 <- as_tibble(read.csv("D:\\MS research\\RRMR2021ReceiverLogs\\VUE_export_final\\A69-9007-12077.csv", 
                              na.strings = c("Sensor Fault"))) %>% 
  rename(Date.time.UTC = ï..Date.and.Time..UTC.)

A_13249 <- as_tibble(read.csv("D:\\MS research\\RRMR2021ReceiverLogs\\VUE_export_final\\A69-9007-13249.csv", 
                              na.strings = c("Sensor Fault"))) %>% 
  rename(Date.time.UTC = ï..Date.and.Time..UTC.)







##########
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
