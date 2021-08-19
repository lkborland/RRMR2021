library(tidyverse)
library(glatos)
library(lubridate)
library(gghighlight)
library(rstatix)

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

#Upload tagsheet
tagsheet <- as_tibble(read.csv("D:\\MS research\\Prelim_RRMR2021ReceiverLogs\\RRMRTagSheet.csv")) %>%
              rename(Transmitter = VUE.Tag.ID)

#new variable containing transmitter number and corresponding type of animal
animal_transmit <- select(tagsheet, Transmitter, Tag.Destination)

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





#combine detections by transmitter tag as example to plot accelerometer data over time
#STILL NEED TO ADD CONFIRMATION OF SENSOR UNIT AGREEMENT
ex_comb_1 <- bind_rows(A_12048, A_12049, A_12050, A_12051, A_12052, A_12053, A_12054, A_12055, A_12056, A_12057, A_12058, A_12059, 
                       A_12060, A_12061, A_12062, A_12063, A_12064, A_12065, A_12066, A_12067, A_12068, A_12069, A_12070, A_12071,
                       A_12074, A_12075, A_13249, A_13250, A_13251, A_13254, A_13258, A_13259, A_13260, A_13267, A_13269, A_13271,
                       A_13276, A_13279, A_13280, A_13281, A_13282, A_13283, A_13284, A_13285, A_13286, A_13287, A_13290, A_13291,
                       A_13292, A_13293)

ex_comb_2 <- left_join(ex_comb_1, animal_transmit, by = "Transmitter")


#for ggplot - select out species for visualization
plot_dat_Black <- ex_comb_2 %>% filter(str_detect(Tag.Destination, "Black"))
plot_dat_China <- ex_comb_2 %>% filter(str_detect(Tag.Destination, "China"))
plot_dat_Dung <- ex_comb_2 %>% filter(str_detect(Tag.Destination, "Dungeness"))
plot_dat_Ling <- ex_comb_2 %>% filter(str_detect(Tag.Destination, "Lingcod"))

#ggploting example by species for accelerometer data: highlight 6/10-12
#PRELIMINARY - tell R the time range of seismic survey
prelim_start <- ymd_hms("2021-06-10 00:00:01")
prelim_end <- ymd_hms("2021-06-12 23:59:59")

ex_plot_Black <- ggplot(plot_dat_Black, aes(Date.time.UTC, Sensor.Value)) + 
                  geom_point() + gghighlight((Date.time.UTC >= prelim_start) & (Date.time.UTC <= prelim_end))

      #ex_plot_Black + stat_summary(fun = "mean", color = "red", geom = "point")

  
ex_plot_China <- ggplot(plot_dat_China, aes(Date.time.UTC, Sensor.Value, color = Transmitter)) + 
                  geom_point() #+ gghighlight((Date.time.UTC >= prelim_start) & (Date.time.UTC <= prelim_end))

ex_plot_Dung <- ggplot(plot_dat_Dung, aes(Date.time.UTC, Sensor.Value, color = Transmitter)) + 
                  geom_point() + gghighlight((Date.time.UTC >= prelim_start) & (Date.time.UTC <= prelim_end))

ex_plot_Ling <- ggplot(plot_dat_Ling, aes(Date.time.UTC, Sensor.Value, color = Transmitter)) + 
                  geom_point() + gghighlight((Date.time.UTC >= prelim_start) & (Date.time.UTC <= prelim_end))

#example plotting looking at other estimated timeframes of seismic survey booms
#variables containing days of preliminary seismic boom detections
prelim_june16s <- ymd_hms("2021-06-16 00:00:01")
prelim_june16e <- ymd_hms("2021-06-16 23:59:59")

ex_plot_Dung <- ggplot(plot_dat_Dung, aes(Date.time.UTC, Sensor.Value, color = Transmitter)) + 
  geom_point() + gghighlight((Date.time.UTC >= prelim_june16s) & (Date.time.UTC <= prelim_june16e))

prelim_june18s <- ymd_hms("2021-06-18 00:00:01")
prelim_june18e <- ymd_hms("2021-06-18 23:59:59")

prelim_june19s <- ymd_hms("2021-06-19 00:00:01")
prelim_june19e <- ymd_hms("2021-06-19 23:59:59")

#adding "period" categorizations to data (need to bin as the resolution is too fine to be visible on a plot)
prelim_periods_Dung <- plot_dat_Dung %>% mutate(prelim.period = case_when(Date.time.UTC < prelim_start | (Date.time.UTC > prelim_end & Date.time.UTC < prelim_june16s) ~ "Before and reference",
                                                                          Date.time.UTC >= prelim_start & Date.time.UTC <= prelim_end ~ "During 6.10-12",
                                                                          Date.time.UTC >= prelim_june16s & Date.time.UTC <= prelim_june16e ~ "June 16",
                                                                          Date.time.UTC > prelim_june16e ~ "After June 16"))

prelim_periods_Ling <- plot_dat_Ling %>% mutate(prelim.period = case_when(Date.time.UTC < prelim_start | (Date.time.UTC > prelim_end & Date.time.UTC < prelim_june16s) ~ "Before and reference",
                                                                          Date.time.UTC >= prelim_start & Date.time.UTC <= prelim_end ~ "During 6.10-12",
                                                                          Date.time.UTC >= prelim_june16s & Date.time.UTC <= prelim_june16e ~ "June 16",
                                                                          Date.time.UTC > prelim_june16e ~ "After June 16"))

#Ensure data manipulation worked propoerly and assess # of points per period
#table(prelim_periods_Dung$prelim.period)   

#create violin plot of preliminary Dungeness crab data across periods for visualization of acceleration values
# Violin plot with trimmed tails and adding median points as data is skewed (mean unappropriate)
p <- ggplot(prelim_periods_Dung, aes(x=prelim.period, y=Sensor.Value, fill=prelim.period)) + 
      geom_violin(trim=FALSE) +  stat_summary(fun.data=mean_sdl, geom="pointrange", color="black") +
      scale_x_discrete(limits=c("Before and reference", "During 6.10-12", "June 16", "After June 16")) +
      labs(x = "Period of survey", y = "Acceleration values", title = "Dungeness Crab Acceleration by Period", fill="Period",
      caption = "Based on preliminary collected data")
  #stat_summary(fun=median, geom="point", size=2, color="red")
p

s <- ggplot(prelim_periods_Ling, aes(x=prelim.period, y=Sensor.Value, fill=prelim.period)) + 
      geom_violin(trim=FALSE) +  stat_summary(fun.data=mean_sdl, geom="pointrange", color="black") +
      scale_x_discrete(limits=c("Before and reference", "During 6.10-12", "June 16", "After June 16")) +
      labs(x = "Period of survey", y = "Acceleration values", title = "Lingcod Acceleration by Period", fill="Period",
      caption = "Based on preliminary collected data")
s


#evaluate statistical differences in accelerometer value by period for preliminary data
#first, check for extreme outliers for a repeated measures ANOVA
dung_out <- prelim_periods_Dung %>% group_by(prelim.period) %>% identify_outliers(Sensor.Value)
#dung_swtest <- prelim_periods_Dung %>% group_by(prelim.period) %>% shapiro_test(Sensor.Value)
table(dung_out$is.extreme)

ling_out <- prelim_periods_Ling %>% group_by(prelim.period) %>% identify_outliers(Sensor.Value)
#ling_swtest <- prelim_periods_Ling %>% group_by(prelim.period) %>% shapiro_test(Sensor.Value)
table(ling_out$is.extreme)

#convert period to factors and data frames
dc_fried <- prelim_periods_Dung %>% convert_as_factor(prelim.period, Transmitter) %>% 
            select(Transmitter, prelim.period, Sensor.Value) %>% filter(!is.na(Sensor.Value))
dc_fried_df <- as.data.frame(dc_fried)

lc_fried <- prelim_periods_Ling %>% convert_as_factor(prelim.period, Transmitter) %>% 
            select(Transmitter, prelim.period, Sensor.Value) %>% filter(!is.na(Sensor.Value))
lc_fried_df <- as.data.frame(lc_fried)


#non-parametric Friedman test for Dungeness and lingcod
dc_fried_test <- dc_fried_df %>% friedman_test(Sensor.Value ~ prelim.period | Transmitter)
dc_fried_test

friedman.test(Sensor.Value ~ prelim.period | Transmitter, data=dc_fried_df)
#Friedman test (non %>% friendly) for Dungeness and lingcod
#create matrix with all values for each period


#create example plotting for all accelerometer data for all species
ex_plot <- ggplot(plot_dat, aes(Date.time.UTC, Sensor.Value, color = Tag.Destination)) + 
            geom_point()
ex_plot + facet_grid( ~ .Tag.Destination)

#Overlay moving average over time
ex_plot <- ggplot(plot_dat, aes(x = Date.time.UTC, y = Sensor.Value, colour=Tag.Destination)) + 
  geom_point(size=4) + 
  facet_grid(Tag.Destination~.) + 
  stat_summary(fun=mean, aes(group=1), geom="line", colour="blue") +
  stat_summary(fun=mean, aes(group=1), geom="point", colour="blue", size=3, shape=4)
  
#highlight data timeperiod 
  #geom_rect(data=rects, inherit.aes=FALSE, aes(xmin=2021-06-10, xmax=2021-06-13, ymin=0,
                                                       #ymax=max(ex_comb_2$Sensor.Value), group=group), color="transparent", fill="orange", alpha=0.3)

rects <- data.frame(start=prelim_start, end=prelim_end, group=seq_along(prelim_start))



#use GLATOS to assess prelim data: 
#NEED TO UPLOAD DATA INTO GLATOS FORMAT
#summarize_detections()

#abacus_plot()