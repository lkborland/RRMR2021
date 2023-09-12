# KUD plotting - spatial stuff
library(rgdal)
library(rgeos)
library(raster)

library(tidyverse)
library(cowplot)
library(showtext)
library(sysfonts)
showtext_auto()

library(plotly)
library(hrbrthemes)

# Add fonts from Google.
font_add_google("Roboto Mono", "Roboto Mono")
font_add_google("Open Sans", "Open Sans")
font_add_google("Special Elite", "Special Elite")
# Set ggplot theme
theme_set(theme_minimal(base_family = "Roboto Mono"))

#make periods factors
#set variable for levels of factors
period_lvl <- c("May 10-17", "May 18-25", "May 26-June 2", "June 3-10", "June 11", "June 12-16", "June 18", "June 19-26", "June 27-July 4", "July 5-12")


noise_labels <- c("Before", "Before", "Before", "Before", "Noise", "No noise", "Noise",
                  "After", "After", "After")

per_lvl <- c("p0", "p1", "p2", "p3", "11", "p5", "18", "p7", "p8", "p9")



noise_labels_9 <- c("Before", "Before", "Before", "June 11", "June 17", "June 18",
                    "After", "After", "After")
noise_lvl_9 <- c("2021-06-01", "2021-06-03", "2021-06-08", "2021-06-11", "2021-06-17", 
                 "2021-06-18", "2021-06-21", "2021-06-25", "2021-07-01")
noise_colors_9 <- c("#C7695F", "#C7695F", "#C7695F", "#91F6FA", "#91F6FA", "#91F6FA", 
                             "#1B5094", "#1B5094", "#1B5094")
                             

KUD_labels <- c("Before", "During", "After")
noise_colors <- c("#FB8F85", "#BE90E6", "#91F7FA", "#B3E685", "#FFD9B2")
noise_colors_2 <- c("#5F8DC7", "#1B5094", "#91F6FA", "#FBDAD0", "#C7695F")
noise_colors_repeat <- c("#C7695F", "#C7695F", "#C7695F", "#C7695F", "#91F6FA", "#FBDAD0", "#91F6FA", "#1B5094", "#1B5094", "#1B5094")

#

coarse.period.lvl <- c("Before", "During", "After")

violin_BlackR_accel <- ggplot(periods_BlackR_accel, aes(x=survey.period, y=Sensor.Value, fill=survey.period)) + 
  geom_violin(trim=TRUE) +  stat_summary(fun.data=mean_sdl, geom="pointrange", color="black") +
  scale_x_discrete(limits=c("May 10-17", "May 18-25", "May 26-June 2", "June 3-10", "June 11", "June 12-16", "June 18", "June 19-26", "June 27-July 4", "July 5-12")) +
  labs(x = "Period of survey", y = expression(paste("Acceleration (m/s"^"2", ")")), title = "Black Rockfish Acceleration by Period", fill="Period")+  
  scale_fill_manual(values = noise_colors_repeat, breaks = period_lvl, labels = noise_labels) 

violin_BlackR_accel

violin_ChinaR_accel <- ggplot(periods_ChinaR, aes(x=survey.period, y=Sensor.Value, fill=survey.period)) + 
  geom_violin(trim=TRUE) +  stat_summary(fun.data=mean_sdl, geom="pointrange", color="black") +
  scale_x_discrete(limits=c("May 10-17", "May 18-25", "May 26-June 2", "June 3-10", "June 11", "June 12-16", "June 18", "June 19-26", "June 27-July 4", "July 5-12")) +
  labs(x = "Period of survey", y = expression(paste("Acceleration (m/s"^"2", ")")), title = "China Rockfish Acceleration by Period", fill="Period")+  
  scale_fill_manual(values = noise_colors_repeat, breaks = period_lvl, labels = noise_labels) 

violin_ChinaR_accel

violin_BlackR_HOB <- ggplot(periods_BlackR_HOB, aes(x=survey.period, y=HOB, fill=survey.period)) + 
  geom_violin(trim=TRUE) +  stat_summary(fun.data=mean_sdl, geom="pointrange", color="black") +
  scale_x_discrete(limits=c("May 10-17", "May 18-25", "May 26-June 2", "June 3-10", "June 11", "June 12-16", "June 18", "June 19-26", "June 27-July 4", "July 5-12")) +
  labs(x = "Period of survey", y = expression(paste("Height off Bottom (m)")), title = "Black Rockfish HOB by Period", fill="Period")+  
  scale_fill_manual(values = noise_colors_repeat, breaks = period_lvl, labels = noise_labels) 

violin_BlackR_HOB



dailymeantemp <- BlackR_acceleration  %>% dplyr::group_by(detect_day) %>% mutate(meantemp = mean(TempC)) %>% filter(row_number(TempC) == 1)
ggplot(dailymeantemp, aes(x=Date.time.UTC, y=meantemp)) + geom_point()

dailymeantemp <- Port_O_SBE %>% dplyr::group_by(mm.dd.yyyy) %>% mutate(meantemp = mean(TempC)) %>% filter(row_number(TempC) == 1)
ggplot(dailymeantemp, aes(x=mm.dd.yyyy, y=meantemp)) + geom_point()

n_Lingcod <- periods_Lingcod %>% dplyr::count(survey.period, Transmitter)
n_Lingcod <- n_Lingcod %>% dplyr::group_by(survey.period) %>% dplyr::summarise(n()) %>% dplyr::rename(number = 'n()')


violin_Lingcod <- ggplot(periods_Lingcod, aes(x=survey.period, y=Sensor.Value, fill=survey.period)) + 
  geom_violin(trim=TRUE, scale = "width") +  stat_summary(fun.data=mean_sdl, geom="pointrange", color="black") +
  scale_x_discrete(limits=c("May 10-17", "May 18-25", "May 26-June 2", "June 3-10", "June 11", "June 12-16", "June 18", "June 19-26", "June 27-July 4", "July 5-12")) +
  labs(x = "Period of survey", y = expression(paste("Acceleration (m/s"^"2", ")")), title = "Lingcod Acceleration by Period", fill="Period")+  
  scale_fill_manual(values = noise_colors_repeat, breaks = period_lvl, labels = noise_labels) +
  geom_text(data = n_Lingcod,
            aes(label = number,
                y = 3.6,
                x = survey.period,
                group = survey.period),
            position = position_dodge(width = 0.9),
            check_overlap = TRUE,
            show.legend = FALSE,
            inherit.aes = FALSE)

violin_Lingcod

violin_Dungeness <- ggplot(periods_Dungeness, aes(x=factor(survey.period), y=Sensor.Value, fill=survey.period)) + 
  geom_violin(trim=TRUE, scale = "width") +  stat_summary(fun.data=mean_sdl, geom="pointrange", color="black") +
  scale_x_discrete(limits=c("May 10-17", "May 18-25", "May 26-June 2", "June 3-10", "June 11", "June 12-16", "June 18", "June 19-26", "June 27-July 4", "July 5-12")) +
  labs(x = "Period of survey", y = expression(paste("Acceleration (m/s"^"2", ")")), title = "Dungeness Acceleration by Period", fill="Period")+  
  scale_fill_manual(values = noise_colors_repeat, breaks = period_lvl, labels = noise_labels) 

violin_Dungeness






geom_text(data = n_BRA,
          aes(label = number,
              y = max_val,
              x = survey.period,
              group = survey.period),
          position = position_dodge(width = 0.9),
          check_overlap = TRUE,
          show.legend = FALSE,
          inherit.aes = FALSE)

violin_BR_KUD <- ggplot(BR_ANOVA, aes(x=period, y=area, fill=period)) + 
  geom_violin(trim=TRUE)  + 
  scale_x_discrete(limits=periods.lvl, labels = KUD_labels) +
  labs(x = "Period of survey", y = "Area", title = "Black Rockfish Usage Density", fill="Period") +
  scale_fill_manual(values = noise_colors_repeat, breaks = periods.lvl, labels = KUD_labels, name = "Period")
violin_BR_KUD

violin_CR_KUD <- ggplot(CR_ANOVA, aes(x=period, y=area, fill=period)) + 
  geom_violin(trim=TRUE)  + 
  scale_x_discrete(limits=periods.lvl, labels = KUD_labels) +
  labs(x = "Period of survey", y = "Area", title = "China Rockfish Usage Density", fill="Period") +
  scale_fill_manual(values = noise_colors_repeat, breaks = periods.lvl, labels = KUD_labels, name = "Period")
violin_CR_KUD

violin_LC_KUD <- ggplot(LC_ANOVA2, aes(x=period, y=area, fill=period)) + 
  geom_violin(trim=TRUE)  + 
  scale_x_discrete(limits=periods.lvl, labels = KUD_labels) +
  labs(x = "Period of survey", y = "Area", title = "Lingcod Usage Density", fill="Period") +
  scale_fill_manual(values = noise_colors_repeat, breaks = periods.lvl, labels = KUD_labels, name = "Period")
violin_LC_KUD