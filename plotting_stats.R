library(lattice)



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



#evaluating residuals, outliers, etc. to further visualize structure of data before analyses

#LM evaluate residuals
Dungeness.lm <- lm(Sensor.Value ~ Date.time.UTC, data=periods_Dungeness)
Dungeness.resid <- resid(Dungeness.lm)
(plot(fitted(Dungeness.lm), Dungeness.resid))
qqnorm(Dungeness.resid)

#evaluate statistical differences in accelerometer value by period for preliminary data
#first, check for extreme outliers for a repeated measures ANOVA
br_out <- periods_BlackR %>% group_by(survey.period) %>% identify_outliers(Sensor.Value)
table(br_out$survey.period) #table to show counts of outliers in each period
cr_out <- periods_ChinaR %>% group_by(survey.period) %>% identify_outliers(Sensor.Value)
table(cr_out$survey.period)
dung_out <- periods_Dungeness %>% group_by(survey.period) %>% identify_outliers(Sensor.Value)
table(dung_out$survey.period)
ling_out <- periods_Lingcod %>% group_by(survey.period) %>% identify_outliers(Sensor.Value)
table(ling_out$survey.period)

#plot outliers by period to visualize acceleration
ggplot(data = br_out, aes(x=Date.time.UTC, y=Sensor.Value, color=survey.period)) + geom_point()
ggplot(data = cr_out, aes(x=Date.time.UTC, y=Sensor.Value, color=survey.period)) + geom_point()
ggplot(data = dung_out, aes(x=Date.time.UTC, y=Sensor.Value, color=survey.period)) + geom_point()
ggplot(data = ling_out, aes(x=Date.time.UTC, y=Sensor.Value, color=survey.period)) + geom_point()

table(periods_BlackR$Latitude)

#create variables of counts by period to use in discrete hist to visualize 
#the amount of detections in periods of noise vs no noise
countper_BlackR <- table(periods_BlackR$survey.period)
countper_ChinaR <- table(periods_ChinaR$survey.period)
countper_Dungeness <- table(periods_Dungeness$survey.period)
countper_Lingcod <- table(periods_Lingcod$survey.period)

#bar plots of counts by period
ggplot(periods_BlackR, aes(x=survey.period)) + 
  geom_bar() +
  scale_x_discrete(limits=c("Before", "June 11", "June 12-16", "June 16", "June 16-17", "June 17", "June 17-18", "June 18", "After June 18")) +
  labs(x = "Period of survey", y = "Number of detection events", title = "Black Rockfish Detections by Period",
       caption = "Preliminary analyses")

ggplot(periods_ChinaR, aes(x=survey.period)) + 
  geom_bar() +
  scale_x_discrete(limits=c("Before", "June 11", "June 12-16", "June 16", "June 16-17", "June 17", "June 17-18", "June 18", "After June 18")) +
  labs(x = "Period of survey", y = "Number of detection events", title = "China Rockfish Detections by Period",
       caption = "Preliminary analyses")

ggplot(periods_Dungeness, aes(x=survey.period)) + 
  geom_bar() +
  scale_x_discrete(limits=c("Before", "June 11", "June 12-16", "June 16", "June 16-17", "June 17", "June 17-18", "June 18", "After June 18")) +
  labs(x = "Period of survey", y = "Number of detection events", title = "Dungeness Detections by Period",
       caption = "Preliminary analyses")

ggplot(periods_Lingcod, aes(x=survey.period)) + 
  geom_bar() +
  scale_x_discrete(limits=c("Before", "June 11", "June 12-16", "June 16", "June 16-17", "June 17", "June 17-18", "June 18", "After June 18")) +
  labs(x = "Period of survey", y = "Number of detection events", title = "Lingcod Detections by Period",
       caption = "Preliminary analyses")

#bar plots of NA counts by period
ggplot(periods_BlackR, aes(x=survey.period)) + 
  geom_bar() +
  scale_x_discrete(limits=c("Before", "June 11", "June 12-16", "June 16", "June 16-17", "June 17", "June 17-18", "June 18", "After June 18")) +
  labs(x = "Period of survey", y = "Number of detection events", title = "Black Rockfish Detections by Period",
       caption = "Preliminary analyses")





dotplot(ranef(acc_lmer_BlackR))

model <- ranef(acc_lmer_BlackR,condVar=TRUE)
dotplot(model, main=FALSE,scales = list(x =list(relation = 'free')))[["Salinity"]]

