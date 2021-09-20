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
