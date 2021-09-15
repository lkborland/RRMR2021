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