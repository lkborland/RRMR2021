library(emmeans)
library(lmerTest)


#mixed effects model for acceleration data
#         do adehabitat for more metrics?


#make periods factors
#set variable for levels of factors
period_lvl <- c("May 20-June 10", "June 11", "June 12-16", "June 16", 
                 "June 17", "June 18", "June 19-July 11")

coarse.period.lvl <- c("Before", "During", "After")


#convert periods of acceleration values (by noise/day range) into factors for mixed effects model requirements
periods_ChinaR_SBE$survey.period <- factor(periods_ChinaR_SBE$survey.period)
periods_BlackR_accel_SBE$survey.period <- factor(periods_BlackR_accel_SBE$survey.period)
periods_Lingcod_SBE$survey.period <- factor(periods_Lingcod_SBE$survey.period)
periods_Dungeness_SBE$survey.period <- factor(periods_Dungeness_SBE$survey.period)
periods_BlackR_depth_SBE$survey.period <- factor(periods_BlackR_depth_SBE$survey.period)

#convert individual ID (Transmitter column) to factor to include in mixed model
periods_ChinaR$Transmitter <- factor(periods_ChinaR$Transmitter)
periods_BlackR_accel$Transmitter <- factor(periods_BlackR_accel$Transmitter)
periods_Lingcod$Transmitter <- factor(periods_Lingcod$Transmitter)
periods_Dungeness$Transmitter <- factor(periods_Dungeness$Transmitter)
periods_BlackR_depth$Transmitter <- factor(periods_BlackR_depth$Transmitter)

#factor orders by level, assign levels
periods_ChinaR_SBE$survey.period <- factor(periods_ChinaR_SBE$survey.period, levels = period_lvl[c(1:7)])
periods_BlackR_accel_SBE$survey.period <- factor(periods_BlackR_accel_SBE$survey.period, levels = period_lvl[c(1:7)])
periods_Lingcod_SBE$survey.period <- factor(periods_Lingcod_SBE$survey.period, levels = period_lvl[c(1:7)])
periods_Dungeness_SBE$survey.period <- factor(periods_Dungeness_SBE$survey.period, levels = period_lvl[c(1:7)])
periods_BlackR_depth_SBE$survey.period <- factor(periods_BlackR_depth_SBE$survey.period, levels = period_lvl[c(1:7)])


periods_Dungeness$coarse.period <- factor(periods_Dungeness$coarse.period, levels = coarse.period.lvl[c(1:3)])
periods_Lingcod$coarse.period <- factor(periods_Lingcod$coarse.period, levels = coarse.period.lvl[c(1:3)])
periods_BlackR_accel$coarse.period <- factor(periods_BlackR_accel$coarse.period, levels = coarse.period.lvl[c(1:3)])
periods_ChinaR$coarse.period <- factor(periods_ChinaR$coarse.period, levels = coarse.period.lvl[c(1:3)])



periods_Dungeness$day.night <- factor(periods_Dungeness$day.night)


#mixed effects model for all values
acc_lmer_ChinaR <- lmer(Sensor.Value ~ survey.period + (1 | Transmitter) + (1 | TempC) + (1 | Salinity), data = periods_ChinaR_SBE, REML = FALSE)
anova(acc_lmer_ChinaR)
summary(acc_lmer_ChinaR)
#emmeans(acc_lmer_ChinaR, list(pairwise ~ survey.period), adjust = "tukey")

acc_lmer_BlackR <- lmer(Sensor.Value ~ survey.period + (1 | Transmitter) + (1 | TempC) + (1 | Salinity), data = periods_BlackR_accel_SBE, REML = FALSE)
anova(acc_lmer_BlackR)
summary(acc_lmer_BlackR)

acc_lmer_BlackR <- lmer(Sensor.Value ~ survey.period + (1 | Transmitter) + (1 | TempC) + (1 | Salinity), data = periods_BlackR_accel_SBE, REML = FALSE)
anova(acc_lmer_BlackR)
summary(acc_lmer_BlackR)
#emmeans(acc_lmer_BlackR, list(pairwise ~ survey.period), adjust = "tukey")

acc_lmer_Lingcod <- lmer(Sensor.Value ~ survey.period + (1 | Transmitter) + (1 | TempC) + (1 | Salinity), data = periods_Lingcod_SBE, REML = FALSE)
anova(acc_lmer_Lingcod)
summary(acc_lmer_Lingcod)


acc_lmer_Dungeness <- lmer(Sensor.Value ~ survey.period + (1 | Transmitter) + (1 | TempC) + (1 | Salinity), data = periods_Dungeness_SBE, REML = FALSE)
anova(acc_lmer_Dungeness)
summary(acc_lmer_Dungeness)
#emmeans(acc_lmer_Dungeness, list(pairwise ~ survey.period), adjust = "tukey")

depth_lmer_BlackR <- lmer(Sensor.Value ~ survey.period + (1 | Transmitter) + (1 | TempC) + (1 | Salinity), data = periods_BlackR_depth_SBE, REML = FALSE)
anova(depth_lmer_BlackR)
summary(depth_lmer_BlackR)
#emmeans(acc_lmer_BlackR_d, list(pairwise ~ survey.period), adjust = "tukey")
  
#ANOVA for p-values for the lmer models
Anova(acc_lmer_ChinaR)
Anova(acc_lmer_BlackR)
Anova(acc_lmer_Lingcod)
Anova(acc_lmer_Dungeness)
Anova(depth_lmer_BlackR)



#mixed effects model for random days chosen (attempt to standardize response by observation period)
acc_lmer_ChinaR_9 <- lmer(Sensor.Value ~ survey.period + (1 | Transmitter) + (1 | TempC) + (1 | Salinity), data = ChinaR_accel, REML = FALSE)
anova(acc_lmer_ChinaR_9)
summary(acc_lmer_ChinaR_9)
#emmeans(acc_lmer_ChinaR, list(pairwise ~ survey.period), adjust = "tukey")

acc_lmer_BlackR_9 <- lmer(Sensor.Value ~ survey.period + (1 | Transmitter) + (1 | TempC) + (1 | Salinity), data = BlackR_accel, REML = FALSE)
anova(acc_lmer_BlackR_9)
summary(acc_lmer_BlackR_9)


acc_lmer_Lingcod_9 <- lmer(Sensor.Value ~ survey.period + (1 | Transmitter) + (1 | TempC) + (1 | Salinity), data = Lingcod_accel, REML = FALSE)
anova(acc_lmer_Lingcod_9)
summary(acc_lmer_Lingcod_9)


acc_lmer_Dungeness_9 <- lmer(Sensor.Value ~ survey.period + (1 | Transmitter) + (1 | TempC) + (1 | Salinity), data = Dungeness_accel, REML = FALSE)
anova(acc_lmer_Dungeness_9)
summary(acc_lmer_Dungeness_9)
#emmeans(acc_lmer_Dungeness, list(pairwise ~ survey.period), adjust = "tukey")






  

#Mixed effects model for upper 75
acc_u75lmer <- lmer(u75 ~ Period2 + (1|Trial) + Species, data = acc)
summary(acc_u75lmer)




#MLR - add day/night, presence of noise
lmD <- lm(Sensor.Value ~ survey.period + Transmitter, data = periods_Dungeness)
summary(lmD)




