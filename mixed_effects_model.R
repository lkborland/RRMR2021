
#mixed effects model for acceleration data
#         do adehabitat for more metrics?


#make periods factors
#set variable for levels of factors
period_lvl <- c("May 20-June 10", "June 11", "June 12-16", "June 16", 
                 "June 17", "June 18", "June 19-July 11")

coarse.period.lvl <- c("Before", "During", "After")


#convert periods of acceleration values (by noise/day range) into factors for mixed effects model requirements
periods_ChinaR$survey.period <- factor(periods_ChinaR$survey.period)
periods_BlackR_accel$survey.period <- factor(periods_BlackR_accel$survey.period)
periods_Lingcod$survey.period <- factor(periods_Lingcod$survey.period)
periods_Dungeness$survey.period <- factor(periods_Dungeness$survey.period)

#convert individual ID (Transmitter column) to factor to include in mixed model
periods_ChinaR$Transmitter <- factor(periods_ChinaR$Transmitter)
periods_BlackR_accel$Transmitter <- factor(periods_BlackR_accel$Transmitter)
periods_Lingcod$Transmitter <- factor(periods_Lingcod$Transmitter)
periods_Dungeness$Transmitter <- factor(periods_Dungeness$Transmitter)


#factor orders by level, assign levels
periods_ChinaR$survey.period <- factor(periods_ChinaR$survey.period, levels = period_lvl[c(1:7)])
periods_BlackR_accel$survey.period <- factor(periods_BlackR_accel$survey.period, levels = period_lvl[c(1:7)])
periods_Lingcod$survey.period <- factor(periods_Lingcod$survey.period, levels = period_lvl[c(1:7)])
periods_Dungeness$survey.period <- factor(periods_Dungeness$survey.period, levels = period_lvl[c(1:7)])


periods_Dungeness$coarse.period <- factor(periods_Dungeness$coarse.period, levels = coarse.period.lvl[c(1:3)])



#mixed effects model for all values
acc_lmer_ChinaR <- lmer(Sensor.Value ~ survey.period + (1 | Transmitter), data = periods_ChinaR, REML = FALSE) #note REML here
summary(acc_lmer_ChinaR)

acc_lmer_BlackR <- lmer(Sensor.Value ~ survey.period + (1 | Transmitter), data = periods_BlackR_accel, REML = FALSE)
summary(acc_lmer_BlackR)

acc_lmer_Lingcod <- lmer(Sensor.Value ~ survey.period + (1 | Transmitter), data = periods_Lingcod)
summary(acc_lmer_Lingcod)

acc_lmer_Dungeness <- lmer(Sensor.Value ~ survey.period + (1 | Transmitter), data = periods_Dungeness)
summary(acc_lmer_Dungeness)
  
#ANOVA for p-values for the lmer models
Anova(acc_lmer_ChinaR)
Anova(acc_lmer_BlackR)
Anova(acc_lmer_Lingcod)
Anova(acc_lmer_Dungeness)
  

#Mixed effects model for upper 75
acc_u75lmer <- lmer(u75 ~ Period2 + (1|Trial) + Species, data = acc)
summary(acc_u75lmer)




#MLR - add day/night, presence of noise
lmD <- lm(Sensor.Value ~ coarse.period, data = periods_Dungeness)
summary(lmD)




