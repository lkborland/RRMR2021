### Repeated measures ANOVA for coarse time periods - acceleration data
library(broom)

##Lingcod
# ID = transmitter code; key / time = survey period; value = sensor value
# Convert periods into factor variables
periods_Lingcod_RM <- periods_Lingcod
periods_Lingcod_RM <- periods_Lingcod_RM %>% dplyr::select(Transmitter, Sensor.Value, survey.period) %>%
                      convert_as_factor(Transmitter, survey.period) %>% drop_na()

#regular ANOVA
ANOVA.lingcod <- aov(Sensor.Value~survey.period, data = periods_Lingcod_RM)
summary(ANOVA.lingcod)

ANOVA.2.lingcod <- aov(Sensor.Value~survey.period*Transmitter, data = periods_Lingcod_RM)
summary(ANOVA.2.lingcod)

#compute RM ANOVA
RM.lingcod <- anova_test(data = periods_Lingcod_RM, dv = Sensor.Value, wid = Transmitter, within = survey.period)
get_anova_table(RM.lingcod)

# pairwise comparisons
pwc.lingcod <- periods_Lingcod_RM %>%
  pairwise_t_test(
    Sensor.Value ~ survey.period, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
pwc.lingcod

#there are extreme outliers - need to employ WRS2 robust ANOVA




#Black R acceleration ANOVA
periods_BlackRA_RM <- periods_BlackR_accel
periods_BlackRA_RM <- periods_BlackRA_RM %>% select(Transmitter, Sensor.Value, survey.period) %>%
  convert_as_factor(Transmitter, survey.period) %>% drop_na()

#regular ANOVA
ANOVA.BlackRA <- aov(Sensor.Value~survey.period, data = periods_BlackRA_RM)
summary(ANOVA.BlackRA)

ANOVA.2.BlackRA <- aov(Sensor.Value~survey.period*Transmitter, data = periods_BlackRA_RM)
summary(ANOVA.2.BlackRA)


#Black R depth ANOVA
periods_BlackRD_RM <- periods_BlackR_depth
periods_BlackRD_RM <- periods_BlackRD_RM %>% select(Transmitter, Sensor.Value, survey.period) %>%
  convert_as_factor(Transmitter, survey.period) %>% drop_na()

#regular ANOVA
ANOVA.BlackRD <- aov(Sensor.Value~survey.period, data = periods_BlackRD_RM)
summary(ANOVA.BlackRD)

ANOVA.2.BlackRD <- aov(Sensor.Value~survey.period*Transmitter, data = periods_BlackRD_RM)
summary(ANOVA.2.BlackRD)



#China R accel ANOVA
periods_ChinaR_RM <- periods_ChinaR
periods_ChinaR_RM <- periods_ChinaR_RM %>% select(Transmitter, Sensor.Value, survey.period) %>%
  convert_as_factor(Transmitter, survey.period) %>% drop_na()

#regular ANOVA
ANOVA.ChinaR <- aov(Sensor.Value~survey.period, data = periods_ChinaR_RM)
summary(ANOVA.ChinaR)

ANOVA.2.ChinaR <- aov(Sensor.Value~survey.period*Transmitter, data = periods_ChinaR_RM)
summary(ANOVA.2.ChinaR)




#Dungeness accel ANOVA
periods_Dungeness_RM <- periods_Dungeness
periods_Dungeness_RM <- periods_Dungeness_RM %>% select(Transmitter, Sensor.Value, survey.period) %>%
  convert_as_factor(Transmitter, survey.period) %>% drop_na()

#regular ANOVA
ANOVA.dungeness <- aov(Sensor.Value~survey.period, data = periods_Dungeness_RM)
summary(ANOVA.dungeness)

ANOVA.2.dungeness <- aov(Sensor.Value~survey.period*Transmitter, data = periods_Dungeness_RM)
summary(ANOVA.2.dungeness)