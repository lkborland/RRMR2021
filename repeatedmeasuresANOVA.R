### Repeated measures ANOVA for coarse time periods - acceleration data

##Lingcod
# ID = transmitter code; key / time = survey period; value = sensor value
# Convert periods into factor variables
periods_Lingcod_RM <- periods_Lingcod
periods_Lingcod_RM <- periods_Lingcod_RM %>% select(Transmitter, Sensor.Value, survey.period) %>%
                      convert_as_factor(Transmitter, survey.period) %>% drop_na()

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
