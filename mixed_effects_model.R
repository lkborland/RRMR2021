
#mixed effects model for acceleration data
#         do adehabitat for more metrics?


#make periods factors
#set variable for levels of factors
period_lvl <- c("May 20-June 10", "June 11", "June 12-16", "June 16", 
                 "June 17", "June 18", "June 19-July 11")

periods_Dungeness %>%
  mutate(survey.period = fct_recode(survey.period,
                              "Republican, strong"    = "Strong republican",
                              "Republican, weak"      = "Not str republican",
                              "Independent, near rep" = "Ind,near rep",
                              "Independent, near dem" = "Ind,near dem",
                              "Democrat, weak"        = "Not str democrat",
                              "Democrat, strong"      = "Strong democrat"
  )) 



lmer_accel_Dungeness <- lmer(Sensor.Value ~ Period2 + Species + (1|Trial), data = rel.angle)

summary(u75lmerRel)