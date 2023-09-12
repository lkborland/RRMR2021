library(emmeans)
library(lmerTest)
library(mice)
library(broom.mixed)
#library(imputeTS)


#mixed effects model for acceleration data
#         do adehabitat for more metrics?


#make periods factors
#set variable for levels of factors
period_lvl <- c("May 20-June 10", "June 11", "June 12-16", "June 16", 
                 "June 17", "June 18", "June 19-July 11")
noise_labels <- c("Before", "Noise", "No noise", "Noise",
                  "Noise", "Noise", "After")




noise_labels_9 <- c("Before", "Before", "Before", "June 11", "June 17", "June 18",
                    "After", "After", "After")
noise_lvl_9 <- c("2021-06-01", "2021-06-03", "2021-06-08", "2021-06-11", "2021-06-17", 
                 "2021-06-18", "2021-06-21", "2021-06-25", "2021-07-01")
noise_colors_9 <- c("#C7695F", "#C7695F", "#C7695F", "#91F6FA", "#91F6FA", "#91F6FA", 
                    "#1B5094", "#1B5094", "#1B5094")


KUD_labels <- c("Before", "During", "After")
noise_colors <- c("#FB8F85", "#BE90E6", "#91F7FA", "#B3E685", "#FFD9B2")
noise_colors_2 <- c("#5F8DC7", "#1B5094", "#91F6FA", "#FBDAD0", "#C7695F")
noise_colors_repeat <- c("#C7695F", "#91F6FA", "#FBDAD0", "#91F6FA", "#91F6FA", "#91F6FA", "#1B5094")

#

coarse.period.lvl <- c("Before", "During", "After")



#convert individual ID (Transmitter column) to factor to include in mixed model
periods_ChinaR_SBE$Transmitter <- factor(periods_ChinaR_SBE$Transmitter)
periods_BlackR_accel_SBE$Transmitter <- factor(periods_BlackR_accel_SBE$Transmitter)
periods_Lingcod_SBE$Transmitter <- factor(periods_Lingcod_SBE$Transmitter)
periods_Dungeness_SBE$Transmitter <- factor(periods_Dungeness_SBE$Transmitter)
periods_BlackR_depth_SBE$Transmitter <- factor(periods_BlackR_depth_SBE$Transmitter)
periods_BlackR_HOB_SBE$Transmitter <- factor(periods_BlackR_HOB_SBE$Transmitter)

######## same with 9-day datasets
ChinaR_accel$Transmitter <- factor(ChinaR_accel$Transmitter)
BlackR_accel$Transmitter <- factor(BlackR_accel$Transmitter)
Lingcod_accel$Transmitter <- factor(Lingcod_accel$Transmitter)
Dungeness_accel$Transmitter <- factor(Dungeness_accel$Transmitter)
BlackR_depth$Transmitter <- factor(BlackR_depth$Transmitter)
BlackR_HOB9$Transmitter <- factor(BlackR_HOB9$Transmitter)

#factor orders by level, assign levels
periods_ChinaR_SBE$survey.period <- factor(periods_ChinaR_SBE$survey.period, levels = period_lvl[c(1:7)])
periods_BlackR_accel_SBE$survey.period <- factor(periods_BlackR_accel_SBE$survey.period, levels = period_lvl[c(1:7)])
periods_Lingcod_SBE$survey.period <- factor(periods_Lingcod_SBE$survey.period, levels = period_lvl[c(1:7)])
periods_Dungeness_SBE$survey.period <- factor(periods_Dungeness_SBE$survey.period, levels = period_lvl[c(1:7)])
periods_BlackR_depth_SBE$survey.period <- factor(periods_BlackR_depth_SBE$survey.period, levels = period_lvl[c(1:7)])
periods_BlackR_HOB_SBE$survey.period <- factor(periods_BlackR_HOB_SBE$survey.period, levels = period_lvl[c(1:7)])

#factor orders by level, assign levels for 9-day datasets
ChinaR_accel$survey.period <- factor(ChinaR_accel$survey.period, levels = period_lvl[c(1:7)])
BlackR_accel$survey.period <- factor(BlackR_accel$survey.period, levels = period_lvl[c(1:7)])
Lingcod_accel$survey.period <- factor(Lingcod_accel$survey.period, levels = period_lvl[c(1:7)])
Dungeness_accel$survey.period <- factor(Dungeness_accel$survey.period, levels = period_lvl[c(1:7)])
BlackR_depth$survey.period <- factor(BlackR_depth$survey.period, levels = period_lvl[c(1:7)])
BlackR_HOB9$survey.period <- factor(BlackR_HOB9$survey.period, levels = period_lvl[c(1:7)])

#
periods_Dungeness$coarse.period <- factor(periods_Dungeness$coarse.period, levels = coarse.period.lvl[c(1:3)])
periods_Lingcod$coarse.period <- factor(periods_Lingcod$coarse.period, levels = coarse.period.lvl[c(1:3)])
periods_BlackR_accel$coarse.period <- factor(periods_BlackR_accel$coarse.period, levels = coarse.period.lvl[c(1:3)])
periods_ChinaR$coarse.period <- factor(periods_ChinaR$coarse.period, levels = coarse.period.lvl[c(1:3)])


#
periods_Dungeness$day.night <- factor(periods_Dungeness$day.night)


#mixed effects model for all values
acc_lmer_ChinaR <- lmer(Sensor.Value ~ survey.period + (1 | Transmitter) + (1 | TempC) + (1 | Salinity), data = periods_ChinaR_SBE, REML = FALSE)
anova(acc_lmer_ChinaR)
summary(acc_lmer_ChinaR)
#emmeans(acc_lmer_ChinaR, list(pairwise ~ survey.period), adjust = "tukey")

acc_lmer_BlackR <- lmer(Sensor.Value ~ survey.period + (1 | Transmitter) + (1 | TempC) + (1 | Salinity), data = periods_BlackR_accel_SBE, REML = FALSE)
anova(acc_lmer_BlackR)
summary(acc_lmer_BlackR)

acc_lmer_Lingcod <- lmer(Sensor.Value ~ survey.period + (1 | Transmitter) + (1 | TempC) + (1 | Salinity), data = periods_Lingcod_SBE, REML = FALSE)
anova(acc_lmer_Lingcod)
summary(acc_lmer_Lingcod)

acc_lmer_Dungeness <- lmer(Sensor.Value ~ survey.period + (1 | Transmitter) + (1 | TempC) + (1 | Salinity), data = periods_Dungeness_SBE, REML = FALSE)
anova(acc_lmer_Dungeness)
summary(acc_lmer_Dungeness)


depth_lmer_BlackR <- lmer(Sensor.Value ~ survey.period + (1 | Transmitter) + (1 | TempC) + (1 | Salinity), data = periods_BlackR_depth_SBE, REML = FALSE)
anova(depth_lmer_BlackR)
summary(depth_lmer_BlackR)
#emmeans(acc_lmer_BlackR_d, list(pairwise ~ survey.period), adjust = "tukey")

HOB_lmer_BlackR <- lmer(HOB ~ survey.period + (1 | Transmitter) + (1 | TempC) + (1 | Salinity), data = periods_BlackR_HOB_SBE, REML = FALSE)
anova(HOB_lmer_BlackR)
summary(HOB_lmer_BlackR)
emmeans(HOB_lmer_BlackR, list(pairwise ~ survey.period), adjust = "tukey")

##try with imputed data
HOB_lmer_BlackR <- lmer(HOB ~ survey.period + (1 | Transmitter) + (1 | TempC) + (1 | Salinity), data = BlackR_imputedex, REML = FALSE)
anova(HOB_lmer_BlackR)
summary(HOB_lmer_BlackR)
emmeans(HOB_lmer_BlackR, list(pairwise ~ survey.period), adjust = "tukey")
  
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

HOB_lmer_BlackR_9 <- lmer(HOB ~ survey.period + (1 | Transmitter) + (1 | TempC) + (1 | Salinity), data = BlackR_HOB9, REML = FALSE)
anova(HOB_lmer_BlackR_9)
summary(HOB_lmer_BlackR_9)
emmeans(HOB_lmer_BlackR_9, list(pairwise ~ survey.period), adjust = "tukey")


acc_lmer_Lingcod_9 <- lmer(Sensor.Value ~ survey.period + (1 | Transmitter) + (1 | TempC) + (1 | Salinity), data = Lingcod_accel, REML = FALSE)
anova(acc_lmer_Lingcod_9)
summary(acc_lmer_Lingcod_9)


acc_lmer_Dungeness_9 <- lmer(Sensor.Value ~ survey.period + (1 | Transmitter) + (1 | TempC) + (1 | Salinity), data = Dungeness_accel, REML = FALSE)
anova(acc_lmer_Dungeness_9)
summary(acc_lmer_Dungeness_9)
#emmeans(acc_lmer_Dungeness, list(pairwise ~ survey.period), adjust = "tukey")


################## MEM for 9-day periods for receiver numbers
#mixed effects model for random days chosen (attempt to standardize response by observation period)
rec_lmer_ChinaR_9 <- lmer(n.receiver ~ Date + (1 | Transmitter), data = ChinaR_receivers, REML = FALSE)
#rec_lmer_ChinaR_9 <- lmer(n.receiver ~ survey.period + (1 | Transmitter) + (1 | TempC) + (1 | Salinity), data = ChinaR_receivers, REML = FALSE)
anova(rec_lmer_ChinaR_9)
summary(rec_lmer_ChinaR_9)
#emmeans(acc_lmer_ChinaR, list(pairwise ~ survey.period), adjust = "tukey")

rec_lmer_BlackR_9 <- lmer(n.receiver ~ survey.period + (1 | Transmitter) + (1 | TempC) + (1 | Salinity), data = BlackR_receivers, REML = FALSE)
anova(acc_lmer_BlackR_9)
summary(acc_lmer_BlackR_9)

################ OR REPEATED MEASURES ANOVA? for BR, 9 days ##########################
#dataframe of areas of individuals by three periods
#BlackR.9.df <- BlackR.before.df %>% 
 # inner_join(BlackR.dur.df, by='id') %>% 
  #inner_join(BlackR.after.df, by='id')


BR9_receivers_ANOVA <- BlackR_receivers %>%
  #gather(key = "periods", value = "n.receiver", n.before, n.dur, n.after) %>%
  convert_as_factor(Transmitter, Date) %>% 
  select(-periods)


BR9_receivers_ANOVA <- BR9_receivers_ANOVA %>% spread(key = "Date", value = "n.receiver") %>% drop_na()
BR9_receivers_ANOVA <- BR9_receivers_ANOVA %>% gather(key = "Date", value = "n.receiver", `2021-06-01`:`2021-07-01`)

br9.rec.aov <- anova_test(data = BR9_receivers_ANOVA, dv = n.receiver, wid = Transmitter, within = Date)
get_anova_table(br9.rec.aov)

pwc <- BR9_receivers_ANOVA %>%
  pairwise_t_test(
    n.receiver ~ Date, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
pwc

##################### REPEATED MEASURES ANOVA FOR BR receivers, AVG BY PERIOD
#group_by(periods) %>%
 # summarise(mean(n.receiver))

BR9_receivers2_ANOVA <- BlackR_receivers %>% 
  convert_as_factor(Transmitter, Date)


BR9_receivers2_ANOVA <- BR9_receivers2_ANOVA %>% 
  group_by(Transmitter, periods) %>% 
  summarise(mean.rec = mean(n.receiver, na.rm=TRUE)) %>%
  ungroup()
  
 # spread(key = "Date", value = "n.receiver") %>% 
 # group_by(Transmitter) %>%
 # summarise(across("2021-06-01":"2021-07-01", mean, na.rm = TRUE))

br.rec.aov <- anova_test(data = BR9_receivers2_ANOVA, dv = mean.rec, wid = Transmitter, within = periods)
get_anova_table(br.rec.aov)
pwc <- BR9_receivers2_ANOVA %>%
  pairwise_t_test(
    mean.rec ~ periods, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
pwc
  
  
  
BR9_receivers_ANOVA <- BR9_receivers_ANOVA %>% gather(key = "Date", value = "n.receiver", `2021-06-01`:`2021-07-01`)



############### CR REPEATED MEASURES ANOVA FOR 9 DAYS
CR9_receivers_ANOVA <- ChinaR_receivers %>%
  #gather(key = "periods", value = "n.receiver", n.before, n.dur, n.after) %>%
  convert_as_factor(Transmitter, Date) %>% 
  select(-periods)


CR9_receivers_ANOVA <- CR9_receivers_ANOVA %>% spread(key = "Date", value = "n.receiver") %>% drop_na()
CR9_receivers_ANOVA <- CR9_receivers_ANOVA %>% gather(key = "Date", value = "n.receiver", `2021-06-01`:`2021-07-01`)

cr9.rec.aov <- anova_test(data = CR9_receivers_ANOVA, dv = n.receiver, wid = Transmitter, within = Date)
get_anova_table(cr9.rec.aov)

pwc <- CR9_receivers_ANOVA %>%
  pairwise_t_test(
    n.receiver ~ Date, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
pwc


CR9_receivers2_ANOVA <- ChinaR_receivers %>% 
  convert_as_factor(Transmitter, Date)

CR9_receivers2_ANOVA <- CR9_receivers2_ANOVA %>% 
  group_by(Transmitter, periods) %>% 
  summarise(mean.rec = mean(n.receiver, na.rm=TRUE)) %>%
  ungroup()

CR9_receivers2_ANOVA <- CR9_receivers2_ANOVA %>% spread(key = "periods", value = "mean.rec") %>% drop_na()
CR9_receivers2_ANOVA <- CR9_receivers2_ANOVA %>% gather(key = "periods", value = "mean.rec", After:During)

cr.rec.aov <- anova_test(data = CR9_receivers2_ANOVA, dv = mean.rec, wid = Transmitter, within = periods)
get_anova_table(cr.rec.aov)
pwc <- CR9_receivers2_ANOVA %>%
  pairwise_t_test(
    mean.rec ~ periods, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
pwc


#linear regression forjune 9-18, accel by SEL
BR_June_lm <- lm(Sensor.Value ~ SEL, data = BR_June) #Create the linear regression
summary(BR_June_lm)

CR_June_lm <- lm(Sensor.Value ~ SEL, data = CR_June) #Create the linear regression
summary(CR_June_lm)

Lingcod_June_lm <- lm(Sensor.Value ~ SEL, data = Lingcod_June) #Create the linear regression
summary(Lingcod_June_lm)

Dungeness_June_lm <- lm(Sensor.Value ~ SEL, data = Dungeness_June) #Create the linear regression
summary(Dungeness_June_lm)


  

#Mixed effects model for upper 75
acc_u75lmer <- lmer(u75 ~ Period2 + (1|Trial) + Species, data = acc)
summary(acc_u75lmer)




#MLR - add day/night, presence of noise
lmD <- lm(Sensor.Value ~ survey.period + Transmitter, data = periods_Dungeness)
summary(lmD)




