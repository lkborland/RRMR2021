library(emmeans)
library(lmerTest)
library(mice)
library(broom.mixed)
library(vctrs)

#sampling observations from 7-day periods
set.seed(49)

#br_ex <- BlackR_HOBall_imputed[,c("Date.time.UTC", "HOB", "TempC", "C0mS.cm", "Salinity", "Transmitter", "hour")]


fish1 <- periodsbr.ex %>% dplyr::filter(Transmitter == "A69-9007-12048")
fish1 <- fish1[,c("Sensor.Value", "survey.period", "detect_day", "detect_time", "Transmitter.Serial")]

fishall <- periodsbr.ex
fishall <- fishall[,c("Sensor.Value", "survey.period", "detect_day", "detect_time", "Transmitter.Serial")]
fishlist <- split(fishall, f = fishall$Transmitter.Serial)

brperiodslist <- split(fish1, f = c(fish1$survey.period))

#####
#brperiodslist1 <- brperiodslist$Early %>% spread(detect_day, Sensor.Value)
#brperiodslist2 <- brperiodslist$`July 5-12` %>% distinct(.keep_all = TRUE) %>% spread(detect_day, Sensor.Value)

#brfish1ex_sampletable <- sample(3:6, nrow(brperiodslist1), replace = TRUE)

#ex2 <- cbind(brperiodslist1[c(1, 2)], Sensor.Value = apply(brperiodslist1[c(-1, -2)], 1, sample, replace = TRUE, size = 1))

#brperiodstbl <- brperiodslist %>% transpose() %>% as_tibble()

samp <- function(x, ...){
  if(all(is.na(x))){
    return(NA)
  }
  return(sample(x[!is.na(x)], ...))
}


out <- vector("list", length(brperiodslist))
out2 <- vector("list", length(fishlist))

for (i in seq_along(brperiodslist)) {
  dat2 <- brperiodslist[[i]] %>% distinct(.keep_all = TRUE)
  vect <- dat2 %>% spread(detect_day, Sensor.Value)
  out[[i]] <- cbind(vect[c(1, 2)], Sensor.Value = apply(vect[c(-1, -2)], 1, 
                                                      samp, replace = TRUE, size = 1))
  #mylist <- list(list1, list2)
}

#str(out)

#ex1 <- split(fishlist[[1]], f = c(fishlist[[1]]$survey.period))
#ex1[[1]] %>% spread(detect_day, Sensor.Value)

for (i in seq_along(fishlist)){
  fishind <- fishlist[[i]]
  dat1 <- split(fishind, f = c(fishind$survey.period))
  for (j in seq_along(dat1)) {
    
    dat2 <- dat1[[j]] %>% distinct(.keep_all = TRUE)
    vect <- dat2 %>% spread(detect_day, Sensor.Value)
    out[[j]] <- cbind(vect[c(1, 2, 3)], Sensor.Value = apply(vect[c(-1, -2, -3)], 1, 
                                                          samp, replace = TRUE, size = 1))
  }
  
  out2[[i]] <- out
  
}

out2df <- dplyr::bind_rows(out2)
out2df_5 <- out2df %>% filter(survey.period %in% c("May 26-June 2", "June 11", "June 17",
                                                   "June 18", "June 27-July 4"))

ggplot(out2df_5, aes(x=survey.period, y=Sensor.Value, fill=survey.period)) + 
  geom_violin(trim=TRUE) +  stat_summary(fun.data=mean_sdl, geom="pointrange", color="black") +
  labs(x = "Survey period", y = "Acceleration", title = "BR sampled accel values", fill="Period")




### select nighttime times for assessment of june 11
set.seed(49)

periodsbr.night <- dat_BlackR_accel %>% mutate(survey.period = case_when(Date.time.UTC >= June3_start & Date.time.UTC <= June10_end ~ "June 3-10",
                                                                         Date.time.UTC >= June10_PM & Date.time.UTC <= June11_PM ~ "June 11 6 hr",
                                                                         Date.time.UTC >= June12_start & Date.time.UTC <= June16_end ~ "June 12-16",
                                                                         Date.time.UTC >= June18_start & Date.time.UTC <= June18_end ~ "June 18", 
                                                                         Date.time.UTC >= June19_start & Date.time.UTC <= June26_end ~ "June 19-26"))

periodsbr.night <- periodsbr.night %>% filter(!(Transmitter == "A69-9007-12072"))
periodsbr.night$Date.time.UTC <- round_date(periodsbr.night$Date.time.UTC, "30 seconds")
periodsbr.night <- periodsbr.night %>% dplyr::mutate(detect_day = date(Date.time.UTC)) %>% 
  dplyr::mutate(detect_time = hms::as_hms(Date.time.UTC))
periodsbr.night$detect_time <- as.POSIXct(periodsbr.night$detect_time, format = "%H:%M:%S")
date(periodsbr.night$detect_time) <- today(tzone = "US/Pacific")


fish1 <- periodsbr.night %>% dplyr::filter(Transmitter == "A69-9007-12048")
fish1 <- fish1[,c("Sensor.Value", "survey.period", "detect_day", "detect_time", "Transmitter.Serial")]

fishall <- periodsbr.night
fishall <- fishall[,c("Sensor.Value", "survey.period", "detect_day", "detect_time", "Transmitter.Serial")]
fishlist <- split(fishall, f = fishall$Transmitter.Serial)

brperiodslist <- split(fish1, f = c(fish1$survey.period))

samp <- function(x, ...){
  if(all(is.na(x))){
    return(NA)
  }
  return(sample(x[!is.na(x)], ...))
}


outpm <- vector("list", length(brperiodslist))
out2pm <- vector("list", length(fishlist))


for (i in seq_along(fishlist)){
  fishind <- fishlist[[i]]
  dat1 <- split(fishind, f = c(fishind$survey.period))
  for (j in seq_along(dat1)) {
    
    dat2 <- dat1[[j]] %>% distinct(.keep_all = TRUE)
    vect <- dat2 %>% spread(detect_day, Sensor.Value)
    outpm[[j]] <- cbind(vect[c(1, 2, 3)], Sensor.Value = apply(vect[c(-1, -2, -3)], 1, 
                                                          samp, replace = TRUE, size = 1))
  }
  
  out2pm[[i]] <- outpm
  
}

out2df <- dplyr::bind_rows(out2pm)
out2df <- out2df %>% dplyr::filter(detect_time >= nighttime | detect_time <= morningtime)
#out2df_5 <- out2df %>% dplyr::filter(detect_time <= morningtime) ####### need to filter time periods , don't need to sample for june 11
noise_labels <- c("Week Before", "June 11", "June 12-16", "June 18",
                  "Week After")
per_lvl <- c("June 3-10", "June 11 6 hr", "June 12-16", "June 18", "June 19-26")
noise_colors <- c("#C7695F", "#91F6FA", "#C7695F", "#91F6FA", 
                             "#C7695F")
                             
violin_BlackR_accel <- ggplot(out2df, aes(x=survey.period, y=Sensor.Value, fill=survey.period)) + 
  geom_violin(trim=TRUE) +  stat_summary(fun.data=mean_sdl, geom="pointrange", color="black") +
  scale_x_discrete(limits=per_lvl, labels = noise_labels) +
  labs(x = "Period of survey", y = expression(paste("Acceleration (m/s"^"2", ")")), title = "Black Rockfish Acceleration by Period", fill="Period")+  
  scale_fill_manual(values = noise_colors, breaks = per_lvl, labels = noise_labels) +
  guides(fill = guide_legend(label.position = "left", label.hjust = 1))

violin_BlackR_accel


#out2_b <- out2pm[[1]][[5]] ##need to keep unique IDs when subsetting

pwc_BR_accelsample <- aov(Sensor.Value ~ factor(survey.period) + Error(factor(Transmitter.Serial)), data = out2df)
summary(pwc_BR_accelsample)

library(emmeans)
emm_BR_accelsample <- emmeans(pwc_BR_accelsample, ~ survey.period)
emm_BR_accelsample
#pairs(emm_BR_accelsample, "Bonferroni")
#pwpp(emm_BR_accelsample)
contrast(emm_BR_accelsample, method = "pairwise", adjust = "bonferroni")
test(emm_BR_accelsample, adjust = "bonferroni")

emm_BRA_df <- data.frame(emm_BR_accelsample)

bxplt_BlackR_accelestmeans <- ggplot(emm_BRA_df, aes(x=survey.period, y=emmean, fill=survey.period)) + 
  geom_point() + stat_summary(fun.data=mean_sdl, geom="pointrange", color="black") +
  geom_errorbar(aes(survey.period, ymin = lower.CL, ymax = upper.CL)) +
  scale_x_discrete(limits=per_lvl, labels = noise_labels) +
  labs(x = "Period of survey", y = expression(paste("Est. marginal mean acceleration (m/s"^"2", ")")), title = "Black Rockfish Est. Marginal Mean Acceleration by Period", fill="Period")+  
  scale_fill_manual(values = noise_colors, breaks = per_lvl, labels = noise_labels) +
  guides(fill = guide_legend(label.position = "left", label.hjust = 1))

bxplt_BlackR_accelestmeans


#av5 <- aov(Sensor.Value ~ survey.period, data= out2df)
#summary(av5)
#anova(av5)
#posthoc5 <- emmeans(av5, list(pairwise ~ survey.period), adjust = "tukey")
#posthoc5



##################################################



### select nighttime times for assessment of june 11 - CR acceleration
set.seed(49)

periodscr.night <- dat_ChinaR %>% dplyr::mutate(survey.period = case_when(Date.time.UTC >= June3_start & Date.time.UTC <= June10_end ~ "June 3-10",
                                                                          Date.time.UTC >= June10_PM & Date.time.UTC <= June11_PM ~ "June 11 6 hr",
                                                                          Date.time.UTC >= June12_start & Date.time.UTC <= June16_end ~ "June 12-16",
                                                                          Date.time.UTC >= June18_start & Date.time.UTC <= June18_end ~ "June 18", 
                                                                          Date.time.UTC >= June19_start & Date.time.UTC <= June26_end ~ "June 19-26"))

periodscr.night <- periodscr.night %>% filter(!(Transmitter == "A69-9007-13265" | Transmitter == "A69-9007-13268"))
periodscr.night$Date.time.UTC <- round_date(periodscr.night$Date.time.UTC, "30 seconds")
periodscr.night <- periodscr.night %>% dplyr::mutate(detect_day = date(Date.time.UTC)) %>% 
  dplyr::mutate(detect_time = hms::as_hms(Date.time.UTC))
periodscr.night$detect_time <- as.POSIXct(periodscr.night$detect_time, format = "%H:%M:%S")
date(periodscr.night$detect_time) <- today(tzone = "US/Pacific")


fish1 <- periodscr.night %>% dplyr::filter(Transmitter == "A69-9007-13267")
fish1 <- fish1[,c("Sensor.Value", "survey.period", "detect_day", "detect_time", "Transmitter.Serial")]

fishall <- periodscr.night
fishall <- fishall[,c("Sensor.Value", "survey.period", "detect_day", "detect_time", "Transmitter.Serial")]
fishlist <- split(fishall, f = fishall$Transmitter.Serial)

crperiodslist <- split(fish1, f = c(fish1$survey.period))

samp <- function(x, ...){
  if(all(is.na(x))){
    return(NA)
  }
  return(sample(x[!is.na(x)], ...))
}


outpmcr <- vector("list", length(crperiodslist))
out2pmcr <- vector("list", length(fishlist))

# fishind <- fishlist[[2]]
# dat1 <- split(fishind, f = c(fishind$survey.period))
# dat2 <- dat1[[1]] %>% distinct(.keep_all = TRUE)
# vect <- dat2 %>% spread(detect_day, Sensor.Value)
# outpm[[j]] <- cbind(vect[c(1, 2, 3)], Sensor.Value = apply(vect[c(-1, -2, -3)], 1, samp, replace = TRUE, size = 1))



for (i in seq_along(fishlist)){
  fishind <- fishlist[[i]]
  dat1 <- split(fishind, f = c(fishind$survey.period))
  for (j in seq_along(dat1)) {
    
    dat2 <- data.frame()
    dat2 <- dat1[[j]] %>% distinct(.keep_all = TRUE)
    vect <- dat2 %>% spread(detect_day, Sensor.Value)
    outpmcr[[j]] <- cbind(vect[c(1, 2, 3)], Sensor.Value = apply(vect[c(-1, -2, -3)], 1, 
                                                               samp, replace = TRUE, size = 1))
  }
  
  out2pmcr[[i]] <- outpmcr
  
}

out2dfcr <- dplyr::bind_rows(out2pmcr)
out2dfcr <- out2dfcr %>% dplyr::filter(detect_time >= nighttime | detect_time <= morningtime)


violin_ChinaR_accel <- ggplot(out2dfcr, aes(x=survey.period, y=Sensor.Value, fill=survey.period)) + 
  geom_violin(trim=TRUE, scale = "width") +  stat_summary(fun.data=mean_sdl, geom="pointrange", color="black") +
  scale_x_discrete(limits=per_lvl, labels = noise_labels) +
  labs(x = "Period of survey", y = expression(paste("Acceleration (m/s"^"2", ")")), title = "China Rockfish Acceleration by Period", fill="Period")+  
  scale_fill_manual(values = noise_colors, breaks = per_lvl, labels = noise_labels) +
  guides(fill = guide_legend(label.position = "left", label.hjust = 1))

violin_ChinaR_accel

library(emmeans)
pwc_CR_accelsample <- aov(Sensor.Value ~ factor(survey.period) + Error(factor(Transmitter.Serial)), data = out2dfcr)
summary(pwc_CR_accelsample)

emm_CR_accelsample <- emmeans(pwc_CR_accelsample, ~ survey.period)
emm_CR_accelsample
#pairs(emm_CR_accelsample, "Bonferroni")
pwpp(emm_CR_accelsample)
contrast(emm_CR_accelsample, method = "pairwise", adjust = "bonferroni")

emm_CRA_df <- data.frame(emm_CR_accelsample)

bxplt_ChinaR_accelestmeans <- ggplot(emm_CRA_df, aes(x=survey.period, y=emmean, fill=survey.period)) + 
  geom_point() + stat_summary(fun.data=mean_sdl, geom="pointrange", color="black") +
  geom_errorbar(aes(survey.period, ymin = lower.CL, ymax = upper.CL)) +
  scale_x_discrete(limits=per_lvl, labels = noise_labels) +
  labs(x = "Period of survey", y = expression(paste("Est. marginal mean acceleration (m/s"^"2", ")")), title = "China Rockfish Est. Marginal Mean Acceleration by Period", fill="Period")+  
  scale_fill_manual(values = noise_colors, breaks = per_lvl, labels = noise_labels) +
  guides(fill = guide_legend(label.position = "left", label.hjust = 1))

bxplt_ChinaR_accelestmeans


#av5 <- aov(Sensor.Value ~ survey.period, data= out2df)
#summary(av5)
#anova(av5)
#posthoc5 <- emmeans(av5, list(pairwise ~ survey.period), adjust = "tukey")

lvls_violin <- c("May 26-June 2", "June 11 6 hr", "June 12-16", "June 18", "June 27-July 4")

ggplot(out2dfcr, aes(x=survey.period, y=Sensor.Value, fill=survey.period)) + 
  geom_violin(trim=TRUE) +  stat_summary(fun.data=mean_sdl, geom="pointrange", color="black") +
  scale_x_discrete(limits=lvls_violin) +
  labs(x = "Survey period", y = "Acceleration", title = "CR sampled accel values", fill="Period")

cr_sampled <- out2dfcr

library(e1071)
skewness(cr_sampled$Sensor.Value, type = 2)
library(vegan)


### select nighttime times for assessment of june 11 - Lingcod acceleration
set.seed(49)

periodslc.night <- dat_Lingcod %>% dplyr::mutate(survey.period = case_when(Date.time.UTC >= June3_start & Date.time.UTC <= June10_end ~ "June 3-10",
                                                                          Date.time.UTC >= June10_PM & Date.time.UTC <= June11_PM ~ "June 11 6 hr",
                                                                          Date.time.UTC >= June12_start & Date.time.UTC <= June16_end ~ "June 12-16",
                                                                          Date.time.UTC >= June18_start & Date.time.UTC <= June18_end ~ "June 18", 
                                                                          Date.time.UTC >= June19_start & Date.time.UTC <= June26_end ~ "June 19-26"))

periodslc.night <- periodslc.night %>% filter(!(Transmitter == "A69-9007-13262"))
periodslc.night$Date.time.UTC <- round_date(periodslc.night$Date.time.UTC, "30 seconds")
periodslc.night <- periodslc.night %>% dplyr::mutate(detect_day = date(Date.time.UTC)) %>% 
  dplyr::mutate(detect_time = hms::as_hms(Date.time.UTC))
periodslc.night$detect_time <- as.POSIXct(periodslc.night$detect_time, format = "%H:%M:%S")
date(periodslc.night$detect_time) <- today(tzone = "US/Pacific")


fish1 <- periodslc.night %>% dplyr::filter(Transmitter == "A69-9007-13249")
fish1 <- fish1[,c("Sensor.Value", "survey.period", "detect_day", "detect_time", "Transmitter.Serial")]

fishall <- periodslc.night
fishall <- fishall[,c("Sensor.Value", "survey.period", "detect_day", "detect_time", "Transmitter.Serial")]
fishlist <- split(fishall, f = fishall$Transmitter.Serial)

lcperiodslist <- split(fish1, f = c(fish1$survey.period))

samp <- function(x, ...){
  if(all(is.na(x))){
    return(NA)
  }
  return(sample(x[!is.na(x)], ...))
}


outpmlc <- vector("list", length(lcperiodslist))
out2pmlc <- vector("list", length(fishlist))


for (i in seq_along(fishlist)){
  fishind <- fishlist[[i]]
  dat1 <- split(fishind, f = c(fishind$survey.period))
  for (j in seq_along(dat1)) {
    
    dat2 <- data.frame()
    dat2 <- dat1[[j]] %>% distinct(.keep_all = TRUE)
    vect <- dat2 %>% spread(detect_day, Sensor.Value)
    outpmlc[[j]] <- cbind(vect[c(1, 2, 3)], Sensor.Value = apply(vect[c(-1, -2, -3)], 1, 
                                                                 samp, replace = TRUE, size = 1))
  }
  
  out2pmlc[[i]] <- outpmlc
  
}

out2dflc <- dplyr::bind_rows(out2pmlc)
out2dflc <- out2dflc %>% dplyr::filter(detect_time >= nighttime | detect_time <= morningtime)


violin_Lingcod_accel <- ggplot(out2dflc, aes(x=survey.period, y=Sensor.Value, fill=survey.period)) + 
  geom_violin(trim=TRUE, scale = "width") +  stat_summary(fun.data=mean_sdl, geom="pointrange", color="black") +
  scale_x_discrete(limits=per_lvl, labels = noise_labels) +
  labs(x = "Period of survey", y = expression(paste("Acceleration (m/s"^"2", ")")), title = "Lingcod Acceleration by Period", fill="Period")+  
  scale_fill_manual(values = noise_colors, breaks = per_lvl, labels = noise_labels) +
  guides(fill = guide_legend(label.position = "left", label.hjust = 1))

violin_Lingcod_accel

library(emmeans)
pwc_LC_accelsample <- aov(Sensor.Value ~ factor(survey.period) + Error(factor(Transmitter.Serial)), data = out2dflc)
summary(pwc_LC_accelsample)

emm_LC_accelsample <- emmeans(pwc_LC_accelsample, ~ survey.period)
emm_LC_accelsample
#pairs(emm_LC_accelsample, "Bonferroni")
#pwpp(emm_LC_accelsample)
contrast(emm_LC_accelsample, method = "pairwise", adjust = "bonferroni")



### select nighttime times for assessment of june 11 - BR HOB
set.seed(49)

periodsbrh.night <- periods_BlackR_HOB %>% dplyr::mutate(survey.period = case_when(Date.time.UTC >= June3_start & Date.time.UTC <= June10_end ~ "June 3-10",
                                                                          Date.time.UTC >= June10_PM & Date.time.UTC <= June11_PM ~ "June 11 6 hr",
                                                                          Date.time.UTC >= June12_start & Date.time.UTC <= June16_end ~ "June 12-16",
                                                                          Date.time.UTC >= June18_start & Date.time.UTC <= June18_end ~ "June 18", 
                                                                          Date.time.UTC >= June19_start & Date.time.UTC <= June26_end ~ "June 19-26"))


periodsbrh.night$Date.time.UTC <- round_date(periodsbrh.night$Date.time.UTC, "30 seconds")
periodsbrh.night <- periodsbrh.night %>% dplyr::mutate(detect_day = date(Date.time.UTC)) %>% 
  dplyr::mutate(detect_time = hms::as_hms(Date.time.UTC))
periodsbrh.night$detect_time <- as.POSIXct(periodsbrh.night$detect_time, format = "%H:%M:%S")
date(periodsbrh.night$detect_time) <- today(tzone = "US/Pacific")


fish1 <- periodsbrh.night %>% dplyr::filter(Transmitter == "A69-9007-12049")
fish1 <- fish1[,c("HOB", "survey.period", "detect_day", "detect_time", "Transmitter.Serial")]

fishall <- periodsbrh.night
fishall <- fishall[,c("HOB", "survey.period", "detect_day", "detect_time", "Transmitter.Serial")]
fishlist <- split(fishall, f = fishall$Transmitter.Serial)

brhperiodslist <- split(fish1, f = c(fish1$survey.period))

samp <- function(x, ...){
  if(all(is.na(x))){
    return(NA)
  }
  return(sample(x[!is.na(x)], ...))
}


outpm <- vector("list", length(brhperiodslist))
out2pm <- vector("list", length(fishlist))


for (i in seq_along(fishlist)){
  fishind <- fishlist[[i]]
  dat1 <- split(fishind, f = c(fishind$survey.period))
  for (j in seq_along(dat1)) {
    
    vect <- data.frame()
    dat2 <- dat1[[j]] %>% dplyr::distinct(.keep_all = TRUE)
    countdate <- dat2 %>% dplyr::distinct(detect_day) %>% nrow()
    dates <- vector("character", length = countdate)
    datesdays <- dat2 %>% dplyr::distinct(detect_day)
    dates <- pull(datesdays, detect_day)
    dates <- as.character(dates)
    vect <- dat2 %>% pivot_wider(names_from = detect_day, values_from = HOB, values_fn = mean)
    #vect <- vect %>% unchop(all_of(dates))
    outpm[[j]] <- cbind(vect[c(1, 2, 3)], HOB = apply(vect[c(-1, -2, -3)], 1, #i think the problem is here with vector position
                                                               samp, replace = TRUE, size = 1))
  }
  
  out2pm[[i]] <- outpm
  
}

out2df <- dplyr::bind_rows(out2pm)
out2df <- out2df %>% dplyr::filter(detect_time >= nighttime | detect_time <= morningtime)

violin_BlackR_HOB <- ggplot(out2df, aes(x=survey.period, y=HOB, fill=survey.period)) + 
  geom_violin(trim=TRUE) +  stat_summary(fun.data=mean_sdl, geom="pointrange", color="black") +
  scale_x_discrete(limits=per_lvl, labels = noise_labels) +
  labs(x = "Period of survey", y = expression(paste("Height off bottom (m)")), title = "Black Rockfish HOB by Period", fill="Period")+  
  scale_fill_manual(values = noise_colors, breaks = per_lvl, labels = noise_labels) +
  guides(fill = guide_legend(label.position = "left", label.hjust = 1))

violin_BlackR_HOB

pwc_HOB_sample <- aov(HOB ~ factor(survey.period) + Error(factor(Transmitter.Serial)), data = out2df)
summary(pwc_HOB_sample)

emm_HOB_sample <- emmeans(pwc_HOB_sample, ~ survey.period)
emm_HOB_sample
#pairs(emm_CR_accelsample, "Bonferroni")
pwpp(emm_HOB_sample)
contrast(emm_HOB_sample, method = "pairwise", adjust = "bonferroni")
emm_BRHOB_df <- data.frame(emm_HOB_sample)


bxplt_BlackR_HOBestmeans <- ggplot(emm_BRHOB_df, aes(x=survey.period, y=emmean, fill=survey.period)) + 
  geom_point() + stat_summary(fun.data=mean_sdl, geom="pointrange", color="black") +
  geom_errorbar(aes(survey.period, ymin = lower.CL, ymax = upper.CL)) +
  scale_x_discrete(limits=per_lvl, labels = noise_labels) +
  labs(x = "Period of survey", y = expression(paste("Est. marginal mean HOB (m)")), title = "Black Rockfish Est. Marginal Mean HOB by Period", fill="Period")+  
  scale_fill_manual(values = noise_colors, breaks = per_lvl, labels = noise_labels) +
  guides(fill = guide_legend(label.position = "left", label.hjust = 1))

bxplt_BlackR_HOBestmeans

ggplot(out2df, aes(x=survey.period, y=HOB, fill=survey.period)) + 
  geom_violin(trim=TRUE) +  stat_summary(fun.data=mean_sdl, geom="pointrange", color="black") +
  labs(x = "Survey period", y = "HOB", title = "BR sampled HOB values", fill="Period")


