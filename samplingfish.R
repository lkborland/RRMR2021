

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

periodsbr.night <- dat_BlackR_accel %>% mutate(survey.period = case_when(Date.time.UTC >= May26_start & Date.time.UTC <= June2_end ~ "May 26-June 2",
                                                                         Date.time.UTC >= June10_PM & Date.time.UTC <= June11_PM ~ "June 11 6 hr",
                                                                         Date.time.UTC >= June17_start & Date.time.UTC <= June17_end ~ "June 17",
                                                                         Date.time.UTC >= June18_start & Date.time.UTC <= June18_end ~ "June 18", 
                                                                         Date.time.UTC >= June27_start & Date.time.UTC <= July4_end ~ "June 27-July 4"))

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
#out2df_5 <- out2df %>% dplyr::filter(detect_time <= morningtime) ####### need to filter time periods , don't need to sample for june 11


#out2_b <- out2pm[[1]][[5]] ##need to keep unique IDs when subsetting
av5 <- aov(Sensor.Value ~ survey.period, data= out2df)
summary(av5)
anova(av5)
posthoc5 <- emmeans(av5, list(pairwise ~ survey.period), adjust = "tukey")






##################################################



### select nighttime times for assessment of june 11 - CR acceleration
set.seed(49)

periodscr.night <- dat_ChinaR %>% dplyr::mutate(survey.period = case_when(Date.time.UTC >= May26_start & Date.time.UTC <= June2_end ~ "May 26-June 2",
                                                                         Date.time.UTC >= June10_PM & Date.time.UTC <= June11_PM ~ "June 11 6 hr",
                                                                         Date.time.UTC >= June17_start & Date.time.UTC <= June17_end ~ "June 17",
                                                                         Date.time.UTC >= June18_start & Date.time.UTC <= June18_end ~ "June 18", 
                                                                         Date.time.UTC >= June27_start & Date.time.UTC <= July4_end ~ "June 27-July 4"))

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


outpm <- vector("list", length(crperiodslist))
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

av5 <- aov(Sensor.Value ~ survey.period, data= out2df)
summary(av5)
anova(av5)
posthoc5 <- emmeans(av5, list(pairwise ~ survey.period), adjust = "tukey")

ggplot(out2df, aes(x=survey.period, y=Sensor.Value, fill=survey.period)) + 
  geom_violin(trim=TRUE) +  stat_summary(fun.data=mean_sdl, geom="pointrange", color="black") +
  labs(x = "Survey period", y = "Acceleration", title = "CR sampled accel values", fill="Period")

cr_sampled <- out2df

library(e1071)
skewness(cr_sampled$Sensor.Value, type = 2)
library(vegan)


### select nighttime times for assessment of june 11 - BR HOB
set.seed(49)

periodsbrh.night <- periods_BlackR_HOB %>% dplyr::mutate(survey.period = case_when(Date.time.UTC >= May26_start & Date.time.UTC <= June2_end ~ "May 26-June 2",
                                                                          Date.time.UTC >= June10_PM & Date.time.UTC <= June11_PM ~ "June 11 6 hr",
                                                                          Date.time.UTC >= June17_start & Date.time.UTC <= June17_end ~ "June 17",
                                                                          Date.time.UTC >= June18_start & Date.time.UTC <= June18_end ~ "June 18", 
                                                                          Date.time.UTC >= June27_start & Date.time.UTC <= July4_end ~ "June 27-July 4"))


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
    
    dat2 <- dat1[[j]] %>% distinct(.keep_all = TRUE)
    vect <- dat2 %>% spread(detect_day, HOB)
    outpm[[j]] <- cbind(vect[c(1, 2, 3)], HOB = apply(vect[c(-1, -2, -3)], 1, #i think the problem is here with vector position
                                                               samp, replace = TRUE, size = 1))
  }
  
  out2pm[[i]] <- outpm
  
}

out2df <- dplyr::bind_rows(out2pm)

av5 <- aov(HOB ~ survey.period, data= out2df)
summary(av5)
anova(av5)
emmeans(av5, list(pairwise ~ survey.period), adjust = "tukey")

ggplot(out2df, aes(x=survey.period, y=HOB, fill=survey.period)) + 
  geom_violin(trim=TRUE) +  stat_summary(fun.data=mean_sdl, geom="pointrange", color="black") +
  labs(x = "Survey period", y = "HOB", title = "BR sampled HOB values", fill="Period")