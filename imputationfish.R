# Imputation
library(mice)
library(broom.mixed)
set.seed(49)

################ impute data that is missing  ##########
# Port O SBE data is taken every 10 min, while detections of fish are much more 
# frequent. with a join of these datasets, the model and ANOVA will remove those
# observations that don't have exact SBE data, (every 10 min) so we are missing
# a lot of complexity here. Thus, I impute SBE data to fill in gaps between 
# those 10 minutes to use env. data in the models with the more frequent detections

############ Impute data
### HOB 9 Black Rockfish
init <- mice(BlackR_HOB9, maxit=0)
meth <- init$method
predM <- init$predictorMatrix

predM[!names(predM) %in% c("TempC", "C0mS.cm", "Salinity", "DepthM", "Date.time.UTC")] <- 0
meth[!names(meth) %in% c("TempC", "C0mS.cm", "Salinity", "DepthM")] <- ""

meth[c("TempC", "C0mS.cm", "Salinity", "DepthM")] <- "rf" #impute with random forest method

imp_BR <- mice(BlackR_HOB9, method = meth, predictorMatrix = predM)
BlackR_HOB9_imputed <- complete(imp_BR)



### HOB all Black Rockfish
init2 <- mice(periods_BlackR_HOB_SBE, maxit=0) 
meth2 <- init2$method
predM2 <- init2$predictorMatrix

predM2[!names(predM2) %in% c("TempC", "C0mS.cm", "Salinity", "DepthM", "Date.time.UTC")] <- 0
meth2[!names(meth2) %in% c("TempC", "C0mS.cm", "Salinity", "DepthM")] <- ""

meth2[c("TempC", "C0mS.cm", "Salinity", "DepthM")] <- "rf" #impute with random forest method

imp_BR2 <- mice(periods_BlackR_HOB_SBE, method = meth2, predictorMatrix = predM2)
BlackR_HOBall_imputed <- complete(imp_BR2)

write.csv(BlackR_HOBall_imputed, "D:\\MS research\\RRMR2021ReceiverLogs\\BlackR_HOBall_imputed.csv", row.names=TRUE)

### accel all Black Rockfish
init <- mice(periods_BlackR_accel_SBE, maxit=0) 
meth <- init$method
predM <- init$predictorMatrix

predM[!names(predM) %in% c("TempC", "C0mS.cm", "Salinity", "DepthM", "Date.time.UTC")] <- 0
meth[!names(meth) %in% c("TempC", "C0mS.cm", "Salinity", "DepthM")] <- ""

meth[c("TempC", "C0mS.cm", "Salinity", "DepthM")] <- "rf" #impute with random forest method

imp_BR <- mice(periods_BlackR_accel_SBE, method = meth, predictorMatrix = predM)
BlackR_accelall_imputed <- complete(imp_BR)



### accel all Black Rockfish
init <- mice(periods_BlackR_accel_SBE, maxit=0) 
meth <- init$method
predM <- init$predictorMatrix

predM[!names(predM) %in% c("TempC", "C0mS.cm", "Salinity", "DepthM", "Date.time.UTC")] <- 0
meth[!names(meth) %in% c("TempC", "C0mS.cm", "Salinity", "DepthM")] <- ""

meth[c("TempC", "C0mS.cm", "Salinity", "DepthM")] <- "rf" #impute with random forest method

imp_BR <- mice(periods_BlackR_accel_SBE, method = meth, predictorMatrix = predM)
BlackR_accelall_imputed <- complete(imp_BR)

write.csv(BlackR_accelall_imputed, "D:\\MS research\\RRMR2021ReceiverLogs\\BlackR_accelall_imputed.csv", row.names=TRUE)


### accel all China Rockfish - impute SBE
init <- mice(periods_ChinaR_SBE, maxit=0) 
meth <- init$method
predM <- init$predictorMatrix

predM[!names(predM) %in% c("TempC", "C0mS.cm", "Salinity", "DepthM", "Date.time.UTC")] <- 0
meth[!names(meth) %in% c("TempC", "C0mS.cm", "Salinity", "DepthM")] <- ""

meth[c("TempC", "C0mS.cm", "Salinity", "DepthM")] <- "rf" #impute with random forest method

imp_CR <- mice(periods_ChinaR_SBE, method = meth, predictorMatrix = predM)
ChinaR_accelall_imputed <- complete(imp_CR)

write.csv(ChinaR_accelall_imputed, "D:\\MS research\\RRMR2021ReceiverLogs\\ChinaR_accelall_imputed.csv", row.names=TRUE)


### wind imputation ########################################
## did not work first time - I needed to remove extraneous columns in excel csv file 
## perhaps mice() can only deal with so many columns, even if strictly excluded from imputation
times_BRaccel <- BlackR_accelall_imputed[, c("Date.time.UTC", "Sensor.Value")]
wind_impute_BR_A <- full_join(times_BRaccel, Port_O_wind, by = "Date.time.UTC", multiple = "all")
init <- mice(wind_impute_BR_A, maxit=0)
meth <- init$method
predM <- init$predictorMatrix

predM[!names(predM) %in% c("WSPD..m.s.", "GST..m.s.", "PRES..hPa.",
                           "ATMP..degC.", "WTMP..degC.", "Date.time.UTC")] <- 0 #don't use things that aren't these to predict

meth[!names(meth) %in% c("WDIR..degT.", "WSPD..m.s.", "GST..m.s.", "PRES..hPa.",
                         "ATMP..degC.", "WTMP..degC.")] <- "" #don't impute things that aren't these

meth[c("WDIR..degT.", "WSPD..m.s.", "GST..m.s.", "PRES..hPa.",
       "ATMP..degC.", "WTMP..degC.")] <- "rf" #impute with random forest method

imp_BR <- mice(wind_impute_BR_A, method = meth, predictorMatrix = predM)
BlackR_accel_imputedwind <- complete(imp_BR)

write.csv(BlackR_accel_imputedwind, "D:\\MS research\\RRMR2021ReceiverLogs\\BlackR_accel_imputedwind.csv", row.names=TRUE)

## BR HOB ##########33 HERE ############### HERERERERERERRERER
BlackR_HOBall_imputeddat <- BlackR_HOBall_imputed %>% dplyr::select(-Transmitter.Name, -Sensor.Unit, -Transmitter.Type, -Sensor.Precision, 
                                                                    -BathyRough, -BathyTPI, -BathyTRI, -BathySlope)
times_BRHOB <- BlackR_HOBall_imputeddat[, c("Date.time.UTC", "HOB")]
wind_impute_BR_H <- full_join(times_BRHOB, Port_O_wind, by = "Date.time.UTC", multiple = "all")
init <- mice(wind_impute_BR_H, maxit=0)
meth <- init$method
predM <- init$predictorMatrix

predM[!names(predM) %in% c("WSPD..m.s.", "GST..m.s.", "PRES..hPa.",
                           "ATMP..degC.", "WTMP..degC.", "Date.time.UTC")] <- 0 #don't use things that aren't these to predict

meth[!names(meth) %in% c("WDIR..degT.", "WSPD..m.s.", "GST..m.s.", "PRES..hPa.",
                         "ATMP..degC.", "WTMP..degC.")] <- "" #don't impute things that aren't these

meth[c("WDIR..degT.", "WSPD..m.s.", "GST..m.s.", "PRES..hPa.",
       "ATMP..degC.", "WTMP..degC.")] <- "rf" #impute with random forest method

imp_BR <- mice(wind_impute_BR_H, method = meth, predictorMatrix = predM)
BlackR_HOB_imputedwind <- complete(imp_BR)

write.csv(BlackR_HOB_imputedwind, "D:\\MS research\\RRMR2021ReceiverLogs\\BlackR_HOB_imputedwind.csv", row.names=TRUE)




######## langseth dist imputation
times_BRaccel <- BlackR_accelall_imputed[, c("Date.time.UTC", "Sensor.Value")]
langseth_impute <- dplyr::full_join(times_BRaccel, Langseth_dist, by = "Date.time.UTC", multiple = "all")
init <- mice(langseth_impute, maxit=0)
meth <- init$method
predM <- init$predictorMatrix

predM[!names(predM) %in% c("Lat", "Long", "Distance.to.NS..km.",
                           "Date.time.UTC")] <- 0 #don't use things that aren't these to predict

meth[!names(meth) %in% c("Lat", "Long", "Distance.to.NS..km.")] <- "" #don't impute things that aren't these

meth[c("Lat", "Long", "Distance.to.NS..km.")] <- "rf" #impute with random forest method

#imp_BR <- mice(langseth_impute, method = meth, predictorMatrix = predM)
#langseth_imputed <- complete(imp_BR)

#write.csv(BlackR_accel_imputedwind, "D:\\MS research\\RRMR2021ReceiverLogs\\BlackR_accel_imputedwind.csv", row.names=TRUE)
