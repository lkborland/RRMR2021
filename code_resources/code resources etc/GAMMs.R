##########################################################################################################################
##########################################################################################################################
########################################## ANALYSIS OF TELEMETRY DATA ################################################
###################################### DOING THE GAMM AND Plot ##############################################
##########################################################################################################################
# Save month panels as 700 wide by 800 tall
# For short single panel do 700 wide by 200 tall
library(ggplot2)
library(readxl)
library(qpcR)
library(mgcv)
library(voxel)
library(tidyr)
library(DHARMa)
#source('//fwnew12/Home/rasmusle/My Documents/R_Code/SummarySE.R')
#source('//fwnew12/Home/rasmusle/My Documents/R_Code/Validgam.R')
#setwd("//fwnew12/Home/rasmusle/My Documents/Telemetry/Deacons/DeaconTelemCOMBINED")
set.seed(49)

#Big dataset
# dataset name is BlackR_acceleration_imputedwind
BRA <- BlackR_acceleration_imputedwind

BRA <- BRA %>% drop_na(Sensor.Value.x, TempC, Salinity, Transmitter.Serial,
                                                       SEL.x, SEL.y, hourtime, WSPD..m.s.)
BRA <- BRA %>% dplyr::select(-Sensor.Precision, -Transmitter.Type, 
                                                             -Tag.Destination, -tag.type, 
                                                             -Transmitter.Name, -Flag,
                                                             -Sensor.Unit, -Receiver,
                                                             -hour, -mm.dd.yyyy)

BRA$Transmitter <- as.factor(BRA$Transmitter)
BRA$detect_day <- as.factor(BRA$detect_day)
BRA$detect_time <- as.factor(BRA$detect_time)
#BRA$hourtime <- as.factor(BRA$hourtime)




#hist(log(BRA$Sensor.Value +1)): no need to log transform
hist(BRA$Sensor.Value)

BRA$Datetime.int <- as.integer(BRA$Date.time.UTC)
BRA$Transmitter <- as.factor(BRA$Transmitter)
cs1AR1 <- corCAR1(form = ~ Datetime.int | Transmitter)
#cs1AR1. <- Initialize(cs1AR1, data = BRA)
#corMatrix(cs1AR1.)

gam1_bra <- gam(Sensor.Value ~ s(TempC) + s(Salinity) + Transmitter + s(SEL.x), data = BRA, method = "REML")
gam2_bra <- gam(Sensor.Value ~ s(TempC) + s(Salinity) + Transmitter + s(SEL.y), data = BRA, method = "REML")
gam3_bra <- gam(Sensor.Value ~ s(TempC) + s(Salinity) + s(Transmitter, bs = "re") + s(SEL.x), data = BRA, method = "REML")
gam5_bra <- gam(Sensor.Value ~ s(TempC) + s(Salinity) + s(Transmitter, bs = "re") + 
                  s(SEL.x), data = BRA, 
                method = "REML", correlation = corCAR1(form = ~ Datetime.int | 1))




gam0_bra <- gam(Sensor.Value.x ~ s(Transmitter, bs = "re"), data = BRA, method = "REML", correlation = corCAR1(form = ~ Datetime.int | Transmitter))

gam1_bra <- gam(Sensor.Value.x ~ s(TempC, bs = "gp") + s(Transmitter, bs = "re"), data = BRA, method = "REML", correlation = corCAR1(form = ~ Datetime.int | Transmitter))

gam2_bra <- gam(Sensor.Value.x ~ s(Salinity, bs = "gp")  + s(TempC, bs = "gp") + s(Transmitter, bs = "re"), data = BRA, method = "REML", correlation = corCAR1(form = ~ Datetime.int | Transmitter))

gam3_bra <- gam(Sensor.Value.x ~ hourtime + s(Salinity, bs = "gp", k = 125)  + s(TempC, bs = "gp", k = 150) + s(Transmitter, bs = "re"), data = BRA, 
                method = "REML", correlation = corCAR1(form = ~ Datetime.int | Transmitter))

gam4_bra <- gam(Sensor.Value.x ~ s(WSPD..m.s.) + hourtime + s(Salinity, bs = "gp", k = 125)  + s(TempC, bs = "gp", k = 150) + s(Transmitter, bs = "re"), data = BRA, 
                method = "REML", correlation = corCAR1(form = ~ Datetime.int | Transmitter))

gam5_bra

#gamx_bra <- gam(Sensor.Value ~ s(TempC, bs = "gp") + s(Transmitter, bs = "re") + s(SEL.x) + s(SEL.y) + s(SPL.x) + s(SPL.y) + s(Peak.SPL.x) + s(Peak.SPL.y),data = BRA, method = "REML", correlation = corCAR1(form = ~ Datetime.int | Transmitter), select = TRUE)



#CO2_modG <- gam(log(uptake) ~ s(log(conc), k=5, bs="tp") + s(Plant_uo, k=12, bs="re"),data=CO2, method="REML", family="gaussian")

summary(gam5_bra)
summary(gam0_bra)
summary(gam3_bra)

gam.check(gam3_bra)

coef(gam5_bra)
plot(gam2_bra, residuals = FALSE, pch = 1)
plot(gam1_bra, residuals = FALSE, pch = 1)
plot(gam3_bra, residuals = FALSE, pch = 1)
plot(gam5_bra, residuals = FALSE, pch = 1)

xAIC <- AIC(gam3_bra)
akaike.weights(xAIC)

#######
re0=gam(log(Accelerometer+1)~s(HR,by=Month,bs="cc")+Month+s(FISHID,bs="re"),correlation=corAR1(form = Seq_DecD | 1), data=Accel)

re1i=gam(log(Accelerometer+1)~s(HR,bs="cc")+Month+s(FISHID,bs="re"),correlation=corAR1(form = Seq_DecD | 1), data=Accel)
re1m=gam(log(Accelerometer+1)~s(HR,by=Month,bs="cc")+s(FISHID,bs="re"),correlation=corAR1(form = Seq_DecD | 1), data=Accel)
re2h=gam(log(Accelerometer+1)~s(HR,bs="cc")+s(FISHID,bs="re"),correlation=corAR1(form = Seq_DecD | 1), data=Accel)
re2m=gam(log(Accelerometer+1)~Month+s(FISHID,bs="re"),correlation=corAR1(form = Seq_DecD | 1), data=Accel)
re3=gam(log(Accelerometer+1)~s(FISHID,bs="re"),correlation=corAR1(form = Seq_DecD | 1), data=Accel)

x=cbind(AIC(re0),AIC(re1i),AIC(re1m),AIC(re2h),AIC(re2m),AIC(re3))
akaike.weights(x)

r0=gam(log(Accelerometer+1)~s(HR,by=Month,bs="cc")+Month,correlation=corAR1(form = Seq_DecD | 1), data=Accel)
r1i=gam(log(Accelerometer+1)~s(HR,bs="cc")+Month,correlation=corAR1(form = Seq_DecD | 1), data=Accel)
r1m=gam(log(Accelerometer+1)~s(HR,by=Month,bs="cc"),correlation=corAR1(form = Seq_DecD | 1), data=Accel)
r2h=gam(log(Accelerometer+1)~s(HR,bs="cc"),correlation=corAR1(form = Seq_DecD | 1), data=Accel)
r2m=gam(log(Accelerometer+1)~Month,correlation=corAR1(form = Seq_DecD | 1), data=Accel)
r3=gam(log(Accelerometer+1)~1,correlation=corAR1(form = Seq_DecD | 1), data=Accel)

x=cbind(x,AIC(r0),AIC(r1i),AIC(r1m),AIC(r2h),AIC(r2m),AIC(r3))
a=akaike.weights(x)

summary(re0)
par(mfrow=c(4,3))
plotGAM(gamFit=re0,smooth.cov="HR",groupCovs="Month",rawOrFitted = "FALSE")+
  facet_wrap(~as.numeric(Month),ncol=3,nrow=4)+xlab('Hour of Day')+
  ylab('log(Acceleration) m'~s^-2)+ggtitle('')
validgam(model=re0,count=log(Accel$Accelerometer+1))

plotGAM(gamFit = gam3_bra, smooth.cov = "SEL.x")
plotGAM(gamFit = gam3_bra, smooth.cov = "Salinity")
validgam(model = gam3_bra, count = BRA$Sensor.Value)

resids<-resid(re0,"pearson")
d1=data.frame(cbind(resids,Accel$HR))
colnames(d1)=c('Residuals','Hour')
ggplot(data=d1,aes(x=Hour,y=Residuals))+geom_point()



#Big dataset
# dataset name is BlackR_HOB


Depth$Depth=abs(Depth$Depth)*(-1)
Depth$Month=as.factor(Depth$Month)
Depth$FISHID=as.factor(Depth$FISHID)
Depth$O2=as.factor(Depth$O2)

#Big dataset
# dataset name is ChinaR_acceleration

ChinaR_acceleration <- ChinaR_acceleration %>% drop_na(Sensor.Value, TempC, Salinity, Transmitter.Serial,
                                                       SEL.x, SEL.y)
ChinaR_acceleration$Transmitter.Serial <- as.factor(ChinaR_acceleration$Transmitter.Serial)
gam1_cra <- gam(Sensor.Value ~ s(TempC) + s(Salinity) + Transmitter + s(SEL.x), 
                data = ChinaR_acceleration, method = "REML")
plot(gam1_cra, residuals = FALSE, pch = 1)









########################### Leif code
#Big dataset
Accel <- read_excel("DeaconTelemAccel_COMBINED.xlsx")
Depth <- read_excel("DeaconTelemDepth_COMBINED.xlsx")
Depth$Depth=abs(Depth$Depth)*(-1)
Depth$Month=as.factor(Depth$Month)
Depth$FISHID=as.factor(Depth$FISHID)
Depth$O2=as.factor(Depth$O2)
Accel$Month=as.factor(Accel$Month)
Accel$FISHID=as.factor(Accel$FISHID)
Accel$O2=as.factor(Accel$O2)

#Acceleration Analysis
hist(log(Accel$Accelerometer)+1)

re0=gam(log(Accelerometer+1)~s(HR,by=Month,bs="cc")+Month+s(FISHID,bs="re"),correlation=corAR1(form = Seq_DecD | 1), data=Accel)
re1i=gam(log(Accelerometer+1)~s(HR,bs="cc")+Month+s(FISHID,bs="re"),correlation=corAR1(form = Seq_DecD | 1), data=Accel)
re1m=gam(log(Accelerometer+1)~s(HR,by=Month,bs="cc")+s(FISHID,bs="re"),correlation=corAR1(form = Seq_DecD | 1), data=Accel)
re2h=gam(log(Accelerometer+1)~s(HR,bs="cc")+s(FISHID,bs="re"),correlation=corAR1(form = Seq_DecD | 1), data=Accel)
re2m=gam(log(Accelerometer+1)~Month+s(FISHID,bs="re"),correlation=corAR1(form = Seq_DecD | 1), data=Accel)
re3=gam(log(Accelerometer+1)~s(FISHID,bs="re"),correlation=corAR1(form = Seq_DecD | 1), data=Accel)

x=cbind(AIC(re0),AIC(re1i),AIC(re1m),AIC(re2h),AIC(re2m),AIC(re3))
akaike.weights(x)

r0=gam(log(Accelerometer+1)~s(HR,by=Month,bs="cc")+Month,correlation=corAR1(form = Seq_DecD | 1), data=Accel)
r1i=gam(log(Accelerometer+1)~s(HR,bs="cc")+Month,correlation=corAR1(form = Seq_DecD | 1), data=Accel)
r1m=gam(log(Accelerometer+1)~s(HR,by=Month,bs="cc"),correlation=corAR1(form = Seq_DecD | 1), data=Accel)
r2h=gam(log(Accelerometer+1)~s(HR,bs="cc"),correlation=corAR1(form = Seq_DecD | 1), data=Accel)
r2m=gam(log(Accelerometer+1)~Month,correlation=corAR1(form = Seq_DecD | 1), data=Accel)
r3=gam(log(Accelerometer+1)~1,correlation=corAR1(form = Seq_DecD | 1), data=Accel)

x=cbind(x,AIC(r0),AIC(r1i),AIC(r1m),AIC(r2h),AIC(r2m),AIC(r3))
a=akaike.weights(x)

summary(re0)
par(mfrow=c(4,3))
plotGAM(gamFit=re0,smooth.cov="HR",groupCovs="Month",rawOrFitted = "FALSE")+
  facet_wrap(~as.numeric(Month),ncol=3,nrow=4)+xlab('Hour of Day')+
  ylab('log(Acceleration) m'~s^-2)+ggtitle('')
validgam(model=re0,count=log(Accel$Accelerometer+1))

resids<-resid(re0,"pearson")
d1=data.frame(cbind(resids,Accel$HR))
colnames(d1)=c('Residuals','Hour')
ggplot(data=d1,aes(x=Hour,y=Residuals))+geom_point()

#Depth Analysis
hist(Depth$Depth)

re0=gam(Depth~s(HR,by=Month,bs="cc")+Month+s(FISHID,bs="re"),correlation=corAR1(form = Seq_DecD | 1), data=Depth)
re1i=gam(Depth~s(HR,bs="cc")+Month+s(FISHID,bs="re"),correlation=corAR1(form = Seq_DecD | 1), data=Depth)
re1m=gam(Depth~s(HR,by=Month,bs="cc")+s(FISHID,bs="re"),correlation=corAR1(form = Seq_DecD | 1), data=Depth)
re2h=gam(Depth~s(HR,bs="cc")+s(FISHID,bs="re"),correlation=corAR1(form = Seq_DecD | 1), data=Depth)
re2m=gam(Depth~Month+s(FISHID,bs="re"),correlation=corAR1(form = Seq_DecD | 1), data=Depth)
re3=gam(Depth~s(FISHID,bs="re"),correlation=corAR1(form = Seq_DecD | 1), data=Depth)

x=cbind(AIC(re0),AIC(re1i),AIC(re1m),AIC(re2h),AIC(re2m),AIC(re3))
akaike.weights(x)

r0=gam(Depth~s(HR,by=Month,bs="cc")+Month,correlation=corAR1(form = Seq_DecD | 1), data=Depth)
r1i=gam(Depth~s(HR,bs="cc")+Month,correlation=corAR1(form = Seq_DecD | 1), data=Depth)
r1m=gam(Depth~s(HR,by=Month,bs="cc"),correlation=corAR1(form = Seq_DecD | 1), data=Depth)
r2h=gam(Depth~s(HR,bs="cc"),correlation=corAR1(form = Seq_DecD | 1), data=Depth)
r2m=gam(Depth~Month,correlation=corAR1(form = Seq_DecD | 1), data=Depth)
r3=gam(Depth~1,correlation=corAR1(form = Seq_DecD | 1), data=Depth)

x=cbind(x,AIC(r0),AIC(r1i),AIC(r1m),AIC(r2h),AIC(r2m),AIC(r3))
akaike.weights(x)

summary(re0)
plotGAM(gamFit=re0,smooth.cov="HR",groupCovs="Month",rawOrFitted = "FALSE")+
  facet_wrap(~as.numeric(Month),ncol=3,nrow=4)+xlab('Hour of Day')+
  ylab('Depth (m)')+ggtitle('')

resids<-resid(re0,"pearson")
d1=data.frame(cbind(resids,Depth$HR))
colnames(d1)=c('Residuals','Hour')
ggplot(data=d1,aes(x=Hour,y=Residuals))+geom_point()
testDispersion(re0)
simulationOutput <- simulateResiduals(fittedModel = re0, plot = T)


#Depth Analysis O2
Depth1=subset(Depth,O2!="NA")
Depth1=subset(Depth,O2=="Normoxia" | O2=="Hypoxia")
hist(Depth1$Depth)

re0=gam(Depth~s(HR,by=O2,bs="cc")+O2+s(FISHID,bs="re"),correlation=corAR1(form = Seq_DecD | 1), data=Depth1)
re1i=gam(Depth~s(HR,bs="cc")+O2+s(FISHID,bs="re"),correlation=corAR1(form = Seq_DecD | 1), data=Depth1)
re1m=gam(Depth~s(HR,by=O2,bs="cc")+s(FISHID,bs="re"),correlation=corAR1(form = Seq_DecD | 1), data=Depth1)
re2h=gam(Depth~s(HR,bs="cc")+s(FISHID,bs="re"),correlation=corAR1(form = Seq_DecD | 1), data=Depth1)
re2m=gam(Depth~O2+s(FISHID,bs="re"),correlation=corAR1(form = Seq_DecD | 1), data=Depth1)
re3=gam(Depth~s(FISHID,bs="re"),correlation=corAR1(form = Seq_DecD | 1), data=Depth1)

x=cbind(AIC(re0),AIC(re1i),AIC(re1m),AIC(re2h),AIC(re2m),AIC(re3))
akaike.weights(x)

r0=gam(Depth~s(HR,by=O2,bs="cc")+O2,correlation=corAR1(form = Seq_DecD | 1), data=Depth1)
r1i=gam(Depth~s(HR,bs="cc")+O2,correlation=corAR1(form = Seq_DecD | 1), data=Depth1)
r1m=gam(Depth~s(HR,by=O2,bs="cc"),correlation=corAR1(form = Seq_DecD | 1), data=Depth1)
r2h=gam(Depth~s(HR,bs="cc"),correlation=corAR1(form = Seq_DecD | 1), data=Depth1)
r2m=gam(Depth~O2,correlation=corAR1(form = Seq_DecD | 1), data=Depth1)
r3=gam(Depth~1,correlation=corAR1(form = Seq_DecD | 1), data=Depth1)

x=cbind(x,AIC(r0),AIC(r1i),AIC(r1m),AIC(r2h),AIC(r2m),AIC(r3))
akaike.weights(x)

summary(re0)
par(mfrow=c(2,2))
plotGAM(gamFit=re0,smooth.cov="HR",groupCovs="O2",rawOrFitted = "FALSE",color="Black")+
  facet_wrap(~O2)+xlab('Hour of Day')+
  ylab('Depth (m)')+ggtitle('')
validgam(model=re0,count=Depth1$Depth)

resids<-resid(re0,"pearson")
d1=data.frame(cbind(resids,Depth1$HR))
colnames(d1)=c('Residuals','Hour')
ggplot(data=d1,aes(x=Hour,y=Residuals))+geom_point()

#Accel Analysis O2
Accel1=subset(Accel,O2!="NA")
Accel1=subset(Accel,O2=="Normoxia" | O2=="Hypoxia")
hist(log(Accel$Accelerometer)+1)

re0=gam(log(Accelerometer+1)~s(HR,by=O2,bs="cc")+O2+s(FISHID,bs="re"),correlation=corAR1(form = Seq_DecD | 1), data=Accel1)
re1i=gam(log(Accelerometer+1)~s(HR,bs="cc")+O2+s(FISHID,bs="re"),correlation=corAR1(form = Seq_DecD | 1), data=Accel1)
re1m=gam(log(Accelerometer+1)~s(HR,by=O2,bs="cc")+s(FISHID,bs="re"),correlation=corAR1(form = Seq_DecD | 1), data=Accel1)
re2h=gam(log(Accelerometer+1)~s(HR,bs="cc")+s(FISHID,bs="re"),correlation=corAR1(form = Seq_DecD | 1), data=Accel1)
re2m=gam(log(Accelerometer+1)~O2+s(FISHID,bs="re"),correlation=corAR1(form = Seq_DecD | 1), data=Accel1)
re3=gam(log(Accelerometer+1)~s(FISHID,bs="re"),correlation=corAR1(form = Seq_DecD | 1), data=Accel1)

x=cbind(AIC(re0),AIC(re1i),AIC(re1m),AIC(re2h),AIC(re2m),AIC(re3))
akaike.weights(x)

r0=gam(log(Accelerometer+1)~s(HR,by=O2,bs="cc")+O2,correlation=corAR1(form = Seq_DecD | 1), data=Accel1)
r1i=gam(log(Accelerometer+1)~s(HR,bs="cc")+O2,correlation=corAR1(form = Seq_DecD | 1), data=Accel1)
r1m=gam(log(Accelerometer+1)~s(HR,by=O2,bs="cc"),correlation=corAR1(form = Seq_DecD | 1), data=Accel1)
r2h=gam(log(Accelerometer+1)~s(HR,bs="cc"),correlation=corAR1(form = Seq_DecD | 1), data=Accel1)
r2m=gam(log(Accelerometer+1)~O2,correlation=corAR1(form = Seq_DecD | 1), data=Accel1)
r3=gam(log(Accelerometer+1)~1,correlation=corAR1(form = Seq_DecD | 1), data=Accel1)

x=cbind(x,AIC(r0),AIC(r1i),AIC(r1m),AIC(r2h),AIC(r2m),AIC(r3))
akaike.weights(x)

summary(re0)
plotGAM(gamFit=re0,smooth.cov="HR",groupCovs="O2",rawOrFitted = "FALSE")+
  facet_wrap(~O2,ncol=3)+xlab('Hour of Day')+
  ylab('log(Acceleration) m' ~s^-2)+ggtitle('')
testDispersion(re0)
simulationOutput <- simulateResiduals(fittedModel = re0, plot = T)

resids<-resid(re0,"pearson")
d1=data.frame(cbind(resids,Accel1$HR))
colnames(d1)=c('Residuals','Hour')
ggplot(data=d1,aes(x=Hour,y=Residuals))+geom_point()