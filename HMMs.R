## HMMs 

### BLack rockfish 
#library(tictoc)
library(tidyverse)
library(Hmisc)
library(corrr)
library(flextable)
library(momentuHMM)
library(MuMIn)
library(multcomp)
library(geosphere)


###################################
### Select desired columns of candidate features
m <- BlackR_acceleration_wind
m <- m %>% rename(ID = Transmitter)
m <- m %>% drop_na(ID)

m <- m[with(m, order(ID, Date.time.UTC)),]

#mcenters <- 
#mcentroids <- 

### process data
processedm <- prepData(data=m, type = 'LL', coordNames = c("Longitude", "Latitude"), 
                     covNames=c("TempC","Salinity", "hourtime", "WSPD..m.s.", "survey.period",
                                "GST..m.s.", "ID")) #sensor.value is "data stream"?
## need to add HOB to dataset for cov
plot(processedm$angle)

str(processedm)

### Fit HMM
# initial step distribution natural scale parameters
# distributions for observation processes oriingal attempt
dist <- list(step = "gamma", angle = "wrpcauchy")
# initial parameters
Par0_m1 <- list(step=c(.25,1,.25,.75),angle=c(0.3,0.7))  #(mu_1,mu_2,sd_1,sd_2)
# fit model
m1 <- fitHMM(data = processedm, nbStates = 2, dist = dist, Par0 = Par0_m1,
             estAngleMean = list(angle=FALSE))

DM <- list(step = list(mean = ~ temp * cosinor(hour, period = 24),
                       sd = ~ temp * cosinor(hour, period = 24)),
           angle = list(concentration = ~ temp))

formula_BRA <- ~ temp * cosinor(hour, period = 24)

## Table 1. Candidate covariates for HMM transition probabilities a


#getPar(processedm)
#getParDM(processedm, nbStates = 3, dist = dist)

## next try
nbStates <- 3
stateNames <- c("resting", "foraging", "transit")
dist <- list(step = "gamma", angle = "wrpcauchy", ID = "cat", survey.period = "cat",
             TempC = "gamma", Salinity = "gamma", hourtime = "cat", WSPD..m.s. = "gamma",
             GST..m.s. = "gamma")




#stepPar0 <- c(1,5,0.5,3) # (mu_1,mu_2,sd_1,sd_2)
# initial angle distribution natural scale parameters
#anglePar0 <- c(0,0,1,8) # (mean_1,mean_2,concentration_1,concentration_2)
distBR <- list(step = "gamma", angle = "vm", ID = "cat", survey.period = "cat",
               TempC = "gamma", Salinity = "gamma", hourtime = "cat", WSPD..m.s. = "gamma",
               GST..m.s. = "gamma")
Par0BRA <- 
fitBR <- fitHMM(data = processedm, nbStates = 3,
                    formula = ~ TempC + Salinity + hourtime + WSPD..m.s. + survey.period +
                      GST..m.s. + ID + step + angle,
                dist = distBR,
                Par0 = Par0BRA,
                    stationary = FALSE)




#m <- m %>% dplyr::select(spp,ID,df_heave,hf_heave,m_stheave,sd_stheave,p5_stheave,sd_head,iqr_heave,m_odba) 
#colnames(m) <- c('spp', 'ID', 'df', 'hf', 'ms','ss','p5','sh','iqr','mo')

#### Initial data check
summary(m)
length(which(m$p5<0))/length(m$p5) # check p5 < 0 (all need to be positive numbers)
map(m, ~sum(is.na(.))) # note % of NaNs in full dataset

#### Plot histograms of features for initial exploration
png(filename=paste0(~,'/finalfeat_hist_fulldataset.png'), width = 365, height = 225, units='mm', res = 300)
hist.data.frame(m[,c(3:10)]) 
dev.off

#### Use correlation matrix to identify final set of features
d <- correlate(m[,c(3:10)], quiet = TRUE)
dc<-d %>% 
  fashion()  

dct <- flextable(data = dc) 
dct <- autofit(dct)
dct <- width(dct, j = 3:5, width = 1.5)
dct <- align(dct, align = 'center', part = "body")
dct <- align(dct, align = 'left', part = "all")
dct


#### Select final features in final dataframe to be used in HMM
m<-m %>% dplyr::select(ID,spp,hf,p5,sh,mo)

#### Plot histograms of features across species
mlong<-melt(m, id.vars=c("ID", "spp"))

cairo_ps(filename=paste0(~,'/finalfeat_hist_bySpp.eps'), width=15, height=7)
ggplot(data=mlong, aes(x=value, group=spp, fill=spp))+
  geom_density(adjust=1, alpha=.2)+
  scale_fill_manual(values=c("black", "black", "black", "black"))+
  theme_ipsum() +
  facet_wrap(~variable, nrow=1, scales=c("free")) 
ggtitle("Feature Distributions by Species")
dev.off()
