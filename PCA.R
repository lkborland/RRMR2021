library(corrr)
library(FactoMineR)
library(factoextra)

#PCA only works with numerical - do one individual to understand impacts? compare?
#remove anything that doesn't matter

norm_BRA <- BlackR_acceleration %>% filter(survey.period == "June 11") %>% dplyr::select(-X, -Transmitter, -Station.Name,
                                                  -survey.period, -detect_time, -Date.time.UTC, -detect_day) %>% 
    drop_na() %>% dplyr::select(-Transmitter.Serial, -Sensor.Value, -C0mS.cm, -DepthM)

#filter(Transmitter.Serial == "1383773")

norm_BRA <- scale(norm_BRA)

corr_matrix <- cor(norm_BRA)
pca.BRA <- princomp(corr_matrix)
summary(pca.BRA)

pca.BRA$loadings[, 1:5] #numbers are the components (evaluate from the PCA)

fviz_pca_var(pca.BRA, col.var = "black")



library(kable)
library(sweave)