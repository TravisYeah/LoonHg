.libPaths("D:/library/R")

#load libraries
library(dplyr)
library(quantreg)

## Load loon Contaminant data
setwd("D:/Projects/USGS_R/loons/Travis/2018_03_09")

#read data
data = read.csv("./UseYear/perchHGPredictData2018_03_09.csv")

#get log Hg ppb
data <- data %>% mutate(HgPpbLog = log(HGppm*1000 + 1))

# calculate SSE for predicted vs observed vectors
getSSE <- function(pred, obs) {
  if(length(which(is.na(pred))) != 0 | length(which(is.na(obs))) != 0) stop("Data contains NA values.")
  if(length(pred) != length(obs)) {
    print(pred)
    print(obs)
    stop("Prediction length is not equal to observed length.")
  }
  return(sum(sapply(1:length(pred), function(i) { (pred[i] - obs[i])^2 })))
}

# calculate RMSE_w
getRMSE_w <- function(n, p, SSE, sum_w) {
  return(sqrt((n*SSE)/((n-p)*sum_w)))
}

# calculate PE
getPE <- function(RMSE_w) {
  return(100*(exp(RMSE_w-(RMSE_w^2)/2)-1))
}

# get coefficients to calculate RMSE_w for each site
coefs_per_site <-
  data %>%
  group_by(DOWID) %>%
  summarise(SSE = getSSE(perchHG, HgPpbLog), sum_w = sum(Nofish), n = n())

# calculat RMSE_w
RMSE_w_per_site <-
  coefs_per_site %>%
  mutate(RMSE_w = getRMSE_w(n, 3, SSE, sum_w)) #3 is the number of parameters being estimated?

# calculate PE for each site
PE_per_site <-
  RMSE_w_per_site %>%
  mutate(PE = getPE(RMSE_w))

# record results
write.csv(PE_per_site, "./Model Performance/RMSE_E and PE per site.csv", row.names = F)

# Checkout the prediction errors
PE_per_site %>%
  filter(PE < 0)

PE_per_site %>%
  summarise(mean = mean(PE, na.rm=T), sd = sd(PE, na.rm=T))
