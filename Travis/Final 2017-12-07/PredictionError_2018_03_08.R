.libPaths("D:/library/R")

#load libraries
library(dplyr)
library(quantreg)

## Load loon Contaminant data
setwd("D:/Projects/USGS_R/loons/Travis/Final 2017-12-07")

#read data
data = read.csv("./UseYear/2018_02_27/perchHGPredictData2018_02_27.csv")

data <- data %>% mutate(HgPpbLog = log(HGppm*1000 + 1))

x <- 1

getSSE <- function(pred, obs) {
  x <<- x + 1
  if(length(which(is.na(pred))) != 0 | length(which(is.na(obs))) != 0) stop("Data contains NA values.")
  if(length(pred) != length(obs)) {
    print(pred)
    print(obs)
    stop("Prediction length is not equal to observed length.")
  }
  return(sum(sapply(1:length(pred), function(i) { (pred[i] - obs[i])^2 })))
}

data[which(data$perchHG == 48.79562), ]

test <-
  data %>%
  group_by(DOWID) %>%
  summarise(SSE = getSSE(perchHG, HgPpbLog), cuts = list(unique(SppCut)), n = n())
test$cuts[[1]]
