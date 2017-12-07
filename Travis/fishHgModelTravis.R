#install.packages("tidyverse", dependencies = TRUE, repos = "http://cran.us.r-project.org")
.libPaths('D:/library/R')
.libPaths()
library(data.table, lib = 'D:/library/R')
library(ggplot2, lib = 'D:/library/R')
library(NADA, lib = 'D:/library/R')
setwd('D:/Projects/USGS_R/loons/EricksonFishModel')

## load loon blood
loonBlood <- fread("../LoonAnalysis/LoonHGblood.csv")
loonBlood[ , fishLakeID := LakeID]
loonBlood[ , fishLakeID := gsub( "^0", "", fishLakeID)]

## Load data and reformat it
fishDataRaw <- fread("./inputData/fishUse.csv")
fishData <- fread("./inputData/fishUse.csv")

## Reformat data structure
fishData[ , DOWID := as.character(DOWID)]
fishData[ , HGppmLog  := log(HGppm + 1)]
fishData[ , Lgthcm := Lgthin * 2.54]
fishData[ , LgthinLog := log(Lgthin + 1)]
fishData[ , LgthcmLog := log(Lgthcm + 1)]
fishData[ , SppCut := paste(Spec, Anat, sep = "_")]
fishData[ , sampleEvent := paste(DOWID, YEAR, sep = "_")]

## Edit lake codes
## CHANGE Mantrap to match mantrap used with WQ
fishData[ WATERWAY == "MANTRAP", DOWID := '29015104']

## Change MONONGALIA to match Monogalia used with WQ
fishData[ WATERWAY == "MONONGALIA", DOWID := '34015802']

## Change BIG BIRCH to match BIG BIRCH used with WQ
fishData[ WATERWAY == "BIG BIRCH", DOWID := '77008401']

## Change VERMILION to match EAST VERMILION USED with WQ
fishData[ WATERWAY == "VERMILION", DOWID := '69037801']

## Edit lake names
fishData$WATERWAY[grep("(TAMARACK)", fishData$WATERWAY)]
loonBlood

loonBlood
fishData

## Pool all Tamarack data
fishData[ grep("3024100|3024102|9006700", DOWID), DOWID := "3024101" ]
loonBlood[ grep("3024100|3024102|9006700", fishLakeID), fishLakeID := "3024101"]

## change east fox lake to be west fox lake
loonBlood[ grep("East Fox", Lake), fishLakeID := "18029700"]
#################TODO this creates a column of 
#################NA values since only EAST FOX waterways will get values for id
#fishData[ grep("EAST FOX", WATERWAY), fishLakeID := "18029700"] 
#fishData[ grep("EAST FOX", WATERWAY), ] #shown waterway EAST FOX rows here
#################Trying to fix by creating id col from loonblood

loonBlood
fishData

## Enter in non-detect codes 
ndCodes <- c("K", "KM", "ND")
fishData[ , ND := "no"]
fishData[ Hgcode %in% ndCodes, ND := "yes"]

## create non-detect lables
fishData[ , NDyes := 0]
fishData[ ND == "yes", NDyes := 1]
fishData[ , NDno := 0]
fishData[ ND == "no", NDno := 1]

## merge data 
loonBloodLakeIDs <- loonBlood[ , unique(fishLakeID)]
missingLoonLakes <- loonBloodLakeIDs[!loonBloodLakeIDs %in% fishData[ , unique(DOWID)]]
missingLoonLakes
loonBlood[ !LakeID %in% missingLoonLakes, unique(Lake)]

## Remove data that has too small of number
## Total of 7 fish
dropSpp <- c("SLT_WHORG", "SF_WHORG")
dropSpp

dim(fishData)
fishData <- copy(fishData[ ! SppCut %in% dropSpp, ])
dim(fishData)

## Remove lake evens that have fewer than 5 fish, results in removing 1362 fish
minEvents <- 5
eventDT <- fishData[ , list(Det = sum(NDno), ND = sum(NDyes), total = length(HGppm)),
                     by = sampleEvent]
eventDT[ , prop := round(ND/total, 2)] 
eventDT[ , lakeID := gsub("(\\d+)_(\\d{4})", "\\1", sampleEvent)]
eventDT[ order(total, decreasing = FALSE), ][ total < minEvents,]
dim(eventDT)

sampleEventRemove <- eventDT[ total < minEvents, sampleEvent]
## sampleEventRemove

length(sampleEventRemove)
## sampleEventRemove

## May need line in code to save loon lakes
eventDT[ total < minEvents & lakeID %in%loonBloodLakeIDs, ]
fishData

fishData <- copy(fishData[ ! sampleEvent %in% sampleEventRemove, ])

print(eventDT[ prop > 0, ][order(prop, decreasing = TRUE)], 110)


## Remove sppCuts now that have >= 5 observaitons, also, make sure
## any observaiton with more than 50% ND should be removed

sppCutDT <- fishData[ , list(Det = sum(NDno), ND = sum(NDyes), total = length(HGppm)),
                      by = SppCut]

sppCutDT[ , prop := round(ND/total, 2)]
sppCutDT[ order(prop, decreasing = TRUE)]
sppCutDT[ order(total, decreasing = TRUE)]

sppCutRemove <- sppCutDT[ total <= 5, SppCut]
sppCutRemove

fishData <- copy(fishData[ ! SppCut %in% sppCutRemove, ])

## Need repeate removing lakes with fewer than 5 fish
eventDT <- fishData[ , list(Det = sum(NDno), ND = sum(NDyes), total = length(HGppm)),
                     by = sampleEvent]
eventDT[ , prop := round(ND/total, 2)] 
eventDT[ , lakeID := gsub("(\\d+)_(\\d{4})", "\\1", sampleEvent)]
eventDT[ order(total, decreasing = FALSE), ][ total <5,]

sampleEventRemove <- eventDT[ total < minEvents, sampleEvent]
sampleEventRemove
eventDT[ total < minEvents & lakeID %in%loonBloodLakeIDs, ]
fishData

fishData <- copy(fishData[ ! sampleEvent %in% sampleEventRemove, ])
fishData

## How many samples per 
fishData[ , length(HGppm), by = SppCut][ order(V1, decreasing = FALSE),]
fishData[ , length(HGppm), by = sampleEvent][ order(V1, decreasing = FALSE),][ V1 <=5, ]

fishData[ , Censor := FALSE]
fishData[ NDyes == 1, Censor := TRUE]



## Add back in missing data
## missE <- as.numeric(c("03024101", "29015104", "34015802", "69037801", "77008401"))
## fishDataRaw[ ,unique(DOWID)]
## fishDataRaw[   missE %in% DOWID,]
## str(fishDataRaw)
## missingEvents

# fishData
## Run model
## rm(modelOut)
st <- system.time(
    ## modelOut1 <- fishData[ 1:100,
    ##                       cenreg( Cen(HGppmLog, Censor) ~ LgthinLog : SppCut  +
    ##                                  sampleEvent - 1)]
    modelOut <-
        cenreg( Cen(fishData$HGppmLog, fishData$Censor) ~
                   fishData$LgthcmLog : fishData$SppCut  +
                       fishData$sampleEvent - 1, dist = 'gaussian')
    )

## print(st)/
## save(modelOut, file = "fishHGmodel.rda")
# load("fishHGmodel.rda")
## modelOut
## summary(modelOut)


###########################  HERE  ####################################

# Load the Log Hg (ppm) model
load("modelOutHGppbLog.rda")

# Hold a backup for comparison
saveFishData = fishData

#
#fishData = data.frame(LgthcmLog=log(12+1), SppCut="YP_WHORG", sampleEvent="1002300_2012")
#predict(modelOut@survreg, newdata=fishData)
#fishDataResults=cbind(SaveFishData, modelOut@survreg$linear.predictors)
#colnames(fishDataResults) = c(colnames(fishDataResults)[1:20], "est_HgLogPpb")
loonBlood
test_join=sqldf("SELECT * FROM fishDataResults AS a JOIN loonBlood AS b ON a.DOWID = b.fishLakeID AND a.YEAR = b.UseYear")

#create column for "use year" (closest sampled lake year for loonbloods)
loonBlood[, UseYear := 0]

#subset lake ID's and years in loon bloods and fishDataResults
for(lake in unique(loonBlood$fishLakeID)) {
  subLoon = subset(loonBlood, fishLakeID == lake)
  subFishData = subset(fishDataResults, DOWID == lake)
  print(nrow(subFishData))
  print(lake)
  if(nrow(subFishData) > 0) {
    for(year in subLoon$Year) {
      subLoonYear = subset(subLoon, Year == year)
      closestYear = subFishData$YEAR[which(abs(subFishData$YEAR-year) == min(abs(subFishData$YEAR-year)))[1]]
      #update loonblood UseYear with closest year
      loonBlood[fishLakeID == lake & Year == year, "UseYear"] = closestYear
    }
  }
}

#example
subLoonYear = subset(loonBlood, fishLakeID == '77008900' & Year == 2014)
subFishData = subset(fishDataResults, DOWID == '77008900')
subLoonYear$Year[1]
unique(subFishData$YEAR)
subFishData$YEAR[which(abs(subFishData$YEAR-year) == min(abs(subFishData$YEAR-year)))[1]]

#######################################################################

###############################################  
########## 
########## Warning message:
########## In `[.data.table`(fishData, , `:=`(resids, NULL)) :
########## Adding new column 'resids' then assigning NULL (deleting it).
########## 
########## Warning message:
########## In log(1 - 2 * pnorm(width/2)) : NaNs produced
########## 
fishData[ , resids := NULL]
fishData[ , resids := residuals(modelOut)]
#write.csv("fishDataWithResids.csv", x = fishData, row.names = FALSE)
#write.csv(loonBlood,"D:/projects/usgs_r/loons/ericksonfishmodel/loonBloodTravis.csv", row.names = FALSE)

###################################
### read fishDataWithResids.csv ###
###################################
#fishData = read.csv("fishDataWithResids.csv")
#fishDataRaw = fread("./inputData/fishUse.csv")
#loonBlood = read.csv("loonBloodTravis.csv")
# load("fishHGmodel.rda") #load modelOut
#head(fishData)
#head(fishDataRaw)
# modelOut

fishData[ , length(HGppmLog), by = sampleEvent]
predict(modelOut, c(log(6 + 1), "YP_WHORG", "1002300_2012"))

fishDataPred <- copy(fishData[, list(LgthcmLog, SppCut, sampleEvent)])

fishData[ , SppSampleEvent := paste( sampleEvent, Spec, Anat)]

fishData[, Predicted := modelOut@survreg$linear.predictors]

############################################################################### PLOTS
# ## Plot residules
# lgthResid <- ggplot(fishData, aes(x = LgthcmLog, resids)) +
#   geom_point(alpha = 0.5) +
#   facet_wrap( ~ SppCut, ncol = 7, scales = 'free') +
#   scale_color_manual(values = c("blue", "orange", "black")) +
#   stat_smooth(method = 'lm')
# #ggsave('lgthResid.jpg', lgthResid, width = 14, height = 8.5)
# lgthResid
# 
# lgthResidhist2d <- ggplot(fishData, aes(x = LgthcmLog, resids)) +
#   geom_point(alpha = 0.25) +
#   stat_density2d() + 
#   facet_wrap( ~ SppCut, ncol = 7, scales = 'free')  +
#   scale_color_gradient(trans = "log")
# lgthResidhist2d
# #ggsave('lgthResidhist2d.jpg', lgthResidhist2d, width = 14, height = 8.5)
# 
# 
# sppCutResid <- ggplot(fishData, aes(x = resids)) +
#   geom_histogram() +
#   facet_wrap( ~ SppCut, ncol = 7, scales = 'free') +
#   scale_color_manual(values = c("blue", "orange", "black"))
# sppCutResid
# #ggsave('sppCutResid.jpg', sppCutResid, width = 14, height = 8.5)
# 
# loglengthHist <- ggplot(fishData, aes(x = LgthcmLog)) +
#   geom_histogram() +
#   facet_wrap( ~ SppCut, ncol = 7, scales = 'free') +
#   scale_color_manual(values = c("blue", "orange", "black"))
# loglengthHist
# #ggsave('loglengthHist.jpg', loglengthHist, width = 14, height = 8.5)
# 
# fishData
# 
# ## Plot residules by event
# sampleEventPlot <- fishData[ YEAR > 2000, length(resids),
#                              by = list(sampleEvent)][ V1 > 30, sampleEvent]
# 
# eventResid <- ggplot(fishData[ sampleEvent %in% sampleEventPlot, ], aes(x = resids)) +
#   geom_histogram() +
#   facet_wrap( ~ sampleEvent, ncol = 7, scales = 'free') +
#   scale_color_manual(values = c("blue", "orange", "black"))
# eventResid
# #ggsave('eventResid.jpg', eventResid, width = 14, height = 8.5)
# 
# ypByAnat <- ggplot(fishData[ Spec == "YP", ],
#                    aes(x = LgthcmLog, resids, color = Anat, size = Nofish)) +
#   geom_point(alpha = 0.5) +
#   scale_color_manual(values = c("blue", "orange", "black")) 
# ypByAnat
# #ggsave('ypByAnat.jpg', ypByAnat , width = 14, height = 8.5)
# 
# fishData
# loonBlood
# ## Plot residules by event
# 
# sampleEventPlot <-
#   fishData[ , length(resids),
#             by = list(sampleEvent, Spec, SppSampleEvent)][ V1 >  30, SppSampleEvent ]
# sampleEventPlot
# 
# lgthSampleEvent <- ggplot(fishData[ SppSampleEvent %in% sampleEventPlot, ],
#                           aes(x = LgthcmLog,
#                               y = resids, color = Anat)) +
#   geom_point(alpha = 0.5)  +
#   facet_wrap( ~ SppSampleEvent, ncol = 7, scales = 'free') +
#   scale_color_manual(values = c("blue", "orange", "black"))
# 
# lgthSampleEvent
# #ggsave('lgthSampleEvent.jpg', lgthSampleEvent, width = 14, height = 8.5)
# 
# 
# fishData
# 
# str(modelOut)
# 
# lgthSampleEvent2 <- ggplot(fishData[ SppSampleEvent %in% sampleEventPlot, ],
#                            aes(x = LgthcmLog,
#                                y = Predicted, color = Anat)) +
#   geom_point(alpha = 0.5)  +
#   facet_wrap( ~ SppSampleEvent, ncol = 7, scales = 'free') +
#   scale_color_manual(values = c("blue", "orange", "black")) + 
#   stat_smooth(method = 'lm') 
# ## coord_cartesian(xlim = c(0, 3.5))
# lgthSampleEvent2
# #ggsave('lgthSampleEvent2.jpg', lgthSampleEvent2, width = 14, height = 8.5)
# 
# fishData
# 
# predictedPlot <- ggplot(data = fishData, aes(x = HGppmLog, Predicted)) +
#   geom_point() + stat_smooth(method = 'lm')
# 
# #ggsave("predictedPlot.pdf", predictedPlot)
# 
# 
# predictedPlotYPwhorg <- ggplot(data = fishData[ grep("YP_WHORG", SppCut),],
#                                aes(x = HGppmLog, Predicted)) +
#   geom_point() + stat_smooth(method = 'lm')
# predictedPlotYPwhorg
# #ggsave("predictedPlotYPwhorg.pdf", predictedPlotYPwhorg)

############################################################################### PLOTS END
# 
# fishData
# loonBlood

## Extract coef to mannual estimate lakes effects

coefEst <- coef(modelOut)
head(coefEst)
coefEst[2386] #last row with date in column name      => fishData$sampleEvent9006200_1996
coefEst[2387] #first row without date in column name  => fishData$LgthcmLog:fishData$SppCutBGS_FILSK
coefEst = coefEst[1:2386] #subset data with dates at end of column names

# names(coefEst) <- gsub("fishData\\$sampleEvent", "", names(coefEst))
# head(coefEst)
# coefEstDT <- data.table(ce = coefEst, sampleEvent = names(coefEst))
# head(coefEstDT)
# coefEstDT[ , LakeID :=  gsub("(\\d+)_(\\d+)", "\\1", sampleEvent)]
# coefEstDT[ , Year :=  gsub("(\\d+)_(\\d+)", "\\2", sampleEvent)]

unique(coefEstDT$Year) #check that only years exist in list

coefEstDT[ , Year := as.numeric(Year)]
coefEstDT <- coefEstDT[ !is.na(Year),]

lastYearDT <- coefEstDT[ , list(lastYear = max(Year)), by = LakeID]
setkey(lastYearDT, "LakeID")
setkey(coefEstDT, "LakeID")
coefEstDT <- lastYearDT[coefEstDT]
coefEstDT

############################# TODO
############################# WHAT IS numeric(dim(loonBlood)[1]) DOING????
############################# IT ONLY RETURNS ZEROS
loonBlood[ , useYear := numeric(dim(loonBlood)[1])]
loonBlood[ useYear == 0, useYear := NA]
unique(loonBlood$useYear)


loonBandsToUse <- loonBlood[ !LakeID %in% missingLoonLakes, Band]
loonBlood[, length(Mass), by = list(Band, Year)][ V1 > 1,]

############################# TODO 
############################# useLoonYear length is zero
for(band in loonBandsToUse){
  indexLakeID <-
    loonBlood[ Band == band, fishLakeID][1]
  indexLoonYear <-
    loonBlood[ Band == band, Year][1]
  ## Find year closest to observed year
  useLoonYear <- 
    coefEstDT[ LakeID == indexLakeID, Year][
      which( abs( coefEstDT[ LakeID == indexLakeID, Year] - indexLoonYear ) ==
               min(abs(coefEstDT[ LakeID == indexLakeID, Year] - indexLoonYear)))]
  loonBlood[ Band == band, useYear := useLoonYear]
}

loonBlood[ !is.na(useYear),]
loonBlood[ is.na(useYear),]

coefEstDT
loonBlood
loonBlood[ , fishSampleEvent := paste( fishLakeID, useYear, sep = "_")]
setkey(loonBlood, "fishSampleEvent")
setkey(coefEstDT, "sampleEvent")

coefEstDT[ , c('LakeID', 'lastYear', 'Year') := NULL]
setnames(coefEstDT, "ce", "intercept")

loonBlood <- copy(coefEstDT[loonBlood])

length(coefEst)
## Used 15 cm (15 + 1 from log transform)
loonBlood[ , perchHG := intercept + coefEst[grep("SppCutYP_WHORG",
                                                 names(coefEst))] * log(15 + 1)]
loonBlood

## write.csv(x = loonBlood[ , list(LakeID, Lake, perchHG, useYear, Year, fishLakeID)],
##           file = "perchLoonHGData.csv", row.names = FALSE)

## ## loonBlood2[ , unique(Lake)]

## ## par(mfcol = c(2,2))
## ## plot(modelOut, which = 1)
## ## plot(modelOut, which = 2)
## ## plot(modelOut, which = 3)
## ## plot(modelOut, which = 5)

## ## x11()
## ## plot(modelOut, which = 4)
