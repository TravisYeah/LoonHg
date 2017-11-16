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
fishData[ , HGppbLog  := log(HGppm*1000 + 1)]
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

loonBlood
fishData

## Pool all Tamarack data
fishData[ grep("3024100|3024102|9006700", DOWID), DOWID := "3024101" ]
loonBlood[ grep("3024100|3024102|9006700", fishLakeID), fishLakeID := "3024101"]

## change east fox lake to be west fox lake
loonBlood[ grep("East Fox", Lake), fishLakeID := "18029700"]
#################TODO this creates a column of 
#################NA values since only EAST FOX waterways will get values for id
fishData[ grep("EAST FOX", WATERWAY), fishLakeID := "18029700"]
fishData[ grep("EAST FOX", WATERWAY), ] #shown waterway EAST FOX rows here
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

# fishData = fishData[ ! sampleEvent %in% sampleEventRemove, ]
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

# fishData=fishData[ ! SppCut %in% sppCutRemove, ]
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

# fishData=fishData[ ! sampleEvent %in% sampleEventRemove, ]
fishData <- copy(fishData[ ! sampleEvent %in% sampleEventRemove, ])
fishData

## How many samples per 
fishData[ , length(HGppm), by = SppCut][ order(V1, decreasing = FALSE),]
fishData[ , length(HGppm), by = sampleEvent][ order(V1, decreasing = FALSE),][ V1 <=5,]

fishData[ , Censor := FALSE]
fishData[ NDyes == 1, Censor := TRUE]

load("modelOutHGppbLog.rda")

# hold origonal data
hold = fishData
# new data to predict
fishData = data.frame("LgthcmLog"=log(12 + 1), "SppCut"="YP_WHORG", "sampleEvent"="1002300_2012")

#predict new data
predict(modelOut@survreg, newdata=fishData)

#combine hold data and the model predictions
preds = cbind(hold, modelOut@survreg$linear.predictors)
preds[SppCut == "YP_WHORG",] #check out the yellow perch predictions

#plot relationship between log length (cm) and log hg (ppb)
plot(preds$LgthcmLog, preds$V2, xlab="Log Length (cm)", ylab="Log Hg (ppb)")
abline(0,1,col="red")

#plot relationship between observed log Hg (ppb) and predicted log Hg (ppb)
plot(hold$HGppbLog, preds$V2, xlab="Obs Log Hg (ppb)", ylab="Pred Log Hg (ppb)")
abline(0, 1, col="red")

