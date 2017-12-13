.libPaths('D:/library/R')
library(data.table)
library(ggplot2)
library(NADA)
setwd('D:/Projects/USGS_R/loons/Travis/Final Re-Work 2017-12-08/EricksonFishModel')


## load loon blood
loonBlood <- fread("../LoonAnalysis/LoonHGblood.csv")
loonBlood[ , fishLakeID := LakeID]
loonBlood[ , fishLakeID := gsub( "^0", "", fishLakeID)]

## Load data and reformat it
fishData <- fread("./inputData/fishUse.csv")

# Change lake names to lowercase
loonBlood[, Lake := sapply(Lake, tolower)]
fishData[, WATERWAY := sapply(WATERWAY, tolower)]

#fix last few lake names
loonBlood[ Lake == "monongalia - middle fork crow river", Lake := "monongalia"]
loonBlood[ Lake == "mantrap (west arm)", Lake := "mantrap"]
loonBlood[ Lake == "big birch (ne portion)", Lake := "big birch"]

# Recreate ids by matching names
lake_vals = sort(unique(loonBlood$Lake))
for(i in 1:length(lake_vals)) {
  fishData[WATERWAY == lake_vals[i], DOWID := i]
  loonBlood[Lake == lake_vals[i], fishLakeID := i]
}

## remove loon data with no match fish data
loonBloodLakeIDs <- loonBlood[ , unique(fishLakeID)]
missingLoonLakes <- loonBloodLakeIDs[!loonBloodLakeIDs %in% fishData[ , unique(DOWID)]]
missingLoonLakes
loonBlood[ !LakeID %in% missingLoonLakes, unique(Lake)]

## Reformat data structure
fishData[ , DOWID := as.character(DOWID)]
fishData[ , HGppmLog  := log(HGppm + 1)]
fishData[ , Lgthcm := Lgthin * 2.54]
fishData[ , LgthinLog := log(Lgthin + 1)]
fishData[ , LgthcmLog := log(Lgthcm + 1)]
fishData[ , SppCut := paste(Spec, Anat, sep = "_")]
fishData[ , sampleEvent := paste(DOWID, YEAR, sep = "_")]


# # Edit lake codes
# ## CHANGE Mantrap to match mantrap used with WQ
# fishData[ WATERWAY == "MANTRAP", DOWID := '29015104']
# 
# ## Change MONONGALIA to match Monogalia used with WQ
# fishData[ WATERWAY == "MONONGALIA", DOWID := '34015802']
# 
# ## Change BIG BIRCH to match BIG BIRCH used with WQ
# fishData[ WATERWAY == "BIG BIRCH", DOWID := '77008401']
# 
# ## Change VERMILION to match EAST VERMILION USED with WQ
# fishData[ WATERWAY == "VERMILION", DOWID := '69037801']
# 
# ## Pool all Tamarack data
# fishData[ grep("3024100|3024102|9006700", DOWID), DOWID := "3024101" ]
# loonBlood[ grep("3024100|3024102|9006700", fishLakeID), fishLakeID := "3024101"]
# 
# ## change east fox lake to be west fox lake
# loonBlood[ grep("East Fox", Lake), fishLakeID := "18029700"]
# fishData[ grep("EAST FOX", WATERWAY), fishLakeID := "18029700"]

# Enter in non-detect codes
ndCodes <- c("K", "KM", "ND")
fishData[ , ND := "no"]
fishData[ Hgcode %in% ndCodes, ND := "yes"]
# 
## create non-detect lables
fishData[ , NDyes := 0]
fishData[ ND == "yes", NDyes := 1]
fishData[ , NDno := 0]
fishData[ ND == "no", NDno := 1]

## Remove data that has too small of number
## Total of 7 fish
dropSpp <- c("SLT_WHORG", "SF_WHORG")
dropSpp

fishData <- copy(fishData[ ! SppCut %in% dropSpp, ])

# Remove lake events that have fewer than 5 fish, results in removing 1362 fish
minEvents <- 5
eventDT <- fishData[ , list(Det = sum(NDno), ND = sum(NDyes), total = length(HGppm)),
         by = sampleEvent]
eventDT[ , prop := round(ND/total, 2)]
eventDT[ , lakeID := gsub("(\\d+)_(\\d{4})", "\\1", sampleEvent)]
eventDT[ order(total, decreasing = FALSE), ][ total < minEvents,]
# dim(eventDT)

sampleEventRemove <- eventDT[ total < minEvents, sampleEvent]

eventDT[ total < minEvents & lakeID %in%loonBloodLakeIDs, ]

fishData <- copy(fishData[ ! sampleEvent %in% sampleEventRemove, ])


# Remove sppCuts now that have >= 5 observaitons, also, make sure
# any observaiton with more than 50% ND should be removed

sppCutDT <- fishData[ , list(Det = sum(NDno), ND = sum(NDyes), total = length(HGppm)),
         by = SppCut]

sppCutDT[ , prop := round(ND/total, 2)]
sppCutDT[ order(prop, decreasing = TRUE)]
sppCutDT[ order(total, decreasing = TRUE)]

sppCutRemove <- sppCutDT[ total <= 5, SppCut]

fishData <- copy(fishData[ ! SppCut %in% sppCutRemove, ])

## Need repeat removing lakes with fewer than 5 fish
eventDT <- fishData[ , list(Det = sum(NDno), ND = sum(NDyes), total = length(HGppm)),
         by = sampleEvent]
eventDT[ , prop := round(ND/total, 2)]
eventDT[ , lakeID := gsub("(\\d+)_(\\d{4})", "\\1", sampleEvent)]
eventDT[ order(total, decreasing = FALSE), ][ total < 5,]

sampleEventRemove <- eventDT[ total < minEvents, sampleEvent ]

eventDT[ total < minEvents & lakeID %in%loonBloodLakeIDs, ]

fishData <- copy(fishData[ ! sampleEvent %in% sampleEventRemove, ])

## How many samples per
fishData[ , length(HGppm), by = SppCut][ order(V1, decreasing = FALSE),]
fishData[ , length(HGppm), by = sampleEvent][ order(V1, decreasing = FALSE),][ V1 <=5,]

fishData[ , Censor := FALSE]
fishData[ NDyes == 1, Censor := TRUE]

# Run model
HGppbLog = log(fishData$HGppm*1000 + 1)
Censor = fishData$Censor
LengthInchesLog = fishData$LgthinLog
SppCut = fishData$SppCut
SampleEvent = fishData$sampleEvent
st <- system.time(
    modelOut <- cenreg( Cen(HGppbLog, Censor) ~ LengthInchesLog : SppCut + SampleEvent - 1, dist = 'gaussian')
)
print(st)
save(modelOut, file = "fishHGmodel.rda")
# load("fishHGmodel.rda")
# summary(modelOut)

# Get model residuals
fishData[ , resids := NULL]
fishData[ , resids := residuals(modelOut)]

# Add columns with correct units
fishData[, HGppbLog := log(HGppm*1000 + 1)]

write.csv("fishDataWithResids.csv", x = fishData, row.names = FALSE)

fishData[ , length(HGppbLog), by = sampleEvent]
# predict(modelOut, c(log(6 + 1), "YP_WHORG", "1002300_2012"))

fishDataPred <- copy(fishData[, list(LgthinLog, SppCut, sampleEvent)])

## Plot residules
lgthResid <- ggplot(fishData, aes(x = LgthinLog, resids)) +
    geom_point(alpha = 0.5) +
        facet_wrap( ~ SppCut, ncol = 7, scales = 'free') +
            scale_color_manual(values = c("blue", "orange", "black")) +
                stat_smooth(method = 'lm')
ggsave('lgthResid.jpg', lgthResid, width = 14, height = 8.5)
lgthResid

lgthResidhist2d <- ggplot(fishData, aes(x = LgthinLog, resids)) +
    geom_point(alpha = 0.25) +
    stat_density2d() + 
        facet_wrap( ~ SppCut, ncol = 7, scales = 'free')  +
            scale_color_gradient(trans = "log")
lgthResidhist2d
ggsave('lgthResidhist2d.jpg', lgthResidhist2d, width = 14, height = 8.5)


sppCutResid <- ggplot(fishData, aes(x = resids)) +
    geom_histogram() +
        facet_wrap( ~ SppCut, ncol = 7, scales = 'free') +
            scale_color_manual(values = c("blue", "orange", "black"))
sppCutResid
ggsave('sppCutResid.jpg', sppCutResid, width = 14, height = 8.5)

loglengthHist <- ggplot(fishData, aes(x = LgthinLog)) +
    geom_histogram() +
        facet_wrap( ~ SppCut, ncol = 7, scales = 'free') +
            scale_color_manual(values = c("blue", "orange", "black"))
loglengthHist
ggsave('loglengthHist.jpg', loglengthHist, width = 14, height = 8.5)

## Plot residules by event
sampleEventPlot <- fishData[ YEAR > 2000, length(resids),
                            by = list(sampleEvent)][ V1 > 30, sampleEvent]

eventResid <- ggplot(fishData[ sampleEvent %in% sampleEventPlot, ], aes(x = resids)) +
    geom_histogram() +
        facet_wrap( ~ sampleEvent, ncol = 7, scales = 'free') +
            scale_color_manual(values = c("blue", "orange", "black"))
eventResid
ggsave('eventResid.jpg', eventResid, width = 14, height = 8.5)

ypByAnat <- ggplot(fishData[ Spec == "YP", ],
       aes(x = LgthinLog, resids, color = Anat, size = Nofish)) +
    geom_point(alpha = 0.5) +
        scale_color_manual(values = c("blue", "orange", "black")) 
ypByAnat
ggsave('ypByAnat.jpg', ypByAnat , width = 14, height = 8.5)


## Plot residules by event

fishData[ , SppSampleEvent := paste( sampleEvent, Spec, Anat)]

sampleEventPlot <-
    fishData[ , length(resids),
             by = list(sampleEvent, Spec, SppSampleEvent)][ V1 >  30, SppSampleEvent ]
sampleEventPlot

lgthSampleEvent <- ggplot(fishData[ SppSampleEvent %in% sampleEventPlot, ],
                          aes(x = LgthinLog,
                              y = resids, color = Anat)) +
    geom_point(alpha = 0.5)  +
        facet_wrap( ~ SppSampleEvent, ncol = 7, scales = 'free') +
            scale_color_manual(values = c("blue", "orange", "black"))

lgthSampleEvent
ggsave('lgthSampleEvent.jpg', lgthSampleEvent, width = 14, height = 8.5)

fishData[, Predicted := modelOut@survreg$linear.predictors]

lgthSampleEvent2 <- ggplot(fishData[ SppSampleEvent %in% sampleEventPlot, ],
                          aes(x = LgthinLog,
                              y = Predicted, color = Anat)) +
    geom_point(alpha = 0.5)  +
        facet_wrap( ~ SppSampleEvent, ncol = 7, scales = 'free') +
            scale_color_manual(values = c("blue", "orange", "black")) + 
                stat_smooth(method = 'lm') 
                    ## coord_cartesian(xlim = c(0, 3.5))
lgthSampleEvent2
ggsave('lgthSampleEvent2.jpg', lgthSampleEvent2, width = 14, height = 8.5)

predictedPlot <- ggplot(data = fishData, aes(x = HGppbLog, Predicted)) +
    geom_point() + stat_smooth(method = 'lm')

ggsave("predictedPlot.pdf", predictedPlot)


predictedPlotYPwhorg <- ggplot(data = fishData[ grep("YP_WHORG", SppCut),],
                               aes(x = HGppbLog, Predicted)) +
    geom_point() + stat_smooth(method = 'lm')
predictedPlotYPwhorg
ggsave("predictedPlotYPwhorg.pdf", predictedPlotYPwhorg)

## Extract coef to mannual estimate lakes effects

coefEst <- coef(modelOut)
names(coefEst) <- gsub("SampleEvent", "", names(coefEst))
coefEstDT <- data.table(ce = coefEst, sampleEvent = names(coefEst))
coefEstDT[ , LakeID :=  gsub("(\\d+)_(\\d+)", "\\1", sampleEvent)]
coefEstDT[ , Year :=  gsub("(\\d+)_(\\d+)", "\\2", sampleEvent)]
coefEstDT[ , Year := as.numeric(Year)]
coefEstDT <- coefEstDT[ !is.na(Year),]

# lastYearDT <- coefEstDT[ , list(lastYear = max(Year)), by = LakeID]
# setkey(lastYearDT, "LakeID")
# setkey(coefEstDT, "LakeID")
# coefEstDT <- lastYearDT[coefEstDT]


# loonBandsToUse <- loonBlood[ !LakeID %in% missingLoonLakes, Band]
# loonBlood[, length(Mass), by = list(Band, Year)][ V1 > 1,]

# for(band in loonBandsToUse) {
#     indexLakeID <-
#         loonBlood[ Band == band, fishLakeID][1]
#     indexLoonYear <-
#         loonBlood[ Band == band, Year][1]
#     ## Find year closest to observed year
#     useLoonYear <-       
#         coefEstDT[ LakeID == indexLakeID, Year][
#                        which( abs(coefEstDT[ LakeID == indexLakeID, Year] -
#                                       indexLoonYear) ==
#                                           min(abs(coefEstDT[ LakeID ==
#                                                                 indexLakeID,
#                                                             Year] -
#                                                                 indexLoonYear)))]
#     loonBlood[ Band == band, useYear := useLoonYear]
# }
# Find nearest fish sample year (UseYear) to loon sample year for each lake
loonBlood[, useYear := 0]
for(lake in unique(loonBlood$Lake)) {
  subLoon = subset(loonBlood, Lake == lake)
  subFishData = subset(fishData, WATERWAY == lake)
  if(nrow(subFishData) > 0) {
    for(year in subLoon$Year) {
      subLoonYear = subset(subLoon, Year == year)
      closestYear = subFishData$YEAR[which(abs(subFishData$YEAR-year) == min(abs(subFishData$YEAR-year)))[1]]
      #update loonblood UseYear with closest year
      loonBlood[Lake == lake & Year == year, "useYear"] = closestYear
    }
  }
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


## Used 6 in (6 + 1 from log transform)
loonBlood[ , perchHG := intercept + coefEst[grep("SppCutYP_WHORG",
                            names(coefEst))] * log(12 + 1)]
loonBlood

write.csv(x = loonBlood[ , list(LakeID, Lake, perchHG, useYear, Year)],
          file = "perchLoonHGData.csv", row.names = FALSE)

## ## loonBlood2[ , unique(Lake)]

## ## par(mfcol = c(2,2))
## ## plot(modelOut, which = 1)
## ## plot(modelOut, which = 2)
## ## plot(modelOut, which = 3)
## ## plot(modelOut, which = 5)

## ## x11()
## ## plot(modelOut, which = 4)
