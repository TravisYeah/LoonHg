.libPaths('D:/library/R')
library(data.table, lib = 'D:/library/R')
library(ggplot2, lib = 'D:/library/R')
library(NADA, lib = 'D:/library/R')
setwd("D:/Projects/USGS_R/loons/Travis/Final 2017-12-07")

## load loon blood
loonBlood <- fread("./inputData/LoonHGblood.csv")
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
fishData[ , length(HGppm), by = sampleEvent][ order(V1, decreasing = FALSE),][ V1 <=5,]

fishData[ , Censor := FALSE]
fishData[ NDyes == 1, Censor := TRUE]

########################################################################################## The following is from Travis

#loon lake names
unique(loonBlood$Lake)

#fish lake names
unique(fishData$WATERWAY)

#fixing loon lake names (removing parenthesis that explain a location within a lake)
loonBlood[, LakeFixed := Lake]
loonBlood[grep("Rabbit", Lake), LakeFixed := "Rabbit"]
loonBlood[grep("Mantrap", Lake), LakeFixed := "Mantrap"]
loonBlood[grep("Big Birch", Lake), LakeFixed := "Big Birch"]
loonBlood[grep("Arrowhead", Lake), LakeFixed := "Arrowhead"]
loonBlood[grep("Monongalia", Lake), LakeFixed := "Monongalia"]

#fixed loon lake names
unique(loonBlood$LakeFixed)

#add lowercase lake names to each table
loonBlood[, LakeFixedLower := sapply(LakeFixed, tolower)]
fishData[, WaterwayLower := sapply(WATERWAY, tolower)]

#loon lakes not found in fish data
unique(loonBlood[loonBlood$LakeFixedLower %in% fishData$WaterwayLower,"LakeFixedLower"])
fishData$WaterwayLower[grep("rabbit", fishData$WaterwayLower)]

#finding "missing" lakes in fish data
unique(fishData$WaterwayLower[grep("black|bird", fishData$WaterwayLower)]) # "blackduck"    "blackwater"   "blackhawk"    "black island" "black duck"
unique(fishData$WaterwayLower[grep("south|tamarac", fishData$WaterwayLower)]) # "tamarack" "north tamarack" "south twin" "south center" "south lindstrom" "south fowl" "south" "upper south long" "lower south long" "south mcdougal" "south turtle" "south lida"
unique(fishData$WaterwayLower[grep("north|tamarac", fishData$WaterwayLower)]) # "tamarack" "north tamarack" "north center" "north fowl" "northern light" "north" "north long" "north star" "north mcdougal" "north twin" "north turtle" "north lida" "north star steel" "north brown's"   
unique(fishData$WaterwayLower[grep("mckeown", fishData$WaterwayLower)]) # none (is this misspelled?)
unique(fishData$WaterwayLower[grep("mc", fishData$WaterwayLower)]) # none (is this misspelled?)
unique(fishData$WaterwayLower[grep("point", fishData$WaterwayLower)]) # "many point" "disappointment" "sand point"
unique(fishData$WaterwayLower[grep("eagles|nest", fishData$WaterwayLower)]) # "nest" "eagles nest #4" "eagles nest #1" "eagles nest #3"
unique(fishData$WaterwayLower[grep("east|vermilion", fishData$WaterwayLower)]) # "east moore" "east twin" "east toqua" "east rush" "east pike" "east bearskin" "east pope" "east fox" "rabbit (east portion)" "east crooked" "east solomon" "big stone nwr east pool" "east" "east chub" "east graham" "east leaf" "east battle" "east lost" "east spirit" "east vadnais" "vermilion" "little vermilion" "beast" "east lake sylvia"
unique(fishData$WaterwayLower[grep("arr|ow|head", fishData$WaterwayLower)]) # none

# Finding all names with "(", ")", "-" in fish data
unique(fishData$WaterwayLower[grep("\\(|\\)|-", fishData$WaterwayLower)])

# Only values that may be effected by the symbols above is the rabbit lake
sort(unique(loonBlood$LakeFixedLower))

# Here are the effected four rows from the un-QA'd data (these lakes were converted to just "rabbit" after my QA)
loonBlood[grep("Rabbit|rabbit", loonBlood$Lake),]

# If we are focusing on just lakes (not portions of a lake), let's remove the words specifying positions
# fishData[grep("rabbit", WaterwayLower), WaterwayLower := "rabbit"]

# Find nearest fish sample year (UseYear) to loon sample year for each lake
loonBlood[, UseYear := 0]
for(lake in unique(loonBlood$LakeFixedLower)) {
  subLoon = subset(loonBlood, LakeFixedLower == lake)
  subFishData = subset(fishData, WaterwayLower == lake)
  if(nrow(subFishData) > 0) {
    for(year in subLoon$Year) {
      subLoonYear = subset(subLoon, Year == year)
      closestYear = subFishData$YEAR[which(abs(subFishData$YEAR-year) == min(abs(subFishData$YEAR-year)))[1]]
      #update loonblood UseYear with closest year
      loonBlood[LakeFixedLower == lake & Year == year, "UseYear"] = closestYear
    }
  }
}

# UseYear == 0 indicates that there were not matching fish lake names for that loon lake name
loonBlood
unique(loonBlood$LakeFixedLower)

# Remove missing data
loonBloodFinal = subset(loonBlood, UseYear != 0)

# Now we can do our final join to view joined data
library(sqldf)

test=sqldf("SELECT * FROM fishData AS a JOIN loonBlood AS b ON a.DOWID = b.LakeID AND a.YEAR = b.UseYear")
test2=sqldf("SELECT * FROM fishData AS a JOIN loonBlood AS b ON a.WaterwayLower = b.LakeFixedLower AND a.YEAR = b.UseYear WHERE a.DOWID != b.LakeID")
results=sqldf("SELECT * FROM fishData AS a JOIN loonBlood AS b ON a.WaterwayLower = b.LakeFixedLower AND a.YEAR = b.UseYear")
unique(results[ grep("tamarac", Lake, ignore.case=T) ]$Lake)
unique(results$LakeFixedLower)
unique(results[ grep("vermilion", results$Lake, ignore.case=T) ]$Lake)
unique(results[ grep("monongalia", results$Lake, ignore.case=T) ]$Lake)
unique(results[ grep("eagle", results$Lake, ignore.case=T) ]$Lake)
unique(results[ grep("arrow", results$Lake, ignore.case=T) ]$Lake)
unique(results[ grep("wild|rice", results$Lake, ignore.case=T) ]$Lake)
unique(results$Lake)

results <- data.table(results)
results=cbind(results, HGppbLog = log(results$HGppm*1000 + 1))

# converting ppm to ppb
# results[, HGppbLog := log(HGppm*1000 + 1)]

# Then we run our model (must create named lists to avoid programming issues)
HGppbLog = log(results$HGppm*1000 + 1)
Censor = results$Censor
LengthInchesLog = results$LgthinLog
SppCut = results$SppCut
SampleEvent = results$sampleEvent
st <- system.time(
  modelOut <-
    cenreg( Cen(HGppbLog, Censor) ~
              LengthInchesLog : SppCut  +
              SampleEvent - 1, dist = 'gaussian')
)
modelOut
# save the model
save(modelOut, file = "fishHGmodelTravisFinal.rda")

# load the model
# load("fishHGmodelTravisFinal.rda")

# Join results with the data
results=cbind(results, perchHG=modelOut@survreg$linear.predictors)
head(results)

# Add residuals column
results = cbind(results, resids = residuals(modelOut))

# Remove loonblood year column
results = results[,FISHYEAR := YEAR]
results = results[, -c("YEAR")]
results = results[,-c("V1")]

# Check columns matching lake names
data.frame(sqldf("SELECT LakeID, WATERWAY, LakeFixed FROM results GROUP BY LakeID, WATERWAY, LakeFixed"))

# Corrections
results[LakeID == "18009302", LakeID := "18009301"]
results[LakeID == "34015802", WATERWAY := 'Monongalia - Middle Fork Crow River']
results[LakeID == "34015802", LakeFixed := 'Monongalia - Middle Fork Crow River']
results[WATERWAY == "TAMARACK", LakeID := "000001"]

# Use Year
results[LakeID == "000001", c("FISHYEAR", "Year", "UseYear")]

# Write perch & loon joined data/results for loon analysis
with(results, 
     write.csv(data.frame(LakeID, WATERWAY, perchHG, FISHYEAR, Year, UseYear),
                        file = "./UseYear/perchLoonHGData.csv", row.names = FALSE))









################################# run the analytics (Erickson's plots) /w Travis' edits

#************************* Log Length Inches vs residuals per species
lgthResid <- ggplot(results, aes(x = LgthinLog, resids)) +
  geom_point(alpha = 0.5) +
  facet_wrap( ~ SppCut, ncol = 7, scales = 'free') +
  scale_color_manual(values = c("blue", "orange", "black")) +
  stat_smooth(method = 'lm')
ggsave('./plots/lgthResid.jpg', lgthResid, width = 14, height = 8.5)
lgthResid

# density plot
lgthResidhist2d <- ggplot(results, aes(x = LgthinLog, resids)) +
  geom_point(alpha = 0.25) +
  stat_density2d() +
  facet_wrap( ~ SppCut, ncol = 7, scales = 'free')  +
  scale_color_gradient(trans = "log")
lgthResidhist2d
ggsave('./plots/lgthResidhist2d.jpg', lgthResidhist2d, width = 14, height = 8.5)


sppCutResid <- ggplot(results, aes(x = resids)) +
  geom_histogram() +
  facet_wrap( ~ SppCut, ncol = 7, scales = 'free') +
  scale_color_manual(values = c("blue", "orange", "black"))
sppCutResid
ggsave('./plots/sppCutResid.jpg', sppCutResid, width = 14, height = 8.5)

loglengthHist <- ggplot(results, aes(x = LgthinLog)) +
  geom_histogram() +
  facet_wrap( ~ SppCut, ncol = 7, scales = 'free') +
  scale_color_manual(values = c("blue", "orange", "black"))
loglengthHist
ggsave('./plots/loglengthHist.jpg', loglengthHist, width = 14, height = 8.5)

## Plot residules by event
sampleEventPlot <- results[ YEAR > 2000, length(resids),
                             by = list(sampleEvent)][ V1 > 30, sampleEvent]

eventResid <- ggplot(results[ sampleEvent %in% sampleEventPlot, ], aes(x = resids)) +
  geom_histogram() +
  facet_wrap( ~ sampleEvent, ncol = 7, scales = 'free') +
  scale_color_manual(values = c("blue", "orange", "black"))
eventResid
ggsave('./plots/eventResid.jpg', eventResid, width = 14, height = 8.5)

ypByAnat <- ggplot(results[ results$Spec == "YP", ],
                   aes(x = LgthinLog, resids, color = Anat, size = Nofish)) +
  geom_point(alpha = 0.5) +
  scale_color_manual(values = c("blue", "orange", "black"))
ypByAnat
ggsave('./plots/ypByAnat.jpg', ypByAnat , width = 14, height = 8.5)


## Plot residules by event
sampleEventPlot <-
  results[ , length(resids),
            by = list(sampleEvent, Spec, SppSampleEvent)][ V1 >  30, SppSampleEvent ]
sampleEventPlot

lgthSampleEvent <- ggplot(results[ SppSampleEvent %in% sampleEventPlot, ],
                          aes(x = LgthinLog,
                              y = resids, color = Anat)) +
  geom_point(alpha = 0.5)  +
  facet_wrap( ~ SppSampleEvent, ncol = 7, scales = 'free') +
  scale_color_manual(values = c("blue", "orange", "black"))

lgthSampleEvent
ggsave('./plots/lgthSampleEvent.jpg', lgthSampleEvent, width = 14, height = 8.5)


results

str(modelOut)

lgthSampleEvent2 <- ggplot(results[ SppSampleEvent %in% sampleEventPlot, ],
                           aes(x = LgthinLog,
                               y = Predicted, color = Anat)) +
  geom_point(alpha = 0.5)  +
  facet_wrap( ~ SppSampleEvent, ncol = 7, scales = 'free') +
  scale_color_manual(values = c("blue", "orange", "black")) +
  stat_smooth(method = 'lm')
## coord_cartesian(xlim = c(0, 3.5))
lgthSampleEvent2
ggsave('./plots/lgthSampleEvent2.jpg', lgthSampleEvent2, width = 14, height = 8.5)

write.csv(results, "./UseYear/LoonData.csv")

#******************** Ploting loon Log Hh ppb against predicted fish Log Hg ppb with linear model
predictedPlot <- ggplot(data = results, aes(x = HGppbLog, Predicted)) +
  geom_point() + stat_smooth(method = 'lm')
predictedPlot

ggsave("./plots/predictedPlot.pdf", predictedPlot)


predictedPlotYPwhorg <- ggplot(data = results[ grep("YP_WHORG", SppCut),],
                               aes(x = HGppbLog, y = Predicted)) +
  geom_point() + stat_smooth(method = 'lm')
predictedPlotYPwhorg
ggsave("./plots/predictedPlotYPwhorg.pdf", predictedPlotYPwhorg)
results[ grep("YP_WHORG", SppCut),]

