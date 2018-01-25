.libPaths('D:/library/R')
library(sqldf)
library(data.table, lib = 'D:/library/R')
library(ggplot2, lib = 'D:/library/R')
library(NADA, lib = 'D:/library/R')
setwd("D:/Projects/USGS_R/loons/Travis/Final 2017-12-07")

## load loon blood
loonBlood <- fread("./inputData/LoonHGblood.csv")
loonBlood[ , LakeID := gsub( "^0", "", LakeID)]

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

## Pool all Tamarac data
fishData[ grep("3024100|3024102|9006700", DOWID), DOWID := "3024101" ]
loonBlood[ grep("3024100|3024102|9006700", LakeID), LakeID := "3024101"]

# Fix south tamarac data by name
loonBlood[ grep("tamarac", Lake, ignore.case=T), ]
fishData[ grep("tamarac", WATERWAY, ignore.case=T), ]

# Fix north tamarac data by name
loonBlood[ grep("tamarac", Lake, ignore.case=T), ]
fishData[ grep("tamarac", WATERWAY, ignore.case=T), ]

# Pool all George data
fishData[ grep("GEORGE", WATERWAY, ignore.case = T), DOWID := "02009100"]
loonBlood[ grep("george", Lake, ignore.case = T), LakeID := "02009100" ]

# Pool all clearwater data (CUSTOM ID)
fishData[grep("clearwater", WATERWAY, ignore.case=T), DOWID := "849308240824"]
loonBlood[grep("clearwater", Lake, ignore.case=T), LakeID := "849308240824"]

## change east fox lake to be west fox lake (this was an Erickson change)
loonBlood[ grep("East Fox", Lake), LakeID := "18029700"]
loonBlood[ grep("East Fox", Lake), Lake := "West Fox"]

# Pool all east rabbit data
loonBlood[grep("east rabbit", Lake, ignore.case=T), LakeID := "8235982375"]
fishData[grep("(east.*rabbit)|(rabbit.*east)", WATERWAY, ignore.case=T), DOWID := "8235982375"]

# pool all west rabbit data
loonBlood[grep("west rabbit", Lake, ignore.case=T), LakeID := "18009302"]

#pool all burntside data
loonBlood[grep("burntside", Lake, ignore.case=T), LakeID := "69011800" ]

#pool all little burch
loonBlood[grep("little birch", Lake, ignore.case=T), LakeID := "77008900"]

#pool wild rice data
loonBlood[grep("wild|rice", Lake, ignore.case=T), LakeID := "69037100"]

#pool south turtle data
loonBlood[grep("(south.*turtle)|(turtle.*south)", Lake, ignore.case = T), LakeID := "56037700"]

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

# Then we run our model (must create named lists to avoid programming issues)
HGppbLog = log(fishData$HGppm*1000 + 1)
Censor = fishData$Censor
LengthInchesLog = fishData$LgthinLog
SppCut = fishData$SppCut
SampleEvent = fishData$sampleEvent
st <- system.time(
  modelOut <-
    cenreg( Cen(HGppbLog, Censor) ~
              LengthInchesLog : SppCut  +
              SampleEvent - 1, dist = 'gaussian')
)
modelOut
# save the model
save(modelOut, file = "fishHGmodelHgPpb.rda")

# load the model
load("fishHGmodelHgPpb.rda")

# Join results with the data
results=cbind(fishData, perchHG=modelOut@survreg$linear.predictors)
head(results)

# Add residuals column
results = cbind(results, resids = residuals(modelOut))

# Write all predicted perch hg results
with(results, 
      write.csv(results,
      file = "./UseYear/perchHGPredictData.csv", row.names = FALSE))


##### Join the fish and loon data and results
#loon lake names
unique(loonBlood$Lake)

#fish lake names
unique(results$WATERWAY)

##fixing loon lake names (removing parenthesis that explain a location within a lake)
# loonBlood[, LakeFixed := Lake]
# loonBlood[grep("Rabbit", Lake), LakeFixed := "Rabbit"]
# loonBlood[grep("Mantrap", Lake), LakeFixed := "Mantrap"]
# loonBlood[grep("Big Birch", Lake), LakeFixed := "Big Birch"]
# loonBlood[grep("Arrowhead", Lake), LakeFixed := "Arrowhead"]
# loonBlood[grep("Monongalia", Lake), LakeFixed := "Monongalia"]

##fixed loon lake names
# unique(loonBlood$LakeFixed)

##add lowercase lake names to each table
# loonBlood[, LakeFixedLower := sapply(LakeFixed, tolower)]
# results[, WaterwayLower := sapply(WATERWAY, tolower)]

##loon lakes not found in fish data
# unique(loonBlood[loonBlood$LakeFixedLower %in% results$WaterwayLower,"LakeFixedLower"])
# results$WaterwayLower[grep("rabbit", results$WaterwayLower)]

# #finding "missing" lakes in fish data
# unique(results$WaterwayLower[grep("black|bird", results$WaterwayLower)]) # "blackduck"    "blackwater"   "blackhawk"    "black island" "black duck"
# unique(results$WaterwayLower[grep("south|tamarac", results$WaterwayLower)]) # "tamarack" "north tamarack" "south twin" "south center" "south lindstrom" "south fowl" "south" "upper south long" "lower south long" "south mcdougal" "south turtle" "south lida"
# unique(results$WaterwayLower[grep("north|tamarac", results$WaterwayLower)]) # "tamarack" "north tamarack" "north center" "north fowl" "northern light" "north" "north long" "north star" "north mcdougal" "north twin" "north turtle" "north lida" "north star steel" "north brown's"   
# unique(results$WaterwayLower[grep("mckeown", results$WaterwayLower)]) # none (is this misspelled?)
# unique(results$WaterwayLower[grep("mc", results$WaterwayLower)]) # none (is this misspelled?)
# unique(results$WaterwayLower[grep("point", results$WaterwayLower)]) # "many point" "disappointment" "sand point"
# unique(results$WaterwayLower[grep("eagles|nest", results$WaterwayLower)]) # "nest" "eagles nest #4" "eagles nest #1" "eagles nest #3"
# unique(results$WaterwayLower[grep("east|vermilion", results$WaterwayLower)]) # "east moore" "east twin" "east toqua" "east rush" "east pike" "east bearskin" "east pope" "east fox" "rabbit (east portion)" "east crooked" "east solomon" "big stone nwr east pool" "east" "east chub" "east graham" "east leaf" "east battle" "east lost" "east spirit" "east vadnais" "vermilion" "little vermilion" "beast" "east lake sylvia"
# unique(results$WaterwayLower[grep("arr|ow|head", results$WaterwayLower)]) # none

# Finding all names with "(", ")", "-" in fish data
# unique(results$WaterwayLower[grep("\\(|\\)|-", results$WaterwayLower)])

# Only values that may be effected by the symbols above is the rabbit lake
# sort(unique(loonBlood$LakeFixedLower))

# Here are the effected four rows from the un-QA'd data (these lakes were converted to just "rabbit" after my QA)
# loonBlood[grep("Rabbit|rabbit", loonBlood$Lake),]

# If we are focusing on just lakes (not portions of a lake), let's remove the words specifying positions
# fishData[grep("rabbit", WaterwayLower), WaterwayLower := "rabbit"]

# Find nearest fish sample year (UseYear) to loon sample year for each lake
loonBlood[, UseYear := 0]
for(id in unique(loonBlood$LakeID)) {
  subLoon = subset(loonBlood, LakeID == id)
  subFishData = subset(results, DOWID == id)
  if(nrow(subFishData) > 0) {
    for(year in subLoon$Year) {
      subLoonYear = subset(subLoon, Year == year)
      closestYear = subFishData$YEAR[which(abs(subFishData$YEAR-year) == min(abs(subFishData$YEAR-year)))[1]]
      #update loonblood UseYear with closest year
      loonBlood[LakeID == id & Year == year, "UseYear"] = closestYear
    }
  }
}

# Save all unique lake-id combos before removing data with no matches
unique_loons_full = sqldf("SELECT Lake, LakeID, COUNT(*) cnt FROM loonBlood GROUP BY Lake, LakeID ORDER BY Lake, LakeID")
write.csv(unique_loons_full, "./unique lake-id combinations/unique_loons_full.csv", row.names = F)

# Remove missing data
loonBloodFinal = subset(loonBlood, UseYear != 0)

# Now we can do our final join to view joined data
library(sqldf)

results[, FISHYEAR := YEAR]

# View all lake-id combinations
unique_fish = sqldf("SELECT WATERWAY, DOWID, COUNT(*) cnt FROM results GROUP BY WATERWAY, DOWID ORDER BY WATERWAY, DOWID")
unique_loons = sqldf("SELECT Lake, LakeID, COUNT(*) cnt FROM loonBloodFinal GROUP BY Lake, LakeID ORDER BY Lake, LakeID")

# store unique lake/loon lake-id combinations
write.csv(unique_fish, "./unique lake-id combinations/unique_fish.csv", row.names = F)

# Lakes in loon data that are missing from fish data
loon_fish_no_match = sqldf("SELECT a.Lake, a.LakeID, b.LakeID FROM unique_loons_full a LEFT JOIN unique_loons b ON a.LakeID = b.LakeID")
write.csv(loon_fish_no_match, "./unique lake-id combinations/loon_fish_no_match.csv", row.names=F)

#join loonblood data to results
results=sqldf("SELECT * FROM results AS a JOIN loonBlood AS b ON a.DOWID = b.LakeID AND a.FISHYEAR = b.UseYear")

# Remove loonblood year column
results = results[,-c(1)]
results = results[,-c(3)]

#compare lake names & id's
compare = sqldf("SELECT WATERWAY, DOWID, Lake, LakeID, COUNT(*) cnt FROM results GROUP BY WATERWAY, DOWID, Lake, LakeID")

results <- data.table(results)
results=cbind(results, HGppbLog = log(results$HGppm*1000 + 1))

# Check columns matching lake names
# data.frame(sqldf("SELECT LakeID, WATERWAY, LakeFixed FROM results GROUP BY LakeID, WATERWAY, LakeFixed"))

# Corrections
# results[LakeID == "18009302", LakeID := "18009301"]
# results[LakeID == "34015802", WATERWAY := 'Monongalia - Middle Fork Crow River']
# results[LakeID == "34015802", LakeFixed := 'Monongalia - Middle Fork Crow River']
# results[WATERWAY == "TAMARACK", LakeID := "000001"]

# Use Year
# results[LakeID == "000001", c("FISHYEAR", "Year", "UseYear")]

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

