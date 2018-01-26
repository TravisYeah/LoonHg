.libPaths('D:/library/R')
library(sqldf)
library(data.table, lib = 'D:/library/R')
library(ggplot2, lib = 'D:/library/R')
library(NADA, lib = 'D:/library/R')
library(sqldf)
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

####################### Edit lake codes

# Pool tamarac data
loonBlood[ grep("tamarac", Lake, ignore.case=T), LakeID := "3024102"]
fishData[ grep("3024100|3024102", DOWID, ignore.case=T), DOWID := "3024102"]

# Pool all George data
fishData[ grep("GEORGE", WATERWAY, ignore.case = T), DOWID := "2009100"]
loonBlood[ grep("george", Lake, ignore.case = T), LakeID := "2009100" ]

# Pool all clearwater data (CUSTOM ID)
fishData[grep("clearwater", WATERWAY, ignore.case=T), DOWID := "18003800"]
loonBlood[grep("clearwater", Lake, ignore.case=T), LakeID := "18003800"]

## pool fox lake
loonBlood[ grep("East Fox", Lake), LakeID := "18029700"]
loonBlood[ grep("West Fox", Lake), LakeID := "18029700"]

# Pool all east rabbit data
loonBlood[grep("east rabbit", Lake, ignore.case=T), LakeID := "18009301"]
fishData[grep("(east.*rabbit)|(rabbit.*east)", WATERWAY, ignore.case=T), DOWID := "18009301"]

# pool all west rabbit data
loonBlood[grep("west rabbit", Lake, ignore.case=T), LakeID := "18009302"]
fishData[grep("18009300|18009302", DOWID, ignore.case=T), DOWID := "18009302"]

#pool all burntside data
loonBlood[grep("burntside", Lake, ignore.case=T), LakeID := "69011800" ]

#pool all little burch
loonBlood[grep("little birch", Lake, ignore.case=T), LakeID := "77008900"]

#pool all big burch
loonBlood[grep("77008401", LakeID, ignore.case=T), LakeID := "77008400"]

#pool wild rice data
loonBlood[grep("wild|rice", Lake, ignore.case=T), LakeID := "69037100"]

#pool south turtle data
loonBlood[grep("(south.*turtle)|(turtle.*south)", Lake, ignore.case = T), LakeID := "56037700"]

# mantrap
loonBlood[grep("Mantrap", Lake, ignore.case = T), LakeID := "29015100"]

# east vermilion
loonBlood[grep("East Vermilion", Lake, ignore.case = T), LakeID := "69037800"]

# monongalia
loonBlood[grep("Monongalia", Lake, ignore.case = T), LakeID := "34015800"]

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

########################################
########################################
####### RUN THE MODEL OR LOAD IT #######
########################################
########################################

#### RUN ####

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
save(modelOut, file = "fishHGmodelHgPpbTest.rda")

#### OR ####

# load the model
load("fishHGmodelHgPpb.rda")

############

# Join results with the data
results=cbind(fishData, perchHG=modelOut@survreg$linear.predictors)
head(results)

# Add residuals column
results = cbind(results, resids = residuals(modelOut))

# Write all predicted perch hg results
with(results, 
      write.csv(results,
      file = "./UseYear/perchHGPredictData.csv", row.names = FALSE))

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

# Get the lakes that didn't find a match
loonBlood[UseYear == 0, ]

# # Save all unique lake-id combos before removing data with no matches
# unique_loons_full = sqldf("SELECT Lake, LakeID, COUNT(*) cnt FROM loonBlood GROUP BY Lake, LakeID ORDER BY Lake, LakeID")
# write.csv(unique_loons_full, "./unique lake-id combinations/unique_loons_full.csv", row.names = F)

# Remove missing data
loonBloodFinal = subset(loonBlood, UseYear != 0)

# Change year for fish
results[, FISHYEAR := YEAR]

# Remove columns
results[, YEAR := NULL]
results[, V1 := NULL]

## View all lake-id combinations
# unique_fish_final = sqldf("SELECT WATERWAY, DOWID, COUNT(*) cnt FROM results GROUP BY WATERWAY, DOWID ORDER BY WATERWAY, DOWID")
# unique_loons_final = sqldf("SELECT Lake, LakeID, COUNT(*) cnt FROM loonBloodFinal GROUP BY Lake, LakeID ORDER BY Lake, LakeID")

# # store unique fish final lake-id combinations
# write.csv(unique_fish_final, "./unique lake-id combinations/unique_fish_final.csv", row.names = F)
# write.csv(unique_loons_final, "./unique lake-id combinations/unique_loons_final.csv", row.names = F)

#join loonblood data to results
results=sqldf("SELECT * FROM results AS a JOIN loonBloodFinal AS b ON a.DOWID = b.LakeID AND a.FISHYEAR = b.UseYear")

##compare lake names & id's
compare = sqldf("SELECT WATERWAY, DOWID, Lake, LakeID, COUNT(*) cnt FROM results GROUP BY WATERWAY, DOWID, Lake, LakeID")
write.csv(compare, "./unique lake-id combinations/compare_final_lake_loon_join.csv", row.names=F)

## Add the HGppbLog data column to the results
# results=cbind(results, HGppbLog = log(results$HGppm*1000 + 1))

# Fix loon lake id's
results[results$Lake == "East Fox", "LakeID"] = "18029800"
results[results$Lake == "South Tamarack", "LakeID"] = "3024101"
results[ grep("tamarac", results$Lake, ignore.case=T), "LakeID"] = "3024102"
results[ grep("george", results$Lake, ignore.case = T), "LakeID"] = "2009100"
results[grep("clearwater", results$Lake, ignore.case=T), "LakeID"] = "18003800"
results[ grep("East Fox", results$Lake), "LakeID"] = "18029800"
results[grep("77008400", results$LakeID, ignore.case=T), "LakeID"] = "77008401" #big birch
results[grep("29015100", results$LakeID, ignore.case = T), "LakeID"] = "29015104" #mantrap
results[grep("69037800", results$LakeID, ignore.case = T), "LakeID"] = "69037801" #east vermilion
results[grep("Monongalia - main", results$Lake, ignore.case = T), "LakeID"] = "34015801"
results[grep("Monongalia.*crow", results$Lake, ignore.case = T), "LakeID"] = "34015802"

# Write perch & loon joined data/results for loon analysis
write.csv(results, "./UseYear/perchLoonHGData.csv", row.names = FALSE)

# # Get mean perchHG by id-year combinations
perchHGAvg = sqldf("SELECT LakeID, Year, AVG(perchHG) perchHG FROM results GROUP BY LakeID, Year")

# Write loonData
write.csv(perchHGAvg, "./UseYear/perchHGAvg.csv", row.names = F)

#####################################################################

#************************* Log Length Inches vs residuals per species
# lgthResid <- ggplot(results, aes(x = LgthinLog, resids)) +
#   geom_point(alpha = 0.5) +
#   facet_wrap( ~ SppCut, ncol = 7, scales = 'free') +
#   scale_color_manual(values = c("blue", "orange", "black")) +
#   stat_smooth(method = 'lm')
# ggsave('./plots/lgthResid.jpg', lgthResid, width = 14, height = 8.5)
# lgthResid
# 
# # density plot
# lgthResidhist2d <- ggplot(results, aes(x = LgthinLog, resids)) +
#   geom_point(alpha = 0.25) +
#   stat_density2d() +
#   facet_wrap( ~ SppCut, ncol = 7, scales = 'free')  +
#   scale_color_gradient(trans = "log")
# lgthResidhist2d
# ggsave('./plots/lgthResidhist2d.jpg', lgthResidhist2d, width = 14, height = 8.5)
# 
# 
# sppCutResid <- ggplot(results, aes(x = resids)) +
#   geom_histogram() +
#   facet_wrap( ~ SppCut, ncol = 7, scales = 'free') +
#   scale_color_manual(values = c("blue", "orange", "black"))
# sppCutResid
# ggsave('./plots/sppCutResid.jpg', sppCutResid, width = 14, height = 8.5)
# 
# loglengthHist <- ggplot(results, aes(x = LgthinLog)) +
#   geom_histogram() +
#   facet_wrap( ~ SppCut, ncol = 7, scales = 'free') +
#   scale_color_manual(values = c("blue", "orange", "black"))
# loglengthHist
# ggsave('./plots/loglengthHist.jpg', loglengthHist, width = 14, height = 8.5)
# 
# ## Plot residules by event
# sampleEventPlot <- results[ YEAR > 2000, length(resids),
#                              by = list(sampleEvent)][ V1 > 30, sampleEvent]
# 
# eventResid <- ggplot(results[ sampleEvent %in% sampleEventPlot, ], aes(x = resids)) +
#   geom_histogram() +
#   facet_wrap( ~ sampleEvent, ncol = 7, scales = 'free') +
#   scale_color_manual(values = c("blue", "orange", "black"))
# eventResid
# ggsave('./plots/eventResid.jpg', eventResid, width = 14, height = 8.5)
# 
# ypByAnat <- ggplot(results[ results$Spec == "YP", ],
#                    aes(x = LgthinLog, resids, color = Anat, size = Nofish)) +
#   geom_point(alpha = 0.5) +
#   scale_color_manual(values = c("blue", "orange", "black"))
# ypByAnat
# ggsave('./plots/ypByAnat.jpg', ypByAnat , width = 14, height = 8.5)
# 
# 
# ## Plot residules by event
# sampleEventPlot <-
#   results[ , length(resids),
#             by = list(sampleEvent, Spec, SppSampleEvent)][ V1 >  30, SppSampleEvent ]
# sampleEventPlot
# 
# lgthSampleEvent <- ggplot(results[ SppSampleEvent %in% sampleEventPlot, ],
#                           aes(x = LgthinLog,
#                               y = resids, color = Anat)) +
#   geom_point(alpha = 0.5)  +
#   facet_wrap( ~ SppSampleEvent, ncol = 7, scales = 'free') +
#   scale_color_manual(values = c("blue", "orange", "black"))
# 
# lgthSampleEvent
# ggsave('./plots/lgthSampleEvent.jpg', lgthSampleEvent, width = 14, height = 8.5)
# 
# 
# results
# 
# str(modelOut)
# 
# lgthSampleEvent2 <- ggplot(results[ SppSampleEvent %in% sampleEventPlot, ],
#                            aes(x = LgthinLog,
#                                y = Predicted, color = Anat)) +
#   geom_point(alpha = 0.5)  +
#   facet_wrap( ~ SppSampleEvent, ncol = 7, scales = 'free') +
#   scale_color_manual(values = c("blue", "orange", "black")) +
#   stat_smooth(method = 'lm')
# ## coord_cartesian(xlim = c(0, 3.5))
# lgthSampleEvent2
# ggsave('./plots/lgthSampleEvent2.jpg', lgthSampleEvent2, width = 14, height = 8.5)
# 
# write.csv(results, "./UseYear/LoonData.csv")
# 
# #******************** Ploting loon Log Hh ppb against predicted fish Log Hg ppb with linear model
# predictedPlot <- ggplot(data = results, aes(x = HGppbLog, Predicted)) +
#   geom_point() + stat_smooth(method = 'lm')
# predictedPlot
# 
# ggsave("./plots/predictedPlot.pdf", predictedPlot)
# 
# 
# predictedPlotYPwhorg <- ggplot(data = results[ grep("YP_WHORG", SppCut),],
#                                aes(x = HGppbLog, y = Predicted)) +
#   geom_point() + stat_smooth(method = 'lm')
# predictedPlotYPwhorg
# ggsave("./plots/predictedPlotYPwhorg.pdf", predictedPlotYPwhorg)
# results[ grep("YP_WHORG", SppCut),]

