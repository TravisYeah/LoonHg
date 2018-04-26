.libPaths('D:/library/R')
library(sqldf)
library(data.table, lib = 'D:/library/R')
library(ggplot2, lib = 'D:/library/R')
library(NADA, lib = 'D:/library/R')

setwd("D:/Projects/USGS_R/loons/Travis/2018_03_09")

## load loon blood
loonBlood <- fread("./inputData/LoonHGblood.csv")
loonBlood[ , LakeID := gsub( "^0", "", LakeID)]

## Load data and reformat it
fishDataRaw <- fread("./inputData/fishUse.csv")
fishData <- fread("./inputData/fishUse.csv")

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

## Reformat data structure
fishData[ , DOWID := as.character(DOWID)]
fishData[ , HGppmLog  := log(HGppm + 1)]
fishData[ , Lgthcm := Lgthin * 2.54]
fishData[ , LgthinLog := log(Lgthin + 1)]
fishData[ , LgthcmLog := log(Lgthcm + 1)]
fishData[ , SppCut := paste(Spec, Anat, sep = "_")]
fishData[ , sampleEvent := paste(DOWID, YEAR, sep = "_")]

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

# save the model
save(modelOut, file = "fishHGmodelHgPpb20180302.rda")

#### OR ####

# load the model
load("fishHGmodelHgPpb20180302.rda")

############

# Get coefficients from the model
coefEst = coef(modelOut)


#load library for parallel computing
library(snow)

#create clusters
cl <- makeSOCKcluster(5)

#Add data to clusters
clusterExport(cl, "fishData")
clusterExport(cl, "coefEst")

#calculate predicted Hg for a 12 cm perch each lake
cluster_results = parSapply(cl, 1:nrow(fishData), function(i) {
  lakeyear <- paste(fishData$DOWID[i], fishData$YEAR[i], sep="_")
  lakeCoef <- coefEst[grep(paste0("SampleEvent",lakeyear), names(coefEst), perl=T)]
  if(length(lakeCoef) != 0) {
    return(as.numeric(lakeCoef) + as.numeric(coefEst[grep("YP_WHORG", names(coefEst))]*log(4.7 + 1)))
  } else {
    return(NA)
  }
})

# Stop the clusters
stopCluster(cl)

#bind predictions with data
all_results = cbind(fishData, perchHG = cluster_results)

#write results to file
write.csv(all_results, "./UseYear/perchHGPredictData2018_03_09.csv", row.names = F)

# Find nearest fish sample year (UseYear) to loon sample year for each lake
loonBlood[, UseYear := 0]
for(id in unique(loonBlood$LakeID)) {
  subLoon = subset(loonBlood, LakeID == id)
  subFishData = subset(fishData, DOWID == id)
  if(nrow(subFishData) > 0) {
    for(year in subLoon$Year) {
      subLoonYear = subset(subLoon, Year == year)
      closestYear = subFishData$YEAR[which(abs(subFishData$YEAR-year) == min(abs(subFishData$YEAR-year)))[1]]
      #update loonblood UseYear with closest year
      loonBlood[LakeID == id & Year == year, "UseYear"] = closestYear
    }
  }
}

# Remove lakes with no match
loonBlood = loonBlood[UseYear != 0, ]

loonBlood[, perchHG := NULL]
for(i in 1:nrow(loonBlood)) {
  lakeyear <- paste(loonBlood$LakeID[i], loonBlood$UseYear[i], sep="_")
  lakeCoef <- coefEst[grep(lakeyear, names(coefEst))]
  if(length(lakeCoef) != 0) {
    loonBlood[i, "perchHG"] <- lakeCoef + coefEst[grep("YP_WHORG", names(coefEst))]*log(4.7 + 1)
  } else {
    loonBlood[i, "perchHG"] <- NA
  }
}

# Change year for fish
fishData[, FISHYEAR := YEAR]

# Remove columns
fishData[, YEAR := NULL]

#join loonblood data (with predicted perch Hg) to fishData
results=sqldf("SELECT * FROM fishData AS a JOIN loonBlood AS b ON a.DOWID = b.LakeID AND a.FISHYEAR = b.UseYear")

# Fix loon lake id's
results[results$Lake == "East Fox", "LakeID"] = "18029800"
results[results$Lake == "South Tamarack", "LakeID"] = "3024101"
results[ grep("North Tamarac", results$Lake), "LakeID"] = "3024102"
results[ grep("george", results$Lake, ignore.case = T), "LakeID"] = "2009100"
results[grep("clearwater", results$Lake, ignore.case=T), "LakeID"] = "18003800"
results[ grep("East Fox", results$Lake), "LakeID"] = "18029800"
results[grep("77008400", results$LakeID, ignore.case=T), "LakeID"] = "77008401" #big birch
results[grep("29015100", results$LakeID, ignore.case = T), "LakeID"] = "29015104" #mantrap
results[grep("69037800", results$LakeID, ignore.case = T), "LakeID"] = "69037801" #east vermilion
results[grep("Monongalia - main", results$Lake, ignore.case = T), "LakeID"] = "34015801"
results[grep("Monongalia.*crow", results$Lake, ignore.case = T), "LakeID"] = "34015802"

#remove duplicate column
results <- results[, -c(1)]

# Write perch & loon joined data/results for loon analysis
write.csv(results, "./UseYear/perchLoonHGData2018_03_09.csv", row.names = FALSE)

#remove NA
results <- results[!is.na(results$perchHG), ]

# # Get mean perchHG by id-year combinations
perchHGAvg = data.frame(sqldf("SELECT LakeID, Year, AVG(perchHG) perchHG FROM results GROUP BY LakeID, Year"))

# Write loonData
write.csv(perchHGAvg, "./UseYear/perchHGAvg2018_03_09.csv", row.names = F)
