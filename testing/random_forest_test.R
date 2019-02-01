library(sqldf)
library(data.table)
library(ggplot2)
library(NADA)

# Data at https://doi.org/10.5066/P9TDCH3F

## Load loon blood
loonBlood <- fread("C:/Users/tharrison/Files/projects/loons/inputData/LoonHGblood.csv")
loonBlood[ , LakeID := gsub( "^0", "", LakeID)]

water <- fread("C:/Users/tharrison/Files/projects/loons/WQ Model Data/WATERQUALITY_ALL_LAKES_20July2017.csv")

## Load data and reformat it
fishDataRaw <- fread("C:/Users/tharrison/Files/projects/loons/inputData/fishUse.csv")
fishData <- fread("C:/Users/tharrison/Files/projects/loons/inputData/fishUse.csv")

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

HGppbLog = log(fishData$HGppm*1000 + 1)
Censor = fishData$Censor
LengthInchesLog = fishData$LgthinLog
SppCut = fishData$SppCut
SampleEvent = fishData$sampleEvent



data <- data.frame(HGppbLog, 
                   LengthInchesLog,
                   Waterway = fishData$WATERWAY, 
                   Spec = fishData$Anat, 
                   Anat = fishData$Anat, 
                   NoFish = fishData$Nofish)

### MODEL ###

library(randomForest)

set.seed(123)
train_ix <- sample(nrow(data), .7*nrow(data), replace=F)
train <- data[train_ix, ]
validate <- data[-train_ix, ]

mod <- randomForest(HGppbLog ~ ., data = data, importance = T)
unique(data)
data %>% 
  group_by(Waterway) %>%
  count() %>%
  filter(n > 115)

library(rpart)
water
library(OneR)
test_data <- data.frame(water$LOC_ID, water$PH)
bin(test_data, nbins = 53)
test <- rpart(water$MAJOR_WATERSHED_NAME, water$PH)


