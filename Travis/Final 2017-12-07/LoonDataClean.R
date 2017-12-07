library(data.table)
library(lubridate)
## Load loon Contaminant data
setwd("D:/Projects/USGS_R/loons/Travis/Final 2017-12-07")
list.files("./RawData/")
dt1 <- fread("./RawData/1409727_FINAL_EXCEL_17_Nov_14_1743.csv")
dt2 <- fread("./RawData/1409729_FINAL_EXCEL_17_Nov_14_1740.csv")
dt3 <- fread("./RawData/1409731_FINAL_EXCEL_17_Nov_14_1739.csv")

dtAll <- rbind(dt1, dt2, dt3)
head(dtAll)
dtUse <- dtAll[  , list(SAMPLENAME, MATRIX, ANALYTE, Result, UNITS)]

dtUse[ , Year :=
          as.numeric(gsub("(\\d{2,4}-\\d{2,6}|No Band)(_| _)(\\d{4}) (\\(.*)",
                          "\\3",
                          SAMPLENAME, perl = TRUE))]
dtUse[ , unique(Year)]
dtUse[ , Band :=
      gsub("(\\d{2,4}-\\d{2,6}|No Band)(_| _)(\\d{4}) (\\(.*)",
                      "\\1",
                      SAMPLENAME, perl = TRUE)]
## Fix data mistake
dtUse[ Band == "1038-994250", Band := "1038-94250"] 

dtUse[ , temp := 
      gsub("(\\d{2,4}-\\d{2,6}|No Band)(_| _)(\\d{4}) (\\(.*)",
           "\\4",
           SAMPLENAME, perl = TRUE)]

dtUse[ , Lake :=  gsub("\\(|\\)|( F.*$)|( M.*$)|( A.*$)|( J.*$)|( U.*$)",
              "", temp)]
## Note that Loon Marsh does not have a MN lake ID
dtUse[ 43:46,] # To see Loon Marsh
dtUse[ c(223:240, 493:516),] ## Add in Fox to Lake name
dtUse[ c(223:240, 493:516), Lake := paste(Lake, "Fox")]

dtUse[ , Lake]
dtUse[ , unique(Lake)]


## CLEAN UP Lake data and names
lakeKey <- fread("./RawData/lakeSubs.csv")
lakeKey

dtUse[ , unique(Band)]


for(index in 1:dim(lakeKey)[1]){
    dtUse[ Lake == lakeKey[ index, old], Lake := lakeKey[ index, new]]
}


dtUse[ Lake == "Stumpf Lake", Lake := "Stump Lake"]
dtUse[, Lake := gsub("( Lake)|( Bay)|(Lake )", "", Lake)]
dtUse[ grep("Burntside", Lake), Lake := "Burntside"]
dtUse[ grep("Clearwater", Lake), Lake := "Clearwater"]
dtUse[ grep("George", Lake), Lake := "George"]
dtUse[ grep("Little Birch", Lake), Lake := "Little Birch"]
dtUse[ Lake == "Mckeown", Lake := "McKeown"]
dtUse[ Lake == "North Tamarack", Lake := "North Tamarac"]
dtUse[ grep("Rabbit", Lake), Lake := gsub(" North| South", "", Lake)]
dtUse[ grep("Rabbit", Lake), Lake := gsub("(East|West) (Rabbit)", "\\2 (\\1 Portion)", Lake)]
dtUse[ Lake == "South Tamarack", Lake := "South Tamarac"]
dtUse[ grep("Wild Rice", Lake), Lake := "Wild Rice"]
dtUse[ grep("South Turtle", Lake), Lake := "South Turtle"]
dtUse[ grep("Tamarac", Lake), Lake := "tamarack"]
dtUse[ grep("ermilion", Lake), Lake := "vermilion"]
dtUse[ grep("Monongalia", Lake), Lake:= "Monongalia" ]
# Adding more corrections
  
## Need to remove Marsh
dtUse[ , unique(Lake)]

## Read in Lake ID nums
ld <- fread("./RawData/LakeIDName.csv")
setnames(ld, c("Lake", "rm1", "LakeID", "rm2"))
ld[ , c("rm1", "rm2") := NULL]
setkey(ld, "Lake")
setkey(dtUse, "Lake")

## Check to make sure all of the dtUse Lakes are in the ld
unique(dtUse)[ , Lake][! unique(dtUse[ , Lake]) %in% ld[ , Lake]]

## See which LDs we did not use
ld[ , Lake][!ld[ , Lake] %in% unique(dtUse[ , Lake])]

dtUse <- copy(ld[ dtUse])
dtUse
dtUse[ is.na(LakeID), unique(Lake)]

## Edit Lake codes
## Big Birch, Mantrap, Monogalia, North/South Tamarac.
dtUse[ grep("Mantrap" , Lake), LakeID := '29015104']
dtUse[ grep("Mantrap" , Lake), Lake := 'Mantrap (West Arm)']

dtUse[ grep("Big Birch" , Lake), LakeID := '77008401']
dtUse[ grep("Big Birch" , Lake), Lake := "Big Birch (NE portion)"]

## Clean up Monogalia Lake (has two sublakes in system)
dtUse[ grep("Monongalia", Lake), LakeID := '34015800']
dtUse
dtUse[ grep("Crow River", SAMPLENAME), Lake := "Monongalia - Middle Fork Crow River"]
dtUse[ grep("Crow River", Lake), LakeID := '34015802']

## Merge in Band info
list.files("./RawData/")
band <- fread("./RawData/LoonBands_revised 26jul17.csv")
## Remove loon entires Kevin marked to remove 
band <- copy(band[ Remove == 0,])
setnames(band, "Body (mm)", "BodyLength")
bandUse <- band[ , list(Age, Sex, Date, Band, Mass)]

## Change date 
bandUse[ , Date := as.POSIXct(strptime(Date, "%d-%b-%y"))]

setkey(dtUse, "Band")
setkey(bandUse, "Band")

## only the no Band is missing info about age and sex

dtBoth <- copy(bandUse[dtUse])
dtBoth[ Band == "No Band",]

dtBoth[ Band == "No Band", Age := "Juvenile"]
dtBoth[ Band == "No Band", Sex := "Unknown"]

dtBoth[ Sex == "Unk", Sex := 'Unknown']
dtBoth[ Age == "Juv", Age := 'Juvenile']

dtBoth[ ANALYTE == "% Solids", ANALYTE := "Solids"]

dtBoth[ , temp := NULL]
dtBoth[ , MATRIX := NULL]

colnames(dtBoth)
head(dtBoth)


dtBoth[ Result == "ND" & ANALYTE == "Selenium", Result := 0.2]
dtBoth[ Result == "ND" & ANALYTE == "Aluminum", Result := 0.93]
dtBoth[ Result == "ND" & ANALYTE == "Cadmium", Result  := 0.005]
dtBoth[ Result == "ND" & ANALYTE == "Lead",     Result  := 0.01]
dtBoth[ Result == "ND" & ANALYTE == "Solids", Result := 0.2 ] 

dtBoth[ , Result := as.numeric(Result)]

## Have both Analytes and and Samples merged together
## LOOK AT non-detect data
library(ggplot2)
chems <- ggplot(dtBoth, aes(x = Result)) + geom_histogram() +
    facet_grid( . ~ ANALYTE, scales = "free_x" )
chems

ggsave("hist.pdf", chems, width = 10, height = 4)
sink("ND data.txt")
print(dtBoth[ , list(min = min(Result), Zeros = length(which(Result == 0))), by = ANALYTE])
sink()

## Cast data into wide format (end thing to do)
colnames(dtBoth)
dtBoth[ , SAMPLENAME := NULL]
dtBoth[ , UNITS := NULL]

dtWide <- copy(dcast(dtBoth, Year + Lake + LakeID + Age + Sex +
				Band + Mass + Date  ~ ANALYTE, value.var = 'Result'))

dtWide[ grep("Loon", Lake), LakeID :=  '111111111']
############################################################
############################################################
############################################################
## load in water quliaty data
dWQ <- fread("./WQ Model Data/WATERQUALITY_ALL_LAKES_20July2017.csv")

## Change loon marsh to have the same dummy ID used elsewhere in code
dWQ[ grep("LOON", LOC_DESC), LOC_ID := '111111111']
## fix wrong loc_id code
dWQ[ LOC_DESC == "POINT", LOC_ID := "34_0193_00_201"]

## Change ALK 1/2 of detection limit
dWQ[ ALK_2320B == "< 10", ALK_2320B := '5']
dWQ[ , ALK_2320B := as.numeric(ALK_2320B)]

## Taking a median of Alk and P across different methods of measuring
dWQ[ , ALK := apply(dWQ[ , grep( "ALK", colnames(dWQ)), with = FALSE],
               1, median, na.rm = TRUE)]
dWQ[ , P_3654 := as.numeric(gsub("< ", "", P_3654))]
dWQ[ , Phosp := apply(dWQ[ , grep( "P_", colnames(dWQ)), with = FALSE],
               1, median, na.rm = TRUE)]

## Remove CHLA and take median of other measure types 
dWQ[ , c( "CHLA_10200H_UNCORR", "CHLA_ASTMD373187_UNCORR") := NULL]
dWQ[ , CHLA := apply(dWQ[ , grep( "CHLA", colnames(dWQ)), with = FALSE],
               1, median, na.rm = TRUE)]
dWQ2 <- copy(dWQ[ , list(LOC_ID, LOC_DESC, SAMPLE_DATE, 
                          SECCHI, PH, Phosp, ALK, CHLA, LAKE_AREA, MAX_DEPTH,
                         TSI)])
## Clean up secchi
dWQ2[ , SECCHI := as.numeric(gsub("> ", "", SECCHI))]

dWQ2 <- copy(dWQ2[ , list(SECCHI = median(SECCHI, na.rm = TRUE), Phosp = median(Phosp, na.rm = TRUE),
			PH = median(PH, na.rm = TRUE), ALK = median(ALK, na.rm = TRUE), 
			CHLA = median(CHLA, na.rm = TRUE), LAKE_AREA =median(LAKE_AREA, na.rm = TRUE),
			MAX_DEPTH = median(MAX_DEPTH, na.rm = TRUE), TSI = median(TSI, na.rm = TRUE)), by = list(LOC_ID, LOC_DESC, SAMPLE_DATE)])


## NEXT, convert Date to date formate as well as time, then remove months outsides of
## of May through July (

dWQ2[, SAMPLE_DATE :=  as.POSIXct(SAMPLE_DATE, format = "%m/%d/%Y")]
library(lubridate)
dWQ2[ , Year := year(SAMPLE_DATE)]
dWQ2[ , Month := month(SAMPLE_DATE)]
dWQ3 <- copy(dWQ2)

dWQ3[ , LakeID := as.numeric(gsub( "(\\d{2})_(\\d{4})_(\\d{2})(.*)",
                   "\\1\\2\\3",
                   LOC_ID))]

wqLakes <- dWQ3[ , list(LakeID = round( mean(LakeID))), by = LOC_DESC]
loonLakes <- dtWide[ , list(LakeID =  round(mean(as.numeric(LakeID)))), by = Lake]
loonLakes[ Lake == "Loon", LakeID := 111111111]

wqLakes[ LOC_DESC == "LOON_MARSH", LakeID := 111111111]
wqLakes[ grepl("MANTRAP", LOC_DESC), LakeID := 29015104]
wqLakes[ LOC_DESC == "WEST_FOX", LakeID := 18029700]
wqLakes[ LOC_DESC == "ANNA", LakeID := 56044800]
wqLakes[ LOC_DESC == "STUMP", LakeID := 73009100]

setnames(wqLakes, 'LOC_DESC', 'WQ_Lakes')
setnames(loonLakes, 'Lake', 'Loon_Lakes')

setkey(loonLakes, "LakeID")
setkey(wqLakes, "LakeID")

wqLakes[ loonLakes, allow.cartesian = TRUE][ order(Loon_Lakes), ]

areaDepth <- copy(dWQ3[ , list(
    AREA = mean(LAKE_AREA, na.rm = TRUE),
    MAXdepth = mean(MAX_DEPTH, na.rm = TRUE)),
                       by = list(LOC_DESC, LakeID )])
areaDepth <- areaDepth[ complete.cases(areaDepth),]

## Most Conservative Scenario, pool everything including years
dWQ3[ LOC_DESC == "MANTRAP_WEST_ARM", LakeID := 29015104]


## Extarct out data so only within year
dWQ5a <- copy(dWQ3[ Month %in% 5:7, list(SECCHI = mean(SECCHI, na.rm = TRUE),
                                         PH = mean(PH, na.rm = TRUE),
                                         Phosp = mean(Phosp, na.rm = TRUE),
                                         ALK = mean(ALK, na.rm = TRUE),
                                         CHLA = mean(CHLA, na.rm = TRUE),
                                         TSI = mean(TSI, na.rm = TRUE)
                          ),
                  by = list(LOC_DESC, LakeID)
                  ])

dWQ5a[ LOC_DESC == "LOON_MARSH", LakeID := 111111111]

setkey(areaDepth, "LakeID")
setkey(dWQ5a, "LakeID")

areaDepth[ , LOC_DESC := NULL]
dWQ5 <- copy(areaDepth[ dWQ5a])

## Will need to reformat LOC_ID to be the same as dtWide
dWQ5[ , LakeID := as.character(LakeID)]

## Add zeros to front of Tamarack lakes' numbers
dWQ5[ grep("^302410", LakeID), ]
dWQ5[ grep("^302410", LakeID), LakeID := paste0("0", LakeID)]
dWQ5[ grep("^302410", LakeID), ]
dWQ5[ grep("302410", LakeID), ]

## And also George and Blackbird
dWQ5[ grep("^2009100", LakeID), ]
dWQ5[ grep("^2009100", LakeID), LakeID := paste0("0", LakeID)]
dWQ5[ grep("^3019700", LakeID), ]
dWQ5[ grep("^3019700", LakeID), LakeID := paste0("0", LakeID)]


## Mke sure turtle is the same across 
dWQ[ grep("TURT", LOC_DESC), max(MAX_DEPTH, na.rm = TRUE)]
dWQ2[ grep("TURT", LOC_DESC), max(MAX_DEPTH, na.rm = TRUE)]
dWQ3[ grep("TURT", LOC_DESC), max(MAX_DEPTH, na.rm = TRUE)]
dWQ5[ grep("TURT", LOC_DESC) ]
dtWide[ grep("Turt", Lake) ]


dWQ3[ , Date := as_date(SAMPLE_DATE)]
dWQ3[ , Month:= month(Date)]



write.csv(x = dWQ5, file = "WaterQualityCheck27July17.csv", row.names = FALSE)

setkey(dtWide, 'LakeID')
setkey(dWQ5, 'LakeID')

## Merge data together
dtThree <- copy(dWQ5[dtWide])

write.csv(x = dtWide, file = "./inputData/LoonHGblood.csv")

###################
###################
#### RUN MODEL ####
###################
###################

## Now, merge in Hg model 
getwd()
list.files("../UseYear")
perchHG <- fread("../UseYear/perchLoonHGData.csv")
perchHG[ Lake == "Loon", LakeID := '111111111']
perchHG[ Lake == 'Wild Rice', perchHG := mean(perchHG, na.rm = TRUE) ]

perchHG[ , lakeYearID := paste(LakeID, Year, sep = "_")]
dtThree[ , lakeYearID := paste(LakeID, Year, sep = "_")]

## Merge together data
setkey(dtThree, "lakeYearID")
setkey(perchHG, "lakeYearID")

perchHG2 <- copy(perchHG[ , list(perchHG = mean(perchHG)), by = lakeYearID])

perchHG2[ lakeYearID == '2009100_2012', lakeYearID := '02009100_2012']
perchHG2[ lakeYearID == '2009100_2014', lakeYearID := '02009100_2014']

setkey(perchHG2, "lakeYearID")

dtFour <- perchHG2[dtThree]

## Use West Fox's data for east Fox HG
dtFour[ lakeYearID == "18029800_2011", perchHG :=
           dtFour[ lakeYearID == "18029700_2012", unique(perchHG)]]

## 
dtFour[ grepl('MUD_MONOGALIA', LOC_DESC), perchHG := dtFour[ grepl('MONOGALIA_MIDDLE_FORK_CROW_RIVER', LOC_DESC), unique(perchHG)]]

dtFour[ is.na(perchHG), ]

write.csv(file = "./FinalData/LoonData.csv",
          x = dtFour, row.names = FALSE)


