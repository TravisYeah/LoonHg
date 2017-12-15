.libPaths('D:/library/R')
## Load libraries 
library(data.table)
library(psych)
library(ggplot2)
library(GGally)
library(lme4)
library(lmerTest)
library(lubridate)
setwd('D:/Projects/USGS_R/loons/Travis/Final Re-Work 2017-12-08/LoonAnalysis')

## Load data

d <- fread("./LoonData.csv")

## grab only the columns needed to analyze the data 
## And then reformat the data
dHg <- d[ , list(perchHG, lakeYearID, Year, Lake, SECCHI, PH,
                 Phosp, ALK, CHLA, AREA, MAXdepth, TSI, Age, Sex,
                 Mercury, Mass, Selenium, Date)]
dHg[ , Mass := as.numeric(Mass)]
dHg[ , HgLog := log(Mercury*1000)]
dHg
dHg[ , Date := as.POSIXct(strptime(Date, format = "%Y-%m-%d"))]
dHg[ , Date]
dHg[ , Month := month(Date)]
dHg[ , Day := day(Date)]

dHG2 <- copy(dHg)



## Exculde unkown sex adult
dHG2[ , which(Age == "Adult" & Sex == "Unknown")]
dHG2 <- copy(dHG2[ - dHG2[ , which(Age == "Adult" & Sex == "Unknown")],])

dHG2[ , Sex := factor(Sex)]
levels(dHG2$Sex)[3] <- "Juvenile"
dHG2[ , Sex := factor(Sex)]
dHG2[, HgLog := log(HgLog*1000)]

### code for marginal plots   
perchBySex <- ggplot(data = dHG2, aes(x = perchHG, y = HgLog)) +
    geom_point() + stat_smooth(method = "lm", formula = y ~ x) +
        facet_grid( . ~ Sex) +
            theme_bw() +
                ylab(expression("ln(Loon blood Hg(" * mu*"g/kg wet weight))")) +
                    xlab(expression("ln(Standardized perch Hg(" * mu*"g/kg wet weight) + 1)")) +
                        theme(strip.background = element_blank())
perchBySex
ggsave("perchBySex.pdf", perchBySex, width = 8, height = 4)

onlyHG <- ggplot(data = dHG2, aes(x = perchHG, y = HgLog)) +
    geom_point() + stat_smooth(method = "lm", formula = y ~ x) +
            theme_bw() +
                ylab(expression("ln(Loon blood Hg(" * mu*"g/kg wet weight))")) +
                    xlab(expression("ln(Standardized perch Hg (" * mu*"g/kg wet weight) + 1)")) +
                        theme(strip.background = element_blank())
onlyHG
ggsave("onlyHG.pdf", onlyHG, width = 6, height = 4)

exp_one_trans = function() trans_new("exp_one", function(x) exp(x) - 1, function(x) exp(x) - 1)
exp_trans = function() trans_new("exp", function(x) exp(x), function(x) exp(x))
onlyHGBT <- ggplot(data = dHG2, aes(x = perchHG, y = HgLog)) +
  geom_point() + 
  stat_smooth(method = "lm", formula = y ~ x) +
  coord_trans(x="exp_one", y="exp") +
  theme_bw() +
  ylab(expression("Loon blood Hg(" * mu*"g/kg wet weight)")) +
  xlab(expression("Standardized perch Hg (" * mu*"g/kg wet weight)")) +
  theme(strip.background = element_blank())
onlyHGBT
ggsave("onlyHGBT.pdf", onlyHGBT, width = 6, height = 4)

hgMass <- ggplot(data = dHG2, aes(x = Mass, y = HgLog)) +
    geom_point() + stat_smooth(method = "lm", formula = y ~ x) +
        facet_grid( . ~ Age) +
            theme_bw() +
                ylab(expression("ln(Loon blood Hg(" * mu*"g/kg wet weight))")) +
                    xlab(expression("Mass (kg)")) +
                        theme(strip.background = element_blank())
hgMass
ggsave("hgMass.pdf", hgMass, width = 8, height = 4)


sexPlot <- ggplot(data = dHG2, aes(x = Sex, y = HgLog)) +
    ylab(expression("ln(Loon blood Hg(" * mu*"g/kg wet weight))")) +
        xlab("Loon sex/age category") + 
    geom_boxplot() + theme_bw()
sexPlot
ggsave("sexPlot.pdf", sexPlot, width = 6, height = 4)


ggplot(data = dHG2, aes(x = Month, y = HgLog, color = Sex)) +
    geom_point() +
        scale_color_manual(values = c('blue', 'red', 'black'))

################### CREATE LAT/LONG MAP HERE #########################

library(measurements)
library(ggmap)
#read in coords
coords=fread("D:/Projects/USGS_R/loons/Travis/Final Re-Work 2017-12-08/coords/coordinates.csv")
coordsConv=fread("D:/Projects/USGS_R/loons/Travis/Final Re-Work 2017-12-08/coords/coordinatesConv.csv")
coords[grep("adley", WATERWAY)]
# Convert to correct decimal format
coords[Lat != "", latitude := as.numeric(paste(substring(Lat, 1, 2), ".", substring(Lat, 3), sep=""))]
coords[Long != "", longitude := as.numeric(paste("-",substring(Long, 1, 2), ".", substring(Long, 3), sep=""))]
setorder(coords, WATERWAY)
setorder(coordsConv, WATERWAY)

# Convert from UTM to decimal
coords[, latitude := NA]
coords[, longitude := NA]
for(i in 1:nrow(coords)) {
  coords$latitude[i] = ifelse(is.na(coords$Long[i]), coordsConv$X_Utm[i], coords$latitude[i])
  coords$longitude[i] = ifelse(is.na(coords$Lat[i]), coordsConv$Y_Utm[i], coords$longitude[i])
}
#
coords[ grep("tamarac", WATERWAY, ignore.case=T) , WATERWAY:= "TAMARACK"]
coords[ grep("LITTLE VERMILION", WATERWAY, ignore.case=F), WATERWAY:= "EAST VERMILION"]
coords[ grep("EAGLES NEST #3", WATERWAY, ignore.case=F), WATERWAY:= "EAGLES NEST"]
coords[ grep("AUTO", WATERWAY, ignore.case=F) , WATERWAY:= "Arrowhead [Auto]"]
coords[, WATERWAY := sapply(WATERWAY,tolower)]
coordsUnique <- unique(coords[, c("WATERWAY", "longitude", "latitude")])
coordsUnique <- copy(coordsUnique[!is.na(latitude) & !is.na(longitude)])
coordsUnique[, cnt := seq_len(.N), by="WATERWAY"]
coordsUnique <- copy(coordsUnique[cnt == 1,])
coordsUnique[, "cnt" := NULL]
coordsUnique
coords

setkey(coordsUnique, "WATERWAY")
setkey(dHG2, "Lake")
dHG2[, latitude := NA]
dHG2[, longitude := NA]
dHG2$Lake[1]

for(i in 1:nrow(dHG2)) {
  sub=coordsUnique[WATERWAY == dHG2$Lake[i]]$latitude
  if(length(sub) > 0) {
    dHG2$latitude[i] = sub
  } else {
    dHG2$latitude[i] = NA
  }
  
}

coordResult <- coordSummary[!is.na(latitude) & !is.na(longitude)]
sort(unique(coordSummary$WATERWAY))[which(!(sort(unique(coordSummary$WATERWAY)) %in% unique(coordResult$WATERWAY)))]
coords[grep("auto", WATERWAY)]

# get mn map
minnesota <- get_map("minnesota", zoom = 6)

test = data.frame(hg = coordResult$perchHG, lat = coordResult$latitude, lon=coordResult$longitude)

# exp the hg
test$hg = exp(test$hg) - 1

# heat map with zoom 7 (loon hg)
heat_z7 = ggmap(minnesota) + stat_density2d(data=test, aes(x=lon, y=lat, fill=..level.., alpha=..level..),
                              geom="polygon", size=1, bins=8) +
  scale_fill_continuous(low="blue", high="black", limits=c(min(test$hg), max(test$hg))) +
  # scale_fill_gradient(low="red", high="green")
  scale_alpha(range = c(0,0.6), guide=FALSE)
png(filename="heat_z7.png", width=1280, height=1280)
print(heat_z7)
dev.off()

# heat map with zoom 6 (loon hg)
minnesota <- get_map("minnesota", zoom = 6)
heat_z6 = ggmap(minnesota) + stat_density2d(data=test, aes(x=lon, y=lat, fill=..level.., alpha=..level..),
                                            geom="polygon", size=1, bins=8) +
  scale_fill_continuous(low="blue", high="black", limits=c(min(test$hg), max(test$hg))) +
  # scale_fill_gradient(low="red", high="green")
  # scale_alpha(range = c(0,0.6), guide=FALSE)
png(filename="heat_z6.png", width=1280, height=1280)
print(heat_z6)
dev.off()

# plain plot of points
minnesota <- get_map("minnesota", zoom = 7)
point_map_plain = ggmap(minnesota) +
  geom_point(data=test, aes(x=lon, y=lat))
  # + stat_density2d(data=test, aes(x=lon, y=lat, fill=..level.., alpha=..level..),
                                            # geom="polygon", size=1, bins=8) +
  # scale_fill_continuous(low="blue", high="black", limits=c(min(test$hg), max(test$hg))) +
  # scale_fill_gradient(low="red", high="green")
  # scale_alpha(range = c(0,0.6), guide=FALSE)
png(filename="point_map_plain.png", width=1280, height=1280)
print(point_map_plain)
dev.off()

# plotting hg of fish (not loons)
test = data.frame(hg = exp(coordResult$HgLog), lat = coordResult$latitude, lon=coordResult$longitude)
heamap_fish = ggmap(minnesota) + stat_density2d(data=test, aes(x=lon, y=lat, fill=..level.., alpha=..level..),
                                            geom="polygon", size=1, bins=8) +
  scale_fill_continuous(low="blue", high="black", limits=c(min(test$hg), max(test$hg))) +
  # scale_fill_gradient(low="red", high="green")
  scale_alpha(range = c(0,0.7), guide=FALSE)
png(filename="heamap_fish.png", width=1280, height=1280)
print(heamap_fish)
dev.off()

######################################################################

write.csv(file = "LoonDataFinalUsed.csv", x = dHG2, row.names = FALSE)

## Analysis

out <- lm(HgLog ~ perchHG + Sex + Mass:Age, data = dHG2)
summary(out)
confint(out, level = 0.95)

par(mfcol = c(2,2))
plot(out)

pdf("LoonHGresid.pdf")
par(mfcol = c(2,2))
plot(out)
dev.off()


#########################
Lake <- dHG2[ , list(
    perchHGLake = mean(perchHG),
    ## HgLogLake = mean(HgLog),
    ALKLake = mean(ALK),
    SECCHILake = mean(SECCHI),
    PHLake = mean(PH),
    PhospLake = mean(Phosp),
    CHLALake = mean(CHLA),
    MAXdepthLake = mean(MAXdepth),
    AreaLake = mean(AREA)
    ), by = Lake]

Lake[ order(perchHGLake, decreasing = TRUE), list(Lake, perchHGLake, MAXdepthLake)]
ggpairs(Lake[ , -1, with = FALSE])
corr.test(Lake[ , -1, with = FALSE])
cor.test(Lake$PHLake, Lake$perchHGLake)

summary(lm(perchHGLake ~ PHLake + ALKLake +
               SECCHILake + MAXdepthLake + PhospLake + AreaLake,
           data = Lake))

ggplot(data = d, aes(x = Mercury, y = Selenium)) + geom_point() +
    scale_x_continuous(trans = 'log')+
        scale_y_continuous(trans = 'log') +
            stat_smooth(method = 'lm')

###
HgSeHist <- ggplot(dHg, aes(x = Selenium)) + geom_histogram() +
    scale_x_continuous(trans = 'log') +
        facet_grid( . ~ Sex) + theme_minimal()
print(HgSeHist)
ggsave("HgSeHist.pdf", HgSeHist, width = 6, height = 4)

dHg[ , HgLog := log(Mercury)]
dHg[ , SeLog := log(Selenium)]

HgSeBoxplot <- ggplot(dHg, aes(x = Sex, y = Selenium)) + geom_boxplot() + 
	theme_minimal()
print(HgSeBoxplot)
ggsave("HgSeBoxplot.pdf", HgSeBoxplot, width = 6, height = 4)

ggplot(dHg, aes(y = SeLog, x = HgLog)) +
    geom_point() + stat_smooth(method = 'lm') +
        facet_grid( . ~ Sex)

### Start of Se analysis
dSe <- dHg[ SeLog != -Inf,]
dSe <- copy(dSe[ - dSe[ , which(Age == "Adult" & Sex == "Unknown")],])
dSe
summary(lm(SeLog ~ Sex  + Mass, data = dSe))
round(confint(lm(SeLog ~ Sex  + Mass, data = dSe)), 2)

dSe[ , Sex := factor(Sex)]
levels(dSe$Sex)[3] <- "Juvenile"
sexPlotSe <- ggplot(data = dSe, aes(x = Sex, y = SeLog)) +
    ylab(expression("ln(Loon blood Se (" * mu*"g/g wet weight))")) +
        xlab("Loon sex/age category") + 
    geom_boxplot() + theme_bw()
sexPlotSe
ggsave("sexPlotSe.pdf", sexPlotSe, width = 6, height = 4)

cor(dSe$SeLog, dSe$HgLog)
cor.test(dSe$SeLog, dSe$HgLog)

LakeSe <- dSe[ , list(
    SeLogLake = mean(SeLog),
    ALKLake = mean(ALK),
    SECCHILake = mean(SECCHI),
    PHLake = mean(PH),
    PhospLake = mean(Phosp),
    CHLALake = mean(CHLA),
    MAXdepthLake = mean(MAXdepth),
    AreaLake = mean(AREA)
    ), by = Lake]

ggpairs(LakeSe[ , -1, with = FALSE])
corr.test(LakeSe[ , -1, with = FALSE])

## convert to molar
dSe[ , SeMol := Selenium/78.96]
dSe[ , HgMol := Mercury/1000/200.59] 

dSe[ , HgToSe := HgMol / SeMol]


ggplot(data = dSe, aes(x = HgToSe)) +
    geom_histogram() +
        facet_grid( . ~ Sex)

ggplot(data = dSe, aes(x = Sex, y = HgToSe)) +
    geom_boxplot(notch = TRUE)

ggplot(data = dSe, aes(x = Mercury, y = HgToSe)) +
    geom_point() + stat_smooth(method = 'lm')

summary(lm(HgToSe ~ Sex  + Mass, data = dSe))
round(confint(lm(HgToSe ~ Sex  + Mass, data = dSe)), 2)
