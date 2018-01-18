setwd("D:/Projects/USGS_R/loons/Travis/Final 2017-12-07")

# load data
perchHG=read.csv("./UseYear/perchLoonHGData.csv")

################### CREATE LAT/LONG MAP HERE #########################

library(measurements)
library(ggmap)
library(sqldf)
#read in coords
coords=fread("D:/Projects/USGS_R/loons/Travis/Final 2017-12-07/coords/coordinates.csv")
coordsConv=fread("D:/Projects/USGS_R/loons/Travis/Final 2017-12-07/coords/coordinatesConv.csv")
coords[grep("adley", WATERWAY)]
# Convert to correct decimal format
coords[Lat != "", latitude := as.numeric(paste(substring(Lat, 1, 2), ".", substring(Lat, 3), sep=""))]
coords[Long != "", longitude := as.numeric(paste("-",substring(Long, 1, 2), ".", substring(Long, 3), sep=""))]
setorder(coords, WATERWAY)
setorder(coordsConv, WATERWAY)

## Create unique dowid & lat/long groupings
locations = sqldf("SELECT WATERWAY, AVG(latitude) latitude, AVG(longitude) longitude FROM coords GROUP BY WATERWAY")
join = sqldf("SELECT a.WATERWAY, b.perchHG, a.latitude, a.longitude FROM perchHG b
             LEFT JOIN locations a
             ON a.WATERWAY = b.WATERWAY
             ")
lakeHG = sqldf("SELECT WATERWAY, AVG(perchHG) perchHG, latitude, longitude FROM 'join' GROUP BY WATERWAY, latitude, longitude")
lakeHG=lakeHG[!is.na(lakeHG$WATERWAY),]
##

# Convert from UTM to decimal
# coords[, latitude := NA]
# coords[, longitude := NA]
# for(i in 1:nrow(coords)) {
#   coords$latitude[i] = ifelse(is.na(coords$Long[i]), coordsConv$X_Utm[i], coords$latitude[i])
#   coords$longitude[i] = ifelse(is.na(coords$Lat[i]), coordsConv$Y_Utm[i], coords$longitude[i])
# }
#
# coords[ grep("tamarac", WATERWAY, ignore.case=T) , WATERWAY:= "TAMARACK"]
# coords[ grep("LITTLE VERMILION", WATERWAY, ignore.case=F), WATERWAY:= "EAST VERMILION"]
# coords[ grep("EAGLES NEST #3", WATERWAY, ignore.case=F), WATERWAY:= "EAGLES NEST"]
# coords[ grep("AUTO", WATERWAY, ignore.case=F) , WATERWAY:= "Arrowhead [Auto]"]
# coords[, WATERWAY := sapply(WATERWAY,tolower)]
# coordsUnique <- unique(coords[, c("WATERWAY", "longitude", "latitude")])
# coordsUnique <- copy(coordsUnique[!is.na(latitude) & !is.na(longitude)])
# coordsUnique[, cnt := seq_len(.N), by="WATERWAY"]
# coordsUnique <- copy(coordsUnique[cnt == 1,])
# coordsUnique[, "cnt" := NULL]
# coordsUnique
# coords
# 
# setkey(coordsUnique, "DOWID")
# setkey(dHG2, "Lake")
# dHG2[, latitude := NA]
# dHG2[, longitude := NA]
# dHG2$Lake[1]
# 
# for(i in 1:nrow(dHG2)) {
#   sub=coordsUnique[WATERWAY == dHG2$Lake[i]]$latitude
#   if(length(sub) > 0) {
#     dHG2$latitude[i] = sub
#   } else {
#     dHG2$latitude[i] = NA
#   }
#   
# }
# 
# coordResult <- coordSummary[!is.na(latitude) & !is.na(longitude)]
# sort(unique(coordSummary$WATERWAY))[which(!(sort(unique(coordSummary$WATERWAY)) %in% unique(coordResult$WATERWAY)))]
# coords[grep("auto", WATERWAY)]

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