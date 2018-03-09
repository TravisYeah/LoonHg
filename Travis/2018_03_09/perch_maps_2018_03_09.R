# perch_maps_v2
.libPaths('D:/library/R')
setwd("D:/Projects/USGS_R/loons/Travis/2018_03_09")

# load data
perchHG=read.csv("./UseYear/perchHGPredictData2018_03_09.csv")

################### CREATE LAT/LONG MAP HERE #########################

library(measurements)
library(ggmap)
library(data.table)
library(sqldf)
#read in coords
coords=fread("./coords/coordinates.csv")
coordsConv=fread("./coords/coordinatesConv.csv")
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
lakeHG = sqldf("SELECT WATERWAY, perchHG, EXP(perchHG) perchHGBT, latitude, longitude FROM lakeHG")
lakeHG = lakeHG[!is.na(lakeHG$WATERWAY),]
lakeHG = lakeHG[!is.na(lakeHG$longitude) & !is.na(lakeHG$latitude), ]

write.csv(lakeHG, "./coords/coordsData2018_03_09.csv", row.names=F)

# plain plot of perch hg points zoom 6
LogHgPpb=lakeHG$perchHG
minnesota_z6 = get_map("minnesota", zoom = 6)
point_map_plain = ggmap(minnesota_z6) +
  geom_point(data=lakeHG, aes(x=longitude, y=latitude, color=LogHgPpb)) + 
  scale_colour_gradient(low="yellow", high="blue", limits=c(min(LogHgPpb), max(LogHgPpb)))
png(filename="./Maps/point_map_plain_z6_2018_03_09.png", width=700, height=700)
print(point_map_plain)
dev.off()

# plain plot of BT perch hg points zoom 6
HgPpb=lakeHG$perchHGBT
point_map_plain_BT = ggmap(minnesota_z6) +
  geom_point(data=lakeHG, aes(x=longitude, y=latitude, color=HgPpb)) + 
  scale_colour_gradient(low="yellow", high="blue", limits=c(min(HgPpb), max(HgPpb)))
png(filename="./Maps/point_map_plain_BT_2018_03_09.png", width=700, height=700)
print(point_map_plain_BT)
dev.off()

