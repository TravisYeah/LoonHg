# perch_maps_v2

setwd("D:/Projects/USGS_R/loons/Travis/Final 2017-12-07")

# load data
perchHG=read.csv("./UseYear/perchLoonHGData.csv")

################### CREATE LAT/LONG MAP HERE #########################

library(measurements)
library(ggmap)
library(data.table)
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

#BT hg
# lakeHG <- as.data.table(lakeHG)
# lakeHG <- lakeHG[, perchHGBT := exp(perchHG)]

# get mn map
minnesota <- get_map("minnesota", zoom = 7)

# heat map with zoom 7 & 8 bins (perch hg)
heat_z7 = ggmap(minnesota) + stat_density2d(data=lakeHG, aes(x=longitude, y=latitude, fill=..level.., alpha=..level..),
                                            geom="polygon", size=1, bins=8) +
  scale_fill_continuous(low="blue", high="black", limits=c(min(lakeHG$perchHG), max(lakeHG$perchHG))) +
  # scale_fill_gradient(low="red", high="green")
  scale_alpha(range = c(0,0.6), guide=FALSE)
png(filename="./Maps/heat_z7b8.png", width=1280, height=1280)
print(heat_z7)
dev.off()

# heat map with zoom 7 & 12 bins (perch hg)
heat_z6 = ggmap(minnesota) + stat_density2d(data=lakeHG, aes(x=longitude, y=latitude, fill=..level.., alpha=..level..),
                                            geom="polygon", size=1, bins=12) +
  scale_fill_continuous(low="blue", high="black", limits=c(min(lakeHG$perchHG), max(lakeHG$perchHG))) +
  # scale_fill_gradient(low="red", high="green")
  # scale_alpha(range = c(0,0.6), guide=FALSE)
png(filename="./Maps/heat_z7b12.png", width=1280, height=1280)
print(heat_z6)
dev.off()

# plain plot of perch hg points 
LogHgPpb=lakeHG$perchHG
point_map_plain = ggmap(minnesota) +
  geom_point(data=lakeHG, aes(x=longitude, y=latitude, size=1, color=LogHgPpb))
# + stat_density2d(data=test, aes(x=lon, y=lat, fill=..level.., alpha=..level..),
# geom="polygon", size=1, bins=8) +
 # + scale_fill_continuous(low="blue", high="black", limits=c(min(lakeHG$perchHG), max(lakeHG)))
# scale_fill_gradient(low="red", high="green")
# scale_alpha(range = c(0,0.6), guide=FALSE)
png(filename="./Maps/point_map_plain.png", width=700, height=700)
print(point_map_plain)
dev.off()

# plotting perch hg z6 b8
minnesota <- get_map("minnesota", zoom = 6)
heatmap_fishz6b8 = ggmap(minnesota) + stat_density2d(data=lakeHG, aes(x=longitude, y=latitude, fill=..level.., alpha=..level..),
                                                geom="polygon", size=1, bins=8) +
  scale_fill_continuous(low="blue", high="black", limits=c(min(lakeHG$perchHG), max(lakeHG$perchHG))) +
  # scale_fill_gradient(low="red", high="green")
  scale_alpha(range = c(0,0.7), guide=FALSE)
png(filename="./Maps/heat_z6b8.png", width=1280, height=1280)
print(heatmap_fishz6b8)
dev.off()

# plotting perch hg z6 b12
heatmap_fishz6b12 = ggmap(minnesota) + stat_density2d(data=lakeHG, aes(x=longitude, y=latitude, fill=..level.., alpha=..level..),
                                                 geom="polygon", size=1, bins=12) +
  scale_fill_continuous(low="blue", high="black", limits=c(min(lakeHG$perchHG), max(lakeHG$perchHG))) +
  # scale_fill_gradient(low="red", high="green")
  scale_alpha(range = c(0,0.7), guide=FALSE)
png(filename="./Maps/heat_z6b12.png", width=1280, height=1280)
print(heatmap_fishz6b12)
dev.off()


######################################################################