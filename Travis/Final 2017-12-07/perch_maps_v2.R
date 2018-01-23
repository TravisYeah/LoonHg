# perch_maps_v2
.libPaths('D:/library/R')
setwd("D:/Projects/USGS_R/loons/Travis/Final 2017-12-07")

# load data
perchHG=read.csv("./UseYear/perchHGPredictData.csv")

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
lakeHG = lakeHG[!is.na(lakeHG$longitude) & !is.na(lakeHG$latitude), ]

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
png(filename="./Maps/point_map_plain_z7.png", width=700, height=700)
print(point_map_plain)
dev.off()

# plain plot of perch hg points zoom 6
LogHgPpb=lakeHG$perchHG
minnesota_z6 = get_map("minnesota", zoom = 6)
point_map_plain = ggmap(minnesota_z6) +
  geom_point(data=lakeHG, aes(x=longitude, y=latitude, size=1, color=LogHgPpb)) + 
  scale_fill_continuous(low="blue", high="black", limits=c(min(LogHgPpb), max(LogHgPpb)))
png(filename="./Maps/point_map_plain_z6.png", width=700, height=700)
print(point_map_plain)
dev.off()

# plotting perch hg z6 b8
minnesota <- get_map("minnesota", zoom = 6)
heatmap_fishz6b8 = ggmap(minnesota) + stat_density2d(data=lakeHG, aes(x=longitude, y=latitude, fill=..level.., alpha=..level..),
                                                geom="polygon", size=1, bins=8) +
  # scale_fill_continuous(low="blue", high="black", limits=c(min(lakeHG$perchHG), max(lakeHG$perchHG))) +
  scale_fill_gradient(low="red", high="green") + 
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

#unique lat long
lakeHG
data = sqldf("SELECT COUNT(perchHG) perchHG, latitude, longitude FROM lakeHG GROUP BY latitude, longitude")
sqldf("SELECT * FROM data WHERE perchHG > 2")

# plotting perch hg with interpolated lat/long points
ggmap(minnesota) + 
  geom_tile(data = data, aes(x = longitude, y = latitude, fill = perchHG), alpha = 0.8) +
  stat_contour(data = data, aes(x = longitude, y = latitude, z = perchHG)) +
  ggtitle("Rwandan rainfall") +
  xlab("Longitude") +
  ylab("Latitude") +
  scale_fill_continuous(name = "perchHG (Hg)",
                        low = "white", high = "blue") +
  theme(plot.title = element_text(size = 25, face = "bold"),
        legend.title = element_text(size = 15),
        axis.text = element_text(size = 15),
        axis.title.x = element_text(size = 20, vjust = -0.5),
        axis.title.y = element_text(size = 20, vjust = 0.2),
        legend.text = element_text(size = 10)) +
  coord_map()

  
png(filename="./Maps/density1.png", width=1280, height=1280)
print(density1)
dev.off()

#Trying interpolation
library(sp)
library(raster)
#minnesota shapefile
us<-getData('GADM', country='USA', level=2)
minnesota=subset(us, NAME_1=="Minnesota")
plot(minnesota)

sp = SpatialPoints(lakeHG[,c("longitude", "latitude")], 
                    proj4string = CRS("+proj=longlat +datum=WGS84"))
sp = SpatialPointsDataFrame(sp, lakeHG)
# cuts = seq(min(lakeHG$perchHG), max(lakeHG$perchHG), length.out=6)
projection(cn) <- "+proj=longlat +datum=WGS84"
# the CRS we want
laea <- CRS("+proj=laea  +lat_0=0 +lon_0=-80")
clb <- spTransform(cn, laea)
pts <- spTransform(sp, laea)
plot(clb, axes=TRUE)
points(pts, col='red', cex=.5)

######################################################################