# perch_maps_v2
.libPaths('D:/library/R')
setwd("D:/Projects/USGS_R/loons/Travis/Final 2017-12-07")

# load data
perchHG=read.csv("./UseYear/2018_02_27/perchHGPredictData2018_02_27.csv")

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
lakeHG = sqldf("SELECT WATERWAY, perchHG, EXP(perchHG) perchHGBT, latitude, longitude FROM lakeHG")
lakeHG = lakeHG[!is.na(lakeHG$WATERWAY),]
lakeHG = lakeHG[!is.na(lakeHG$longitude) & !is.na(lakeHG$latitude), ]

write.csv(lakeHG, "./coords/coordsData2018_02_27.csv", row.names=F)

# get mn map
minnesota <- get_map("minnesota", zoom = 7)

# heat map with zoom 7 & 8 bins (perch hg)
heat_z7 = ggmap(minnesota) + stat_density2d(data=lakeHG, aes(x=longitude, y=latitude, fill=..level.., alpha=..level..),
                                            geom="polygon", size=1, bins=8) +
  scale_fill_continuous(low="blue", high="black", limits=c(min(lakeHG$perchHG), max(lakeHG$perchHG))) +
  # scale_fill_gradient(low="red", high="green")
  scale_alpha(range = c(0,0.6), guide=FALSE)
png(filename="./Maps/heat_z7b8_2018_02_27.png", width=1280, height=1280)
print(heat_z7)
dev.off()

# heat map with zoom 7 & 12 bins (perch hg)
heat_z6 = ggmap(minnesota) + stat_density2d(data=lakeHG, aes(x=longitude, y=latitude, fill=..level.., alpha=..level..),
                                            geom="polygon", size=1, bins=12) +
  scale_fill_continuous(low="blue", high="black", limits=c(min(lakeHG$perchHG), max(lakeHG$perchHG))) +
  # scale_fill_gradient(low="red", high="green")
  # scale_alpha(range = c(0,0.6), guide=FALSE)
png(filename="./Maps/heat_z7b12_2018_02_27.png", width=1280, height=1280)
print(heat_z6)
dev.off()

# plain plot of perch hg points 
point_map_plain = ggmap(minnesota) +
  geom_point(data=lakeHG, aes(x=longitude, y=latitude, size=1, color=LogHgPpb))
png(filename="./Maps/point_map_plain_z7_2018_02_27.png", width=700, height=700)
print(point_map_plain)
dev.off()

# plain plot of perch hg points zoom 6
LogHgPpb=lakeHG$perchHG
minnesota_z6 = get_map("minnesota", zoom = 6)
point_map_plain = ggmap(minnesota_z6) +
  geom_point(data=lakeHG, aes(x=longitude, y=latitude, size=1, color=LogHgPpb)) + 
  scale_fill_continuous(low="blue", high="black", limits=c(min(LogHgPpb), max(LogHgPpb)))
png(filename="./Maps/point_map_plain_z6_2018_02_27.png", width=700, height=700)
print(point_map_plain)
dev.off()

# plain plot of BT perch hg points zoom 6
HgPpb=lakeHG$perchHGBT
point_map_plain = ggmap(minnesota_z6) +
  geom_point(data=lakeHG, aes(x=longitude, y=latitude, size=1, color=HgPpb)) + 
  scale_colour_gradient(low="yellow", high="blue", limits=c(min(HgPpb), max(HgPpb)))
png(filename="./Maps/point_map_plain_BT_2018_02_27.png", width=700, height=700)
print(point_map_plain)
dev.off()

# plotting perch hg z6 b8
minnesota <- get_map("minnesota", zoom = 6)
heatmap_fishz6b8 = ggmap(minnesota) + stat_density2d(data=lakeHG, aes(x=longitude, y=latitude, fill=..level.., alpha=..level..),
                                                geom="polygon", size=1, bins=8) +
  # scale_fill_continuous(low="blue", high="black", limits=c(min(lakeHG$perchHG), max(lakeHG$perchHG))) +
  scale_fill_gradient(low="red", high="green") + 
  scale_alpha(range = c(0,0.7), guide=FALSE)
png(filename="./Maps/heat_z6b8_2018_02_27.png", width=1280, height=1280)
print(heatmap_fishz6b8)
dev.off()

# plotting perch hg z6 b12
heatmap_fishz6b12 = ggmap(minnesota) + stat_density2d(data=lakeHG, aes(x=longitude, y=latitude, fill=..level.., alpha=..level..),
                                                 geom="polygon", size=1, bins=12) +
  scale_fill_continuous(low="blue", high="black", limits=c(min(lakeHG$perchHG), max(lakeHG$perchHG))) +
  # scale_fill_gradient(low="red", high="green")
  scale_alpha(range = c(0,0.7), guide=FALSE)
png(filename="./Maps/heat_z6b12_2018_02_27.png", width=1280, height=1280)
print(heatmap_fishz6b12)
dev.off()

#unique lat long
data = sqldf("SELECT COUNT(perchHG) perchHG, latitude, longitude FROM lakeHG GROUP BY latitude, longitude")

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

# load spatial libraries
library(sp)
library(raster)
library(gstat)

################ ordinary kriging model
z<-lakeHG$perchHG
x <- lakeHG[,"longitude"]
y <- lakeHG[,"latitude"]
d <- data.frame(z,x,y)
idm <- gstat(formula=z~1, locations=~x+y, data=d)
us<-getData('GADM', country='USA', level=2)
minnesota=subset(us, NAME_1=="Minnesota")
r <- raster(minnesota, resolution=1/50)
g <- as(r, 'SpatialGrid')
idp <- interpolate(r, idm)
idp <- mask(idp, minnesota)
plot(idp)

################## minimizing the kriging model
locations = SpatialPointsDataFrame(coords = lakeHG[,c("longitude", "latitude")],
                                   data = lakeHG,
                                   proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
RMSE <- function(observed, predicted) {
  sqrt(mean((predicted - observed)^2, na.rm=TRUE))
}

f1 <- function(x, test, train) {
  nmx <- x[1]
  idptemp <- x[2]
  if (nmx < 1) return(Inf)
  if (idptemp < .001) return(Inf)
  m <- gstat(formula=perchHG~1, locations=train, nmax=nmx, set=list(idp=idptemp))
  p <- predict(m, newdata=test, debug.level=0)$var1.pred
  RMSE(test$perchHG, p)
}

set.seed(20180209)
i <- sample(nrow(locations), 0.2 * nrow(locations))
tst <- locations[i,]
trn <- locations[-i,]
opt <- optim(c(8, .5), f1, test=tst, train=trn)

m <- gstat(formula=z~1, locations=~x+y, data=d, nmax=opt$par[1], set=list(idp=opt$par[2]))
idw <- interpolate(r, m)
idw <- mask(idw, minnesota)
png("./maps/Kriging_2018_02_27.png", height = 720, width = 720)
plot(idw, col=colorRampPalette(c("yellow", "darkblue"))(255))
dev.off()

m <- gstat(formula=z~1, locations=~x+y, data=d, nmax=25, set=list(idp=opt$par[2]))
idw <- interpolate(r, m)
idw <- mask(idw, minnesota)
plot(idw, col=colorRampPalette(c("yellow", "darkblue"))(255))
