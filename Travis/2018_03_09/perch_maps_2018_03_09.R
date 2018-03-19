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
# library(raster)
library(rgdal)

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

#write results
write.csv(lakeHG, "./coords/coordsData2018_03_09.csv", row.names=F)

#categorize perch Hg colors
categorizeColor <- function(xs) {
  colors <- rep("NULL", length(xs))
  for(i in 1:length(xs)) {
    if(xs[i] < .1) {
      colors[i] <- "green"
    } else if (xs[i] < .18) {
      colors[i] <- "yellow"
    } else if (xs[i] < .4) {
      colors[i] <- "orange"
    } else {
      colors[i] <- "red"
    }
  }
  return(colors)
}

#remove bad points
tail(lakeHG[order(lakeHG$longitude),], 10)
lakeHG <- subset(lakeHG, !(lakeHG$WATERWAY %in% c("SAGANAGA","BOOT","WASHBURN","BALL CLUB",
                                                  "HARRIET", "SQUARE", "BEAR", "ST. LOUIS R.",
                                                  "ST. LOUIS BAY", "BONE", "GOLF COURSE POND")))

#convert map to raster
states <- map_data("state")
minnesota <- subset(states, region == "minnesota")

# plain plot of perch hg points zoom 6.5
LogHgPpb=log(exp(lakeHG$perchHG)/1000 + 1)
`Perch Hg`=categorizeColor(LogHgPpb)
point_map_plain = ggplot(data=minnesota, mapping = aes(x = long, y = lat)) +
  geom_polygon(color="black", fill="gray") +
  geom_point(data=lakeHG, aes(x=longitude, y=latitude, color=`Perch Hg`), size=1) +
  scale_colour_manual(name="Perch Hg", values=c("green", "yellow", "orange", "red"),
                      labels=c("< 0.1 ug/g", "0.1 to 0.18 ug/g", "0.18 to < 0.4 ug/g", "> 0.4 ug/g"),
                      guide = "legend") +
  theme(legend.position = c(.825, .375), panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        panel.background = element_blank()) + 
  guides(colour = guide_legend(override.aes = list(size=3))) +
  xlab("longitude") +
  ylab("latitude")
point_map_plain
png(filename="./Maps/point_map_plain_z6_2018_03_09.png", width=700, height=700)
print(point_map_plain)
dev.off()

# plain plot of BT perch hg points zoom 6
HgPpb=lakeHG$perchHGBT
point_map_plain_BT = ggmap(minnesota) +
  geom_point(data=lakeHG, aes(x=longitude, y=latitude, color=HgPpb)) + 
  scale_colour_gradient(low="yellow", high="blue", limits=c(min(HgPpb), max(HgPpb)))
png(filename="./Maps/point_map_plain_BT_2018_03_09.png", width=700, height=700)
print(point_map_plain_BT)
dev.off()

