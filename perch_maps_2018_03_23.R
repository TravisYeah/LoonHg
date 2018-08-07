# perch_maps_v2
# load data https://doi.org/10.5066/P9QHT2DG
perchHG=read.csv("perchHGPredictData2018_03_09.csv")

################### CREATE LAT/LONG MAP HERE #########################
library(measurements)
library(ggmap)
library(data.table)
library(sqldf)

library(rgdal)

#read in coords
coords=fread("coordinates.csv")
coordsConv=fread("coordinatesConv.csv")
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
write.csv(lakeHG, "coordsData2018_03_09.csv", row.names=F)

#categorize perch Hg colors
categorizeColor <- function(xs) {
  colors <- rep("NULL", length(xs))
  for(i in 1:length(xs)) {
    if(xs[i] < .093) {
      colors[i] <- "green"
    } else if (xs[i] < .167) {
      colors[i] <- "yellow"
    } else if (xs[i] < .372) {
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

#order data by perchHg
lakeHG <- lakeHG[order(lakeHG$perchHG),]

#convert map to raster
states <- map_data("state")
minnesota <- subset(states, region == "minnesota")
counties <- subset(map_data("county"), region == "minnesota")

# plain plot of perch hg points
HgPpb <- (exp(lakeHG$perchHG)-1)/1000
`Perch Hg` <- categorizeColor(HgPpb)
point_map_plain = ggplot(data=minnesota, mapping = aes(x = long, y = lat)) +
  geom_polygon(color="black", fill="gray") +
  geom_polygon(data=counties, aes(x=long, y=lat, group=group), fill=NA, color="white") +
  geom_point(data=lakeHG, aes(x=longitude, y=latitude, color=`Perch Hg`), size=1) +
  scale_color_manual(name="Perch Hg", 
                     values=c("green"="green", "yellow"="yellow", "orange"="orange", "red"="red"),
                     breaks=c("green", "yellow", "orange", "red"),
                     labels=c("< 0.093 ug/g", "0.093 to < 0.167 ug/g", "0.167 to < 0.372 ug/g", "> 0.372 ug/g")) +
  theme(legend.position = c(.825, .375), panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        panel.background = element_blank()) + 
  guides(colour = guide_legend(override.aes = list(size=3))) +
  xlab("Longitude") +
  ylab("Latitude")
point_map_plain
pdf("perch_Hg_map_2018_03_23.pdf", width=8.5, height=11)
print(point_map_plain)
dev.off()

