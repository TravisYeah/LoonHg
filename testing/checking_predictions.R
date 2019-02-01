library(dplyr)
library(data.table)

data <- fread('C:/Users/tharrison/Files/projects/loons/UseYear/perchHGPredictData2018_03_09.csv')
b <- data.frame(perchhg = data$perchHG, loghgppb = log(data$HGppm*1000), Spec = data$Spec, Lgthcm = data$Lgthcm, Waterway = data$WATERWAY, perchppm = exp(data$perchHG)/1000, hgppm = data$HGppm, year = data$YEAR)
c <- b %>% filter(Spec == 'YP' & Lgthcm >= 10 & Lgthcm <= 14) # perch between 10 and 14 inches
d <- c %>% select(hgppm, perchppm)

plot(d)
abline(0, 1)
text(d$hgppm, d$perchppm, labels=c$Waterway, cex=.5, adj = c(0, -1))
text(d$hgppm, d$perchppm, labels=c$year, cex=.5, adj = c(0, -2))

lakes <- c %>% group_by(Waterway) %>% count()
lakes
write.csv(lakes, 'C:/Users/tharrison/Files/tmp/lake_counts.csv', row.names = F)
c <- c %>% select(perchhg, loghgppb)

write.csv(c, "C:/Users/tharrison/Files/tmp/horiz.csv")
nrow(c)




fit <- lm(loghgppb ~ perchhg, data=c)

summary(fit)
confint(fit)

plot(c)
abline(0, 1)


#############

c <- data %>% filter(Lgthcm > 15 & Lgthcm < 30) %>% mutate(loghgppb = log(HGppm*1000)) %>% select(perchHG, loghgppb)
plot(c)
abline(0, 1)
c %>% filter(perchHG < loghgppb) %>% count()/nrow(c)


b$bins <- cut(test$perchhg, breaks = 10)
m <- tapply(b$perchhg, b$bins, mean)
sd <- tapply(b$perchhg, b$bins, sd)

means <- data.frame(meanloghgppb = m, sd = sd, bins = names(m))

minx <- min(data$perchHG) - .5
maxx <- max(data$perchHG) + .5
miny <- min(log(data$HGppm*1000)) - .5
maxy <- max(log(data$HGppm*1000)) + .5

p1 <- ggplot(data = data, aes(x = perchHG, y = log(HGppm*1000),
                              xmin = minx,
                              xmax = maxx,
                              ymin = miny, 
                              ymax = maxy)) +
  geom_point()

p2 <- ggplot(data = means, aes(x = bins, 
                         y = meanloghgppb, 
                         xmin = minx,
                         xmax = maxx,
                         ymin = miny, 
                         ymax = maxy)) +
  geom_errorbar(color = "red") + 
  geom_point(size = 3, color = "red") + 
  theme_void()



p1 + geom_abline(slope=1)

library(grid)
grid.newpage()
pushViewport( viewport( layout = grid.layout( 1 , 1 , widths = unit( 1 , "npc" ) ) ) ) 
print( p1 + theme(legend.position="none") , vp = viewport( layout.pos.row = 1 , layout.pos.col = 1 ) )
print( p2 + theme(legend.position="none") , vp = viewport( layout.pos.row = 1 , layout.pos.col = 1 ) )

data %>% filter(perchHG < log(HGppm*1000)) %>% count()
nrow(data)

diff <- log(data$HGppm*1000) - data$perchHG
mean(diff)

735/33300
exp(1.62)





