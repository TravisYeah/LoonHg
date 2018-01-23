.libPaths("D:/library/R")

## Load libraries 
library(data.table)
library(psych)
library(ggplot2)
library(GGally)
library(lme4)
library(lmerTest)
library(lubridate)

setwd("D:/Projects/USGS_R/loons/Travis/Final 2017-12-07")

## Load data

d <- fread("./FinalData/LoonData.csv")

## grab only the columns needed to analyze the data 
## And then reformat the data
dHg <- d[ , list(perchHG, lakeYearID, Year, Lake, SECCHI, PH,
                 Phosp, ALK, CHLA, AREA, MAXdepth, TSI, Age, Sex,
                 Mercury, Mass, Selenium, Date)]
dHg = d
dHg[ , Mass := as.numeric(Mass)]
dHg[ , HgLog := log(Mercury)]
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

### code for marginal plots   
perchBySex <- ggplot(data = dHG2, aes(x = perchHG, y = HgLog)) +
    geom_point() + stat_smooth(method = "lm", formula = y ~ x) +
        facet_grid( . ~ Sex) +
            theme_bw() +
                ylab(expression("ln(Loon blood Hg(" * mu*"g/g wet weight))")) +
                    xlab(expression("ln(Standardized perch Hg(" * mu*"g/g wet weight) + 1)")) +
                        theme(strip.background = element_blank())
perchBySex
ggsave("./plots/perchBySex.pdf", perchBySex, width = 8, height = 4)

onlyHG <- ggplot(data = dHG2, aes(x = perchHG, y = HgLog)) +
    geom_point() + stat_smooth(method = "lm", formula = y ~ x) +
            theme_bw() +
                ylab(expression("ln(Loon blood Hg(" * mu*"g/g wet weight))")) +
                    xlab(expression("ln(Standardized perch Hg (" * mu*"g/g wet weight) + 1)")) +
                        theme(strip.background = element_blank())
onlyHG
ggsave("./plots/onlyHG.pdf", onlyHG, width = 6, height = 4)

hgMass <- ggplot(data = dHG2, aes(x = Mass, y = HgLog)) +
    geom_point() + stat_smooth(method = "lm", formula = y ~ x) +
        facet_grid( . ~ Age) +
            theme_bw() +
                ylab(expression("ln(Loon blood Hg(" * mu*"g/g wet weight))")) +
                    xlab(expression("Mass (kg)")) +
                        theme(strip.background = element_blank())
hgMass
ggsave("./plots/hgMass.pdf", hgMass, width = 8, height = 4)


sexPlot <- ggplot(data = dHG2, aes(x = Sex, y = HgLog)) +
    ylab(expression("ln(Loon blood Hg(" * mu*"g/g wet weight))")) +
        xlab("Loon sex/age category") + 
    geom_boxplot() + theme_bw()
sexPlot
ggsave("./plots/sexPlot.pdf", sexPlot, width = 6, height = 4)

ggplot(data = dHG2, aes(x = Month, y = HgLog, color = Sex)) +
    geom_point() +
        scale_color_manual(values = c('blue', 'red', 'black'))

write.csv(file = "./FinalData/LoonDataFinalUsed.csv", x = dHG2, row.names = FALSE)

## Analysis

out <- lm(HgLog ~ perchHG + Sex + Mass:Age, data = dHG2)
summary(out)
out$fitted.values
confint(out, level = 0.95)

par(mfcol = c(2,2))
plot(out)

pdf("./plots/LoonHGresid.pdf")
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
ggsave("./plots/HgSeHist.pdf", HgSeHist, width = 6, height = 4)

dHg[ , HgLog := log(Mercury)]
dHg[ , SeLog := log(Selenium)]

HgSeBoxplot <- ggplot(dHg, aes(x = Sex, y = Selenium)) + geom_boxplot() + 
	theme_minimal()
print(HgSeBoxplot)
ggsave("./plots/HgSeBoxplot.pdf", HgSeBoxplot, width = 6, height = 4)

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
ggsave("./plots/sexPlotSe.pdf", sexPlotSe, width = 6, height = 4)

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
