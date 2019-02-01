## Load libraries 
library(data.table)
library(psych)
library(ggplot2)
library(GGally)
library(lme4)
library(lmerTest)
library(lubridate)
library(dplyr)

## Load data https://doi.org/10.5066/P9TDCH3F
setwd('C:/Users/tharrison/Files/projects/loons/UseYear')
d <- fread("LoonData2018_03_09.csv")
dHg <- d
# grab only the columns needed to analyze the data
# And then reformat the data
dHg[ , Mass := as.numeric(Mass)]
dHg[ , HgLog := log(Mercury)]
dHg[ , Date := as.POSIXct(strptime(Date, format = "%Y-%m-%d"))]
dHg[ , Month := month(Date)]
dHg[ , Day := day(Date)]
dHg[ , perchHG := as.numeric(perchHG)]

dHG2 <- copy(dHg)

## Exculde unkown sex adult
dHG2[ , which(Age == "Adult" & Sex == "Unknown")]
dHG2 <- copy(dHG2[ - dHG2[ , which(Age == "Adult" & Sex == "Unknown")],])

dHG2[ , Sex := factor(Sex)]
levels(dHG2$Sex)[3] <- "Juvenile"
dHG2[ , Sex := factor(Sex)]

## code for marginal plots
perchBySex <- ggplot(data = dHG2, aes(x = perchHG, y = HgLog)) +
    geom_point() + stat_smooth(method = "lm", formula = y ~ x) +
        facet_grid( . ~ Sex) +
            theme_bw() +
                ylab(expression("ln(Loon blood Hg(ng/g wet wt))")) +
                    xlab(expression("ln(Standardized perch Hg(ng/g wet wt) + 1)")) +
                        theme(strip.background = element_blank())
perchBySex
ggsave("perchBySex2018_12_06.pdf", perchBySex, width = 8, height = 4)

perchBySexMaleFit <- lm(HgLog ~ perchHG, data = dHG2 %>% filter(Sex == "Female"))
summary(perchBySexMaleFit)
confint(perchBySexMaleFit)
perchBySexFemaleFit <- lm(HgLog ~ perchHG, data = dHG2 %>% filter(Sex == "Male"))
summary(perchBySexFemaleFit)
confint(perchBySexFemaleFit)
perchBySexJuvenileFit <- lm(HgLog ~ perchHG, data = dHG2 %>% filter(Sex == "Juvenile"))
summary(perchBySexJuvenileFit)
confint(perchBySexJuvenileFit)

onlyHG <- ggplot(data = dHG2, aes(x = perchHG, y = HgLog)) +
    geom_point() + stat_smooth(method = "lm", formula = y ~ x) +
            theme_bw() +
                ylab(expression("ln(Loon blood Hg(ng/g wet wt))")) +
                    xlab(expression("ln(Standardized perch Hg (ng/g wet wt) + 1)")) +
                        theme(strip.background = element_blank())
onlyHG
ggsave("onlyHG2018_12_06.pdf", onlyHG, width = 6, height = 4)

hgMass <- ggplot(data = dHG2, aes(x = Mass, y = HgLog)) +
    geom_point() + stat_smooth(method = "lm", formula = y ~ x) +
        facet_grid( . ~ Age) +
            theme_bw() +
                ylab(expression("ln(Loon blood Hg(ng/g wet wt))")) +
                    xlab(expression("Mass (kg)")) +
                        theme(strip.background = element_blank())
hgMass
ggsave("hgMass2018_12_06.pdf", hgMass, width = 8, height = 4)

hgMassFitAdult <- lm(HgLog ~ Mass, data = dHG2 %>% filter(Age == "Adult"))
summary(hgMassFitAdult)
confint(hgMassFitAdult)
hgMassFitJuvenile <- lm(HgLog ~ Mass, data = dHG2 %>% filter(Age == "Juvenile"))
summary(hgMassFitJuvenile)
confint(hgMassFitJuvenile)

sexPlot <- ggplot(data = dHG2, aes(x = Sex, y = HgLog)) +
    ylab(expression("ln(Loon blood Hg(ng/g wet wt))")) +
        xlab("Loon sex/age category") +
    geom_boxplot() + theme_bw()
sexPlot
ggsave("sexPlot2018_12_06.pdf", sexPlot, width = 6, height = 4)

sexSeFemale <- lm(HgLog ~ perchHG, data = dHG2 %>% filter(Sex == "Female"))
summary(sexSeFemale)
confint(sexSeFemale)
sexSeMale <- lm(HgLog ~ perchHG, data = dHG2 %>% filter(Sex == "Male"))
summary(sexSeMale)
confint(sexSeMale)
sexSeJuvenile <- lm(HgLog ~ perchHG, data = dHG2 %>% filter(Sex == "Juvenile"))
summary(sexSeJuvenile)
confint(sexSeJuvenile)

ggplot(data = dHG2, aes(x = Month, y = HgLog, color = Sex)) +
    geom_point() +
        scale_color_manual(values = c('blue', 'red', 'black'))

ggplot(data = dHG2, aes(x = Mass, y = HgLog, color = Sex)) +
  geom_point() +
  scale_color_manual(values = c('blue', 'red', 'black'))

## Analysis
outA <- lm(HgLog ~ perchHG + Sex + Mass:Sex, data = dHG2)
outB <- lm(HgLog ~ perchHG + Sex + Mass:Age, data = dHG2)
outC <- lm(HgLog ~ perchHG + Age + Mass:Sex, data = dHG2)
outD <- lm(HgLog ~ perchHG + Age + Mass:Age, data = dHG2)

summary(outA)
summary(outB)
summary(outC)
summary(outD)

AIC(outA)
AIC(outB)
AIC(outC)
AIC(outD)

confint(outA, level = 0.95)
confint(outB, level = 0.95)
confint(outC, level = 0.95)
confint(outD, level = 0.95)

pdf("LoonHGresid2018_03_09.pdf")
par(mfcol = c(2,2))
plot(outA)
plot(outB)
plot(outC)
plot(outD)
dev.off()

confint(outA, level = 0.95)


#########################

Lake <- dHG2[ , list(
    perchHGLake = mean(perchHG),
    ALKLake = mean(ALK),
    SECCHILake = mean(SECCHI),
    PHLake = mean(PH),
    PhospLake = mean(Phosp),
    CHLALake = mean(CHLA),
    MAXdepthLake = mean(MAXdepth),
    AreaLake = mean(AREA)
    ), by = LakeID]

## plot correlations
ggpairsplot = ggpairs(Lake[ , -1, with = FALSE])
ggsave("Hgpairs2018_03_09.pdf", ggpairsplot, width = 8, height = 8)

corr.test(Lake[ , -1, with = FALSE])
plot(Lake$PHLake, Lake$perchHGLake)

# lake ph correlation with perch Hg
cor.test(Lake$PHLake, Lake$perchHGLake)

summary(lm(perchHGLake ~ PHLake + ALKLake +
               SECCHILake + MAXdepthLake + PhospLake + AreaLake,
           data = Lake))

ggplot(data = d, aes(x = Mercury, y = Selenium)) + geom_point() +
    scale_x_continuous(trans = 'log')+
        scale_y_continuous(trans = 'log') +
            stat_smooth(method = 'lm')
##
HgSeHist <- ggplot(dHg, aes(x = Selenium)) + geom_histogram() +
    scale_x_continuous(trans = 'log') +
        facet_grid( . ~ Sex) + theme_minimal()
print(HgSeHist)
ggsave("HgSeHist2018_03_09.pdf", HgSeHist, width = 6, height = 4)

dHg[ , HgLog := log(Mercury)]
dHg[ , SeLog := log(Selenium)]
dHg[ , LogPpmSe := log(Selenium*1000 )]

HgSeBoxplot <- ggplot(dHg, aes(x = Sex, y = Selenium)) + 
  geom_boxplot() + 
  theme_minimal()
print(HgSeBoxplot)
ggsave("HgSeBoxplot2018_03_09.pdf", HgSeBoxplot, width = 6, height = 4)

## Start of Se analysis
dSe <- dHg[ SeLog != -Inf,]
dSe <- copy(dSe[ - dSe[ , which(Age == "Adult" & Sex == "Unknown")],])
dSe

## Se Log model
SeLogModel <- lm(SeLog ~ Sex  + Mass:Age, data = dSe)
summary(SeLogModel)
round(confint(SeLogModel), 2)

dSe[ , Sex := factor(Sex)]
levels(dSe$Sex)[3] <- "Juvenile"
sexPlotSe <- ggplot(data = dSe, aes(x = Sex, y = SeLog)) +
    ylab(expression("ln(Loon blood Se ("*mu*"g/g wet wt))")) +
        xlab("Loon sex/age category") + 
    geom_boxplot() + theme_bw()
sexPlotSe
ggsave("sexPlotSe2018_03_09.pdf", sexPlotSe, width = 6, height = 4)

sexPlotSe <- ggplot(data = dSe, aes(x = Sex, y = SeLog)) +
  ylab(expression("ln(Loon blood Se ("*mu*"g/g wet weight))")) +
  xlab("Loon sex/age category") + 
  geom_boxplot() + theme_bw()
sexPlotSe
ggsave("sexPlotSe2018_03_09.pdf", sexPlotSe, width = 6, height = 4)
expression("ln(Loon blood Se ("*mu*"g/g wet weight))")

## SE vs Hg with unkown sex
sexPlotSeVsHg <- ggplot(data = dSe, aes(x = HgLog, y = LogPpmSe)) +
  ylab(expression("ln(Loon blood Se (n/g wet wt))")) +
  xlab(expression("ln(Loon blood Hg (n/g wet wt))")) + 
  geom_point() + stat_smooth(method = 'lm') +
  facet_grid( . ~ Sex) +
  theme_bw() +
  theme(strip.background = element_blank())
sexPlotSeVsHg
ggsave("sexPlotSeVsHgUnkown2018_03_09.pdf", sexPlotSeVsHg, width = 6, height = 4)


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

## Hg to Se ration model
HtToSeModel <- lm(HgToSe ~ Sex  + Mass:Age, data = dSe)
summary(HtToSeModel)
round(confint(HtToSeModel), 2)
