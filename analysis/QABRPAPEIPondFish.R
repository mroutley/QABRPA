library(lattice)
library(MASS)
library(nlme)
data <- read.csv("../data/QABRPAPEIPondFishCUE.csv", header=TRUE)
attach(data)
eels <- data.frame(Site = as.factor(Waterbody), Sector = as.factor(Shoreline.Sector), CUE = American.Eel.CUE)
detach(data)
rm(data)

bwplot(CUE ~ Site, data = eels)

attach(eels)
Means <- tapply(CUE, list(Site), mean)
Vars <- tapply(CUE, list(Site), var)
xyplot(Vars ~ Means, panel = function(x, y) {
    panel.abline(a=0, b=1)
    panel.xyplot(x, y)
}, xlab="CUE means", ylab="CUE variances")
detach(eels)

fitted_poisson <- fitdistr(eels$CUE, "Poisson")$estimate[[1]]
# error_poisson <- fitdistr(eels$CUE, "Poisson")$sd[[1]]

data <- read.csv("../data/QABRPAPEIPondFishRichness.csv", header=TRUE)
attach(data)
species <- data[c(4:11)] 
detach(data)
rm(data)
shannon <- diversity(species, "shannon")
count <- specnumber(species)