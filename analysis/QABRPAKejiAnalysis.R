# ==============================================
# = QABRPA Analysis of Keji Annual Decay Rates
# Contact Matthew Routley matt@routleynet.org  =
# ==============================================
library(lattice)
# Import and manipulate data
data <- read.csv("../data/QABRPAKEJIAnnualDecayRates.csv", header=TRUE)
data[data==999] <- NA
attach(data)
decay <- data.frame(Years = get(names(data)[3]), Stand = get(names(data)[4]), Site = get(names(data)[5]), ADR = get(names(data)[6]), Decay = get(names(data)[10]))
decay <- transform(decay, Years = as.factor(decay$Years), ADR = as.factor(decay$ADR))
detach(data)
rm(data)
decay$SiteByStand <- with(decay, Site:Stand)[drop=TRUE] # Creates an interaction column for use with lme4
# summary(decay)
# Start with one-year decay data
decay.1 <- decay[decay$Years == 1, ]
decay.2 <- decay[decay$Years == 2, ]
# Variance componenets estimation
library(nlme)
library(ape)
decay.lme <- lme(Decay ~ 1, data = decay.1, random = ~ 1|Stand/Site/ADR, na.action = na.exclude)
plot(varcomp(decay.lme))
detach("package:ape")
detach("package:nlme")
# Plot individual decay values
xyplot(Decay ~ ADR | Stand, data = decay.1, groups=Site, ylab="Decay (%)", auto.key = TRUE)
# Boxplot of decay values for Site and Stand combinations
bwplot(Decay ~ Site | Stand, data = decay.1, ylab="Decay (%)")
# Estimate the one-year decay rate and its standard error
library(lme4)
library(arm)
decay.stand <- lmer(Decay ~ 1 + (1|Stand), data = decay.1, method="ML")
decay.site <- update(decay.stand, . ~ 1 + (1|Site))
decay.ADR <- update(decay.stand, . ~ 1 + (1|ADR))
decay.siteByStand <- update(decay.stand, . ~ 1 + (1|SiteByStand))
decay.ADR <- update(decay.stand, . ~ 1 + (1|SiteByStand/ADR))
display(decay.ADR)
mean.decay <- fixef(decay.ADR)[[1]]
se.decay <- se.fixef(decay.ADR)[[1]]
mean.decay + (1.2816 * se.decay)
mean.decay - (1.2816 * se.decay)
# Take a look at two-year data
decay2.siteByStand <- update(decay.siteByStand, . ~ ., data = decay.2)
display(decay2.siteByStand)
mean.decay2 <- fixef(decay2.siteByStand)[[1]]
se.decay2 <- se.fixef(decay2.siteByStand)[[1]]
bwplot(Decay ~ Years | Stand, data = decay, group = Site, ylab="Decay (%)")


iterations <- 1000
adrs <- 80
data <- decay.1$Decay
samples <- matrix(nrow = iterations, ncol = adrs)
for (t in 2:adrs) {
	for (i in seq(iterations)) {
	samples[i, t] <- sqrt(var(sample(data, t, replace=TRUE), na.rm=TRUE))
	}
}
rm(i, t)
samples <- as.data.frame(samples)
names(samples) <- as.character(1:adrs)
samples <- data.frame(adrs = 1:adrs, sd = mean(samples, na.rm=TRUE))
xyplot(sd ~ adrs, data = samples, xlab = "Number of ADRs", ylab = "Standard deviation of decay rate")
save(samples, file="QABRPAKeijiSample.Rdata")
