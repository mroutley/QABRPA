library(lattice)
library(MASS)
library(nlme)
data <- read.csv("../data/QABRPAKejiLichenRichness.csv", header=TRUE)
names(data) <- tolower(names(data))
levels(data$lichen.species) <- tolower(levels(data$lichen.species))
data <- transform(data, date = as.Date(data$date, format="%B %d, %Y"))
attach(data)
lichens <- data.frame(year = as.factor(format(date, "%Y")), plot = as.factor(site.id), tree = as.factor(tree.id), species = as.factor(lichen.species))
detach(data)
rm(data)
data <- read.csv("../data/QABRPAKejiLichenIAP.csv", header=TRUE)
names(data) <- tolower(names(data))
attach(data)
iap <- data.frame(Year = as.factor(year), Plot = as.factor(plot), Tree = as.factor(tree), IAP = index.of.air.purity)
detach(data)
rm(data)



lichens.sum <- aggregate(!is.na(lichens$species), list(Year=lichens$year, Plot=lichens$plot, Tree=lichens$tree), sum, na.rm=T)
names(lichens.sum)[4] <- "Sum"
lichens.sum <- merge(lichens.sum, iap, all.x=TRUE, sort=TRUE)

bwplot(Sum ~ Year | Plot, groups = Tree, data = lichens.sum, type="b", ylab="Total species count")

tmp <- by(lichens.sum$Sum, list(lichens.sum$Year, lichens.sum$Plot), fitdistr, "poisson")
mus <- rep(NA, prod(dim(tmp)))
for (i in seq(prod(dim(tmp)))) {
	try(mus[i] <- tmp[[i]][[1]])
}
rm(tmp, i)


iterations <- 10000
durations <- seq(5, 10, 5)
changes <- seq(-0.02, 0.02, 0.01)
GenerateSimulation <- function(duration, change) {
	dist <- mean(mus)
	plots <- 6
	time <- rep(c("initial", "final"), each=plots)
	initial <- NULL; final <- NULL
	initial <- rpois(plots, lambda=dist)
	final <- rpois(plots, lambda=dist*(1+change)^duration)
	data.frame(time=time, plot=rep(1:plots, 2), count=c(initial, final))
}
TestSimulation <- function(simulated) {
	simulation.model <- lme(count ~ time, random= ~ 1|plot, data=simulated)
	ifelse(anova(simulation.model)$"p-value"[2] < 0.05, 1, 0)
}
RunSimulation <- function() {
	powerSequence <- data.frame(expand.grid(change=changes, duration=durations))
	powerSequence$power <- NA
	k <- 1 # Tracks position in sequence
	for (duration in durations) {
		for (change in changes){
			powerList <- NULL
			for (i in seq(iterations)) {
				simulated <- GenerateSimulation(duration, change)
				powerList[i] <- TestSimulation(simulated)
			}
			powerSequence$power[k] <- sum(powerList, na.rm=TRUE)/iterations
			k <- k+1
		}
	}
	powerSequence # Return value
}
powerSequence <- RunSimulation()
PowerPlot <- function() {
	xyplot(power ~ change, groups = duration, data = powerSequence, 
		panel= function(x,y,...){
			panel.xyplot(x, y, type="b", ...)
			panel.grid(h=-1, v=-1)
			panel.abline(h=0.7, col="red")
		}, 
		xlab = "Change (%)", ylab = "Estimated power", auto.key = list(points = TRUE, lines = TRUE, title="Time (years)", columns = length(unique(powerSequence$duration))))
}
PowerPlot()