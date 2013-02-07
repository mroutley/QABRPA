# ====================================================================
# = Simulation code for determining the power of decay rate analyses
# Matthew Routley matt@routleynet.org =
# ====================================================================
# Requires an input file specifiying simulation parameters
# For example,
# iterations <- 1000
# changes <- 2
# se <- 2.1
# intervals <- c(1, 5)
# within <- 12
# plots <- 6
source("QABRPADecaySimulationInput.R")
# The following assumes that the error in estimating decay rate is independent of the actual rate of decay.
within <- LETTERS[1:within]
plots <- LETTERS[1:plots]
types <- LETTERS[1:2]
powerSequence <- data.frame(expand.grid(change=changes, interval=intervals))
powerSequence$power <- NA
k <- 1 # Tracks position in sequence
for (interval in intervals) {
	for (change in changes){
		powerList <- NULL
		for (i in seq(iterations)) {
			sample_length <- length(within)*length(plots)*length(types)
			simulated <- as.data.frame(expand.grid(types = types, plots = plots, within = within))
			simulated$decay[simulated$types==types[1]] = rnorm(sample_length/length(types), mean=change*interval, sd=interval*se)
			simulated$decay[simulated$types==types[2]] = rnorm(sample_length/length(types), mean=0, sd=interval*se)
			sample.stat <- lm(decay ~ types, data = simulated)
			powerList[i] <- summary(sample.stat)$"coefficients"[8] < 0.05
		}
		powerSequence$power[k] <- sum(powerList, na.rm=TRUE)/iterations
		k <- k+1
	}
}
powerSequence # Return value
powerSequence <- c(title = "Forillon Decay", powerTable = powerSequence, se = se, iterations = iterations)
save(powerSequence, file="QABRPADecaySimulation.Rdata")
