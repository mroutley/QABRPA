# ==========
# = George =
# ==========
data <- read.delim("../data/QABRPA KEJI Streamflow george.txt", header=TRUE)
attach(data)
data$date <- paste(Year, Month, Day, sep="-")
detach(data)
George <- data[ , c(18, 4:7)]
rm(data)
George <- transform(George, date = as.Date(George$date))
names(George)[5] <- "Discharge"
George.ts <- ts(George$Discharge, start=c(George[1,4]), freq=365)
plot(George.ts, main="Mersey George Lake Discharge", ylab="Discharge") 
# =====================
# = Mersey Mill Falls =
# =====================
data <- read.delim("../data/QABRPA KEJI Streamflow mersey.txt", header=TRUE)
attach(data)
data$date <- paste(Year, Month, Day, sep="-")
detach(data)
Mill <- data[ , c(18, 4:7)]
rm(data)
Mill <- transform(Mill, date = as.Date(Mill$date))
names(Mill)[5] <- "Discharge"
Mill.ts <- ts(Mill$Discharge, start=c(Mill[1,4]), freq=365)
plot(Mill.ts, main="Mersey Mill Falls Discharge", ylab="Discharge")
# =============
# = Moose Pit =
# =============
data <- read.delim("../data/QABRPA KEJI Streamflow moosepit.txt", header=TRUE)
attach(data)
data$date <- paste(Year, Month, Day, sep="-")
detach(data)
Moose <- data[ , c(18, 4:7)]
rm(data)
Moose <- transform(Moose, date = as.Date(Moose$date))
names(Moose)[5] <- "Discharge"
Moose.ts <- ts(Moose$Discharge, start=c(Moose[1,4]), freq=365)
plot(Moose.ts, main="Moose Pit Brook Discharge", ylab="Discharge")
# ============
# = Combined =
# ============
Mersey.ts <- ts.union(George.ts, Mill.ts, Moose.ts)
plot(Mersey.ts, main="Discharge from Mersey George Lake, Mersey Mill Falls, and Moose Pit Brook")
oldPar <- par()
lineTypes <- c("solid", "dotted", "dotdash")
par(mfrow=c(3,1))
ts.plot(aggregate(Mersey.ts, 1, mean), ylab = "Mean discharge", lty = lineTypes)
ts.plot(aggregate(Mersey.ts, 1, max), ylab = "Maximum discharge", lty = lineTypes)
ts.plot(aggregate(Mersey.ts, 1, min), ylab = "Minimum discharge", lty = lineTypes)
par(oldPar)
# acf(Mersey.ts, na.action = na.pass)
George.stl <- stl(George.ts, "periodic", na.action=na.exclude)
plot(George.stl)
Mill.stl <- stl(Mill.ts, "periodic", na.action=na.exclude)
plot(Mill.stl)
Moose.stl <- stl(Moose.ts, "periodic", na.action=na.exclude)
plot(Moose.stl)

George.ag.stl <- stl(aggregate(George.ts, 1, mean), "periodic", na.action=na.exclude)
plot(George.stl)

spectrum(Mersey.ts, na.action=na.exclude, span=c(3,3))
par(oldPar)