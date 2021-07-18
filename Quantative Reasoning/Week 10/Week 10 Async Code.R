#Week 10 Async Code
chwBal <- ChickWeight # Copy the dataset
chwBal$TimeFact <- as.factor(chwBal$Time) #  Time to a factor
list <- rowSums(table(chwBal$Chick,chwBal$TimeFact))==12
list <- list[list==TRUE] # Keep only those with 12 observations
list <- as.numeric(names(list)) # Extract the row indices
chwBal <- chwBal[chwBal$Chick %in% list,] # Match against the data

# Run ezANOVA
#install.packages("ez")
library("ez")
ezANOVA(data=chwBal, dv=.(weight), within=.(TimeFact), wid=.(Chick), detailed=TRUE)

?beaver1
cor(beaver1$temp[1:100],beaver2$temp)
ts.plot(beaver1$temp[1:100])
ts.plot(beaver2$temp)

db1 <- diff(beaver1$temp[1:100])
db2 <- diff(beaver2$temp)
ts.plot(db1)
ts.plot(db2)
cor(db1, db2)

acf(beaver1$temp)
acf(beaver2$temp)
acf(ldeaths)
acf(faithful$eruptions)
acf(USAccDeaths)
acf(UKgas)
acf(Loblolly$height)
acf(beaver2$temp)

#install.packages("changepoint")
library(changepoint)
dEUstocks <- diff(EuStockMarkets)
dSMI <- dEUstocks[,2]

dSMIcp <- cpt.var(dSMI)
plot(dSMIcp,cpt.col="grey",cpt.width=5)
dSMIcp


dEUstocks <- diff(EuStockMarkets)
dSMI <- dEUstocks[,2]

dSMI<-EuStockMarkets[,2]
dSMIcp <- cpt.mean(dSMI)
plot(dSMIcp,cpt.col="grey",cpt.width=5)
dSMIcp
