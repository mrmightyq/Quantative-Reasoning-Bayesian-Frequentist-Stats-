summary(ChickWeight)
colnames(ChickWeight)
dim(ChickWeight)

summary(ChickWeight$weight)
head(ChickWeight$weight)
mean(ChickWeight$weight)
myChkWts <- ChickWeight$weight
quantile(myChkWts,.50)

hist(myChkWts)
quantile(myChkWts,.025)
quantile(myChkWts,.975)
mean(myChkWts)
median(myChkWts)


samplingDistribution<-replicate(1000,mean(sample(myChkWts,size=11,replace = TRUE)),simplify = TRUE)
hist(samplingDistribution)
abline(v=quantile(samplingDistribution,.025),col="red", lwd=3, lty=2)
abline(v=quantile(samplingDistribution,.975),col="red", lwd=3, lty=2)


samplingDistribution<-replicate(1000,mean(sample(myChkWts,size=10000000,replace = TRUE)),simplify = TRUE)
hist(samplingDistribution)
abline(v=quantile(samplingDistribution,.025),col="red", lwd=3, lty=2)
abline(v=quantile(samplingDistribution,.975),col="red", lwd=3, lty=2)
