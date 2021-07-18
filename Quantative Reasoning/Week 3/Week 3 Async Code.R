#Gumballs
gumballs <- rep.int(1:2,25)
gumballs <- factor(gumballs, labels=c("Red","Blue"))
gumballs
sample(gumballs,size=10,replace=TRUE)
sum(sample(gumballs,size=10,replace=TRUE)=="Red")

sum(sample(gumballs,size=1000,replace=TRUE)=="Red")


runif(n=10,min=1,max=5)
rnorm(n=10,mean=5,sd=1)
rbinom(n=10,size=1,prob=.5)
runif(n=100,min=0,max=50)
runif(n=50,min=0,max=100)
runif(n=10)


#Toast
toast <- runif(100,0,180)
head(toast)
tail(toast)
mean(toast)
hist(toast)

set.seed(5)
toastAngleData <- runif(1000,0,180)
mean(toastAngleData)

mean(sample(toast,size=14,replace=TRUE))
samplingDistribution<-replicate(10000,mean(sample(toastAngleData,size=14,replace = TRUE)),simplify = TRUE)
hist(samplingDistribution)


quantile(0:100,probs=0.75)
qnorm(0.50)
qnorm(0.025)
qnorm(0.975)
qnorm(0.25)
qnorm(0.75)

mean(replicate(1000000,mean(sample(0:100, size=3))))
