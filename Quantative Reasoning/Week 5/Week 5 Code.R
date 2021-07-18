library(BEST)
data(mtcars)
carsBEST <- BESTmcmc(mtcars$mpg[mtcars$am==0],
                     mtcars$mpg[mtcars$am==1])
plot(carsBEST)
summary(carsBEST)
carsBEST
plotAll(carsBEST)

library(effsize)
cohen.d(mtcars$mpg[mtcars$am==0],mtcars$mpg[mtcars$am==1])


set.seed(54321)
carsTdist <- rt(n=10000,df=18.332)
hist(carsTdist)
lowTvalues <- carsTdist[carsTdist<=-3.7671]
hiTvalues <- carsTdist[carsTdist>=3.7671]
length(lowTvalues)+length(hiTvalues)

t.test(mtcars$mpg[mtcars$am==0],mtcars$mpg[mtcars$am==1])
#####Homework#########

#Question 6
data(PlantGrowth)

PlantGrowth$weight[PlantGrowth$group=="ctrl"]
PlantGrowth$weight[PlantGrowth$group=="trt1"]

t.test(PlantGrowth$weight[PlantGrowth$group=="ctrl"],PlantGrowth$weight[PlantGrowth$group=="trt1"])

#Question 7
plantBEST <- BESTmcmc(PlantGrowth$weight[PlantGrowth$group=="ctrl"],
                      PlantGrowth$weight[PlantGrowth$group=="trt1"])
plot(plantBEST)
summary(plantBEST)
plotAll(plantBEST)

#Question 9
t.test(PlantGrowth$weight[PlantGrowth$group=="ctrl"],PlantGrowth$weight[PlantGrowth$group=="trt2"])

plantBEST2 <- BESTmcmc(PlantGrowth$weight[PlantGrowth$group=="ctrl"],
                      PlantGrowth$weight[PlantGrowth$group=="trt2"])
plot(plantBEST2)
summary(plantBEST2)
plotAll(plantBEST2)

#Question 10
jk <- t.test(rnorm(100000,mean=17.1,sd=3.8),rnorm(100000,mean=17.2,sd=3.8))
jk$conf.int

#tests <- c()
#for (i in 1:100) {
 # tests <- c(tests, t.test(x=PlantGrowth$group=="ctrl", y=PlantGrowth$group=="trt1"))
#}



#Quiz 
x <- seq(from=-3,to=3,by=.1)
plot(x, dt(x,df=30))
abline(v=-2.04)
abline(v=2.04)
abline(v=2.5,col="green")


X1 <-  c(32, 48, 23, 23, 23, 21, 28)
X2 <- c(51, 32, 33, 50, 26, 66, 27)
df <- data.frame(mpg=X1, wt=X2)
View(df)

length(df$mpg)== length(df$wt)
length(X1)
length(df$mpg)
median(X1)
max(X2)
min(df$wt)
X2[1]
df$wt[1]
