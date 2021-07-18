x <- c(1,2,3)
y <- c(1,3,2)
plot(x,y)
cor(x,y)

x <- c(1,2,3)
y <- c(3,1,2)
plot(x,y)
cor(x,y)


round( cor(iris[,1:4]), 2 )
round( cor(iris[,1:3]), 2 )


wood <- rnorm(24)
heat <- rnorm(24)
cor.test(wood,heat)

#Bayesian Correlation 
library("BayesFactor")

bfCorTest <- function (x,y) # Get r from BayesFactor
{
  zx <- scale(x) # Standardize X
  zy <- scale(y) # Standardize Y
  zData <- data.frame(x=zx,rhoNot0=zy) # Put in a data frame
  bfOut <- generalTestBF(x ~ rhoNot0, data=zData) # linear coefficient
  mcmcOut <- posterior(bfOut,iterations=10000) # posterior samples
  print(summary(mcmcOut[,"rhoNot0"])) # Get the HDI for rho
  return(bfOut) # Return Bayes factor object
}

bfCorTest(iris[,"Sepal.Length"],iris[,"Sepal.Width"])



set.seed(314)
badBoatMF <- ftable(Titanic, row.vars=2, col.vars="Survived")
badBoatMF
ctBFout <- contingencyTableBF(badBoatMF,sampleType="poisson",posterior=FALSE)
ctBFout

ctMCMCout <- contingencyTableBF(badBoatMF,sampleType="poisson",posterior=TRUE,iterations=10000)
summary(ctMCMCout)
maleProp <- ctMCMCout[,"lambda[1,1]"]/ctMCMCout[,"lambda[1,2]"]
mean(maleProp)
femaleProp <- ctMCMCout[,"lambda[2,1]"]/ctMCMCout[,"lambda[2,2]"]
mean(femaleProp)
diffProp <- maleProp - femaleProp
hist(diffProp)
mean(diffProp)
abline(v=quantile(diffProp,c(0.025)), col="black")
abline(v=quantile(diffProp,c(0.975)), col="black")
