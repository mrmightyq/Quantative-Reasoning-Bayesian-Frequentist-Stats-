#Week 7 Homework 
cor.test(rock$area,rock$perm)
?rock

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

bfCorTest(rock[,"area"],rock[,"perm"])


UCBAdmissions[,,1]
chisq.test(UCBAdmissions[,,1])



set.seed(314)
ctBFout <- contingencyTableBF(UCBAdmissions[,,1],sampleType="poisson",posterior=FALSE)
ctBFout

ctMCMCout <- contingencyTableBF(UCBAdmissions[,,1],sampleType="poisson",posterior=TRUE,iterations=10000)
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
