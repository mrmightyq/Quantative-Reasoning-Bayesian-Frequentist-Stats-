# Week 9 Ascyn Code
exp(1)
exp(-1)
exp(2)
exp(0)


logistic <- function(logistX)
{
  exp(logistX)/(exp(logistX)+1)
}

logistic(-6)
logistic(6)
logistic(2)
logistic(0)
logistic(-2)


set.seed(1234)
logistX <- rnorm(n=1000,mean=0,sd=1)
binomY <- round(exp(logistX)/(exp(logistX)+1))
logistX <- logistX/1.41 + rnorm(n=1000,mean=0,sd=1)/1.41
glmOut <- glm(binomY ~ logistX, family=binomial())
summary(glmOut)
anova(glmOut, test="Chisq")

set.seed(1234)
logistX <- rnorm(n=1000,mean=0,sd=1)
binomY <- round(exp(logistX)/(exp(logistX)+1))
logistX <- logistX/1.41 + rnorm(n=1000,mean=0,sd=1)/1.41
glmOut <- glm(binomY ~ logistX, family=binomial())
summary(glmOut)
anova(glmOut, test="Chisq")
exp(coef(glmOut))

install.packages("MCMCpack")    # Download MCMCpack package

library(car)
data(Chile)
ChileY <- Chile[Chile$vote == "Y",] # Grab the Yes votes
ChileN <- Chile[Chile$vote == "N",] # Grab the No votes
ChileYN <- rbind(ChileY,ChileN) # Make a new dataset with those
ChileYN <- ChileYN[complete.cases(ChileYN),] # Get rid of missing
ChileYN$vote <- factor(ChileYN$vote,levels=c("N","Y")) # Fix the factor
ChileYN$age10 <- ChileYN$age/10

library(MCMCpack) # Load the package
ChileYN$vote <- as.numeric(ChileYN$vote) - 1 # Adjust the outcome
set.seed(271) # Control randomization
bayesLogitOut <- MCMClogit(formula = vote ~ age10 + income, data = ChileYN)
summary(bayesLogitOut) # Summarize the results

ageLogOdds <- as.matrix(bayesLogitOut[,"age10"])
ageOdds <- apply(ageLogOdds,1,exp) # Transform with exp()
mean(ageOdds) # The point estimate for age/10 in plain odds
quantile(ageOdds,c(0.025)) # Lower bound of HDI
quantile(ageOdds,c(0.975)) # Upper bound of HDI


glmOut <- glm(vote ~ age10 + income, data = ChileYN, family=binomial())
summary(glmOut)
exp(confint(glmOut))
anova(glmOut, test="Chisq")
exp(coef(glmOut))
table(round(predict(glmOut,type="response")),ChileYN$vote)
