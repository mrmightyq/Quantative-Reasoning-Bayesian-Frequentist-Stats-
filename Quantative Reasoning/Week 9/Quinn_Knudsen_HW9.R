?mtcars()
glmOut <- glm(vs ~ gear+hp,data=mtcars, family=binomial())
summary(glmOut)
anova(glmOut, test="Chisq")
round(exp(coef(glmOut)),2)
data(mtcars)
cor.test(mtcars$hp,mtcars$vs)
library(BaylorEdPsych)
#library(remotes)
#install_version("BaylorEdPsych", "0.5")
PseudoR2(glmOut)
#mtcars$rankP <- predict(glmOut, newdata = mtcars, type = "response")
#round(mtcars,2)
## odds ratios and 95% CI
#exp(cbind(OR = round(coef(glmOut),2), round(confint(glmOut),2)))
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
bayesLogitOut <- MCMClogit(formula = vote ~ age + statusquo, data = ChileYN)
summary(bayesLogitOut) # Summarize the results
glmOut <- glm(vote ~ age + statusquo, data = ChileYN, family=binomial())
summary(glmOut)
anova(glmOut, test="Chisq")
exp(coef(glmOut))
exp(confint((glmOut)))
PseudoR2(glmOut)
tab2 <- table(round(predict(glmOut,type="response")),ChileYN$vote)
sum(diag(tab2))/sum(tab2)


plotodds <-function(logodds) {
  odds <- exp(logodds)
  hist(odds, breaks =100)
  abline(v=quantile(odds, 0.025))
  abline(v=quantile(odds,0.975))
}

plotodds(bayesLogitOut[,3])
