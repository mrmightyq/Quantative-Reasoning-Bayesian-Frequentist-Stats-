#Week 6 Homework 

library(car)
library(multcomp)
library(BayesFactor)

data <- InsectSprays
summary(data)

#Question 2
insectResults <- aov(count~spray,data=data)
summary(insectResults)
TukeyHSD(insectResults, which = "spray")
summary(glht(insectResults, linfct = mcp(spray = "Tukey")))


#Question 6

insectBayesOut <- anovaBF(count~spray,data=data)
insectBayesOut #very strong evidence from odds ratio
## 3:1 not worth mentioning, 3:1-20:1 positive evidence for favored hypothesis
## 20:1 to 150:1 strong evidence
## 150:1 + very strong evidence for favored hypothesis 

mcmcOut2 <-posterior(insectBayesOut,iterations=10000)
boxplot(as.matrix(mcmcOut2[,2:7]))
summary(mcmcOut2)

plot(BESTmcmc(data[data$spray=="F",1],
              data[data$spray=="C",1]))

insectBEST<-BESTmcmc(data[data$spray=="F",1],
         data[data$spray=="C",1])
summary(insectBEST)


t.test(data[data$spray=="F",1],
                data[data$spray=="C",1])
