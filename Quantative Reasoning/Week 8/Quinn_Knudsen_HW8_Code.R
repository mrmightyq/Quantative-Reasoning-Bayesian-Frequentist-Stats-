#Week 8 Homework 
myCars <- data.frame(mtcars[,1:6])

cor(myCars)


lm.1 <- lm(mpg~wt+hp,data=myCars)
summary(lm.1)

library(QuantPsyc)
QuantPsyc::lm.beta(lm.1)

(-3.87783*(3))+(-0.03177*(110))+37.22727 

library("BayesFactor")

stateOutBF <- lmBF(mpg~wt+hp,data=myCars, posterior=FALSE)
stateOutBF


stateOutBF <- lmBF(mpg~wt+hp,data=myCars, posterior=TRUE, iterations=100000)
summary(stateOutBF)


library(car)
vif(lm.1)

lm.vif <- lm(mpg~.,data=myCars)
summary(lm.vif)
vif(lm.vif)
?mtcars
