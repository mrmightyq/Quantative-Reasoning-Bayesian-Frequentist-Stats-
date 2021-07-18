#Week 6 Async
set.seed(1)
pgrp1 <- sample(precip,20, replace=TRUE)
pgrp2 <- sample(precip,20, replace=TRUE)
pgrp3 <- sample(precip,20, replace=TRUE)
v1 <- var(precip)
v2 <-var(c(pgrp1,pgrp2,pgrp3))
pgrp3 <- pgrp3-5
v3 <- var(c(pgrp1,pgrp2,pgrp3))
barplot(c(v1,v2,v3),names.arg=c("Original Data","3 Samples","3rd Grp+5"))

#Next Section 6.4
set.seed(10)
precipAmount <- sample(precip,60,replace=TRUE) #Takes 60 random observations of US cities 
precipGrp <- as.factor(rep(seq(from=1,to=3,by=1),20)) #assigns a random factor group (1-3) with equal (20 observations per group)
precipDF <- data.frame(precipAmount, precipGrp) #stores the cities and the groups in a dataframe 
boxplot(precipAmount~precipGrp,data=precipDF) #creates a boxplot of precipitation per group
precipOut <- aov(precipAmount~precipGrp,data=precipDF) #runs an anova (frequentist) and stores it in precipOut
summary(precipOut) #summarizes precipOut output 
#cannot reject the null that the groups are equal 

library(BayesFactor)
chickBayesOut <- anovaBF(weight~feed, data=chickwts)
chickBayesOut #Good because odds ratio
## 3:1 not worth mentioning, 3:1-20:1 positive evidence for favored hypothesis
## 20:1 to 150:1 strong evidence
## 150:1 + very strong evidence for favored hypothesis 



set.seed(10)  
precipAmount <- sample(precip,60,replace=TRUE)
precipGrp <- as.factor(rep(seq(from=1,to=3,by=1),20))
precipDF <- data.frame(precipAmount, precipGrp)
precipBayesOut <- anovaBF(precipAmount ~ precipGrp, data=precipDF)
precipBayesOut
mcmcOut <- posterior(precipBayesOut,iterations=10000)
summary(mcmcOut)

precipDF$precipAmount[precipDF$precipGrp==3] <- precipDF$precipAmount[precipDF$precipGrp==3] - 7
precipBayesOut <- anovaBF(precipAmount ~ precipGrp, data=precipDF)
precipBayesOut
mcmcOut <- posterior(precipBayesOut,iterations=10000)
summary(mcmcOut)

