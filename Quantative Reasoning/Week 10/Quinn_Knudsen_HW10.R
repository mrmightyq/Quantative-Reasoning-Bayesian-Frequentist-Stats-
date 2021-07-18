library(nlme)
library(Hmisc)
library(car)
blm <-data ("Blackmore")
blm <- Blackmore
boxplot(exercise~age, data=blm)
blm$age_bin <- cut2(blm$age,c(8,10,12,14,16,18))
boxplot(exercise~age_bin, data=blm)

blm <- Blackmore[Blackmore$age <=12,]
table(blm$subject,blm$age)
summary(blm)
list <- rowSums(table(myData$subject,myData$age))==3
list <- list[list==TRUE]
list <- as.numeric(names(list))
blm2 <- blm[blm$subject%in% list,]
blm2$age <- as.factor(blm2$age)
summary(aov(exercise~age,data=blm2))
out<-aov(exercise~age,data=blm2)
library(multcomp)
TukeyHSD(out, which = "age")
summary(glht(out, linfct = mcp(age = "Tukey")))
library(ez)
ezANOVA(data=blm2,dv=.(exercise),within=.(age),wid=(subject),detailed=TRUE)

#ezDesign(myData,age,exercise,subject)

#Question 5 
air <- AirPassengers
airdiff<-diff(air)
plot(airdiff)
library(changepoint)
aircp<-cpt.var(airdiff)
plot(aircp,cpt.col="grey",cpt.width=5)
aircp


#Question 6 
aircp<-cpt.mean(airdiff)
plot(aircp,cpt.col="grey",cpt.width=5)
aircp

#Question 7 find article 


#Question 8
library(bcp)
bcpair<-bcp(as.vector(airdiff))
plot(bcpair)
