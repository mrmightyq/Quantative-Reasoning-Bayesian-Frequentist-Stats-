# Answer/Helper File for Week 9, Logistic Regression
library(readr)
week9hiringdata <- read_csv("B:/GLOBAL/2063-BASUS/FLORHAM-PARK/NTH/NTH-T/TalentManagement/03_Data & Analytics (Quinn & Jake)/Quinn/Syracuse/Quantative Reasoning/Week 9/Week-9-Logistic-Regression/R-Clinic/week9hiringdata.csv")
# setwd("~/Dropbox/Teaching/Statistics/Handouts")
hiredata <- week9hiringdata
View(hiredata)
summary(hiredata)
str(hiredata)
# PCA analysis to look at the internal structure
# PCA not covered in online IST777
library(psych)
summary(principal(hiredata[,5:10],nfactors = 2))
loadings(principal(hiredata[,5:10],nfactors = 2))

cor(hiredata[,4:10])
lapply(hiredata[,4:10],hist)
library(apaTables)
library(dplyr)
dat_CorrCoup<- hiredata %>%
  select(recommend,vision,issues,trends,consult,lead,collab)
apa.cor.table(dat_CorrCoup, filename="Corr.TableCouple.doc")


hist(hiredata$recommend) #not really outliers on the high end

hist(hiredata$vision)

hist(hiredata$issues)

hist(hiredata$trends )

hist(hiredata$consult)

hist(hiredata$lead )

hist(hiredata$collab)
par(mfrow=c(4,2))

for(i in 4:10){
  hist(hiredata[,i])
}
library(ggplot2)
ggplot(data=hiredata) + aes(x=vision) + geom_histogram(binwidth=1,col="skyblue",fill="navy")
library(png)
library(grid)

sales$image <- "musk.png"
x11()



glmOut <- glm(formula = hired ~ recommend, family = binomial(link="logit"), data = hiredata)
summary(glmOut)
exp(coef(glmOut)) # Convert log odds to odds
exp(confint(glmOut)) # Look at confidence intervals
anova(glmOut, test="Chisq")   # Compare null model to predictor models

hiredata$recInv <- 4 - hiredata$recommend # Invert the sense
cor(hiredata$recInv,hiredata$recommend)

glmOut <- glm(formula = hired ~ recInv, family = binomial(link="logit"), data = hiredata)
summary(glmOut)
exp(coef(glmOut)) # Convert log odds to odds
exp(confint(glmOut)) # Look at confidence intervals
anova(glmOut, test="Chisq")   # Compare null model to predictor models

#install.packages("BaylorEdPsych")
library(BaylorEdPsych)
PseudoR2(glmOut) # Get Pseudo R-squared values

#install.packages("MCMCpack")    # Download MCMCpack package
library(MCMCpack) # Load the package 
bayesLogitOut <- MCMClogit(formula = hired ~ recInv, data = hiredata) # Run the model
summary(bayesLogitOut) # Show model summary
plot(bayesLogitOut) # Show diagnostic plots

recLogOdds <- as.matrix(bayesLogitOut[,"recInv"]) # Make a matrix to prepare for next step
recOdds <- apply(recLogOdds,1,exp) # Transform whole list of log odds into odds
hist(recOdds, main=NULL) # Make a histogram
abline(v=quantile(recOdds,c(0.025)),col="black") # Line for HDI	
abline(v=quantile(recOdds,c(0.975)),col="black") # Line for HDI

mean(recOdds) # Mean of the plain odds distribution
quantile(recOdds,c(0.025)) # Lower bound of HDI
quantile(recOdds,c(0.975)) # Upper bound of HDI


# Third phase is to look for another predictor

lmOut <- lm(recInv ~ vision + issues + trends + consult + lead + collab, data=hiredata)
summary(lmOut)
# This stepwise regression gives a clearer view of what contributes most to recommend
# library(MASS)
stepOut <- stepAIC(lmOut, direction="both")
stepOut$anova 

# Pick vision as a promising one because it seems least related to recommend
hiredata$visInv <- 5 - hiredata$vision

# Add vision to the model
glmOut <- glm(formula = hired ~ recInv + visInv, family = binomial(link="logit"), data = hiredata)
summary(glmOut)
exp(coef(glmOut)) # Convert log odds to odds
exp(confint(glmOut)) # Look at confidence intervals
anova(glmOut, test="Chisq")   # Compare null model to predictor models


#install.packages("BaylorEdPsych")
library(BaylorEdPsych)
PseudoR2(glmOut)
