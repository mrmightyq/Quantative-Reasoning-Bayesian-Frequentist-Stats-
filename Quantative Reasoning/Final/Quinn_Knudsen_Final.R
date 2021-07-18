library(readr)
vax <- read_csv("B:/GLOBAL/2063-BASUS/FLORHAM-PARK/NTH/NTH-T/TalentManagement/03_Data & Analytics (Quinn & Jake)/Quinn/Syracuse/Quantative Reasoning/Final/schoolvax.csv")
summary(vax)
str(vax)

###### 1 Create New Variables #####

# Public Dummy
vax$pubpriv <- as.factor(vax$pubpriv)
summary(vax)
vax$public_dummy <- ifelse(vax$pubpriv== 'PUBLIC', 1,0)
vax[c(2,13)]
#vax$public_dummy<-as.factor(vax$public_dummy)

#Enrollment Indicator 
enrollmean <- mean(vax$enrollment)
vax$hi_enrollment <- ifelse(vax$enrollment > enrollmean , 1,0)
vax[c(3,14)]
#vax$hi_enrollment<-as.factor(vax$hi_enrollment)

#Religious Indicator 
vax$relig_exempt <- ifelse(vax$religious > 0 , 1,0)
vax[c(7,15)]
#vax$relig_exempt<-as.factor(vax$relig_exempt)

##### 2 Calculate means for each of the numeric variables
library(dplyr)
means <- vax[-c(1:2)] %>%
  summarise_all(mean)
knitr::kable(round(means,2))
means <- as.data.frame(means)
t(round(means,2))

#### 3 Develop a linear regression model that predicts medical exemptions "medical" from “pubpriv” and “enrollment”. 
model1 <- lm(medical ~ pubpriv+enrollment, data=vax)
summary(model1)

#### 5 Run a logistic regression model using public_dummy and enrollment to predict relig_exempt. 
model2 <- glm(relig_exempt~public_dummy+enrollment,data=vax)
summary(model2)
anova(model2, test="Chisq")
exp(coef(model2))
exp(confint((model2)))
library(BaylorEdPsych)
PseudoR2(model2)
tab2 <- table(round(predict(model2,type="response")),vax$relig_exempt)
sum(diag(tab2))/sum(tab2)


# Partioning the data train 80% test 20%
set.seed(23)
ind <- sample(2, nrow(vax), replace =T, prob = c(0.8, 0.2))
train <- vax[ind==1,]
test <- vax[ind==2,]


model1_glm <- train(as.factor(relig_exempt)~public_dummy+enrollment,
                    data = train,
                    method = "glm",
                    family = "binomial")

library(caret)
glm.prd <- predict(model1_glm, vax)
confusionMatrix(glm.prd, as.factor(vax$relig_exempt))
