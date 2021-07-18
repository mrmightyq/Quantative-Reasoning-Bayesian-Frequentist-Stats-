hist(as.numeric(london$GoldMedals)) 
hist(as.numeric(london$Silver)) 
hist(as.numeric(london$Bronze)) 


summary(as.numeric(london$GoldMedals)) 
summary(as.numeric(london$Silver)) 
summary(as.numeric(london$Bronze)) 

london$tot_medals <- (london$GoldMedals + london$Silver + london$Bronze)
london$borda <- ((london$GoldMedals*3) + (london$Silver*2) + (london$Bronze))
london$dowdall <- ((london$GoldMedals*1) + (london$Silver*.5) + (london$Bronze*.333333333333333333))

summary(london$borda)
summary(london$dowdall)


df <- london %>% select(Country, tot_medals, borda, dowdall, Income, PopnSize)
summary(df)
cor(df[,-1])
library(apaTables)
apa.cor.table(df)

df <- scale(df[,-1])
df <- as.data.frame(df)
lm1 <- lm(borda ~ Income + PopnSize, data = df)
summary(lm1)
library(QuantPsyc)
model.beta <- lm.beta(lm1)
model.beta
