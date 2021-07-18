#Week 6 Asycn Code
library(BayesFactor)
chickBayesOut <- anovaBF(weight~feed, data=chickwts)
chickBayesOut #Good because odds ratio
## 3:1 not worth mentioning, 3:1-20:1 positive evidence for favored hypothesis
## 20:1 to 150:1 strong evidence
## 150:1 + very strong evidence for favored hypothesis 

