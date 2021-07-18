#Code for Week 4

library(psych)

summary(mtcars)
describe(mtcars)

mtcars$am == 0
mean(mtcars$mpg[ mtcars$am ==0])
mean(mtcars$mpg[ mtcars$am ==1])
sd(mtcars$mpg[ mtcars$am ==0])
sd(mtcars$mpg[ mtcars$am ==1])


#Draw one sample as a demo
sample(mtcars$mpg[ mtcars$am ==0], size=19,replace=TRUE)
sample(mtcars$mpg[ mtcars$am ==1], size=13,replace=TRUE)

meanDiffs <- replicate(100,
                       mean(sample(mtcars$mpg[ mtcars$am ==0], size=19,replace=TRUE))-
                       mean(sample(mtcars$mpg[ mtcars$am ==1], size=13,replace=TRUE)))
hist(meanDiffs)



#Boxplot
boxplot(mpg~am,data=mtcars)

mean( sample(mtcars$mpg[ mtcars$am == 0 ],size=19,replace=TRUE))
mean(mtcars$mpg[ mtcars$am == 0 ])      

# First inferenetial test
t.test(mtcars$mpg[mtcars$am==0], mtcars$mpg[mtcars$am==1])


#Confidence Interval animation
library(animation)
conf.int(level=0.95,100)



plot(seq(-4,4,.01),dt(seq(-4,4,.01),df=30))
abline(v=qt(0.975,df=30))
abline(v=qt(0.025,df=30))

t.test(mtcars$mpg[mtcars$vs==0],mtcars$mpg[mtcars$vs==1])
mean(mtcars$mpg[mtcars$vs==0])#V-shaped
