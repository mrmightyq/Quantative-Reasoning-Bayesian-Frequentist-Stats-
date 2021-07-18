# Week 8 Ascyn Code

#install.packages("animation")
library(animation) # Load the animation library

par(mfrow=c(1,1)) # Configure the plot window as a single space

x <- seq(-10, 10, length= 30) # Create some X values
y <- x # Use the same for Y
f <- function(x,y) { z <- x*2 + y - 3 } # Z is a function of X and Y
z <- outer(x,y,f) # Run the function on all X and Y

ani.record(reset = TRUE) # Empty the animation buffer

# Change the perspective many times; theta is the azimuth direction
for (i in 1:70) {
  persp(x, y, z, theta = i, phi = 30, expand = 0.5, col = "lightcoral")
  ani.record()  # record the current frame
}

oopts = ani.options(interval = 0.1) # Set the delay between frames
ani.replay() # Replay the stored frames


#install.packages("BayesFactor")
library("BayesFactor")

stateData <- data.frame(state.x77)
stateOut <- lm(Life.Exp ~ HS.Grad + Income + Illiteracy,data=stateData)
summary(stateOut)

stateOutMCMC <- lmBF(Life.Exp ~ HS.Grad + Income + Illiteracy,data=stateData, posterior=TRUE, iterations=100000)
summary(stateOutMCMC)
rsqList <- 1 - (stateOutMCMC[,"sig2"] / var(stateData$Life.Exp))
mean(rsqList)
quantile(rsqList,c(0.025))
quantile(rsqList,c(0.975))

stateOutBF <- lmBF(Life.Exp ~ HS.Grad + Income + Illiteracy,data=stateData)
stateOutBF
