# Week 3 - R-Studio Clinic #4

# Instructions: Examine the code and comments below.
# Add code can comments as instructed to perform various tasks.

set.seed(1234) 						                # Control randomization
testPop <- rnorm(100000, mean=100, sd=10)	# Create simulated population of test scores
# testPop now contains the simulated population of test scores from which
# we will sample and calculate sample means. But first, let's check our
# work to see what the testPop look like.

# Question 1 - Write a line of code below this comment to calculate
# the mean of testPop.
mean(testPop)



# Question 2 - Write a line of code below this comment to produce
# a histogram of testPop.
hist(testPop)

# Question 3 - Add a comment under these two lines of code explaining what
# is going on.
sampleTestScores <- function(n) {sample(testPop, size=n, replace=TRUE)}
mean(sampleTestScores(100))
# Add comment here
#Oh NOOOO sample size variance 
#This code takes 100 observations from testPop and calculates the mean value 


# Question 4 - Add a comment under this lines of code explaining what
# is going on.
samplingDistribution <- replicate(1000, mean(sampleTestScores(100)))
# Add comment here
mean(samplingDistribution)


# Question 5 -  Write two lines of code to display 
# the minimum and maximum values in samplingDistribution.



# Question 6 - Write a comment describing why the minimum and 
# maximum are three or four points away from the mean of testPop
# Add comment here


# Question 7 - Write a comment describing the differences between 
# these two histograms and the cause of those differences.
par(mfrow=c(2,1))
hist(testPop, xlim=c(50,140))
hist(samplingDistribution, xlim=c(50,140))
par(mfrow=c(1,1))
# Add comment here.


# Question 8 - Write two lines of code to calculate the standard 
# deviation of testPop and the standard deviation of samplingDistribution.
# Add a comment to describe why these two values are so different.


# Add comment here.
