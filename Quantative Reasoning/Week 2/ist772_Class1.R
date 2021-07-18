load("/Users/jason/Dropbox/IST 772 - Syracuse/tinydata.rdata") # Don't need if success with the dialog

# Let's find out what data we have available here
ls()

# Let's view each of these 
View(poldata2020)

# Alternatively
head(poldata2020)

# What are some characteristics of the data? 
dim(poldata2020) # dimensions
names(poldata2020) # Variable names 
length(names(poldata2020)) # Number of variables or
dim(poldata2020)[2]

# Manipulating variables
# Let's create a new variable and learn more about it

politician_age = poldata2020$age

# We can summarize this variable easily

summary(politician_age)

# Skill check: what kind of distribution is this? Symmetrical, left or right skewed?

# We can individually take summary statistics

mean(politician_age)
sd(politician_age)

# Histograms and distributions
# We can learn more about the distributions of variables by creating a histogram

hist(politician_age)

# We might also want to label the histogram

# We can also use ggplot2, a graphics package, to draw the histogram
library(ggplot2)
