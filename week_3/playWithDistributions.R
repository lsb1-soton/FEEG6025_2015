# Meta -----
# Play with different distributions
# code by: b.anderson@soton.ak.uk (@dataknut)

# Functions ----
checkFolder <- function(name) { #checks if folder exists and if not, creates it
  if(dir.exists(plotsf)) {
    print(paste0(plotsf, " folder exists, no need to create"))
  } else {
    print(paste0(plotsf, " folder does not exist, creating")) 
    dir.create(plotsf)
  }
}

# Housekeeping ----
# Working directory is:
getwd()

# change the working directory to where you want to save results (the plots)
setwd("~/UoS One Drive/PG/Southampton/FEEG6025 Data Analysis & Experimental Methods for Engineers/Week 3")

# Set name of folder for storing plots (saves re-typing each time we save a plot)
plotsf <- "plots"
# Use the checkFolder functon to see if the plots folder exists, if not create it so we can save the plots
checkFolder(plotsf)  

# clear the workspace
rm(list=ls())

# create a useful separator
sep <- "\n################\n"

# Check working directory is:
getwd()

# print the separator
cat(sep)

# Testing Normal with a mean of 500 and standard deviation of 100 ----
normTest <- rnorm(1000, mean = 500, sd = 100)
summary(normTest)
# if you want to force the x axis to have a specific range a - b, use the option xlim = c(a,b)
hist(normTest)
dev.copy(png,"plots/normTestHist.png")
dev.off()
# use a qnormal plot to test for normality
# R lets us add a theoretical line to the plot for comparison
qqnorm(normTest, main = "Q-Q plot for normal distribution")
  qqline(normTest, col = 2)
dev.copy(png,"plots/normTestQQ.png")
dev.off()

# Create a +ve skewed version by force ----
normTestPSkew <- normTest[normTest > mean(normTest)] # values > than the mean
summary(normTestPSkew)
hist(normTestPSkew, main = "Example of positive skew")
dev.copy(png,"plots/normTestPSkewHist.png")
dev.off()
# use a qnormal plot to test for normality
qqnorm(normTestPSkew, main = "Q-Q plot for positive skew")
  qqline(normTestPSkew, col = 2)
dev.copy(png,"plots/normTestPSkewQQ.png")
dev.off()
# to test for skew need
# packages.install("e1071")
# or
# packages.install("moments")
# library(e1071)
# skewness(normTestPSkew)

# Create a -ve skewed version by force ----
normTestNSkew <- normTest[normTest < mean(normTest)] # values < than the mean
summary(normTestNSkew)
hist(normTestNSkew, main = "Example of negative skew")
dev.copy(png,"plots/normTestNSkewHist.png")
dev.off()
# use a qnormal plot to test for normality
qqnorm(normTestNSkew, main = "Q-Q plot for negative skew")
  qqline(normTestNSkew, col = 2)
dev.copy(png,"plots/normTestNSkewQQ.png")
dev.off()
# skewness(normTestNSkew)

cat(sep)
# Testing Poisson, lambda = average number of events per unit time ----
poisTest <- rpois(1000, lambda = 3)
summary(poisTest)
hist(poisTest)
# Save it into the working directory. 
# Seriously, this is what you have to do.
# When we start using ggplot instead of plot things will get better.
# Things can only get better.
dev.copy(png,"plots/poisTestHist.png")
dev.off()

cat(sep)
# Testing Binomial ----
binomTest <- rbinom(1000, 20, 0.2)
hist(binomTest)
summary(binomTest)
dev.copy(png,"plots/binomTestHist.png")
dev.off()

cat(sep)
# Testing Exponential ----
expTest <- rexp(1000, 1)
hist(expTest)
summary(expTest)
dev.copy(png,"plots/expTestHist.png")
dev.off()

cat(sep)
# Testing standard normal - R defaults to mean = 0 & sd = 1 if you don't specifiy ----
stNormTest <- rnorm(1000)
summary(stNormTest)
hist(stNormTest)
# add lower & upper 95% lines
abline(v = c(-1.96,1.96), col="red")
# add text labels
text(-1.96,100, "-1.96", col="red")
text(1.96,100, "1.96", col="red")
text(-1.96, 180, pos = "4", "<----------", col = "red")
text(1.96, 180, pos = "2", "---------->", col = "red")
text(0, 180, "95% of the distribution is here", col = "red", pos = 3) # how to make background a different colour?
# save it
dev.copy(png,"plots/stNormTestHist.png")
dev.off()

cat(sep)
# Calculating confidence intervals using the initial normal distribution ----
m <- mean(normTest)
s <- sd(normTest)
n <- length(normTest)
se <- s/sqrt(n)

error <- qnorm(0.975)*se

normTestCIu <- m + error
normTestCIl <- m - error

hist(normTest, sub = "Red line = mean, green lines = 95% CI of the mean - not central 95% of the distribution")
# add the mean
abline(v = m, col="red")
# add lower & upper 95% lines
abline(v = c(normTestCIl,normTestCIu), col="green")

# save it
dev.copy(png,"plots/NormTestHistCI.png")
dev.off()

# tell the user the answer rounding the answer to 3 decimal places
print(paste0("normTest upper 95% CI: ", round(normTestCIu, 3)))
print(paste0("normTest mean: ", round(m, 3)))
print(paste0("normTest lower 95% CI: ", round(normTestCIl, 3)))

# now go back to the top and change the value of 1000 to 10,000 in:
# normTest <- rnorm(1000, mean = 500, sd = 100)
# then re-run the whole script - what do you notice about the new 95% CIs?

