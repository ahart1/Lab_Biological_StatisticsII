# Amanda Hart
# BioStatsII Lab 3

#### Question 1
# Generate 100 random normal numbers, mean=24, std dev=10, find proportion greater than or equal to 2 std dev from mean
Numbers <- rnorm(n=100,mean=24,sd=10)
print(Numbers)
plot(Numbers)
# What proportion is within 2 std dev of mean (greater than 44, less than 4)
MeetCriteria <- length(which(Numbers>=44))+length(which(Numbers<=4))
Proportion <- MeetCriteria/length(Numbers)
print(Proportion)

# Flip a fair coin 6 times
coin <- c("Heads","Tails")
sample(coin, size=6, replace=T)

# Probability of getting 6 heads on those six flips (P(X=6) given n=6)
dbinom(x=6, size=6, prob=0.5)

# How much more likely is it to get 3 heads than 6?
# Probability to get 3 heads
ThreeHeads <- dbinom(x=3, size=6, prob=0.5)
SixHeads <- dbinom(x=6, size=6, prob=0.5)
Answer <- ThreeHeads-SixHeads
print(Answer)

# For a std normal random variable find number x such that P(-x=<X=<x)=0.24
# Remaining area= 1-0.24=0.76
# Half of this remaining area(0.38) is left of the number x
x <- qnorm(0.38, mean=0, sd=1, lower.tail=TRUE, log.p=FALSE)
abs(x) # Gives the absolute value of the output

# Mean rate of arrivals=3.5 alewives per hour, plot prob distribution function for number of arrivals
# Create vector with number of fish per hour
Alewife <- seq(0,10)
Results <- dpois(Alewife, lambda=3.5, log=FALSE)
plot(Results, type="l",xlab="Arrival Rate (number fish/hour)", ylab="Probability", main="Probability Distribution for Alewife Arrivals")

# 95% CI for number of alewives arriving per day
# Upper confidence interval
UpperCI <-qpois(0.975, lambda=3.5, lower.tail=TRUE, log.p=FALSE)  
print(UpperCI)
# Lower confidence interval
LowerCI <- qpois(0.025, lambda=3.5, lower.tail=TRUE, log.p=FALSE)
print(LowerCI)
# A lower CI of 0 seems incorrect since there is then a 0.025 probability of having an arrival rate of less than 0 fish/hour 
# This is unreasonable since you can't have less than 0 fish per hour arriving and poisson distributions can't be negative
# The problem is likely due to rounding error



#### Question 2
setwd("/Users/arhart/Research/BioStatsIICoding")
RIKZ <- read.table(file="RIKZ.txt", header=TRUE)

# Extract residuals from the RIKZ.lm1 model
# First add Richness data
RIKZ$Richness <- rowSums(RIKZ[,2:76]>0)
# Then fit model
RIKZ.lm1 <- lm(Richness~NAP, data=RIKZ)
# To extract residuals
Residuals <-residuals(RIKZ.lm1)
print(Residuals)
#OR
print(RIKZ.lm1$residuals)

# Check for normality using residuals (need MASS package)
library(MASS)
plot(RIKZ.lm1)
# Residuals vs. Fitted and Normal Q-Q plots allow you to check for normality

# Check whether the variances are equal
plot(RIKZ.lm1)
# Residuals vs. Fitted data shows whether variances are equal (should be equally positive and negative if equal variance)

# Are the assumptions met?
# Check for normality: Residuals vs. Fitted and NormalQ-Q pots
plot(RIKZ.lm1)
# Get a summary of model, residuals, coefficients, F-statistic
summary(RIKZ.lm1)
# Residuals are not equally distributed about zero, they are more frequently negative with several very large positive outliers
# The data is not normally distributed, there are several large positive residuals that do not fall on the line from the Q-Q plot
# More of the data with negative values falls on the line, and is therefore more normally distributed (although the entire data set is not normally distributed)

# Compute AIC for the two RIKZ models using AIC()
# The two models
RIKZ.lm1 <- lm(Richness~NAP, data=RIKZ)
RIKZ.lm2 <- lm(Richness~NAP+factor(week), data=RIKZ)
# Compute AIC
AIC(RIKZ.lm1)
AIC(RIKZ.lm2)

# Use logLik() function to get log-likelihood and number of parameters, then calculate AIC by hand (AIC=-2*ln(likelihood)+2par)
Model1Lik <- logLik(RIKZ.lm1)
print(Model1Lik)
AIC1 <- -2*(-126.9767)+2*3
print(AIC1)
#OR
AIC1 <- -2*Model1Lik+2*3 
print(AIC1)
Model2Lik <- logLik(RIKZ.lm2)
print(Model2Lik)
# For Model 2
AIC2 <- -2*(-110.4516)+2*6
print(AIC2)
#OR
AIC2 <- -2*(Model2Lik)+2*6
print(AIC2)

# Calculate AICc for each model (AICc=-2*ln(likelihood)+2par*n/(n-par-1)
n_Param <- nrow(RIKZ)
AICc1 <- -2*Model1Lik+2*3*n_Param/(n_Param-3-1)
print(AICc1)
AICc2 <- -2*Model2Lik+2*6*n_Param/(n_Param-6-1)
print(AICc2)



#### Question 3
#Fit poisson model to RIKZ species richness that includes NAP, exposure and week
RIKZ_poisson <- glm(Richness~NAP+factor(week)+factor(exposure),data=RIKZ, family=poisson)
summary(RIKZ_poisson)

# Plot deviance residuals vs. linear predictor (Richness explained by NAP)
plot(y=RIKZ_poisson$residuals, x= RIKZ_poisson$linear.predictors, xlab="Linear predictor", ylab="Deviance residuals")

# Compare new model with original RIKZ_poisson model using deviance and AIC
# Original
RIKZ_poisson1 <- glm(Richness~NAP,data=RIKZ, family=poisson)
RIKZ_poisson1$deviance
RIKZ_poisson1$aic
# New
RIKZ_poisson2 <- glm(Richness~NAP+factor(week)+factor(exposure),data=RIKZ, family=poisson)
RIKZ_poisson2$deviance
RIKZ_poisson2$aic
# Model 2 has smaller deviance and a smaller AIC than model 1, which suggests that model 2 (RIKZ_poisson2) fits the data better.

# Model coefficients and 95% CI 
# Original
Model1Coef <- RIKZ_poisson1$coefficients
print(Model1Coef)
confint(RIKZ_poisson1, level=0.95)
# New
Model2Coef <- RIKZ_poisson2$coefficients
print(Model2Coef)
confint(RIKZ_poisson2, level=0.95)

#Calculate BIC for exposure term
BICexposure10 <- (-2.240)^2-log(45)
print(BICexposure10)
BICexposure11 <- (-1.958)^2-log(45)
print(BICexposure11)




