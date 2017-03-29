# Amanda Hart
# BioStatsII Lab 7

#### Question 1
Data <- read.csv("Laengelmavesi2.csv")
PikeData <- Data[Data$species=="Pike","length"]

# Define function to calculate coefficient of variation
CVcalc <- function(data) {
  sd(data)/mean(data)
}

# Estimate CV
CVEstimate <- CVcalc(PikeData)

# Jackknife estimate of CV
library(bootstrap)
CVJackknife <- jackknife(PikeData,CVcalc)

#???????? correct estimate, estimate sampling error, plot histogram



#### Question 2
HakeData <- read.csv("hake.csv")
HakeAbund <- HakeData[,3]

BootstrapAbund <- function(data, NumSample){
  boot_index <- matrix(data, nrow=length(data), ncol=NumSample)
  for(i in 1:NumSample){
    boot_index[,i] <- sample(data, length(data), replace=TRUE)
  }
  # Calculate mean abundance per tow for each bootstrap data set
  MeanAbund <- apply(boot_index, 2, mean)
}

Results <- BootstrapAbund(data=HakeAbund, NumSample=5000)
StdDevBootstrap <- sd(Results)

# Standard error of original sample mean
StdErrOriginal <- sd(HakeAbund)/sqrt(length(HakeAbund))

# Calculate bias
MeanResults <- mean(Results)
MeanOriginal <- mean(HakeAbund)
Bias <- MeanResults - MeanOriginal

# ?????? calculate 95% CI, bonus


#### Question 3
library(ISLR)

set.seed(1)
training <- sample(1:length(Wage),length(Wage)/2)

# linear model for effect of age on wage
wage_lm.2 <- lm(wage~poly(age,2), data=Wage)
wage_lm.3 <- lm(wage~poly(age,3), data=Wage)
wage_lm.4 <- lm(wage~poly(age,4), data=Wage)
wage_lm.5 <- lm(wage~poly(age,5), data=Wage)
wage_lm.6 <- lm(wage~poly(age,6), data=Wage)

# Data= Wage$wage
# predict(wage_lm.2,Wage) [-train]


# Actual <- Wage$wage
# Predict <- predict(wage_lm.i, Wage)
# SquaredError <- (Actual-Predict)[-train]^2
# mean(SquaredError)

