# Amanda Hart
# BioStatsII Lab 4

#### Question1

# Create InitWeight vector with random data, same number rows as PlantGrowth
print(PlantGrowth)
Length <- nrow(PlantGrowth)
InitWeight <- rnorm(Length,mean=5)

# create dataframe with PlantGrowth and InitWeight
newdata <- data.frame(PlantGrowth,InitWeight)
print(newdata)

# Sort data by order (low to high)
WeightOrder <- order(InitWeight)
# Organize dataframe using this order
SortedNewdata <- newdata[WeightOrder,]
print(SortedNewdata)

# Save as .csv file
write.csv(SortedNewdata, file="BioStatsLab4_SortedNewdata.csv", row.names=FALSE)



#### Question 2
# Compare speed to calculate std dev of matrix x
x <- matrix(rnorm(1000000), 100000, 10)
# for()loop
system.time(
  {StdDev1 <- NULL;
    for(i in 1:nrow(x)) {
      StdDev1[i] <- sd(x[i,])
    }}
)

# apply() loop
system.time({StdDev2 <- apply(x, 1, FUN=sd)})

#  Vector based calculations
system.time(
  {RowMeans <- rowMeans(x);
  Variance <- (rowSums((x-RowMeans)^2))/9;
  StdDev3 <- Variance ^0.5
  }  
)


# Use for() to add grid lines to a plot
plot(NULL, xlim=c(0,10), ylim=c(0,10), xaxs="i", yaxs="i", xlab="", ylab="")
for(i in 1:10){
  abline(h=i)
  abline(v=i)
}


#### Question 3
# Use if statement to color lines (%% gives the remainder after dividing)
plot(NULL, xlim=c(0,10), ylim=c(0,10), xaxs="i", yaxs="i", xlab="", ylab="")
for(i in 1:9){
  abline(v=i)
  
  if(i%%2==0){
    abline(h=i, col="Red")
  }
   else{
      abline(h=i, col="Blue")
   }
}

# Part 2 of Question 3
# These calculations assume data is formatted as a .csv with three columns: 
# a species column (where species are labeled using numbers in sequential order), length, and age
setwd("/Users/ahart2/Research/BioStatsIILab")
AgeLengthData <- read.csv("agelength.csv")

# Calculates number of animals in data set
numAnimals <- length(unique(AgeLengthData$species))
print(numAnimals)

Slope <- vector(length=numAnimals)

for(i in 1:numAnimals){
  # give rows corresponding to species
  Animal <- which(AgeLengthData$species==i)
  #print(Animal)
  
  if(length(Animal)>1){
    # Produce a linear model lm() for rows associated with species i and saved in Animal
    Age_Length_Regress <- lm(length~age, AgeLengthData[Animal,]) # this is for all data together
    #plot(lm(length~age, AgeLengthData))
    
    # extract slope
    Slope[i] <- Age_Length_Regress$coefficients["age"] 
  }
  else{
    Slope[i] <-NA
    print(paste("Insufficient data for species", i, sep=" "))
  }
}

print(Slope)
Slope <- na.omit(Slope)
print(Slope)

# Plot slopes using histogram
hist(Slope, xaxs="i", yaxs="i",)
# Include total number of species included
legend(x=1.4, y=5, legend=(paste("n =",length(Slope),sep=" ")), bty="n")

                           






