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

#  vector based calculations


