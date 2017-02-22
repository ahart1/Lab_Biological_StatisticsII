# Amanda Hart
# BioStatsII Lab 2

# Create 2x2 matrix
Amat <- matrix(1:4, nrow=2, ncol=2)
print(Amat)

# Create 2x3 matrix
Bmat <- matrix(5:10, nrow=2, ncol=3)
print(Bmat)

# Combine Amat and Bmat into 2x5 matrix
Cmat <- cbind(Amat, Bmat)
print(Cmat)

# Combine Amat and Bmat into 5X2 matrix, use transpose function t()
Dmat <- t(Cmat)
print(Dmat)

# Create xfactor so 1 is immature, 2 is mature
maturity <- c(1,1,2,1,2,2,2,1,1,1)
xfactor <- factor(maturity, labels=c("immature", "mature"))
print(xfactor)

# Create list data containing Amat, Bmat, xfactor
data <- list(Amat, Bmat, xfactor)
print(data)

# Extract first row of Amat from data list= data[[# of item in list]][item#]
data[[1]][1,]

data[[2]][1,1] <- NA
print(data)


#### Question 2
setwd("/Users/arhart/Research/BioStatsIICoding")
# Read in Laengelmavesi2.csv data file
data <- read.csv("Laengelmavesi2.csv", header=TRUE)
print(data)

# Display number of observations for each fish
summary (data$species)

# Find mean length, weight, height 
Means <- colMeans(data[,2:4], na.rm=TRUE)
print(Means)

# Find range of Perch lengths
PerchLength <- subset(data$length, data$species=="Perch")
range(PerchLength, na.rm=TRUE)

# Mean length of fish with weight greater than 1000g
# May calculate just values of weight greater than 1000: data$weight[data$weight > 1000]
# This produces a list of lengths associated with weights over 1000
Lengths <- data[data$weight > 1000, colnames(data)=="length"]
mean(Lengths, na.rm=TRUE)

# Mean length for each species
#make list of species
# partition length between species
# Mean


#### Question 3
setwd("/Users/arhart/Research/BioStatsIICoding")
# Read in Laengelmavesi2.csv data file
data <- read.csv("Laengelmavesi.csv", header=TRUE)
print(data)

# Steve had a graph like this in class: plot(data)
plot(x=data$length, y=data$weight, xlab="Mean Length(cm)", ylab="Mean Weight (g)", main="Laengelmavesi fish mean lengths and weights", xlim=c(0,50), ylim=c(0,750), xaxs="i", yaxs="i", pch=20, col=1:6 )
legend(x=0,y=800, legend= data$species, pch=20, bty="n", col=1:6)
#get 0 to cross at same place, change size of words