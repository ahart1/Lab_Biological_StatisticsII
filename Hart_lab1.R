# Amanda Hart
# BioStatsII Lab 1

### Question 1

part1 <- 7+5*(4+3)
print(part1)

part2 <- exp(-5*(0.2+0.15))
print(part2)

part3 <- sqrt(1+2*(3+2))/log((3^2)+2)
print(part3)


### Question 2
# Positive integers from 1 to 99
seq(1,99)

# Odd integers between 1 and 99
seq(1,99, by=2)

# Numbers 1,1,1,2,2,2,3,3,3
vector1 <- c(rep(1,3), rep(2,3), rep(3,3))
print(vector1)

# Numbers -5,-4,-3,-5,-4,-3,-5,-4,-3
vector2 <- rep(c(-5,-4,-3),3)
print(vector2)

# Numbers 1, 1/2, 1/3...1/10
# Need MASS package installed to print vector as list of fractions rather than list of decimals
library(MASS)
vector3 <- 1/(seq(1,10))
print(fractions(vector3))

#Cubes 1,8,27,64,125,216
vector4 <- (seq(1,6))^3
print(vector4)


### Question 3
y <- c(3,2,15,-1,22,1,9,17,5)

# First and last values
y[-c(2:8)]
# OR
y[c(1,length(y))]
# OR
c(head(y, n=1),tail(y, n=1))

# To return the last value of any vector use:
# tail(vectorname, n=1)
tail(y, n=1)
# OR
y[length(y)]

# Values greater than the mean of y
y[y > (mean(y))]

# Positions of values greater than the mean of y
which(y > (mean(y)))

# All values positive?
y > 0

# Any values = mean?
y == mean(y)

# Any values = median?
y == median(y)


### Question 4
library(MASS)

# First 5 rows of hills dataframe
head(hills, n=5)

# Fastest time in hills
max(hills$time)

# hill races with 3 fastest times
TopThree <- sort(hills$time, decreasing=TRUE)[1:3]
hills[ hills$time %in% TopThree,]

# Display record time for Cairngorm
hills[rownames(hills)=="Cairngorm",3]
# OR more generally 
hills[rownames(hills)=="Cairngorm",colnames(hills)=="time"]

# Number of hill races that have a climb greater than the mean
sum(hills$climb > mean(hills$climb))

# Name of hill races with climb greater than the mean 
rownames(hills[hills$climb > mean(hills$climb),])

# Names and times of races at least 10 miles long and clim greater than 4000ft
subset(hills[hills$dist >= 10 & hills$climb > 4000,], select=(colnames(hills)=="time"))

# Index positions of hills with climb greater than 5000ft or record less than 20 min
which(hills$climb > 5000 | hills$time < 20)

# Standard deviation of times for all except the Bens of Jura
sd(hills[-which(rownames(hills)=="Bens of Jura"),colnames(hills)=="time"])

# Range of average speed for the races
range(hills$dist/hills$time)

# Fastest average speed (name and speed)
max(hills$dist/hills$time)
RowMaxSpeed <- which(hills$dist/hills$time==max(hills$dist/hills$time))
rownames(hills[RowMaxSpeed,])

# Mean time for race with names starting with A-K
# A-K
PossibleLET <- LETTERS[1:11]
# First letters in each rownames
FirstLetters <- substr(rownames(hills),1,1)
# List True/False if the values in FirstLetters match a value in PossibleLET
MATCH <- FirstLetters %in% PossibleLET
print(MATCH)
# List index values where MATCH=True (the first letter matches one of the possible letters)
which(MATCH==TRUE)
# List values from the "time" column for those columns 
DATA<-hills[MATCH==TRUE, colnames(hills)=="time" ]
# Calculate the mean time
mean(DATA)
