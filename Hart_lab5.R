# Amanda Hart
# BioStatsII Lab 5

#### Question 1

wtlen <- function(Lengths, a, b){
  for(i in 1:length(a)){
  Weights <- a[i]*(Lengths^b[i])
  }
  return(Weights)
}

LengthData <- c(100,200,300)
a_Data <- c(0.0454, 0.0039)
b_Data <- c(3.05, 2.90)

wtlen(LengthData, a=a_Data, b=b_Data)
# Still can't get weights to be calculated twice



# May want this to be a data table that is refered to

#Question 1 example in class
wtlen <- function(lengths,a,b){
  weights <- a*lengths^b
  return(weights)
}

length_data <- c(100,200,300)
mola <- wtlen(length_data, 0.0454, 3.05)
print(mola)

oar <- wtlen(length_data, 0.0039, 2.90)
print(oar)






#### Question 2
cor_vec <- function(X,Y,...){
  MeanX <- mean(X)
  NormX <- X-MeanX
  
  MeanY <- mean(Y)
  NormY <- Y-MeanY
  
  plot(NormX, NormY,...)
  Cor_val <- cor(NormX, NormY)
  
  return (Cor_val)
}

xx <- seq(from=1, to=100, by=1)
yy <- 0.2 + xx*0.5 + rnorm(n=length(xx), mean=0, sd=5)

cor_vec(xx,yy,cex=2,col="blue")


#### Question 3
# Note that FileName must be in format: "file.csv" 

plotstuff <- function (FileName, ColNum1=1, ColNum2=2){
  Data <- read.csv(FileName)
  x_vals <- Data[,ColNum1]
  y_vals <- Data [,ColNum2]
  plot(x_vals,y_vals)
}

plotstuff("Laengelmavesi.csv")
plotstuff("Laengelmavesi.csv", ColNum1=2, ColNum2=3)
plotstuff("Laengelmavesi.csv", ColNum1="length", ColNum2="height")


#### Question 4
# This function projects forward where abundance (N) is decreasing 9% each year 
# It returns the year where abundance drops below the value provided (argument N=)
devil.loop <- function(N, year=2017){
  while(N>500){
    #N<-N*0.91   # For constant mortality
    N <- rbinom(n=1,size=N, prob=0.91) # Tells us how many survived (number of successes)
    year<-year+1
  }
  return(year)
}

Results <- NULL
for(i in 1:1000){
  Results[i] <- devil.loop(N=5000, year=2017)
}
print(Results)

hist(Results)



#### Question 5
# Vectors passed to this function must be of equal length
# CalcRegress function requires that data be subdivided by species prior to passing to function
VectorA <- c(1,2,3,4,5,6,7,8,9,10)
VectorB <- c(1,2,3,4,5,6,7,8,9,10)

CalcRegress <- function(Vector1, Vector2, species, logcalc=FALSE){
  
plot(Vector1, Vector2, main="species")

if(length(Vector1) > 1 & length(Vector2) > 1) {
  if(logcalc==TRUE){
    Results <- lm(log(Vector1)~log(Vector2))
  }else{
    Results <- lm(Vector1~Vector2)
  }
} else{
  print("Insufficient data")
}

abline(Results)

return(Results)
}

# is the lm() correct?
# is the log correct?


CalcRegress(VectorA, VectorB, species=Fish, logcalc=FALSE)
CalcRegress(VectorA, VectorB, species=Shark, logcalc=TRUE)


# This portion of code subsets the data
data <- read.csv("Laengelmavesi2.csv", header=TRUE)


unique(data$species)
