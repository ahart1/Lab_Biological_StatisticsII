# Amanda Hart
# BioStatsII Lab 6

#### Question 1
layout(mat=matrix(c(1,1,1,2,3,4),nrow=3, ncol=2, byrow=FALSE), widths=c(2,1))
layout.show(4)

#### Question 2
library(maps)
#Long <- 71.5 to 69W
#Lat <- 41.1 to 44N
# New Bedofrd, Boston, Portland us.cities data frame points() plot()
plot(seq(71.5,69,-0.5), seq(41.1,44,0.5), xlim=c(-71.5,-69),
      ylim=c(41.1,44),
     xlab=expression(paste("Longitude (",degree,"W)")),
     ylab=expression(paste("Latitude (",degree,"N)")))
map("state",c("Massachusetts","Maine","New Hampshire"),
    fill=TRUE,
    col="darkgreen",
    xlim=c(-71.5,-69),
    ylim=c(41.1,44),
    wrap=TRUE,
    add=TRUE
    )

# add points in New Bedford, Boston, Portland
# change projection

# us.cities is part of maps package but I can't reference it even when maps is installed
# type city and state name must also be included
# vector lat/long, use points() function to overlay



#### Question 3
# Shade lower 50% of probability density for standard normal distribution
X_vals <- c(-5,0,seq(0,-5, by=-0.01))
Y_vals <- c(0,0,dnorm(seq(0,-5,by=-0.01),0,1))
normal<-dnorm(seq(-5,5,0.01),0,1)
plot(seq(-5,5,0.01),normal,type="l",xaxs="i", yaxs="i")
polygon(X_vals, Y_vals,col="grey")

# High resolution coastline data
load("gomLLhigh.Rdata")
poly2 <- which(gomLLhigh$PID==2)
plot(gomLLhigh$X[poly2],gomLLhigh$Y[poly2], type="l", 
     xlab=expression (paste("Longitude (",degree,"W)", sep="")),
     ylab=expression(paste("Latitude (", degree, "N)",sep="")),
     main="Martha's Vineyard"
)
polygon(gomLLhigh$X[poly2],gomLLhigh$Y[poly2], col="dark green")



#### Question 4
setwd("/Users/arhart/Research/BioStatsIILab")
Survey <- read.csv("neus_data.csv",header=TRUE)
Names <- read.csv("neus_svspp.csv", header=TRUE)
SurveyData <- merge(Survey, Names, by.x="SVSPP", by.y="SVSPP")
# Double check that species named the correct way/merge worked the way we thought it should
# end should be a turtle?

# Set up layout
layout(mat=matrix(c(1,1,1,2,3,4),nrow=3, ncol=2, byrow=FALSE), widths=c(2,1))
layout.show(4)

# Set up map projection
library(maps)
#Long <- 71.5 to 69W
#Lat <- 41.1 to 44N
# New Bedofrd, Boston, Portland us.cities data frame points() plot()
plot(seq(71.5,69,-0.5), seq(41.1,44,0.5), xlim=c(-71.5,-69),
     ylim=c(41.1,44),
     xlab=expression(paste("Longitude (",degree,"W)")),
     ylab=expression(paste("Latitude (",degree,"N)")))
map("state",c("Massachusetts","Maine","New Hampshire"),
    fill=TRUE,
    col="darkgreen",
    xlim=c(-71.5,-69),
    ylim=c(41.1,44),
    wrap=TRUE,
    add=TRUE
)

# Bathymetry data 
library(RNetCDF)

bath_file <- "crm.nc"
my.nc <- open.nc(bath_file) #open ncdf file
ncdat <- read.nc(my.nc) #read netcdf dataset into a list object
close.nc(my.nc) #close the ncdf file
  
lonvec <- ncdat$lon
latvec <- ncdat$lat
d2 <- ncdat$Band1

#create a color scheme
dmin=min(d2)
dmax=max(d2)
NwaterColors = round(abs(dmin))
NlandColors = round(abs(dmax))
#taking first 1/3 of topo.colors of length NwaterColors for water
waterColorVec = topo.colors(NwaterColors*3)[1:NwaterColors]
#taking second 2/3 of topo.colors of length NlandColors for land
landColorVec = topo.colors(NlandColors*3/2)[round(NlandColors/2+1):round(1.5*NlandColors)]
#combine
allColorVec = c(waterColorVec,landColorVec)

#make an image plot
image(lonvec,latvec,as.matrix(d2), col=allColorVec,useRaster=TRUE, xlab="", ylab="")
mtext(side=1,expression(paste("Longitude (",degree,"W)",sep="")),line=2.5,cex=0.8)
mtext(side=2,expression(paste("Latitude (",degree,"N)",sep="")),line=2.5,cex=0.8)
mtext(side=3, "Silver hake catch rates, spring 2015", line=1)




###############################################
SilverHakeData <- SurveyData[SurveyData$COMNAME=="SILVER HAKE",]
SpringHakeData <- SilverHakeData[SilverHakeData$SEASON=="SPRING",]
Spring2015Hake <- SpringHakeData[SpringHakeData$YEAR=="2015",]

HakeBio <- Spring2015Hake[, "BIOMASS"]
HakeLat <- Spring2015Hake[, "LAT"]
HakeLong <- Spring2015Hake[, "LON"]
points(HakeLong, HakeLat, cex=log(HakeBio), pch=19, col=alpha("orange",alpha=0.1))

###????????????? lecture 2, change size so proportional


legend(legend=c("5 kg","10 kg","50 kg"), x=-71.5, y=44.0, pch=19, col="orange", bg="transparent", pt.cex=2)

#?????????????? maybe this would work for scaling the legend points, i don't know it won't plot
points(c(5,10,50),pch=19,cex=log(c(5,10,50)), col="orange")


############ Histograms# ??????/ what is catch rate, only abundance and biomass
# Histogram of catch rates
hist(Spring2015Hake$BIOMASS, freq=TRUE, main="Histogram of catch rates", xlab="Catch rate (kg per hr)", ylab="Frequency", col="light grey", xaxs="i", yaxs="i") 
#box(which="plot")

# I can't figure out how to fix axis
hist(Spring2015Hake$DEPTH,main="Depth of positive tows", xlab="Depth (m)", ylab="Frequency", col="light grey")


# This isn't right, plot proportion, I need to take frequencies from histogram and turn into a portion of the whole
Prop <- Spring2015Hake$LENGTH/length(Spring2015Hake$LENGTH)
plot(Spring2015Hake$LENGTH, Prop, main="Length frequency", xlab="Length (cm)", ylab="Proportion")

Plot3 <- hist(Spring2015Hake$LENGTH, freq=FALSE, plot=FALSE)
plot(Plot3, type="l")
# These next two lines look ok but not really, is density the correct function?
plot(density(Spring2015Hake$LENGTH, na.rm=TRUE))




