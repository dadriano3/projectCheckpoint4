#libraries
library(tidyverse)
library(lubridate)
library(ggplot2)
library(ggmap)
library(skimr)
library(Hmisc)
library("ggmap")
library(maptools)
library(maps)

rawMetoriteData <- read.csv('Meteorite_Landings.csv',header = TRUE)
head(rawMetoriteData)
#remove all NA values/datasets missing information
rawMetoriteData <-drop_na(rawMetoriteData)
#separation of data set into 3 frames, ID number is shared universally
idNum <- rawMetoriteData['id']
#1) Data the tells physical properties
#2) Location properties
#3) time properties
#this is done to compartmentalize data mutations
#1) Physical properties
nameType <- rawMetoriteData['nametype']
mass <- rawMetoriteData['mass..g.']
class <- rawMetoriteData['recclass']
physProps <- cbind(idNum,nameType,mass,class)
#2) Location Properties, Geo Location discarded as redundant
lat <- rawMetoriteData['reclat']
long <- rawMetoriteData['reclong']
foundType <- rawMetoriteData['fall']
locProps <- cbind(idNum,lat,long,foundType)
#3) Time properties
timeInfo <- rawMetoriteData['year']
#extract just the year from the data
timeInfo <- format(as.Date(timeInfo$year, format="%d/%m/%Y"),"%Y")
year <- as.data.frame(timeInfo)
timeProp <- cbind(idNum,year)
#conversion of data types from "char" to "numeric"
timeProp <- as.data.frame(apply(timeProp, 2, as.numeric))  # Convert all variable types to numeric
sapply(timeProp, class)
#generation of histograms
#meteors by year found with regression
#freqency table as frame for curve fit
funcyear <- as.data.frame(table(timeProp[[2]]))
x <- funcyear[[1]]
y <- funcyear[[2]]
xy <- as.data.frame(cbind(x,y))
hist(timeProp[[2]], main = "Year Meteorite was found", xlim = c(1583,2013), xlab = "Year", ylab = "Number of Meteorites Found", breaks = 200)
lm(y ~ poly(x, 10, raw=TRUE))
plot(x,y, main = "Year Meteorite was found", xlab = "Year", ylab = "Number of Meteorites Found")
fit3 <- lm(y~poly(x,3,raw=TRUE), data=xy)
x_axis <- 1550:2019
newy <-predict(fit3, data.frame(x=x_axis))
lines(x_axis,newy , col='red')
summary(fit3)$adj.r.squared
#generation of plot correlating mass of meteor with frequency
massTable <- as.data.frame(table(physProps[[3]]))
plot(massTable[[1]],massTable[[2]])
x <- massTable[[1]]
y <- massTable[[2]]
xy <- as.data.frame(cbind(x,y))
fit3 <- lm(y~poly(x,10,raw=TRUE), data=xy)
x_axis <- 0:5000
newy <-predict(fit3, data.frame(x=x_axis))
lines(x_axis,newy , col='red')
summary(fit3)$adj.r.squared
#meteor class distribution as bar chart
barplot(table(physProps[[4]]))
#Map Data
#gets map
map("world", fill=TRUE, col="white", bg="lightblue", ylim=c(-60, 90), mar=c(0,0,0,0))
#places find sites on map
points(locProps[[2]],locProps[[3]], col="red", pch=16)
#bar chart for finds vs artifacts
barplot(table(physProps[[2]]))
#pie chart for finds vs fell
pie(table(locProps[[4]]))

#reassembly of overall dataset after mutations for multiple variable analysis
mutatedData <- cbind(physProps,timeProp[2],locProps[4])
x1 <- filter(mutatedData, fall == "Fell")
x2 <- filter(mutatedData, fall == "Found")
y1 <- x1[[5]]
y2 <- x2[[5]]
tabl1 <- table(x1[[5]])
tbl2 <- table(x2[[5]])
plot(tbl2, col = "red")
points(tabl1)