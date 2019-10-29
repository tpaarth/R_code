##SMDM DAY-1 13.07.2019
setwd("C:/Users/paart/Documents/PGP_BABI/R_Programming/datasets")
getwd()

#import libraries
library(lattice)
library(rpivotTable)

#read CardioGoodFitness.csv

myData<-read.csv("CardioGoodFitness.csv",header=TRUE)

names(myData) #returns all cols

mean(myData$Miles)
attach(myData) #to attach the current dataset to the session
mean(Miles)
summary(myData)

##fetch data product wise
by(myData,INDICES=Product,FUN=summary)

freq=table(Miles)

freq ##returns values and their freq
names(freq)[freq==max(freq)]## fetches mode

#histogram
hist(Miles)
hist(Age)

#from the lattice package
histogram(~Miles|Product) #hist miles v product

##standard deviation
sd(Miles)

##box plot - needs max,min,median,q1,q3
summary(Miles)
boxplot(Miles,horizontal = TRUE)
boxplot(Miles~Product,horizontal=TRUE)

##pivottable
rpivotTable(myData)

##categorize income into groups
summary(Income)
myData$incomeGroup=cut(Income,breaks=(c(29000,40000,50000,60000,80000,105000)))
rpivotTable(myData)

##coeff of correlation
cor(Usage,Miles)
cor(Miles,Usage)

install.packages("ggplotgui")
library("ggplotgui")

install.packages("esquisse")
library(esquisse)
