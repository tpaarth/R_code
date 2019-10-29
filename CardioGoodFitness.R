setwd("C:/Users/paart/Documents/PGP_BABI/R_Programming/datasets")
getwd()

mydata<-read.csv("CardioGoodFitness.csv",header=TRUE)
mydata
attach(mydata)
summary(mydata)
by(mydata,INDICES=Product,FUN=summary)
by(mydata,INDICES=Gender,FUN=summary)
hist(Age,col="red")
boxplot(Age)
boxplot(Age,horizontal = TRUE,col="green",main="Boxplot age")
boxplot(Age~Gender,horizontal = TRUE,col=c("red","green"),main="comparative box plot")
boxplot(Gender,horizontal = TRUE,col="red")
boxplot(Gender)
mydata
boxplot(Income~Product,horizontal=TRUE)
