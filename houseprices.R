getwd()
setwd("C:/Users/paart/Documents/PGP_BABI/R_Programming/datasets")
getwd()
houseprices<-read.csv("houseprices-1.csv")
head(houseprices,10)
tail(houseprices,10)
dim(houseprices)
summary(houseprices)
View(houseprices)

table(houseprices$Bedrooms)

housepricessubset<-houseprices[which(houseprices$Price<=50000),]
housepricessubset

cheap_3bhk<-houseprices[which(houseprices$Price<=50000  & houseprices$Bedrooms==3),]
cheap_3bhk

###visualizations
table(houseprices$Price)
barplot(table(houseprices$Price))


