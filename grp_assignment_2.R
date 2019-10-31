# Problem 2 - LeslieSalt.csv
setwd('C:/Users/paart/Documents/PGP_BABI/Handouts/Advanced Statistics/grp assignment')
library('psych')

# Reading the file 
lds = read.csv('Dataset_LeslieSalt.csv',header = T)
str(lds)

# Treatement of data
lds$County = as.factor(lds$County)
lds$Flood = as.factor(lds$Flood)

# Dep var - Price
# Build an eq that explains Price


# Is the relationship linear?
names(lds)
str(lds)

model1 = lm(Price~County+Size+Elevation+Sewer+Date+Flood+Distance,data = lds)
summary(model1)
anova(model1)

attach(lds)
newdata = data.frame(County,Size,Elevation,Sewer,Date,Flood,Distance)
pdt = predict(model1)
actual=lds$Price


plot(actual,col="Red",xlab="Actual Price")
lines(actual,col="Red",xlab = "Actual") 
plot(pdt,col="Green")
lines(pdt,col="Green")
lines(actual,col="Red",xlab = "Actual") 

# Corr
cor(subset(lds,sselect = -c(County)))
