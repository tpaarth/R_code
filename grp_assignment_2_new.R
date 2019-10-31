# Problem 2 - LeslieSalt.csv
setwd('C:/Users/paart/Documents/PGP_BABI/Handouts/Advanced Statistics/grp assignment')
library('psych')

# Reading the file 
lds = read.csv('Dataset_LeslieSalt.csv',header = T)
str(lds)

# Treatement of data
lds$County = as.factor(lds$County)
lds$Flood = as.factor(lds$Flood)
str(lds)
# Dep var - Price
# Build an eq that explains Price

# Identifying factors that are impacting price - 

## Is County impacting Price?
# 1. Test of normality
shapiro.test(lds$Price)
# Inference - 
# p-value = 0.01025 < 0.05 i.e. H0 is rejected
# Data is not normal

# 2. Test of homogenity of variance
fligner.test(lds$Price~lds$County)
# Inference - 
# p-value = 0.01071 < 0.05 i.e. H0 is rejected
# var.equal = False

# 3. Mann-Whitney test
wilcox.test(lds$Price~lds$County)
# Inference - 
# p-value = 0.598 > 0.05 i.e. H0 is accepted
# There is no significant diff impact of County on the Price

## Is Flood impacting Price?

# 3. Mann-Whitney test
wilcox.test(lds$Price~lds$Flood)
# Inference - 
# p-value = 0.06008 > 0.05 i.e. H0 is accepted
# There is no significant diff impact of Flood on the Price

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

plot(pdt,col="Green")
lines(pdt,col="Green")
plot(actual,col="Red",xlab="Actual Price")
lines(actual,col="Red",xlab = "Actual") 
lines(pdt,col="Green")

plot(model1$fitted,model1$residuals)
abline(h=0)
