# Problem 3 - All Greens Franchise.csv
setwd('C:/Users/paart/Documents/PGP_BABI/Handouts/Advanced Statistics/grp assignment')

# Libraries
library('psych')
library('corrplot')
install.packages('corrplot')

# Read the file
agf = read.csv('Dataset_All Greens Franchise.csv')
str(agf)

# Correlation Matrix
cor_agf = cor(agf)
cor_agf

corrplot(cor_agf,method = 'number')

# scatter plot to chk reln beteen x1 & x2
plot(agf$X1,agf$X2)

# lm - all
lm_all = lm(X1~.,data = agf)
summary(lm_all)

pdt = predict(lm_all)
act = agf$X1

plot(act,col="Red",xlab = "actual")
lines(act,col="Red",xlab="actual")
plot(lm_all$fitted.values,col="Green",xlab = "predicted")
lines(lm_all$fitted.values,col="Green",xlab="predicted")
lines(act,col="Red",xlab="actual")

# lm - without X2
lm_minusX2 = lm(X1~.-X2,data = agf)
summary(lm_minusX2)

# lm - without X2
lm_minusX2 = lm(X1~X2,data = agf)
summary(lm_minusX2)

# lm - x1 with x3
lm_x3 = lm(X1~X3,data = agf)
summary(lm_x3)

# lm - x1 with x4
lm_x4 = lm(X1~X4,data = agf)
summary(lm_x4)

# lm - x1 with x5
lm_x5 = lm(X1~X5,data = agf)
summary(lm_x5)

