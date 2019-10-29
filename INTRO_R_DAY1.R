getwd()
install.packages("MASS")
library(MASS)
installed.packages(MASS)

input.data=Cars93
View(input.data)

head(input.data)

#structure of the dataset
str(input.data)
summary(input.data)

#setting variables
x<-3
x

y=x+5
y

m="Money"
m

v1=c(1,2,3,3,4,6,9,10)
v1
class(v1)
str(v1)
v2=c("Introduction","To","R")
v2
class(v2)
v3=c(FALSE,TRUE,TRUE,FALSE)
v3
class(v3)

s1=c(1:5)
s1

s2=seq(from=1,to=15,by=2)
s2

s3=seq(from=1,to=15,length.out=5)
s3 ##split 1 to 15 into 4 parts

s4=rep(7,times=5)
s4

v.a=c(1,1,4,5,3,2)
v.b=c(4,1,5,5,1,2)
v.a==v.b
v.a[3]
v.a[1:3]
v.a[c(1,3,5)]
v.a[-3]##all values except the 3rd
v.a+v.b
v.a*v.b
mean(v.a)

##factors##
factor.v.a=factor(v.a)
factor.v.a #distinct values

wday=c("Mon","Fri","Mon","Wed","Wed","Sat")
factor.wday = factor(wday)
factor.wday

##ordered factors##
speed.vector=c("Fast","Fast","Slow","Express","Slow","X-Fast")
speed.factor=factor(speed.vector,ordered=TRUE,levels=c("Slow","Express","Fast","X-Fast"))
speed.factor

a=speed.factor[1]
b=speed.factor[4]
a
b

b>a
b<a

library()
search()
detach(package:MASS)

update.packages("MASS")
data()
dsap<-AirPassengers
dsap
example(subset)
library()
install.packages("rpivotTable")
library(rpivotTable)
