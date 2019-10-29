getwd()

##Q1
input.data<-read.csv("Bank500.csv")

##Q2
#a.
nrow(input.data)
#b.
ncol(input.data)
#c.

head(input.data)
#d.
tail(input.data)
#e.
head(input.data,20)

##Q3
str(input.data)

##Q4
summary(input.data)

##Q5
class(input.data$contact)
factor.contact = factor(input.data$contact)
levels(factor.contact)

##Q6
input.data$age
str(input.data$age)
##Q7
names(input.data)

##Q8
age_below_40=subset(input.data,input.data$age<=40)
##Q9
class(age_below_40)
##Q10
nrow(age_below_40)

age_below_40
##Q11
head(age_below_40[order(age_below_40$age,decreasing=TRUE),])



##Q13
hist(input.data$age)

##Q14
ggplot(input.data,aes(x=input.data$job,y=input.data$marital,))+geom_col(stat = "identity")
table(input.data$marital)
barplot(table(input.data$marital))        

##Q15
plot(input.data$duration,input.data$balance,xlab="duration",ylab="balance")
ggplot(input.data, aes(x=duration,y=balance))+geom_point()

##Q16
boxplot(input.data$balance,xlab="balance",horizontal = TRUE,main="Boxplot balance")

##Q17
boxplot(input.data$balance~input.data$job,horizontal=TRUE,xlab="Balance",ylab="Job")
ggplot(input.data, aes(x=job, y=balance,))+geom_boxplot()

##Q19
ggplot(input.data,aes(x=education,y=balance))+geom_point()

##Q20
ggplot(input.data,aes(x=age,y=balance))+geom_point()

##Q21
ggplot(input.data,aes(x=age,y=balance))+geom_point(aes(colour=factor(housing=="yes")))

##Q22
means = tapply(input.data$balance, input.data$numMonth, mean)
barplot(means, xlab = "Month", ylab = "Average Balance", main = "Monthly Average Balance")

