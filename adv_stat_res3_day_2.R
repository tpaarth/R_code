######Adv Stats - Res 3 - Day 2 - 7/9/2019#######
setwd('C:/Users/paart/Documents/PGP_BABI/Handouts/Advanced Statistics/Datasets - Day 1')
datacsv<-read.csv(file = 'hsb2.csv',header = T)
str(datacsv)

##convert prog into categorical data as ANOVA works on categorical data
datacsv$prog <- as.factor(datacsv$prog)
str(datacsv)

### Is there a significant difference in read scores amongst different programs
# ANOVA for H0:There is no sig diff in read score amongst progs

an3 <- aov(read~prog,data = datacsv)
summary(an3)

## P-value = 4.28e-09 < 0.05 - H0 rejected - diff is significant

# Post hoc test - 
# to test if the result of ANOVA is uniform amongst all the combinations
TukeyHSD(an3)

# Combinations 2-1 and 3-2 are in agreement with the results of ANOVA. 
# However, 3-1 combination has a p-value = 0.1557097 > 0.05 
# indicating that there is no significant diff between categories 1 and 3
# Maybe these categories can be merged?

####  Interactivty effect
# To see the Interactivty effect of race and gender on read scores

datacsv$race <- as.factor(datacsv$race)
datacsv$Gender <- as.factor(datacsv$Gender)
str(datacsv)

an4 <- aov(datacsv$read~datacsv$Gender+datacsv$race+datacsv$Gender*datacsv$race)
# or
an4 <- aov(read~Gender+race+Gender*race,data = datacsv)

summary(an4)
# Inference: 
# P-value = 0.000779 < 0.05 i.e. H0 is rejected.
# Therefore Race makes a signigicant difference.

# Exercise: If there is a significant difference of SES on science score. 
# follow with post hoc test

## convert SES to categorical data
datacsv$ses <- as.factor(datacsv$ses)

##ANOVA
an5 <- aov(science~ses,data=datacsv)
summary(an5)

# p-value 0.00027 < 0.05 H0 rejected - there is a sign diff
# post hoc 
TukeyHSD(an5)

# Inference: 
# Combination 3-1 is in agreement with anova.
# Perhaps category 2 can be combined with 1 or 3

# Exercise: If there is a sign diff in socst score with interaction
# of SES and school type

datacsv$schtyp <- as.factor(datacsv$schtyp)
str(datacsv)

# ANOVA
an6 <- aov(socst~ses+schtyp+ses*schtyp,data = datacsv)
summary(an6)

# Inference: 
# For SES, P-value=1.13e-05 <0.05, H0 is rejected
# SES makes an impact on socst score

# Post Hoc
TukeyHSD(an6)

# Exercise: If there is any impact of SES and prog type on writing 
# ability of an individual

an7 <- aov(write~ses+prog+ses*prog, data = datacsv)
summary(an7)

# Inference: 
# P-values for SES and PROF < 0.05 i.e. H0 is rejected
# Both ses and prog have an impact. however ses+prog don't have impact


# Exercise: Deteregent Vs Heat
getwd()
deter_csv <- read.csv('Detergent Vs heat.csv')
str(deter_csv)
summary(deter_csv)
# unpivoting data
install.packages("reshape2")
library('reshape2')
melt(deter_csv)
new_deter<-melt(deter_csv)
new_deter
library(dplyr)

# rename columns
names(new_deter)[2] <- "DetergentType"
names(new_deter)[3] <- "Whiteness"
new_deter
str(new_deter)

# Is the detergent type creating a sign diff in the whiteness.
an8 <- aov(Whiteness~DetergentType,data = new_deter)
summary(an8)

# Inference: 
# p-value =0.264 >0.05 i.e. H0 rejected
# Type of Detergent is making a difference in the whitness.

# Post hoc
TukeyHSD(an8)

# Is the temp of water creating a sign effect on the whitness.
an9 <- aov(Whiteness~WaterTemp,data = new_deter)
summary(an9)

# Inference: 
# p-value = 0.256 > 0.05 i.e. H0 accepted 
# water Temp is not mkaing a difference in whiteness
# Interactivity

an10 <- aov(Whiteness~DetergentType+WaterTemp+DetergentType*WaterTemp,data = new_deter)
summary(an10)

# Post Hoc analysis
TukeyHSD(an10)

############################# LINEAR REGRESSION ###################################

## Make a linear model to predict read on the basis of write

# read - dependent
# write - independent

model <- lm(read~write,data = datacsv)
model
# y_cap = beta0 + beta1*x
# read = 18.16 + 0.64*write

## to find the significance of the model
anova(model)

# Inference: p-value = 2.2e-16 (< 0.05 ) indicates the model is sign 
# and write can be used as an independent variable to predict
# the dependent variable - read

# To check if the error term is normally distributed
shapiro.test(model$residuals)

# Inference: p-value = 0.3 > 0.05 
# i.e. H0 accepted - error term is normally distributed

# summary of the model 
summary(model)
# Inference: The value R-squared = 0.3561 which means the variablity 
# explained in the dependent variable - read - is only 35%
# because of write and you have enough reason to add an 
# independent variable.

# scatter plot between write and read

plot(datacsv$write,datacsv$read)

# let's add the best fit line from the model
abline(model)

# to check if there is a linear relationship between independent 
# and dependent variables, plot a graph between the predicted values
# from the model and the residual values

plot(model$fitted,model$residuals)
abline(h=0) # add a zero line for clarity

# Inference: In case of linearity, the data points are randomly 
# distributed above and below the h=0 line
# 
# In case the points are funneling around the line, then there is 
# no linerality between independent and dependent variables


# Exercise - dependent - science independent - socst
model1 <- lm(science~socst,data = datacsv)
model1

# y_cap = beta0 + beta1*x
# socst = 29.3716 + 0.4289*science

#significance of the model

anova(model1)
# p-value = 3.958e-12 < 0.05 
# i.e H0 rejected - model is significant


#check if error is normal
shapiro.test(model1$residuals)
# p-value = 0.789 > 0.05 i.e. H0 accepted -  error is normal

summary(model1)
# R-squared:  0.2163 i.e. variablity in science is 21% i.e. we can a
# add new independent variable.
