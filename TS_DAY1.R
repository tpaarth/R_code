# In-class exercise

At<-c(310,365,395,415,450,465,480,495,515,530)
Ft1<-c(315,375,390,405,435,480,495,520,510,485)
Ft2<-c(370,305,455,390,535,345,390,515,535,525)

passengers <- data.frame(At,Ft1,Ft2)

passengers$MFE1 <- passengers$At - passengers$Ft1
passengers$MFE2 <- passengers$At - passengers$Ft2

passengers$MAD1 <- abs(passengers$At - passengers$Ft1)
passengers$MAD2 <- abs(passengers$At - passengers$Ft2)

passengers$MSE1 <- (passengers$MFE1)*(passengers$MFE1)
passengers$MSE2 <- (passengers$MFE2)*(passengers$MFE2)

passengers$MAPE1 <- (passengers$MAD1/passengers$At)*100
passengers$MAPE2 <- (passengers$MAD2/passengers$At)*100

# MFE 
MFE1 = sum(passengers$MFE1)/(length(passengers)-1)
MFE2 = sum(passengers$MFE2)/(length(passengers)-1)
MFE1
MFE2

# MAD
MAD1 = sum(passengers$MAD1)/(length(passengers)-1)
MAD2 = sum(passengers$MAD2)/(length(passengers)-1)

MAD1
MAD2
# MSE
MSE1 = sum(passengers$MSE1)/(length(passengers)-1)
MSE2 = sum(passengers$MSE2)/(length(passengers)-1)
MSE1
MSE2

# MAPE
MAPE1 = sum(passengers$MAPE1)
MAPE2 = sum(passengers$MAPE2)

MAPE1
MAPE2

totals <- c("CAL","CUL","ATION",MFE1,MFE2,MAD1,MAD2,MSE1,MSE2,MAPE1,MAPE2)


newdf <- rbind(passengers, totals)
newdf 

# ######## Install and load necessary packages ########
install.packages('tseries')
install.packages('forecast')

library('tseries')
library('forecast')

# Using AirPassengers in-built dataset for conceptual learning
data("AirPassengers")
AirPassengers

class(AirPassengers)

# To identify a time series in R, the class of the dataset should be "ts"

# Let's plot the "ts" object, in order to understand the variation
plot(AirPassengers)

# Inference - 
# 1. Overall, there is an increase in the number of passengers with respect to time
# 2. There are peaks and troughs in number of passengers in ever year.
# 3. With time, the fluctuation about the mean is not constant. Therefore, the 
#    timeseries exhibits a non-constant variance. This leads to a series which is 
#    non-stationary.

# To draw a trend line on this plot
abline(reg = lm(AirPassengers~time(AirPassengers)))
# Inference - this line shows the trend


# ############################
# Components of a time series
# ############################

# There are four components in a time series :

# 1. Trend - 
# Long term overall component - either upward or downward. 
# To capture the trend component, the dataset should be collected over atleast 5 years.
# AirPassengers exhibits an upward trend

# 2. Cyclical - 
# In AirPassengers exhibits a the cyclical component because of the ups and downs in the data
# If the difference between 2 consequtive peaks/troughs is greater than one year, then
# the data set is said to exhibit a cyclical component.

# 3. Seasonal - 
# If the difference between 2 consequtive peaks/troughs is maximum one year, then
# the data set is said to exhibit seasonal component.

# 4. Irregular - 
# This component does not have any periodicity and is because of unforseen events

# The AirPassengers dataset shows Trend, Seasonal and Irregular components.

# Stationary Time series - 
# You can only forecast a stationary time series.
# To identify whether a given time series is stationary, 
# the mean and variance should remain constant over time. 
# (There should be no trend component i.e. steady best fit line.
# Stationary time series is also known as white noise. 
# If the series is non-stationary, it needs to be made stationary before forecasting

# nhtemp dataset
data("nhtemp")
plot(nhtemp)
abline(lm(nhtemp~time(nhtemp)))
# Inference - cyclical+irregular

# airmiles dataset
data("airmiles")
plot(airmiles)
abline(lm(airmiles~time(airmiles)))
# Inference - non-cyclic, trend, irregular, non-seasonal


# ############################################
# Test to check whether series is stationary
# ############################################
#  kpss.test(<ts>)
# H0: Series is stationary
# H1: Series is non-stationary

kpss.test(AirPassengers)
# Inference:
# p-value = 0.01 < 0.05 i.e. H0 rejected i.e. non-stationary series

kpss.test(nhtemp)
# p-value = 0.01 < 0.05 i.e. H0 rejected i.e. non-stationary series

kpss.test(airmiles)
# p-value = 0.01 < 0.05 i.e. H0 rejected i.e. non-stationary series


# In order to make the AirPassengers dataset stationary, 
# both mean and variance have to be made constant somehow. 
# This can be achieved by using trasnformations. 

# To make the mean constant, we can use the following tranformations - 
# log
# sin
# cos
# tan
# sinh 
# cosh 
# tanh 
# exponential

# To make the mean constant, we do "differencing" i.e.
# we create a new series by taking the difference of consecutive terms in the original series.
# Final series after trasnformation and differencing should be stationary.

# Transforming the AirPassengers series
plot(log(AirPassengers))
# Inference - 
# variance appears constant

# Differencing 
plot(diff(log(AirPassengers)))
# Test if the new series stationary
kpss.test(diff(log(AirPassengers)))
# Inference - p-value = 0.1 > 0.05 i.e. H0 accpeted - new transformed series is stationary

# Transforming nhtemp
plot(log(nhtemp))
# Differencing nhtemp
plot(diff(log(nhtemp)))
# Stationary test
kpss.test(diff(log(nhtemp)))


# Transforming airmiles
plot(log(airmiles))
# Differencing airmiles
plot(diff(log(airmiles)))
# Stationary test
kpss.test(diff(log(airmiles)))

# Transforming airmiles
plot(sin(airmiles))
# Differencing airmiles
plot(diff(sin(airmiles)))
# Stationary test
kpss.test(diff(sin(airmiles)))

# ###############################
# To create an object of class ts
# ###############################

setwd('C:/Users/paart/Documents/PGP_BABI/Handouts/Time Series Forecasting/datasets')
df <- read.csv('AirPassengers.csv',header = T)

class(df)
AirPassengersTs <- ts(df[-1], ### we need to exclude the TravelDate colm as we are specifying the date already
                    start = c(1949,1),
                    end = c(1960,12),
                    frequency = 12)

class(AirPassengersTs)
str(AirPassengersTs)

plot(AirPassengersTs)

View(AirPassengersTs)
