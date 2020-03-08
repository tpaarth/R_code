library(tseries)
library(forecast)
# to work on a TimeSeries, the class of the object should be "ts"
data("AirPassengers")
class(AirPassengers)
AirPassengers

# Nature of data - 
# For the AirPassengers dataset, the frequency is 12 (no of repetitions in a year)

# To plot the AirPassengers dataset, to see the kind of component, the ts data set is exhibiting
plot(AirPassengers)
# The TS exhibits a gradual increase in the number of airpassengers with some kind of 
# seasonality. 

# To superimpose a trend line on the plot,
abline(reg = lm(AirPassengers~time(AirPassengers)))
# The trend line indicates overall there is an increase in the number of airpassengers
# with time.

# #####
# Models to be used for forecasting
# #####

# 1. ARIMA - Autoregressive Integrated Moving Average Model
# 2. SES - Simple Exponential Smoothing
# 3. Holt-Winters trend model
# 4. Holt-Winters seasonal model

# 1. ARIMA
# Objective is to forecast the number of airpassengers 
# going to fly in the next 12 months using ARIMA

# Assumptions of ARIMA - 
# a. Series should be stationary
# The current series is non-stationary. It has to be made stationary.
# For a TS to be stationary, the mean and variance should remain constant over time.
# ARIMA can only work on stationary series.

# To make the variance constant, lets use the log transformation
plot(log(AirPassengers))
# The log function has made the variance constant

# To make the mean constant, let's do 1st order differencing 
plot(diff(log(AirPassengers)))

# Statistical test to check for stationality
kpss.test(AirPassengers)
# H0 - series is stationary
# P is low so series is not stationary in it's original form

kpss.test(diff(log(AirPassengers)))
# Inference - p-value = 0.1 > 0.05 i.e. H0 accpeted - transformed series is stationary

# ARIMA is made up of 3 components 
# AR + I + MA
# AR - Autoregression - takes care of forecast based on previous value
# I - Integration
# MA - Moving Avg

# AR - Autoregressive component (giving weightage to previous values)
# This component handles the number of previous periods to be considered for the 
# future forecasts. The AR part is designated by the p-coordinate in the ARIMA model
# And the name of the graph is PACF graph.
# The equation for AR is Yt+1 = B1Yt + B2Yt-2 + ... + BnYt-n
# where n is number of periods to be considered for the forecast. It is autoregression
# because the dependent variable is regressing on itself.

# I - Integration
# The I in ARIMA is the order of integration which is the number of times
# you performed differencing to make the series stationary. This is designnated by 
# the D coordinate. The D for the AirPassengers data set is 1

# MA - Moving Average
# The MA is the components take care of the auto correlation between the errors. 
# This is designated as the Q coordinate in ARIMA model that is derived from the ACF plot
# The equation for MA is
# Et+1 = A1Et + A2Et-1 + ... + AnEt-n
# where E (epsilon) is the amount of errors in previous periods

# The moving average component best models the irregularity in your data.

# The final co-ordinates for ARIMA model are - (P,D,Q)
# For AirPassengers D = 1 (could be diff based on the transformation we use)


# We use the ACF and PACF plot to get the P and Q co-ordinates for the ARIMA model
# P is from the PACF plot 
# PACF - Partial Auto Correlation Function plot
# And the ACF plot gives us the value of Q
# ACF - Auto Correlation Function Plot

# ---------
# ACF Plot
# ---------
# The original series is lagged by one period at a time and 
# ACF(1) = Correlation between original and lagged 1 series
# ACF(2) = Correlation between original and lagged 2 series

# This correlation can be calculated using the ACF function in R followed by plotting 
# of these plots. This plot will then help in deteremining the Q value for the ARIMA model.

# ACF should be calculated on the series that has been made stationary.

diff_station_series <- diff(log(AirPassengers))
acf(diff_station_series)
# From the ACF plot, Q = (number of significant correlations - 1) 
# [significant are the number of vertical lines crossing the significant zone - shown by 
# the dotted line - in either +ve or -ve direction]

# In this case Q = 2-1 = 1
# Therefore the Q coordinate in ARIMA for AirPassengers is 1.

# PACF plot to determine the value of P coordinate
# The PACF function gives the partial corellation between original series 
# and the lagged series if the intervining effect of lag is removed. For example,
# pacf(2) is the correlation between original series and lag(2) series and the influence 
# of lag(1) is removed. Similarly pacf(3) is between original and lag(3) by removing 
# the influence of lag(1) and lag(2). In R, the pacf function calculates these values
# and puts it on a pacf plot to get the value of P.

pacf(diff(log(AirPassengers)))
# The value of P = 2 for the AirPassengers ds 
# [PACF graph starts from PACF 1 and ACF graph starts from ACF 0 so we don't subtract 1 in 
# this case from the number of significant lines]

# The final input co-ordinates for the ARIMA model (p,d,q) <- (2,1,1)

# To build the ARIMA model with (2,1,1)

# For ARIMA model, the transformed series has to be considered or the series which is 
# stationary. In ARIMA code, we won't use the differenced series but instead let ARIMA 
# do the differencing based on (p,d,q)

station_series <- log(AirPassengers)
fit <- arima(log(AirPassengers),c(2,1,1),seasonal = list(order = c(2,1,1),period = 12))
fit

# Using the fit model forecast the number of AirPassengers for the next 12 months
pred <- predict(fit, n.ahead = 12*12) #12*12 - freq = 12 * no_years = 12
# Since we have used the log transformation, we need to use the anti-log to get final values
predf <- 2.718 ^ pred$pred
predf

# The forecast for January 1961 is 450 passengers which is consistent with December 1960 

# Accuracy function for metrics
accuracy(fit)

# practically MAPE of 5% is acceptable 

# Q:Make a model using ARIMA to forecast the US GDP for the next 6 periods and report the 
# accuracy of the model

# Q: Use ARIMA model to forecast the shoe sales for the next 12 periods.

setwd('C:/Users/paart/Documents/PGP_BABI/Handouts/Time Series Forecasting/datasets/fwdtsf')
ShoeSales <- read.csv('Shoe Sales.csv',header = T)
str(ShoeSales)
ShoeSales[-c(1,2)]

# converting dataframe to ts
ShoeSalesTs <- ts(ShoeSales[-c(1,2)]
                  ,start = c(2011,1)
                  ,end = c(2015,12)
                  ,frequency = 12)
class(ShoeSalesTs)
# Plot the TS
plot(ShoeSalesTs)

# importing necessary packages
library('tseries')
library('forecast')

# Test to check whether series is stationary
kpss.test(ShoeSalesTs)

# Making the series stationary
ShoeSalesTs_stat <- diff(log(ShoeSalesTs))
plot(ShoeSalesTs_stat)

# testing the new series stationary
kpss.test(ShoeSalesTs_stat)

# determining (p,d,q)

# d = 1 (as only 1st level differencing)
# q - coordinate
acf(ShoeSalesTs_stat)
# q=0

# p - coorditnate
pacf(ShoeSalesTs_stat)
# q = 1

# build arima
fit <- arima(log(ShoeSalesTs)
             ,c(0,1,0)
             ,seasonal = list(order = c(0,1,0),period = 12))
fit

pred <- predict(fit, n.ahead = 12*6) 
predf <- 2.718 ^ pred$pred
predf
accuracy(fit)

ShoeSales


# ##### diff 2nd level

ShoeSalesTs_stat2 <- diff(diff(log(ShoeSalesTs)))
plot(ShoeSalesTs_stat2)

# testing the new series stationary
kpss.test(ShoeSalesTs_stat2)

# determining (p,d,q)

# d = 1 (as only 1st level differencing)
# q - coordinate
acf(ShoeSalesTs_stat2)
# q=2-1 = 1

# p - coorditnate
pacf(ShoeSalesTs_stat2)
# p = 4

# build arima
fit <- arima(log(ShoeSalesTs)
             ,c(0,2,1)
             ,seasonal = list(order = c(0,2,1),period = 12))
fit

pred <- predict(fit, n.ahead = 12*6) 
predf <- 2.718 ^ pred$pred
predf
accuracy(fit)


##### Parsimony Rule #####
# original MAPE - 2.09
# improved MAPE for (pdq) = (021) MAPE - 1.88


# By selecting data from jan2013 to dec2015 and checking 
ShoeSalesTsN <- ts(ShoeSales[-c(1,2)]
                  ,start = c(2013,1)
                  ,end = c(2015,12)
                  ,frequency = 12)
plot(diff(ShoeSalesTsN))
kpss.test(diff(ShoeSalesTsN))

acf(diff(ShoeSalesTsN))
# q = 0
pacf(diff(ShoeSalesTsN))
# p = 0
fit <- arima(log(ShoeSalesTsN)
             ,c(0,1,0)
             ,seasonal = list(order = c(0,1,0),period = 12))
accuracy(fit)
# MAPE improved further to 1.476352

# By selecting data from jan2014 to dec2015 and checking 
ShoeSalesTsN2 <- ts(ShoeSales[-c(1,2)]
                   ,start = c(2014,1)
                   ,end = c(2015,12)
                   ,frequency = 12)

plot(ShoeSalesTsN2)
plot(diff(ShoeSalesTsN2))
kpss.test(diff(ShoeSalesTsN2))
acf(diff(ShoeSalesTsN2))
# q = 0
pacf(diff(ShoeSalesTsN2))
# p = 0
fit <- arima(log(ShoeSalesTsN2)
             ,c(0,1,0)
             ,seasonal = list(order = c(0,1,0),period = 12))
accuracy(fit)

########################################################
# Simple Exponential Smoothing SES 	- HoltWinters() in r
# Holt's linear trend method 		- HoltWinters() in r 
# Holt-Winters seasonal method 		- HoltWinters() in r 
########################################################
# If the ts graph shows a predominent trend, we have a choice between ARIMA and Holt's linear trend model.
# If the ts graph shows a seasonality without trend, we can go for simple expoenential smoothing(SES)
# If the ts graph shows a seasonality with trend, we have a choice between ARIMA and Holt-Winters seasonal model

# Syntax for Holt-Winters - 
# HoltWinters(x = <object to ts class>
           # ,alpha = <T/F> ---- based on model
           # ,beta  = <T/F> ---- based on model
           # ,gamma = <T/F> ---- based on model)
		   
# alpha,beta,gamma are known as smoothing parameters and like correlation their value is between 0 and 1

# Specify ONLY those parameters that are needed to be set as FALSE

#							alpha	beta	gamma
# SES 					- 	T		F		F		[a=T,b-F,g=F]
# Holts linear Trend 	- 	T		T		F		[a=T,b=T,g=F]
# Holt-Winters 			- 	T		T		T		[a=T,b=T,g=T]
# depending on the model, we specify the value of alpha beta gamma. 

# By default, all parameters have T value. We only need to specify the F parameters explicitly.


data("nhtemp")
plot(nhtemp)
# test for stationary
kpss.test(nhtemp)
# p-value indicates that this series is not stationary. For AMRIMA it's important to make TS stationary
# using SES model, we need to forecast the temperature
modelses <- HoltWinters(nhtemp
                        ,beta  = FALSE
                        ,gamma = FALSE)
plot(modelses) # red line is the forecast plot

# The above plot is the actual vs the forecasted temperature for the historical dataset

# Using the modelses, what is the forecasted temperature for the next 10 period?
# The name of the function for prediction/forecast is forecast()
forecastses <- forecast(modelses,10)
forecastses
# Statistically, a series exhibiting stationary nature is called white noise

# Plot for the forecast
plot(forecastses)

# Assumptions: 
# 1. The residuals should be normally distributed if the model is stable
# To check normality, we can use the Sharpiro's test

# 2. The errors should not be auto-correlated
# To check auto-correlation we can use the Box test

shapiro.test(forecastses$residuals)
# H0: Normal Distribution
# p-value = 0.6238 > 0.05 therefore residuals are normal

Box.test(forecastses$residuals,type = "Ljung-Box")
# H0: There is no Auto-correlation between residuals
# p-value = 0.968 > 0.05 therefore the box test suggests that we don't have enough evidence that
# there is non 0 auto-correlation between residuals

# Therefore SES may not be accurate but the model is stable

##### Holt's trend model ######
data("airmiles")
plot(airmiles)
# The plot indicates a pure trend, so one of the potential model for forecasting could be Holt's 
# trend model

modeltrend <- HoltWinters(airmiles
                          ,gamma = FALSE)
modeltrend
plot(modeltrend)
# The plot suggests a good fit between the actual and the forecasted airmiles
# Forecasting for 10 period
forecasttrend <- forecast(modeltrend,10)
forecasttrend

plot(forecasttrend)

# Checking the assumptions
shapiro.test(forecasttrend$residuals)
# p-value = 0.4196 > 0.05 i.e. normal distribution
Box.test(forecasttrend$residuals,type = "Ljung-Box")
# p-value = 0.6193 > 0.05 i.e. no auto-correlation
modeltrend # gives us the values of coefficients

##### Holt-Winters model - seasonality and trend  ######
data("AirPassengers")
plot(AirPassengers)
# The dataset exhibits both seasonality and trend. 
# We can apply the Holt-Winters seasonal model for forecasting.

modelseasonal <- HoltWinters(AirPassengers)
plot(modelseasonal)
# The plot suggests a good fit between the actual and the forecasted airpassengers
# Forecasting for 10 period
forecastseasonal <- forecast(modelseasonal,10)
plot(forecastseasonal)

# checking the assumptions
shapiro.test(forecastseasonal$residuals)
# p-value = 0.3297 > 0.05 i.e. normal residuals
Box.test(forecastseasonal$residuals,type = "Ljung-Box")
# p-value = 8.288e-07< 0.05 i.e. H0 rejected - there is auto-correlation between residuals

# Since the box test failed, the model is not stable as it's not able handle the 
# auto-correlation betwen residuals. 
# Therefore in this case ARIMA is a better model for forecasting this dataset as MA handles 
# the auto-correlation between residuals.


# To Decompose the Ts into trend part, season part and irregular part
plot(decompose(AirPassengers))
