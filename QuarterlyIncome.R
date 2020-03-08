# QuarterlyIncome.csv dataset
setwd('C:/Users/paart/Documents/PGP_BABI/Handouts/Time Series Forecasting/datasets')
income <- read.csv('QuarterlyIncome.csv')
str(income)
income

# Converting to ts
incomeTs <- ts(income[-c(1,2)]
              ,start = c(2000,4)
              ,end = c(2014,1)
              ,frequency = 4)

plot(incomeTs)
# appears to be irregular. probably ARIMA?

# testing if ts is stationary
kpss.test(diff(log(incomeTs)))
# p-value = 0.1 i.e. stationary using log-dif

# d = 1
# p and q
acf(diff(log(incomeTs)))
# q = 1-1 = 0
pacf(diff(log(incomeTs)))
# p = 0

fit <- arima(log(incomeTs)
             ,c(0,1,0)
             ,seasonal = list(order = c(0,1,0),period = 4))
accuracy(fit)
# MAPE - 1.714285

pred <- predict(fit, n.ahead = 4*4) 
predf <- 2.718 ^ pred$pred
predf
accuracy(fit)


modelseasonal1 <- HoltWinters(incomeTs)
plot(incomeTs)
# The plot suggests a good fit between the actual and the forecasted airpassengers
# Forecasting for 10 period
forecastseasonal1 <- forecast(modelseasonal1,10)
plot(forecastseasonal1)

# checking the assumptions
shapiro.test(forecastseasonal1$residuals)
Box.test(forecastseasonal1$residuals,type = "Ljung-Box")
accuracy(forecastseasonal1)



