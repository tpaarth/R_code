# USGDP.csv dataset
setwd('C:/Users/paart/Documents/PGP_BABI/Handouts/Time Series Forecasting/datasets')
gdp <- read.csv('USGDP.csv')
str(gdp)
gdp

# Converting to ts
gdpTs <- ts(gdp[-c(1)]
               ,start = c(1929)
               ,end = c(1992)
               ,frequency = 1)

plot(gdpTs)
# appears to be irregular. probably ARIMA?

# testing if ts is stationary
kpss.test(diff(log(gdpTs)))
# p-value = 0.1 i.e. stationary using log-dif

# d = 1
# p and q
acf(diff(log(gdpTs)))
# q = 2-1 = 1
pacf(diff(log(gdpTs)))
# p = 1

fit <- arima(log(gdpTs)
             ,c(1,1,1)
             ,seasonal = list(order = c(1,1,1),period = 1))
accuracy(fit)
# MAPE - 0.4773071

pred <- predict(fit, n.ahead = 4*4) 
predf <- 2.718 ^ pred$pred
predf
accuracy(fit)

##### running holt trend model ######
modeltrend1 <- HoltWinters(gdpTs
                          ,gamma = FALSE)
modeltrend1
plot(modeltrend1)
# The plot suggests a good fit between the actual and the forecasted airmiles
# Forecasting for 10 period
forecasttrend1 <- forecast(modeltrend1,10)
forecasttrend1

plot(forecasttrend1)

# Checking the assumptions
shapiro.test(forecasttrend1$residuals)
# p-value = 0.4196 > 0.05 i.e. normal distribution
Box.test(forecasttrend1$residuals,type = "Ljung-Box")
# p-value = 0.6193 > 0.05 i.e. no auto-correlation
modeltrend # gives us the values of coefficients
