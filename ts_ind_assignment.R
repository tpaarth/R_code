
#  With time, the fluctuation about the mean is not constant. Therefore, the 
#  timeseries exhibits a non-constant variance. This leads to a series which is 
#  non-stationary.
# Importing all the relevant libraries
##install.packages("forecast")
rm(list=ls())
library('forecast')
library('tseries')
library('stats')
library('tidyverse')
#### I. Loading the gas dataset ####
data(gas)
###class(gas) # TS class data
###force(gas)
###View(gas)
###str(gas)

# Creating TS object
gasTS <- ts(gas,
            start = c(1956,1),
            end = c(1995,8),
            frequency = 12)
class(gasTS)
force(gasTS)
View(gasTS)
str(gasTS)
# Observations: 
# 1. gas is a time-series object with 476 observations distributed monthly from 1956 to 1995
# 2. From 1956 to 1969, the yearly gas production appears to be relatively uniform as the graph does not show any obvious trend in the gas production pattern.
# 3. Post 1970 to 1982, a sharp upward/positive trend can be observed in the production of gas units.
# 4. Post 1982, yearly production pattern is relatively uniform. However, fluctuations in the monthly productions patterns are observed.

# initial plot of the TS
plot(gasTS)
seasonplot(gasTS)
monthplot(gasTS)
tsdisplay(gasTS)
abline(reg = lm(gasTS~time(gasTS)))
force(gasTS)

# Taking a subset of the series for analysis
gas.subset <- window(gas,start = 1970)
force(gas.subset)
plot(gas.subset)

## Plot 1: Seasonal plot Year-wise (using ggseasonalplot()) 
ggseasonplot(gas.subset, 
             year.labels=TRUE, 
             year.labels.left=TRUE) + 
  ylab("degree") + 
  ggtitle("Seasonal plot: Gas Production Data")

## Plot 2: Polar Seasonal plot Year-wise (using ggseasonplot()) 
ggseasonplot(gas.subset, 
             polar=TRUE) + 
  ylab("degree") + 
  ggtitle("Polar seasonal plot: Gas Production Data")


## Plot 3: Seasonal plot Month-wise (using monthplot()) 
monthplot(gas.subset)

## Step 1: Decomposition of TS using decompose() 
gas.subset.decompose<-decompose(gas.subset, type = "multiplicative")
gas.subset.decompose
plot(gas.subset.decompose)

## Step 1: Decomposition of TS using stl() - for additive??
gas.subset.decompose1<-stl(gas.subset, s.window='p') # here we assume seasonlity is uniform in each year
gas.subset.decompose1
plot(gas.subset.decompose1)

# By observing the scales of the above graphs,
# it can be interpreted that Trend and Seasonality
# components impact the production of gas units.

gas.subset.decompose2<-stl(gas.subset, s.window=7) # here we assume seasonlity is non-uniform in each year
gas.subset.decompose2
plot(gas.subset.decompose2)

# Seasonality appears to be increasing over time
# Still the trend component is more dominant

# we can take the log values to trasform from multiplicative to additive series
# log(Yt) = log(Tt) + log(St) + log(It)
gas.subset.decompose.log<-stl(log10(gas.subset), s.window='p') 
gas.subset.decompose.log
plot(gas.subset.decompose.log)


# De-seasonalizing the series 
# In additive - subtracting the seasonal component from the actual series
# In multiplicative - convert to log scale, and subtract seasonal component
#                     or add trend and remainder components

gas.subset.des <- 10^(gas.subset.decompose.log$time.series[,2] + gas.subset.decompose.log$time.series[,3])
gas.subset.des


### comparing actual and de-seasonalized time series 

ts.plot(gas.subset.des,
        gas.subset,
        col = c("red","black"),
        main = "Comparision of actual v de-seasonalized gas time series")

legend("topleft", 
       legend = c("De-seasonalized","Actual"), 
       col = c("red","black"), 
       lty = 1,
       cex=.60)

# Observation:
# This comparision graph also indicates that trend component is dominant

## Spliting data into training and test data sets 2/3 - training 1/3 -testing
gas.subset.train <- window(gas.subset, 
                   start=c(1970,1), 
                   end=c(1991, 12), 
                   freq=12) 

force(gas.subset.train)

gas.subset.test <- window(gas.subset, 
                  start=c(1992,1), 
                  freq=12)

force(gas.subset.test)

# plotting train and test datasets
autoplot(gas.subset.train, series="Train") + 
  autolayer(gas.subset.test, series="Test") + 
  ggtitle("Gas Production Traning and Test data") + 
  xlab("Year") + ylab("Sales") + 
  guides(colour=guide_legend(title="Forecast -"))

