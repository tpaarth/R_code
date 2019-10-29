## ===============================================================================================================
## EXPLORATORY DATA ANALYTICS 
## ===============================================================================================================

## REFERENCES:
##  An Introduction to Data Cleaning with R - Edwin de Jonge and Mark Van Der Loo
##    https://cran.r-project.org/doc/contrib/de_Jonge+van_der_Loo-Introduction_to_data_cleaning_with_R.pdf

library(gapminder)
data = gapminder

## Explore Structure and Summary of Input data
str(data)
summary(data)


## ===============================================================================================================
## EXPLORATORY DATA ANALYTICS - MISSING VALUES TREATMENT
## ===============================================================================================================

##  Options Available:
##    1.  Remove records having missing values
##    2.  Impute values


## For now, we will omit year field for our analysis
data1 = data[,-3]
summary(data1)

## Randomly insert 30 missing values in continent and lifeExp columns
n = nrow(data1)
n

set.seed(10)
for(i in 2:3) {
  data1[sample(1:n, 30, replace = FALSE), i] = NA
}

summary(data1)   ## Note 30 Missing values in continent and lifeExp columns

## Let us examine the rows with missing values - Incomplete Rows
data1[!complete.cases(data1),]    

attach(data1)

##  GUIDELINES:
##  A safe maximum threshold for missing values in a particular column is 5%.
##  If missing data for a column > 5%, we need to consider leaving out that variable

## Build a function to calculate percentage of missing values in Columns and Rows
pMiss = function(x){
  sum(is.na(x))/length(x)*100
}

## Find Percentage of missing values in each column
col_miss = apply(data1,2,pMiss)  ## 2 is for Columns
col_miss

## OBSERVATIONS:
## Except continent and lifeExp columns have 1.76% missing values

## Find Percentage of missing values in each Row
row_miss = apply(data1,1,pMiss)  ## 1 is for Rows
row_miss

## OBSERVATIONS:
## Row 29 has 20% missing variables
data1[29,]

## Identify rows with high missing values
high_miss_rows = data1[row_miss > 20,]
high_miss_rows   ## 2 Rows have more than 20% missing values

## Keep only the rows with less than 30% missing values
low_miss_rows = data1[row_miss < 20,]
low_miss_rows

## Using mice package
library(mice)
md.pattern(data1)

## IMPUTING MISSING VALUES USING mice PACKAGE
## If any variable contains missing values, the mice package regresses 
##   it over the other variables and predicts the missing values. 
##   Some of the available models in mice package are:
##    *  PMM (Predictive Mean Matching) - suitable for numeric variables
##    *  logreg(Logistic Regression) - suitable for categorical variables with 2 levels
##    *  polyreg(Bayesian polytomous regression) - suitable for categorical variables with more than or equal to two levels
##    *  Proportional odds model - suitable for ordered categorical variables with more than or equal to two levels

## Limitations: Having imbalanced Factor variables when converted to 
## dummy variables results in mice not being able convert it into matrix
## and invert - This causes an error.  Safer to use method = 'cart'

data_imputes = mice(data1, m = 3, maxit = 7, method = 'cart', seed = 500)   
## m: Number of times model should run, maxit: Max number of iterations

## Methods mice used for imputing
data_imputes$method

## What are the values determined for each variable?
data_imputes$imp

## Now let us first examine the values mice determined
data_imputes$imp$continent

data_imputes$imp$lifeExp

## Which of the 3 datasets created should we use?
stripplot(data_imputes, pch = 20, cex = 1.2)

## OBSERVATIONS:
##  For lifeExp, Iteration 2 has captured the pattern well - which we can use for our imputation

## Impute Data using 'complete' function from mice package
imputed_data = complete(data_imputes, 2)

## Let us look at the same rows 25,29,88,145,159,193

data[c(25,29,88,145,159,193),]   ## Original Data without Month and Day columns

data1[c(25,29,88,145,159,193),]         ## Data with Randomly inserted missing values

imputed_data[c(25,29,88,145,159,193),]     ## Data with Imputation for missing values

summary(data)

options(scipen = 999)  ## Disable scientific notation
summary(imputed_data)

densityplot(data_imputes)

## OBSERVATIONS:
## Red lines - Density of imputed data for each imputed dataset
## Blue line - Density of observed data 
## We expect the Red and Blue distributions to be similar
