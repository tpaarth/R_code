### Factor Analysis

# Install and import relevant packages
install.packages('psych')
install.packages('GPArotation')

library('psych')
library('GPArotation')


### Using the in built bfi dataset/dataframe ####

# EDA - 
str(bfi)
# Observations - 
# Total no. of observations is 2800 and the total number of variables in the study are 28.
# These 28 variables in the dataframe bfi are the personality items from an 'international
# personality item measurement tool' where the first 25 variables measure the personality
# and the last 3 are the demographic variables gender, eduction and age.

# To get an idea of the number of cases in 2800 having missing data, we can use 
# sum(complete.cases(bfi[1:25]))

sum(complete.cases(bfi[1:25]))
# Inference - 
# 2436 or 87% of the cases in the dataset are complete. As per industry std we should have
# atleast 85% complete cases. Therefore this dataset is acceptable.

# The first 25 variables that have 87% complete cases will be used for doing factor analysis.
# The scale that is used to measure each variable is 1 to 6 where 
# 1 - very inaccurate
# 2 - moderately inaccruate
# 3 - slightly inaccurate
# 4 - slightly accurate
# 5 - moderately accurate
# 6 - very accurate

# Exercise
# Is there a sign diff bwteeen A1 and gender
# 1 - male 2- female


# Create a new variable gender_c
bfi$gender_c <- factor(bfi$gender,levels = c(1,2) ,labels = c("Male","Female"))
class(bfi$gender_c)

# 1. Check normality
shapiro.test(bfi$A1)
# Inference - p-value < 2.2e-16 - H0 is rejected - data not normal - use wilcox

# 2. Use wilcox
wilcox.test(A1~gender_c,data = bfi)
# Inference - p-value < 2.2e-16 - H0 is rejected - 
# This makes A1 a significant variable for our study.

# 1. Check normality of A2
shapiro.test(bfi$A2)
# Inference - p-value < 2.2e-16 - H0 is rejected - data not normal - use wilcox

# 2. Use wilcox
wilcox.test(A2~gender_c,data = bfi)
# Inference - p-value < 2.2e-16 - H0 is rejected - 
# This makes A2 a significant variable for our study.

# For factor analysis, the first 25 variables measuring the different personality
# traits are taken into analysis. This factor analysis on these 25 variables will give 
# us a smaller set of derived variables which are called factors. Each factor will be
# relatively independent of each other and each factor within itself will have a set
# of correlated variables.

# Mathematical model of factor analysis - will lead us in finding out - 
# 1. Is there any set of factors that can be extracted
# 2. How many factors from the 25 variables which are relatively independent of each other.
# 3. How many of these variables, in totality, measure the personality of an individual.(Eigen value)
# 4. Out of the 25 variables, which variable has the max impact on the personality.(Communality)
# 5. Which variables are correlated for a particular factor
# 6. The regression score which can be used for doing subsequent regression analysis

## Test 1: Bartletts test of sphericity. Check the correlation between variables
# To determine is there a significant correlation between the variables leading to a factor 
# solution. Use bartletts test of sphericity.

cortest.bartlett(bfi[1:25])

# H0: There is no correlation between the variables
# Inference - 
# p-value = 0 < 0.05 - H0 is rejected - 
# there is sufficient evidence of enough correlation between the 25 variables. 
# In other words, the correlation matrix is significant different from the identity matrix
# (identity matrix - n x n matrix where diagonal values are 1. Others are 0)

## Test 2: KMO test - Kaiser-Mayer-Olkin test
# To determine if factor analysis for these 25 variables returns atleast one factor
# This test returns the MSA(Measure of Sample Adequacy) statistic. This stat helps 
# in determining how good is the factor for the study.

KMO(bfi[1:25])
# This test returns an overall MSA value and an MSA value for each variable. 
# Statistically, 0 < MSA < 1. MSA statistic determines how good is the factor. 
# 
# For all practical purposes, the overall MSA >= 0.7. 
# To increase the overall MSA, 
# 1. Increase the sample size.
# 2. Eliminate the variables that are pulling down the overall MSA.

## Test 3: Scree plot - to determine the number of factors
# A plot to determine the number of factors can be made from these 25 variables. 
# This is known as a Scree plot. 

scree(bfi[1:25])
# This plot helps us in determining the number of factors. 
# Statistically, all the factors with Eigen value >=1 
# can be considered as good factors for the study.
# 
# Inference - 
# We can conclude that the number factors is 4 
# (All the 'FA' points above/on the Eigen value = 1 line)


# To run factor analysis, 
# 1.The first input required is the number of factors. (n factors = 4)

# 2.The second input is the algo for doing factor analysis - Principal Axis Factoring - (PA) - Factor Loading
# A1 = b11F1 + b12F2 + b13F3 + b14F4
# A2 = b21F1 + b22F2 + b23F3 + b24F4
# The factor loading matrix is the correlation between the factor and the variable.

# Communality - 
# Higher is the communality, more important is that variable in measuring the dependent variable. 
# in this case, the personality.
# Communality is calulated at the variable level. 
# In this example, it's determined by the following - 
# Communality of A1 = b11^2 + b12^2 + b13^2 + b14^2
# Communality of A2 = b21^2 + b22^2 + b23^2 + b24^2

# Eigen value - 
# This value tells us which factor is more important in measuring the personality of an 
# individual. (given by e.values)

# Factor scores -
# These scores are for each factor and are used as input for regression.

# ###DAY 2
# To run the factor analysis with the number of factors as 4
 
fa4.out <- fa(bfi[1:25],nfactors = 4,fm = "pa", rotate = "none")

# From the factor analysis with 4 factors, we need to create a factor loading 
# diagram

fa.diagram(fa4.out)

# communality 
fa4.out$communality
# Inference - 
# From the communality table traits N3 N1 and N2 are very important while the 
# traits A1 and O4 are least important. This indicates that in order to improve
# an individuals personality, traits N3 N1 and N2 need to be worked upon.
# As a thumb rule, communality < 0.35 is not a strong variable.

#### Now let's determine which factors are the most important.

# Eigen value tells the importance of a factor
fa4.out$e.values[1:4]
# Percentage of variance explained by each factor
100*fa4.out$e.values[1:4]/length(fa4.out$e.values)
# higher is the cumilative sum of percentage of variance, better is the model.
# Practically, a sum of ~65% to ~70% is considered to be a good model.

# Factor Loading Matrix
# This matrix will return the beta coefficients between each variable and factor
# like beta11, beta12 etc
print(fa4.out$loadings,digits = 3)


# for the Variable E2 the max loading from the loading matrix is -0.596.
# Therefore in the loading diagram it is shown as -0.6 on PA1

# The first iteration is to remove A1 and O4.

## lets remove A1 and O4 and re-run the factor analysis

# 1. Bartletts - 
newset<-subset(bfi,select = -c(A1,O4)) ##remove A1 and O4
cortest.bartlett(newset[1:23])

# 2. KMO - 
KMO(newset[1:23])
# Overall MSA = 0.85

# 3. scree plot
scree(newset[1:23])
# No. of factors = 4

# 4. Factor analysis
fa4_newset.out <- fa(newset[1:23],nfactors = 4,fm = "pa", rotate = "none")
fa.diagram(fa4_newset.out)
fa4_newset.out$e.values[1:4]
100*fa4_newset.out$e.values[1:4]/length(fa4_newset.out$e.values)
# The percentage variation increased to 47%

# communality

elim_set <- fa4_newset.out$communality
sort(elim_set)

# Iteration 3 - eliminate A4 O5

newset_2 <- subset(newset,select = -c(A4,O5))
cortest.bartlett(newset_2[1:21])
KMO(newset_2[1:21])                   
scree(newset_2[1:21])
fa4_newset_2.out <- fa(newset_2[1:21],nfactors = 3,fm = "pa", rotate = "none")
fa4_newset_2.out$communality


fa.diagram(fa4_newset_2.out)
fa4_newset_2.out$e.values[1:4]
100*fa4_newset_2.out$e.values[1:4]/length(fa4_newset_2.out$e.values)

elim_set<-fa4_newset_2.out$communality
sort(elim_set)#remove o2,o1 

# Iteration 3 - remove O2,O1
newset_3 <- subset(newset_2,select = -c(O2,O1))
cortest.bartlett(newset_3[1:19])
scree(newset_3[1:19])
fa4_newset_3.out <- fa(newset_3[1:19],nfactors = 3,fm = "pa", rotate = "none")
fa4_newset_3.out$communality

fa.diagram(fa4_newset_3.out)
fa4_newset_3.out$e.values[1:4]
sum(100*fa4_newset_2.out$e.values[1:4]/length(fa4_newset_2.out$e.values))

elim_set<-fa4_newset_3.out$communality
sort(elim_set)#remove o3,a2

# Iteration 4 - remove o3,a2

newset_4 <- subset(newset_3,select = -c(O3,A2))
scree(newset_4)
fa4_newset_4.out <- fa(newset_4[1:17],nfactors = 3,fm = "pa", rotate = "none")
fa4_newset_4.out$communality
fa.diagram(fa4_newset_4.out)
sum(100*fa4_newset_4.out$e.values[1:3]/length(fa4_newset_4.out$e.values))


## Rotation of factors - 
# To get relatively independent fators, instead rotate="none" there is a 
# recommended method known as "varimax"
# To use varimax rotation, instead of rotate = "none" use rotate = "varimax"
# To get the factor loading matrix, instead of $loadings, $structure

# Using the personality dataset what is the max variance explained with 
# varimax rotation so that we get relatively independent factors and 
# what's the number of factors

setwd('C:/Users/paart/Documents/PGP_BABI/Handouts/Advanced Statistics/Datasets - Day 4')

person <- read.csv('personality.csv',header = T)
View(person)
str(person)

cortest.bartlett(person)
KMO(person)

scree(person)
fa_person.out <- fa(person,nfactors = 5,fm="pa",rotate = "varimax")
fa.diagram(fa_person.out)
sort(fa_person.out$communality)
fa_person.out$e.values[1:5]
sum(100*fa_person.out$e.values[1:5]/length(fa_person.out$e.values))

# eliminate pers35     pers17     pers41     pers12    

person_1<- subset(person,select = -c(pers35,pers17,pers41,pers12))
scree(person_1)
fa_person_1.out <- fa(person_1,nfactors = 5,fm="pa",rotate = "varimax")
fa.diagram(fa_person_1.out)
sort(fa_person_1.out$communality)
fa_person_1.out$e.values[1:5]
sum(100*fa_person_1.out$e.values[1:5]/length(fa_person_1.out$e.values))

# eliminate pers44     pers30      

person_2<- subset(person_1,select = -c(pers44,pers30))
scree(person_2)
fa_person_2.out <- fa(person_2,nfactors = 5,fm="pa",rotate = "varimax")
fa.diagram(fa_person_2.out)
sort(fa_person_2.out$communality)
fa_person_2.out$e.values[1:5]
sum(100*fa_person_2.out$e.values[1:5]/length(fa_person_2.out$e.values))

# eliminate pers11     pers10     

person_3<- subset(person_2,select = -c(pers11,pers10))
scree(person_3)
fa_person_3.out <- fa(person_3,nfactors = 5,fm="pa",rotate = "varimax")
fa.diagram(fa_person_3.out)
sort(fa_person_3.out$communality)
fa_person_3.out$e.values[1:5]
sum(100*fa_person_3.out$e.values[1:5]/length(fa_person_3.out$e.values))

# eliminate pers25    pers27     

person_4<- subset(person_3,select = -c(pers25,pers27))
scree(person_4)
fa_person_4.out <- fa(person_4,nfactors = 5,fm="pa",rotate = "varimax")
fa.diagram(fa_person_4.out)
sort(fa_person_4.out$communality)
fa_person_4.out$e.values[1:5]
sum(100*fa_person_4.out$e.values[1:5]/length(fa_person_4.out$e.values))

# eliminate pers29    pers20    

person_5<- subset(person_4,select = -c(pers29,pers20))
scree(person_5)
fa_person_5.out <- fa(person_5,nfactors = 5,fm="pa",rotate = "varimax")
fa.diagram(fa_person_5.out)
sort(fa_person_5.out$communality)
fa_person_5.out$e.values[1:5]
sum(100*fa_person_5.out$e.values[1:5]/length(fa_person_5.out$e.values))

# eliminate pers43    pers38   

person_6<- subset(person_5,select = -c(pers43,pers38))
scree(person_6)
fa_person_6.out <- fa(person_6,nfactors = 5,fm="pa",rotate = "varimax")
fa.diagram(fa_person_6.out)
sort(fa_person_6.out$communality)
fa_person_6.out$e.values[1:5]
sum(100*fa_person_6.out$e.values[1:5]/length(fa_person_6.out$e.values))

# eliminate pers02    pers16   

person_7<- subset(person_6,select = -c(pers02,pers16))
scree(person_7)
fa_person_7.out <- fa(person_7,nfactors = 5,fm="pa",rotate = "varimax")
fa.diagram(fa_person_7.out)
sort(fa_person_7.out$communality)
fa_person_7.out$e.values[1:5]
sum(100*fa_person_7.out$e.values[1:5]/length(fa_person_7.out$e.values))

# eliminate pers37    pers34   

person_8<- subset(person_7,select = -c(pers37,pers34))
scree(person_8)
fa_person_8.out <- fa(person_8,nfactors = 5,fm="pa",rotate = "varimax")
fa.diagram(fa_person_8.out)
sort(fa_person_8.out$communality)
fa_person_8.out$e.values[1:5]
sum(100*fa_person_8.out$e.values[1:5]/length(fa_person_8.out$e.values))

# eliminate pers26    pers33   

person_9<- subset(person_8,select = -c(pers26,pers33))
scree(person_9)
fa_person_9.out <- fa(person_9,nfactors = 5,fm="pa",rotate = "varimax")
fa.diagram(fa_person_9.out)
sort(fa_person_9.out$communality)
fa_person_9.out$e.values[1:5]
sum(100*fa_person_9.out$e.values[1:5]/length(fa_person_9.out$e.values))



####### PCA - Principal Componenet Analysis #################

# In PCA, we are reducing a larger number of variables into components
# whereas in factor analysis we are reducing a larger number of variables
# into factors.

# For PCA, the algorithm assumes there is no unique variance or there is no
# error associated with the measured variables

# 1. Bartletts
cortest.bartlett(bfi[1:25])

# 2. KMO
KMO(bfi[1:25])

# 3. scree plot
scree(bfi[1:25])
# for PCA using the scree plot, the no. of components are 6

# 4. PCA
pc6.out <- principal(bfi[1:25],nfactors = 6,rotate = "varimax")
fa.diagram(pc6.out)
# From fa diagram, there is existance of 6 components

pc6.out$communality

# 5.eigen values
pc6.out$values[1:6]

# 6. Percentage of variance expalined
sum(100*pc6.out$values[1:6]/length(pc6.out$values))
# The percentage of variance explained from PCA is 57% which is higher
# compared to factor analysis because the alogrithm assumes there is no
# error in the measurement of the variables. Therefore, factor analysis
# is always recommended and PCA is used only when we are interested to know
# the components and no further modelling is involved on the basis of the 
# components.

library(foreign) ##for SAV files

data <- read.spss('C:/Users/paart/Documents/PGP_BABI/Handouts/Advanced Statistics/Datasets - Day 4/personality.sav',to.data.frame = T,use.value.labels = T)
str(data)
data1<- read.spss('C:/Users/paart/Documents/PGP_BABI/Handouts/Advanced Statistics/Datasets - Day 4/SAQ.sav',to.data.frame = T,use.value.labels = T)
str(data1)

# In data1 23 variables are present from Q01 to Q23.
# Factor Analysis on data1
# Since all datatypes are factors, we need to convert them to int datatype
data2<- as.data.frame(lapply(data1, as.integer))
str(data2)
# 1. Bartletts
cortest.bartlett(data2)

# 2. KMO
KMO(data2)

# 3. scree plot
scree(data2)
# FA - 2 factors
# PC - 5 factors

# 4. Factor Analysis
fa_data2.out <- fa(data2,nfactors = 2,fm="pa",rotate = "varimax")
fa.diagram(fa_data2.out)
sort(fa_data2.out$communality)
fa_data2.out$e.values[1:2]
sum(100*fa_data2.out$e.values[1:2]/length(fa_data2.out$e.values))
# Percentage of variance explained = 39.25%

# 5. PCA
pcdata2.out <- principal(data2,nfactors = 5,rotate = "varimax")
fa.diagram(pcdata2.out)
sort(pcdata2.out$communality)
pcdata2.out$e.values[1:5]
sum(100*pcdata2.out$values[1:5]/length(pcdata2.out$values))
# Percentage of variance explained = 54.61%

# Note - for getting independent factors, besides varimax rotation, a 
# few others are 'equimax' and 'quartimax'. They are also known as 
# orthogonal rotations. 
# Non-orthogonal(oblique) rotations are used to get correlated factors and the 
# methods are 'oblimin' and 'promax'

# Lets eliminate Q23 and Q22 and check the FA
data2_subset<- subset(data2,select = -c(Q23,Q22))
scree(data2_subset)
fa_data2_subset.out <- fa(data2_subset,nfactors = 3,fm="pa",rotate = "varimax")
fa.diagram(fa_data2_subset.out)
sort(fa_data2_subset.out$communality)
fa_data2_subset.out$e.values[1:2]
sum(100*fa_data2_subset.out$e.values[1:2]/length(fa_data2_subset.out$e.values))
