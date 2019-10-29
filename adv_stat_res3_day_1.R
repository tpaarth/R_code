setwd('C:/Users/paart/Documents/PGP_BABI/Handouts/Advanced Statistics/Datasets - Day 1')
install.packages('foreign',dependencies = T)
library(foreign)

datacsv<-read.csv(file = 'hsb2.csv',header = T)
str(datacsv)

###convert gender to categorical data as ANOVA works on categorical data
datacsv$Gender <- as.factor(datacsv$Gender) 
str(datacsv)
class(datacsv$Gender)

#####H0: is there a significant difference in read scores amongst genders#####

##1. Check if the data of variable 'read' is normal
shapiro.test(datacsv$read)
## since p-value = 0.005 < alpha 0.05 i.e.
### H0 is rejected i.e. data is NOT normally distributed hence
### T-test cannot be used


## Assuming normal data and Performing T-test with 
####H0: There is no signficant difference in reading score amongst genders
t.test(datacsv$read~datacsv$Gender, var.equal = T)

### p-value = 0.4553  > 0.05 i.e. HO is accepted 
### i.e. there is no significant difference in reading score amongst genders

## Since actual data is not normal (2 factors)we need to perform Mann-Whitney U-Test
wilcox.test(datacsv$read~datacsv$Gender)

### p-value = 0.4029 > 0.05 i.e. H0 is accepted
### i.e. there is no significant difference in reading score amongst genders


###2. Check the homogeneity of variance
fligner.test(datacsv$read~datacsv$Gender)

### p-value = 0.4968 > 0.05 i.e. H0 is accepted
### i.e. there is homogeneity in variance. Therefore, var.equal = T

##### Impact of program on write score #####

##H0: Is there a significance difference in the write score amongst people of different programs
##1. check the normality
shapiro.test(datacsv$write)
###p-value = 9.867e-07 < 0.05 i.e. H0 is rejected - data is not normal

##2. check the homogeneity
fligner.test(datacsv$write~datacsv$prog)
##p-value = 0.1964 > 0.05 i.e. H0 accepeted -  var.equl = T

## assuming the data to be normal and homogeneity of var = T
## Therefore ANOVA can be used(no. of progs > 2)

an <- aov(write~prog,data = datacsv)
summary(an)

## p-value = 0.0103 < 0.05 
## i.e. H0 is rejected - there is a significant difference in write score amongst people of diff programs
## Therefore, program impacts the write score. (based on the assumption that data is normal)

## However, data is not normal therefore kruskal-wallis test needs to be perfomed
## kruskal test - Not Normal & var.equal = T
kruskal.test(datacsv$write~datacsv$prog)

## p-value = 4.047e-08 < 0.05 i.e. H0 is rejected

## (Assume data is normal) however, there is no homogeneity of variance then welch-test 
## welch test - Normal Data & var.equal = F
install.packages("onewaytests")
library(onewaytests)
welch.test(write~prog,data = datacsv)

## p.value    : 4.735652e-08 < 0.05 i.e. H0 is rejected - there is a significat difference in write score amongst people of diff prog


###H0: the is no significant diff in the science score among diff school types

###1. check normality
shapiro.test(datacsv$science)

###p-value = 0.03478 < 0.05 i.e. H0 is rejected - data is not normal

###2. homogeneity of var
fligner.test(datacsv$science~datacsv$schtyp)
####p-value = 0.09162 > 0.05 i.e H0 is accepted - var.equal = T

###use t-test - since schtype holds only 2 values
t.test(datacsv$science~datacsv$schtyp,var.true = T)
####p-value = 0.2954 > 0.05 i.e. H0 is accepted - no difference in science and schtyp

###no normality
wilcox.test(datacsv$science~datacsv$schtyp)
#####p-value = 0.3195 > 0.05 i.e. H0 is accepted - no difference in science and schtyp

##### Difference of socst score among diff races #####

# 1. Check normality of data
shapiro.test(datacsv$race)
# p-value < 2.2e-16 < 0.05 - H0 is rejected - data is not normal

# 2. Check homogeneity of variance
fligner.test(datacsv$socst~datacsv$race)
# p-value = 0.8526 > 0.05 - H0 accepted - var.equal = T

# 3. Data not normal & var.equal = T therefore kruskal test
kruskal.test(datacsv$socst~datacsv$race)
# p-value = 0.0204 < 0.05 - H0 rejected - Difference is significant.

