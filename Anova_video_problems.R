##One Factor ANOVA

setwd("C:/Users/paart/Documents/PGP_BABI/R_Programming/datasets")
library(plyr)
library(ggplot2)
library(lattice)
library(MASS)

golf_data <- read.csv('Golfball.csv',header = T)
attach(golf_data)
golf_data
model <- aov(Distance~Design, data = golf_data)
summary(model)
print(summary(model),digits = 6)

# Post Hoc Analysis
TukeyHSD(model)

##Two Factor ANOVA

setwd('C:/Users/paart/Documents/PGP_BABI/Handouts/Advanced Statistics/Reference/ANOVA Datasets & Presentations')
# Read the data
paul_newfood_csv <- read.csv('paul-newfood.csv')
paul_newfood_csv
# Prepare the data
str(paul_newfood_csv)
paul_newfood_csv$PriceLevel <- as.factor(paul_newfood_csv$PriceLevel)
paul_newfood_csv$AdLevel <- as.factor(paul_newfood_csv$AdLevel)
str(paul_newfood_csv)

# 1. Is Sales normally distributed
shapiro.test(paul_newfood_csv$Sales)
# Inference: p-value = 0.01969 < 0.05 i.e. 
# H0 rejected - data is not normally distributed

# 2. 2-factor,not normal - Mann-Whitney test
wilcox.test(paul_newfood_csv$Sales~paul_newfood_csv$AdLevel)
# Inference: p-value = 0.4095 > 0.05 i.e. 
# H0 accepted - AdLevel impacts Sales

# 3. Test the homogeneity of variance
fligner.test(paul_newfood_csv$Sales~paul_newfood_csv$AdLevel)
# Inference: p-value = 0.3481 > 0.05 i.e.
# H0 accpeted - var.equal = T

# using Anova
model1 <- aov(Sales~PriceLevel+AdLevel,data = paul_newfood_csv)
summary(model1)
# Inference - 
# PriceLevel - p-value = 0.000182 < 0.05 - H0 rejected - makes an impact
# AdLevel - p-value = 0.969624 > 0.05 - H0 acceptedd - makes no impact

# interaction 
paul_newfood_csv$Price <- factor(paul_newfood_csv$PriceLevel,levels = c(1,2,3),labels = c("Low","Medium","High"))
paul_newfood_csv$Advertisement <- factor(paul_newfood_csv$AdLevel,levels = c(1,2),labels = c("Low","High"))

interaction.plot(paul_newfood_csv$Price
                 ,paul_newfood_csv$Advertisement
                 ,paul_newfood_csv$Sales
                 ,col = c("Red","Blue")
                 ,main = "Interaction between Price and Advertisement")

model2 <- aov(Sales~Price+Advertisement+Price*Advertisement,data = paul_newfood_csv)
summary(model2)

# Inference - 
# Price: 
# p-value = 0.000159 < 0.05 i.e. H0 rejected - makes an impact
# Advertisement: 
# p-value = 0.968450 > 0.05 i.e. H0 accepted - no significant impact
# Price:
# Advertisement: p-value = 0.190898 > 0.05 
# i.e. H0 accepted - interaction model makes no impact
# It appears that Ads was not effective though price was highly effective
# and statistically signigicant.

# Is StoreSize making a significant diff
# correlation between sales and storesize
cor(paul_newfood_csv$Sales,paul_newfood_csv$StoreSize)
cor.test(paul_newfood_csv$Sales,paul_newfood_csv$StoreSize)
# p-value = 0.03636 < 0.05 i.e. H0 rejected - storesize impact sales
# So we need to remove the linear effect due to StoreSize and then 
# run ANCOVA. Concomitant variable to be removed


# ANCOVA - Analysis of Co-Variance
# interaction effect adjusted for concomitant variable StoreSize
model3 <- aov(Sales~StoreSize+Price+Advertisement+Price*Advertisement,data = paul_newfood_csv)
summary(model3)

# Inference - 
# StoreSize:
# p-value = 0.000448 < 0.05 i.e. H0 rejected - makes an impact
# Price:
# p-value = 9.51e-06 < 0.05 i.e. H0 rejected - makes an impact
# Advertisement 
# p-value = 0.002458 < 0.05 i.e. H0 rejected - makes an impact
# Results of ANOVA indicated that Ads did not significantly impact Sales. However,
# it was also discovered that StoreSize was impacting Sales. Using ANCOVA, 
# we found Ad was indeed highly significant and its impact was masked by
# the concomitant variable StoreSize. When removed the linear effect of StoreSize,
# Ad became a major driver.
