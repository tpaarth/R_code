# Problem 1 - cereal.csv
setwd('C:/Users/paart/Documents/PGP_BABI/Handouts/Advanced Statistics/grp assignment')
library('psych')
# Reading the file 
cereal_data = read.csv('cereal.csv',header = T)

# data treatment
cereal_data[cereal_data==6]=5
cereal_data[,c(12)]=6-cereal_data[,c(12)]
cereal_data[,c(25)]=6-cereal_data[,c(25)]

View(cereal_data)


# Checking the no. of complete cases
sum(complete.cases(cereal_data[1:26]))

# check the structure of cereal
str(cereal_data)
# 1: Test of spheracity - Bartlett's test
cortest.bartlett(cereal_data[2:26])
# Inference - 
# p-value = 0 < 0.5 i.e. H0 is rejected
# Variables are correlated and FA can be performed.

# 2: KMO test - to check the MSA value
KMO(cereal_data[2:26])
# Inference - 
# Overall MSA =  0.85 > 0.7
# FA is likely to return atleast one factor

# 3: Scree plot - to determine the no. of factors
scree(cereal_data[2:26])
# no of factors from the scree plot = 3

# 4: Factor Analysis
fa4.out = fa(cereal_data[2:26],nfactors =4, fm = "pa", rotate = "varimax")

# 5: Factor loading diagram
fa.diagram(fa4.out)

# 6: Communality check - to find out variables that can be eliminated in the next iteration
sort(fa4.out$communality)

# 7: Eigen Values
fa4.out$e.values[1:4]
sum(100*fa4.out$e.values[1:4]/length(fa4.out$e.values))

# 8: factor loading matrix
print(fa4.out$loadings,digits = 3)


####Iteration 2 - eliminating Easy and Process
cereal2 = subset(cereal_data,select = -c(Easy,Process))
str(cereal2)
names(cereal2)
KMO(cereal2[2:24])
scree(cereal2[2:24])
fa3.out = fa(cereal2[2:24],nfactors =4, fm = "pa", rotate = "varimax")
fa.diagram(fa3.out)
sort(fa3.out$communality)
fa3.out$e.values[1:4]
sum(100*fa3.out$e.values[1:4]/length(fa3.out$e.values))

####Iteration 3 - eliminating Soggy 
cereal3 = subset(cereal2,select = -c(Soggy))
names(cereal3)
KMO(cereal3[2:23])
scree(cereal3[2:23])
fa3.out = fa(cereal3[2:23],nfactors =4, fm = "pa", rotate = "varimax")
fa.diagram(fa3.out)
sort(fa3.out$communality)
fa3.out$e.values[1:4]
sum(100*fa3.out$e.values[1:4]/length(fa3.out$e.values))

####Iteration 4 - eliminating Crisp 
cereal4 = subset(cereal3,select = -c(Crisp))
names(cereal4)
KMO(cereal4[2:22])
scree(cereal4[2:22])
fa3.out = fa(cereal4[2:22],nfactors =4, fm = "pa", rotate = "varimax")
fa.diagram(fa3.out)
sort(fa3.out$communality)
fa3.out$e.values[1:4]
sum(100*fa3.out$e.values[1:4]/length(fa3.out$e.values))

####Iteration 5 - eliminating Boring 
cereal5 = subset(cereal4,select = -c(Boring))
names(cereal5)
KMO(cereal5[2:21])
scree(cereal5[2:21])
fa3.out = fa(cereal5[2:21],nfactors =4, fm = "pa", rotate = "varimax")
fa.diagram(fa3.out)
sort(fa3.out$communality)
fa3.out$e.values[1:4]
sum(100*fa3.out$e.values[1:4]/length(fa3.out$e.values))

####Iteration 6 - eliminating Economical 
cereal6 = subset(cereal5,select = -c(Economical))
names(cereal6)
KMO(cereal6[2:20])
scree(cereal6[2:20])
fa3.out = fa(cereal6[2:20],nfactors =4, fm = "pa", rotate = "varimax")
fa.diagram(fa3.out)
sort(fa3.out$communality)
fa3.out$e.values[1:4]
sum(100*fa3.out$e.values[1:4]/length(fa3.out$e.values))
print(fa3.out$loadings,digit=3)

####Iteration 7 - eliminating Plain 
cereal7 = subset(cereal6,select = -c(Plain))
names(cereal7)
KMO(cereal7[2:19])
scree(cereal7[2:19])
fa3.out = fa(cereal7[2:19],nfactors =4, fm = "pa", rotate = "varimax")
fa.diagram(fa3.out)
sort(fa3.out$communality)
fa3.out$e.values[1:4]
sum(100*fa3.out$e.values[1:4]/length(fa3.out$e.values))
print(fa3.out$loadings,digit=3)


