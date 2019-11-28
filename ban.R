setwd('C:/Users/paart/Documents/PGP_BABI/Handouts/Data Mining/grp assign')
bank<-read.csv('Thera Bank-Data Set.csv',header = T)
str(bank)

# removing the 1st row
bankDS <- bank[,-1]
head(bankDS)
ncol(bankDS)
nrow(bankDS)

# let's create the training and testing sets
samp_size = floor(0.70*nrow(bankDS))
set.seed(100)
train_rows = sample(seq_len(nrow(bankDS)),size = samp_size) #pickup 'samp_size' random row nos 
trainDS = bankDS[train_rows,]
testDS = bankDS[-train_rows,]


# import the libraries
library(rpart)
library(rpart.plot)

# setting the control parameters - (Pre pruning, post pruning & Greedy Algorithm negator)
r.ctrl = rpart.control(minsplit=20, minbucket = 7, cp = 0, xval = 5)

# creating the tree for dep var - personal loan
tree1 = rpart(formula = Personal.Loan ~ ., 
              data = trainDS, 
              method = "class",
              control = r.ctrl )

rpart.plot(tree1)

# finding the apt complexity paramter value for pruning
printcp(tree1)
plotcp(tree1)
## CP = 0.006

# pruning the tree
ptree1 = prune(tree1, cp= 0.006, "CP")
rpart.plot(ptree1)
