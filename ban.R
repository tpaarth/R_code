setwd('C:/Users/paart/Documents/PGP_BABI/Handouts/Data Mining/grp assign')
bank<-read.csv('Thera Bank-Data Set.csv',header = T)

str(bank)
seed = 100
# removing the 1st row
bankDS <- bank[,-1]
bankDS$Personal.Loan <- as.factor(bankDS$Personal.Loan)
bankDS$Online <- as.factor(bankDS$Online)
bankDS$Education <- as.factor(bankDS$Education)
bankDS$Securities.Account <- as.factor(bankDS$Securities.Account)
bankDS$CD.Account <- as.factor(bankDS$CD.Account)
bankDS$Online <- as.factor(bankDS$Online)
bankDS$CreditCard <- as.factor(bankDS$CreditCard)
str(bankDS)

head(bankDS)
ncol(bankDS)
nrow(bankDS)

# let's create the training and testing sets
samp_size = floor(0.70*nrow(bankDS))
set.seed(100)
train_rows = sample(seq_len(nrow(bankDS)),size = samp_size) #pickup 'samp_size' random row nos 
trainDS = bankDS[train_rows,]
testDS = bankDS[-train_rows,]

# Treatment of missing values
trainDS[!complete.cases(trainDS),] #no of complete cases
trainDS = na.omit(trainDS) # omitting missing values from the original DS

testDS[!complete.cases(testDS),]
testDS = na.omit(testDS)
############################################################
############# problem 3 - CART         #####################
############################################################

# import the libraries
library(rpart)
library(rpart.plot)

# setting the control parameters - (Pre pruning, post pruning & Greedy Algorithm negator)
r.ctrl = rpart.control(minsplit=20, 
                       minbucket = 7, 
                       cp = 0, 
                       xval = 5)

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





############################################################
############# problem 3 - Random Forest#####################
############################################################

# import the libraries
library(randomForest)

# let's build a random forest
rForest = randomForest(Personal.Loan ~.,
                       data = trainDS,
                       ntree = 501,
                       nodesize = 10,
                       importance = TRUE,
                       na.action=na.exclude)

print(rForest)

plot(rForest)
legend("topright", c("OOB", "0", "1"), text.col=1:6, lty=1:3, col=1:3)

rForest$err.rate



importance(rForest)

# tuning the forest
set.seed(seed)
trForest = tuneRF(x = trainDS[,-c(9)],
                  y = trainDS$Personal.Loan,
                  mtryStart = 3,
                  stepFactor = 0.5,
                  ntreeTry = 150,
                  improve = 0.0001,
                  nodesize = 10,
                  trace = TRUE,
                  plot = TRUE,
                  doBest = TRUE,
                  importance = TRUE)

varImpPlot(trForest)
# Testing the tuned forest
trainDS1 = trainDS
trainDS1$predictedPL = predict(trForest,trainDS1,type = "class")
trainDS1$probPL = predict(trForest,trainDS1,type = "prob")
View(trainDS1)

tbl = table(trainDS1$Personal.Loan,trainDS1$predictedPL)
tbl

# testing the forest on the training dataset
testDS1 = testDS
testDS1$predictedPL = predict(trForest,testDS1,type="class")
testDS1$probPL = predict(trForest,testDS1,type="prob")

ttbl = table(testDS1$Personal.Loan,testDS1$predictedPL)
ttbl

View(testDS1)

# plotting the AUC
library(MLmetrics)
MLmetrics::AUC(testDS1$probPL[,2],
               testDS1$predictedPL)#returns the AUC


