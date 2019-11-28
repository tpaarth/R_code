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

# 