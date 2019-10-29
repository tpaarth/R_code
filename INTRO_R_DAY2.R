###INTRO TO R DAY2 13.07.2019###
dataframe.1 = data.frame(v.a,v.b,v2)
dataframe.1 

#show structure of dataframe
str(dataframe.1)

#adding a new row/merging dataframes
new.row = data.frame(v.a=9,v.b=9,v2="New_Row_Added")

dataframe.2=rbind(dataframe.1,new.row)
dataframe.2

#adding a new col/merging dataframes
new.col = c("Time","To","Now","Add","A","New","Column")
dataframe.3=cbind(dataframe.2,new.col)
dataframe.3
names(dataframe.3)

#renaming columns in dataframe
colnames(dataframe.3)=c("Session","Students","Topic","New Topic")
dataframe.3

names(dataframe.3)

#dimensions of a dataframe
nrow(dataframe.3)
ncol(dataframe.3)
dim(dataframe.3)# r c

#display specific row
dataframe.3[3,]

#display specific col
dataframe.3[,4]

#display specific row+col
dataframe.3[3,4]

dataframe.3$Session
dataframe.3$Session[3]

#update value in row 3 of Session
dataframe.3$Session[3]=66
dataframe.3

#subsetting dataframe
dataframe.sub = dataframe.3[,1:2]
dataframe.sub

dataframe.sub = dataframe.3[,c("Session","Students")]

dataframe.sub3 = subset(dataframe.3,Students>=5)
dataframe.sub3

dataframe.sub4 = subset(dataframe.3, Topic=="Introduction")
dataframe.sub4

#sorting dataframes
dataframe.3[order(dataframe.3$Students),c(1:2)]
dataframe.3[order(dataframe.3$Students,decreasing = TRUE),]

install.packages("ggplot2")
library(ggplot2)
 