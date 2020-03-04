#loading necessary libraries
#rm(list=ls())
library('twitteR')
library("ROAuth")
library("NLP")
library("syuzhet")
library("tm")
library("SnowballC")
library("stringi")

setwd('C:/pgpbabi')
# read and basic data formatting
twitter_ds <- read.csv('TwitterData2.csv',stringsAsFactors = FALSE)
str(twitter_ds)
twitter_ds$text <- as.character(twitter_ds$text)
twitter_ds$created <- as.Date(twitter_ds$created,format = '%d-%m-%y')


## Data transformations - 
# Syn - gsub(pattern to be replaced, what to replace with, date source)

# converting all text to lower case
twitter_txt <- twitter_ds$text
twitter_txt <- tolower(twitter_txt)
head(twitter_txt)

# Applying transformations

twitter_txt2 <- twitter_ds$text
twitter_txt2 <- tolower(twitter_txt2)

data_tran <- function(x){
  print(x)
  gsub(x,"",twitter_txt2)
}

# remove - rt,@usernames,punctuations,hyperlinks,tabs,blank start,blank end
trans <- c("rt",
           "@\\w+",
           "[[:punct:]]",
           "http\\w+",
           "[ |\t]{2,}",
           "^ ",
           " $")

for(i in trans){
  twitter_txt2 <- data_tran(i)
}

head(twitter_txt2)

# removing accented characters
twitter_txt2 <- gsub("[^[:space:][a-zA-Z]]*", "", twitter_txt2)   
twitter_txt2 <- gsub("[\r\n]", " ", twitter_txt2)
head(twitter_txt2)


# Building a corpus
## creating a VectorSource object
tweet_source <- VectorSource(twitter_txt2) 
class(tweet_source)

# use tweet_source to create corpus Vcourpus - RAM , PCourpus - disk
tweet_corpus <- VCorpus(tweet_source)

# viewing the 10th tweet
tweet_corpus[[10]] # metadata details
content(tweet_corpus[[10]])

## Removing stopwords

# List standard English stop words
stopwords("en")
# Print text without standard stop words
removeWords(tweet_corpus, stopwords("en"))

# adding new stopwords
####new_stops <- c("coffee", "bean",stopwords("en"))
hinglish <- c("women","divyadutta","thats","gt","rt","shabanaazmi","sheerqorma","asked","samjhe","nahi","caanrcnpr",
              "yrs","hm","abp","dm","kuch","godi","kaun","rubaika","caanrcnpr","calculation","like","new","s","hai","ko","ahead","ke",
              "metro","station","kaun","f","ke","skte","ab","ko","tum","mujhe","abpnews","insta","amitshah","ji","jit","thanks","aapne",
              "qampa","delete","swa","ka","amp","yeh","kaha","rubika","much","look","employer","see","say","news","anchor","can","boss"
              ,"puchye","draft","se","tweets","pic","aap","best","apne","tv","sm","go","creating","seen","interview",
              "interviewing","doesnt","minute","videoclip","video","clip","realising","meme","isupportdrsandeepmittalhere",
              "bulaya","xoxo","walo","called","time","road","called","false","pa","even","caa","nrc","just",
              "swarabhaskar","swarabhasker","swara","swaras","swarabhas","bhaskers","bhaskar","bhasker","harmonium","abbaharmonium","abba",
              "rubikaliyaquat","swarabhaskarkarbhaskarrubikaliyaquat","swarabhaskerrubikaliyaquat","rubika","liyaquat")
new_stops <- c(stopwords("en"),hinglish)
length(new_stops)

tweet_corpus2<- tm_map(tweet_corpus,removeWords , new_stops) 

# Remove Single letter words
removeSingle <- function(x) gsub(" . ", " ", x)   
tweet_corpus2 <- tm_map(tweet_corpus2, content_transformer(removeSingle))
writeLines(strwrap(tweet_corpus2[[750]]$content,60))
content(tweet_corpus2[[758]])

# replace words with the correct ones
replaceWord <- function(corpus, oldword, newword)
{
  tm_map(corpus, content_transformer(gsub), pattern=oldword, replacement=newword)
}

# tweet_corpus2<- replaceWord(tweet_corpus2, "abba", "abbaharmonium")
tweet_corpus2<- replaceWord(tweet_corpus2, "brains", "no_brains")
tweet_corpus2<- replaceWord(tweet_corpus2, "impressio", "false_impression")
tweet_corpus2<- replaceWord(tweet_corpus2, "beauty", "no_beauty")
# tweet_corpus2<- replaceWord(tweet_corpus2, "swarabhaskar", "swarabhaskar")
# tweet_corpus2<- replaceWord(tweet_corpus2, "swarabhasker", "swarabhaskar")
# tweet_corpus2<- replaceWord(tweet_corpus2, "swara", "swarabhaskar")
# tweet_corpus2<- replaceWord(tweet_corpus2, "swaras", "swarabhaskar")
# tweet_corpus2<- replaceWord(tweet_corpus2, "swarabhas", "swarabhaskar")
# tweet_corpus2<- replaceWord(tweet_corpus2, "swaras", "swarabhaskar")
# tweet_corpus2<- replaceWord(tweet_corpus2, "bhaskers", "swarabhaskar")
# tweet_corpus2<- replaceWord(tweet_corpus2, "bhasker", "swarabhaskar")

# tweet_corpus2<- replaceWord(tweet_corpus2, "swarabhaskar","")
# tweet_corpus2<- replaceWord(tweet_corpus2, "swara","")
# tweet_corpus2<- replaceWord(tweet_corpus2, "bhaskar","")
# tweet_corpus2<- replaceWord(tweet_corpus2, "rubikaliyaquat","")
# tweet_corpus2<- replaceWord(tweet_corpus2, "rubika","")
# tweet_corpus2<- replaceWord(tweet_corpus2, "liyaquat","")
# tweet_corpus2<- replaceWord(tweet_corpus2, "abbaharmonium","")



#### auto mode ######
# Creating a term document matrix
tdm<- TermDocumentMatrix(tweet_corpus2, control= list(wordLengths= c(1, Inf)))
tdm

# Find the terms used most frequently
(freq.terms <- findFreqTerms(tdm, lowfreq = 25))
term.freq <- rowSums(as.matrix(tdm))
term.freq <- subset(term.freq, term.freq > 25)
df <- data.frame(term = names(term.freq), freq= term.freq)

# Frequency analysis
(freq.terms <- findFreqTerms(tdm, lowfreq = 10))
term.freq <- rowSums(as.matrix(tdm))
term.freq <- subset(term.freq, term.freq > 10)
df1 <- data.frame(term = names(term.freq), freq= term.freq)
df1

(freq.terms <- findFreqTerms(tdm, lowfreq = 55))
term.freq <- rowSums(as.matrix(tdm))
term.freq <- subset(term.freq, term.freq > 55)
df2 <- data.frame(term = names(term.freq), freq= term.freq)

(freq.terms <- findFreqTerms(tdm, lowfreq = 85))
term.freq <- rowSums(as.matrix(tdm))
term.freq <- subset(term.freq, term.freq > 85)
df3 <- data.frame(term = names(term.freq), freq= term.freq)

#####plotting the graph of frequent terms
library(ggplot2)
p1=ggplot(df1, aes(reorder(term, freq),freq)) + 
  theme_bw() + 
  geom_bar(stat = "identity")  + 
  coord_flip() +
  labs(list(title="@10", x="Terms", y="Term Counts")) + 
  theme(axis.text.y = element_text(size=7))


p2=ggplot(df, aes(reorder(term, freq),freq)) + 
  theme_bw() + 
  geom_bar(stat = "identity")  + 
  coord_flip() +
  labs(list(title="@25", x="Terms", y="Term Counts"))+
  theme(axis.text.y = element_text(size=7))


p3=ggplot(df2, aes(reorder(term, freq),freq)) +
  theme_bw() + 
  geom_bar(stat = "identity")  + 
  coord_flip() +
  labs(list(title="@55", x="Terms", y="Term Counts"))

p4=ggplot(df3, aes(reorder(term, freq),freq)) + 
  theme_bw() + 
  geom_bar(stat = "identity")  + 
  coord_flip() +
  labs(list(title="@85", x="Terms", y="Term Counts")) 

# plotting the graph of frequent terms
library(grid)
library(gridExtra)

grid.arrange(p1,p2,ncol=2)
grid.arrange(p3,p4,ncol=2)

### word cloud ####

library(SnowballC)
library(RColorBrewer)
library(wordcloud)
library(topicmodels)
library(data.table)
library(stringi)

#####calculate the frequency of words and sort it by frequency and setting up the Wordcloud

# Creating the wordcloud

word.freq <-sort(rowSums(as.matrix(tdm)), decreasing= F)
pal<- brewer.pal(8, "Dark2")
wordcloud(words = names(word.freq), 
          freq = word.freq, 
          min.freq = 2, 
          random.order = F, 
          colors = pal, 
          max.words = 150)

# keyword association
list1<- findAssocs(tdm, "zaffrabad", 0.4)

corrdf1 <- t(data.frame(t(sapply(list1,c))))
corrdf1

barplot(t(as.matrix(corrdf1)),xlab = "Words",
        ylab = "Corr",
        col = "blue",
        main = "desc",
        border = "black")

list1<- findAssocs(tdm, "bharatbandh", 0.4)
corrdf1 <- t(data.frame(t(sapply(list1,c))))
corrdf1

barplot(t(as.matrix(corrdf1)), beside=TRUE,xlab = "Words",ylab = "Corr",col = "blue",main = "pseudointellect",border = "black")


