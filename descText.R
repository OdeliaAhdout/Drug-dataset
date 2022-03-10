setwd('~/lav/tmp/Drug-dataset')
source('graphEnv.R')

fs <- read.csv('review_condition.csv.gz',stringsAsFactor=F)
##------------------------------text---------------------------------
library('tm')
library(wordcloud)
require('sentR')
library(lubridate)
## separating text by emotion

docs <- Corpus(VectorSource(fs$drugName))
dtm <- TermDocumentMatrix(docs) 
matrix <- as.matrix(dtm) 
words <- sort(rowSums(matrix),decreasing=TRUE) 
df <- data.frame(word = names(words),freq=words)
jpeg("fig/cloudDrug.jpg")
wordcloud(words=df$word,freq=df$freq,min.freq=1,max.words=200,random.order=FALSE,rot.per=0.35,colors=gCol1)
dev.off()

docs <- Corpus(VectorSource(fs$condition))
dtm <- TermDocumentMatrix(docs) 
matrix <- as.matrix(dtm) 
words <- sort(rowSums(matrix),decreasing=TRUE) 
df <- data.frame(word = names(words),freq=words)
jpeg("fig/cloudCondition.jpg")
wordcloud(words=df$word,freq=df$freq,min.freq=1,max.words=200,random.order=FALSE,rot.per=0.35,colors=gCol1)
dev.off()

textL1 <- as.vector(unlist(strsplit(fs$review,split=" ")))
textT <- table(textL1)
textT = textT[rev(order(textT))]
textT[order(textT)]
textT = textT[2:500]

jpeg("fig/compCloud.jpg")
wordcloud(names(textT),textT,random.order=FALSE,colors=gCol1)
dev.off()

cs <- read.csv("train/dict/sentimentDict.csv")
cs$Word <- tryTolower(cs$Word)
sentO <- classify.aggregate(fs$review,cs[!is.na(cs$Positive),"Word"],cs[!is.na(cs$Negative),"Word"])
#setB <- classify.naivebayes(fs$text)
sentO$cat = "neutral"
sentO$cat[sentO$score>0] = "positive"
sentO$cat[sentO$score<0] = "negative"
sentO$drug = fs$drugName
sentO$condition = fs$condition
emos = levels(factor(sentO$cat))
nemo = length(emos)
emo.docs = rep("", nemo)
for(i in 1:nemo){
   tmp = fs$review[sentO$cat == emos[i]]
   emo.docs[i] = paste(tmp, collapse=" ")
}
docs <- Corpus(VectorSource(emo.docs))
tdm = TermDocumentMatrix(docs)
tdm = as.matrix(tdm)
colnames(tdm) = emos
mean(sentO$score[!sentO$score==0])
sum(sentO$score[sentO$score<0])/sum(sentO$score[sentO$score>0])
jpeg("fig/compCloudSent.jpg")
comparison.cloud(tdm,colors=gCol1,scale=c(3,.5),random.order=FALSE,title.size=1.5)
dev.off()


t <- sort(table(sentO$condition),decreasing=TRUE)[1:10]
sent1 <- sentO[sentO$condition %in% names(t),]
emos = levels(factor(sent1$condition))
nemo = length(emos)
emo.docs = rep("", nemo)
for(i in 1:nemo){
   tmp = fs$review[sent1$condition == emos[i]]
   emo.docs[i] = paste(tmp, collapse=" ")
}
comm_corpus = Corpus(VectorSource(emo.docs))
tdm = TermDocumentMatrix(comm_corpus)
tdm = as.matrix(tdm)
colnames(tdm) = emos
jpeg("fig/compCloudPol.jpg")
comparison.cloud(tdm,colors=gCol1,scale=c(3,.5),random.order=FALSE,title.size=1.5)
dev.off()

 
t <- sort(table(sentO$drug),decreasing=TRUE)[1:10]
sent1 <- sentO[sentO$drug %in% names(t),]
emos = levels(factor(sent1$condition))
nemo = length(emos)
emo.docs = rep("", nemo)
for(i in 1:nemo){
   tmp = fs$review[sent1$condition == emos[i]]
   emo.docs[i] = paste(tmp, collapse=" ")
}
comm_corpus = Corpus(VectorSource(emo.docs))
tdm = TermDocumentMatrix(comm_corpus)
tdm = as.matrix(tdm)
colnames(tdm) = emos
jpeg("fig/compCloudPol.jpg")
comparison.cloud(tdm,colors=gCol1,scale=c(3,.5),random.order=FALSE,title.size=1.5)
dev.off()

 
