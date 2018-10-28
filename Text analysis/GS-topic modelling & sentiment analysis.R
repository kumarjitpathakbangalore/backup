###############################################################################################################
##Sentiment Analysis##
################################################################################################################
library(ggplot2)
library(tm)
library(wordcloud)
library(syuzhet)
library(Rstem)
library(RColorBrewer)
library(SentimentAnalysis)
library(devtools)
library(sentimentr)
library(dplyr)
library(SnowballC)
library(klaR)
library(e1071)
library(caret)
library(flexclust)
library(hunspell)
library("RSiteCatalyst") 
library("RTextTools")

#Import text file
senti <- read.csv("C:\\Shiva\\Projects\\GS_fault_Text_analytics\\Medallia\\Data\\2016-18 All Medallia Surveys Aug 31_1.csv",header=TRUE)


score.sentiment <- function(sentences, pos.words, neg.words, .progress='none')
{
  require(plyr)
  require(stringr)
  require(tm)
  
  scores <- laply(sentences, function(sentence, pos.words, neg.words){
    sentence <- gsub('[[:punct:]]', "", sentence)
    sentence <- gsub('[[:cntrl:]]', "", sentence)
    sentence <- gsub('\\d+', "", sentence)
    sentence <- tolower(sentence)
    myCorpus <- Corpus(VectorSource(sentence))
    myCorpus <- tm_map(myCorpus, removeNumbers)
    stpwrds=c("BT","GS","service","also","yet","years","see","two","can","however","months","times","time","now","one","support","good")
    myCorpus <- tm_map(myCorpus, removeWords, stpwrds)
    #myStopwords <- c(stopwords('english'))
    #myCorpus <- tm_map(myCorpus, removeWords, myStopwords)
    #myCorpus <- tm_map(myCorpus, stemDocument)
    df <- data.frame(text=sapply(myCorpus, identity),stringsAsFactors=F)
    sentence<- as.character(t(df[1,]))
    #print(sentence)
    
    word.list <- str_split(sentence, '\\s+')
    words <- unlist(word.list)
    pos.matches <- match(words, pos.words)
    neg.matches <- match(words, neg.words)
    pos.matches <- !is.na(pos.matches)
    neg.matches <- !is.na(neg.matches)
    #print(paste(words,pos.matches,neg.matches))
    score <- sum(pos.matches) - sum(neg.matches)
    return(score)
  }, pos.words, neg.words, .progress=.progress)
  scores.df <- data.frame(score=scores, text=sentences)
  return(scores.df)
}

#Positive and negative words
pos <- scan('C:\\Shiva\\Projects\\GS_fault_Text_analytics\\Medallia\\Data\\Positive words.csv', what='character', comment.char=';') #folder with positive dictionary
neg <- scan('C:\\Shiva\\Projects\\GS_fault_Text_analytics\\Medallia\\Data\\Negative words.csv', what='character', comment.char=';') #folder with negative dictionary

pos.words <- c(pos, 'upgrade')
neg.words <- c(neg, 'wtf', 'wait', 'waiting', 'epicfail')

Dataset <- senti

#converting text to charecter
Dataset$Anything.Else.Comment <- as.character(Dataset$Anything.Else.Comment)

#converting text to lower case
senti$Anything.Else.Comment=tolower(senti$Anything.Else.Comment)

#develop word cloud #wordcloud package
wordcloud(senti$Anything.Else.Comment,min.freq=10)


#sentiment scoring
scores <- score.sentiment(senti$Anything.Else.Comment, pos.words, neg.words, .progress='text')

#total score calculation: positive / negative / neutral
stat <- scores
stat <- mutate(stat,sentiment=ifelse(stat$score > 0, 'positive', ifelse(stat$score < 0, 'negative', 'neutral')))

sentiment_score_out <- cbind(senti,stat)

#################################################################################################################
                                                 ##Word cloud##
##################################################################################################################

#separate text by sentiment
sents = levels(factor(sentiment_score_out$sentiment))

# get the labels and percents
labels <- lapply(sents, function(x) 
  paste(x,format(round((length((sentiment_score_out
                                [sentiment_score_out$sentiment ==x,])$text)/
                          length(sentiment_score_out$sentiment)*100),2),nsmall=2),"%"))
nemo = length(sents)

emo.docs = rep("", nemo)
for (i in 1:nemo)
{
  tmp = sentiment_score_out[sentiment_score_out$sentiment == sents[i],]$text
  
  emo.docs[i] = paste(tmp,collapse=" ")
}

# remove stopwords
emo.docs = removeWords(emo.docs, stopwords("english"))
stpwrds=c("BT","GS","services","also","yet","years","see",
          "two","can","however","months","times","time","now","one",
          "support","good","still","btgs","csr","year","day",
          "gov","tui","get","wan","will","last","don","set","way","key")

emo.docs = removeWords(emo.docs, stpwrds)

# to remove numbers
emo.docs=removeNumbers(emo.docs) 

#remove english stopwords
stpwrds1=stopwords("english")
emo.docs = removeWords(emo.docs, stpwrds1)

#create corpus
corpus = Corpus(VectorSource(emo.docs))

#create TDM matrix
tdm = TermDocumentMatrix(corpus)
tdm = as.matrix(tdm)
colnames(tdm) = labels

# comparison word cloud
set.seed(1213)
comparison.cloud(tdm, colors = c(negative="red",neutral="blue",positive="green"),max.words=200,
                 scale = c(3,.5), random.order = FALSE, title.size = 1.0)


#################################################################################################################################################################################
                                              ##Likelihood recommend comment##
#################################################################################################################################################################################

#import data
Dataset <- read.csv("C:\\Shiva\\Projects\\GS_fault_Text_analytics\\Medallia\\Data\\2016-18 All Medallia Surveys Aug 31_2.csv",stringsAsFactors = F)

#positive and negative disctionary
pos <- scan('C:\\Shiva\\Projects\\GS_fault_Text_analytics\\Medallia\\Data\\Positive words.csv', what='character', comment.char=';') 
neg <- scan('C:\\Shiva\\Projects\\GS_fault_Text_analytics\\Medallia\\Data\\Negative words.csv', what='character', comment.char=';') 

pos.words <- c(pos, 'upgrade')
neg.words <- c(neg, 'wtf', 'wait', 'waiting', 'epicfail')

#convert text to charecter
Dataset$Likelihood.to.Recommend.Comment <- as.character(Dataset$Likelihood.to.Recommend.Comment)

#convert text to lower case
Dataset$Likelihood.to.Recommend.Comment=tolower(Dataset$Likelihood.to.Recommend.Comment)

#develop word cloud 
wordcloud(Dataset$Likelihood.to.Recommend.Comment,min.freq=10)

#Sentiment scoring
scores <- score.sentiment(Dataset$Likelihood.to.Recommend.Comment, pos.words, neg.words, .progress='text')

#total score calculation: positive / negative / neutral
stat <- scores
stat <- mutate(stat,sentiment=ifelse(stat$score > 0, 'positive', ifelse(stat$score < 0, 'negative', 'neutral')))

sentiment_score_out <- cbind(Dataset,stat)

#############################################################################################################################################################
                                                    ##Word cloud##
##############################################################################################################################################################

#separate text by sentiment
sents = levels(factor(sentiment_score_out$sentiment))

# get the labels and percents
labels <- lapply(sents, function(x) paste(x,format(round((length((sentiment_score_out
                                                                  [sentiment_score_out$sentiment ==x,])$text)
                                                          /length(sentiment_score_out$sentiment)*100),2),
                                                   nsmall=2),"%"))
nemo = length(sents)

emo.docs = rep("", nemo)
for (i in 1:nemo)
{
  tmp = sentiment_score_out[sentiment_score_out$sentiment == sents[i],]$text
  
  emo.docs[i] = paste(tmp,collapse=" ")
}

# remove stopwords
emo.docs = removeWords(emo.docs, stopwords("english"))
stpwrds=c("BT","GS","Global services","services","take","global","also","yet","years","see",
          "two","can","however","months","times","time","past","now","one","good","still","btgs","csr","year","day",
          "gov","tui","get","wan","will","last","don","set","way","key","cet",
          "want","sev","put","want","first","say","case","gsip","even")

emo.docs = removeWords(emo.docs, stpwrds)

# to remove numbers
emo.docs=removeNumbers(emo.docs) 

#remove english remove words
stpwrds1=stopwords("english")
emo.docs = removeWords(emo.docs, stpwrds1)

#remove punctuation
emo.docs <- removePunctuation(emo.docs)

#create corpus
corpus = Corpus(VectorSource(emo.docs))

#create TDM matrix
tdm = TermDocumentMatrix(corpus)
tdm = as.matrix(tdm)

colnames(tdm) = labels

# Sentiment comparison word cloud
set.seed(1213)
comparison.cloud(tdm, colors = c(negative="red",neutral="blue",positive="green"),max.words=300,
                 scale = c(3,.5), random.order = FALSE, title.size = 1.0)

#save evaluation results
write.csv(sentiment_score_out, file='C:\\Shiva\\Projects\\GS_fault_Text_analytics\\Medallia\\Output\\sentiment_out_Likelihood to recommend comment2.csv', row.names=TRUE) 


#########################################################################################################################################################################################
                                               ##Hierarical Clustering#####
########################################################################################################################################################################################
pos_tag_out <- read.csv("C:\\Shiva\\Projects\\GS_fault_Text_analytics\\Medallia\\Data\\likelihood_to_recommand_POS_clean.csv",stringsAsFactors = F)

#Create Corpus
docs <- Corpus(VectorSource(pos_tag_out$POS_tags_clean))

docs<-tm_map(docs,content_transformer(tolower))
docs<-tm_map(docs,content_transformer(removeNumbers))
docs<-tm_map(docs,content_transformer(removePunctuation))
docs<-tm_map(docs,removeWords,stopwords('english'))
docs<-tm_map(docs,stripWhitespace)
docs <- tm_map(docs, stemDocument, language = "english")  

stpwrds=c("bt","gs","pm","year","years","months","month","days","day")
docs<-tm_map(docs,removeWords,stpwrds)

#DTM
dtm<-DocumentTermMatrix(docs)

#print a summary
dtm

findFreqTerms(dtm, lowfreq=10)

#convert dtm to matrix
m <- as.matrix(dtm)


#compute distance between document vectors
d <- dist(m,method="euclidean")

#run hierarchical clustering using Ward's method
set.seed(12)
groups <- hclust(d,method="ward.D")
groups

fit <- cutree(groups,k=25) # cut tree into 5 clusters

#plot dendogram, use hang to ensure that labels fall below tree
plot(groups, cex=0.1, hang=-1)

clusterCut <- cutree(groups, 25)

#cut into 2 subtrees - try 3 and 5
rect.hclust(groups,25)



##################################################################################################################################################
                                                              ##K means clustering##
##################################################################################################################################################

#Inspect most popular words, minimum frequency of 20  
findFreqTerms(dtm, lowfreq=20) 

#25 cluster
set.seed(2)
fit.km = flexclust::kcca(d, 25, kccaFamily("kmeans"))
pred_train <- predict(fit.km)

#cluster plot
clusplot(df, pred_train, color=T, shade=T, labels=2, lines=0)


#saving model
saveRDS(fit.km,file = "kmeans_25.RDS")

#Merge cluster assignment back to keywords 
df <- data.frame(text = get("content", docs))
kw_with_cluster <- as.data.frame(cbind(df, pred_train)) 
names(kw_with_cluster) <- c("keyword", "kmeans")
clust_out <- cbind(pos_tag_out,kw_with_cluster)

#exporting 25 cluster output
write.csv(clust_out,"C:\\Shiva\\Projects\\GS_fault_Text_analytics\\Medallia\\Output\\likelihood_to_recommand_clust_25.csv")

#10 cluster
set.seed(1)
fit.km = flexclust::kcca(dtm, 10, kccaFamily("kmeans"))
pred_train <- predict(fit.km)

#cluster plot
clusplot(df, pred_train, color=TRUE, shade=TRUE, 
         lines=0)

#saving model
saveRDS(fit.km,file = "kmeans_10.RDS")


#Merge cluster assignment back to keywords 
df <- data.frame(text = get("content", docs))
kw_with_cluster <- as.data.frame(cbind(df, pred_train)) 
names(kw_with_cluster) <- c("keyword", "kmeans")
clust_out <- cbind(pos_tag_out,kw_with_cluster)

#exporting 10 cluster output
write.csv(clust_out,"C:\\Shiva\\Projects\\GS_fault_Text_analytics\\Medallia\\Output\\likelihood_to_recommand_clust_10.csv")

########################################################################################################################################################
                                   ## Supervised learning models for topic modelling##
#########################################################################################################################################################

#Clean text
stpwrds=c("bt","gs","pm","year","years","months","month","days","day")

#tm package # to remove words not relevant to polarity
l=removeWords(Dataset$Likelihood.to.Recommend.Comment.English,stpwrds) 

# to remove punctuations such as fullstop,comma and apostaphy
l=removePunctuation(l,preserve_intra_word_dashes=TRUE)

# to remove numbers
l=removeNumbers(l) 

# stopwords are words such as "I","me","is" etc
stpwrds1=stopwords("en") 
fix(stpwrds1)
l=removeWords(l,stpwrds1)


# CREATE THE DOCUMENT-TERM MATRIX
doc_matrix <- create_matrix(l, language="english", removeNumbers=TRUE,
                            stemWords=TRUE, removeSparseTerms=.998)

inspect(doc_matrix)

Dataset$Main_topic <- as.factor(Dataset$Main_topic)

# Create container
container <- create_container(doc_matrix, factor(Dataset$Main_topic), trainSize=1:800, testSize=801:885, virgin=FALSE)

#Traing model SVM and Random forest
SVM <- train_model(container,"SVM")
RF <- train_model(container,"RF")

# saving the models
saveRDS(SVM,file = "SVM.RDS")
saveRDS(RF,file = "RF.RDS")

#Testing model on test data
SVM_CLASSIFY <- classify_model(container, SVM)
RF_CLASSIFY <- classify_model(container, RF)


#summary
summary(SVM_CLASSIFY)
summary(RF_CLASSIFY)

#combining model output
classify <- cbind(SVM_CLASSIFY,RF_CLASSIFY)


## Create analytics
analytics <- create_analytics(container,SVM_CLASSIFY)
summary(analytics)


#cross validation
SVM <- cross_validate(container, 4, "SVM")
RF <- cross_validate(container, 4, "RF")

#exporting output
write.csv(classify, "C:\\Shiva\\Projects\\GS_fault_Text_analytics\\Medallia\\Output\\Supervised learning technique for classification\\DocumentSummary.csv")

#######################################################################################################################################################################
                                                     ##Sentiment Scoring for new text##
########################################################################################################################################################################

##Importing new data
new_data <- read.csv()

#Sentiment scoring
scores <- score.sentiment(new_data$Likelihood.to.Recommend.Comment, pos.words, neg.words, .progress='text')

#total score calculation: positive / negative / neutral
stat <- scores
stat <- mutate(stat,sentiment=ifelse(stat$score > 0, 'positive', ifelse(stat$score < 0, 'negative', 'neutral')))

sentiment_score_out <- cbind(new_data,stat)


#############################################################################################################################################################
                                                          ##Word cloud for new text##
##############################################################################################################################################################

#separate text by sentiment
sents = levels(factor(sentiment_score_out$sentiment))

# get the labels and percents
labels <- lapply(sents, function(x) paste(x,format(round((length((sentiment_score_out
                                                                  [sentiment_score_out$sentiment ==x,])$text)
                                                          /length(sentiment_score_out$sentiment)*100),2),
                                                   nsmall=2),"%"))
nemo = length(sents)

emo.docs = rep("", nemo)
for (i in 1:nemo)
{
  tmp = sentiment_score_out[sentiment_score_out$sentiment == sents[i],]$text
  
  emo.docs[i] = paste(tmp,collapse=" ")
}

# remove stopwords
emo.docs = removeWords(emo.docs, stopwords("english"))
stpwrds=c("BT","GS","Global services","services","take","global","also","yet","years","see",
          "two","can","however","months","times","time","past","now","one","good","still","btgs","csr","year","day",
          "gov","tui","get","wan","will","last","don","set","way","key","cet",
          "want","sev","put","want","first","say","case","gsip","even")

emo.docs = removeWords(emo.docs, stpwrds)

# to remove numbers
emo.docs=removeNumbers(emo.docs) 

#remove english remove words
stpwrds1=stopwords("english")
emo.docs = removeWords(emo.docs, stpwrds1)

#remove punctuation
emo.docs <- removePunctuation(emo.docs)

#create corpus
corpus = Corpus(VectorSource(emo.docs))

#create TDM matrix
tdm = TermDocumentMatrix(corpus)
tdm = as.matrix(tdm)

colnames(tdm) = labels

# Sentiment comparison word cloud
set.seed(1213)
comparison.cloud(tdm, colors = c(negative="red",neutral="blue",positive="green"),max.words=300,
                 scale = c(3,.5), random.order = FALSE, title.size = 1.0)

#########################################################################################################################33#############################################
                                                     ##Testing on whole data set##
########################################################################################################################################################################
##Clustering##

fit.km_25 <- readRDS("kmeans_25.RDS")
fit.km_10 <- readRDS("kmeans_10.RDS")

##Assigning new text to cluster
##25 cluster
pred_train <- predict(new_data,fit.km_25)

#10 cliuster
pred_train <- predict(new_data,fit.km_10)


####################################################################################################################################################
                                                  ##Supervised learning topic prediction##
#####################################################################################################################################################

#Reading trained model
SVM <- readRDS("SVM.RDS")
RF <- readRDS("RF.RDS")


#Clean text
stpwrds=c("bt","gs","pm","year","years","months","month","days","day")

#tm package # to remove words not relevant to polarity
k=removeWords(new_data$Likelihood.to.Recommend.Comment.English,stpwrds)

# to remove punctuations such as fullstop,comma and apostaphy
k=removePunctuation(k,preserve_intra_word_dashes=TRUE)

# to remove numbers
k=removeNumbers(k) 

# stopwords are words such as "I","me","is" etc
stpwrds1=stopwords("en") 
fix(stpwrds1)
k=removeWords(k,stpwrds1)

# create a prediction document term matrix
predMatrix <- create_matrix(k, originalMatrix=doc_matrix)

# create the corresponding container
predSize = length(k)
predictionContainer <- create_container(predMatrix, labels=rep(0,predSize), testSize=1:predSize, virgin=FALSE)

# predict class
results <- classify_model(predictionContainer, SVM)
results

svm_text <- cbind(test,results)
round(prop.table(table(svm_text$Main_topic == svm_text$SVM_LABEL)), 3)

#exporting output
write.csv(svm_text,"C:\\Shiva\\Projects\\GS_fault_Text_analytics\\Medallia\\Output\\Supervised learning technique for classification\\svm_test.csv")

#####################################################################################################################################################

