
##-----------------------------Sentiment Analysis#######################################
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


senti <- read.csv("C:\\Shiva\\Projects\\GS_fault_Text_analytics\\Medallia\\Data\\2016-18 All Medallia Surveys Aug 31_1.csv",header=TRUE)

#After reading u should do this preprocessing on the text coloumn
df_corpus = Corpus(VectorSource(senti$Anything.Else.Comment))

as.character(df_corpus[[1]])
inspect(df_corpus[1:10])

#To avoid the issue with DocumentTermMatrix method, use one of following solutions:
#1) Adding content_transformer avoids the type conversion issue with non-standard transformations
#2) Add the tm_map(PlainTextDocument) after all the cleaning is done

df_corpus_clean = tm_map(df_corpus, content_transformer(tolower))
df_corpus_clean = tm_map(df_corpus_clean, removeNumbers)

df_corpus_clean = tm_map(df_corpus_clean, removePunctuation)
df_corpus_clean = tm_map(df_corpus_clean, removeWords, stopwords(kind="en")) 

df_corpus_clean = tm_map(df_corpus_clean, stripWhitespace)
df_corpus_clean = tm_map(df_corpus_clean, stemDocument) 
as.character(df_corpus_clean[[1]])


df_corpus_clean = DocumentTermMatrix(df_corpus_clean,control=list(minWordLength=2))
inspect(df_corpus_clean)
dim(df_corpus_clean)

df_corpus_clean = removeSparseTerms(df_corpus_clean,0.98)
dim(df_corpus_clean)
df_corpus <- as.matrix(df_corpus)

#####Positive and Negative words#############

Positive <- read.csv("C:\\Shiva\\Projects\\GS_fault_Text_analytics\\Medallia\\Data\\Positive words.csv")
Negative <- read.csv("C:\\Shiva\\Projects\\GS_fault_Text_analytics\\Medallia\\Data\\Negative words.csv")

##########Classify emotion##############


senti$Anything.Else.Comment=as.character(senti$Anything.Else.Comment)
senti$Anything.Else.Comment=tolower(senti$Anything.Else.Comment)

wordcloud(senti$Anything.Else.Comment,min.freq=2)#develop word cloud #wordcloud package
stpwrds=c("BT","GS")

l=removeWords(senti$Anything.Else.Comment,stpwrds) #tm package # to remove words not relevant to polarity
l=removePunctuation(l,preserve_intra_word_dashes=TRUE) # to remove punctuations such as fullstop,comma and apostaphy

l=removeNumbers(l) # to remove numbers
stpwrds1=stopwords("en") # stopwords are words such as "I","me","is" etc

fix(stpwrds1)
l=removeWords(l,stpwrds1)# removes given stpwrds1

sl=c(1:length(l)) # serial number for review
a=data.frame(sl,senti,l)
a$senti_val=get_sentiment(l)#syuzhet package for finding sentiment value of reviews


for(n in 1:length(a[,1]))# sentiment value converted to words
{
  if (a$senti_val[n] > 0) a$sentiword[n]="Positive" else if (a$senti_val[n]<0) a$sentiword[n]="Negative" else a$sentiword[n]="Neutral"
}

s=get_nrc_sentiment(l)[,c(9,10)]#to get the number of negative or positive words in a review
for(n in 1:length(a[,1])) # to set the volume of polarity in data
{
  if (a$sentiword[n]=="positive") a$sentivol[n]=s[n,2] else a$sentivol[n]=s[n,1]
}

ggplot(a,aes(sl,sentiword))+geom_point(aes(shape=factor(a$sentiword),size=a$sentivol))# sentiments of reviews shown through graph as per volume of polarity and type of polarity

####Write sentiment out

write.csv(a,"C:\\Shiva\\Projects\\GS_fault_Text_analytics\\Medallia\\Output\\sentiment_out_anything_else.csv")


#########------------------------Word cloud-------------------------------------##########
#separate text by sentiment
sents = levels(factor(a$sentiword))

# get the labels and percents
labels <- lapply(sents, function(x) paste(x,format(round((length((a[a$sentiword ==x,])$text)/length(a$sentiword)*100),2),nsmall=2),"%"))

nemo = length(sents)
emo.docs = rep("", nemo)
for (i in 1:nemo)
{
  tmp = a[a$sentiword == sents[i],]$text
  
  emo.docs[i] = paste(tmp,collapse=" ")
}

# remove stopwords
emo.docs = removeWords(emo.docs, stopwords("english"))
corpus = Corpus(VectorSource(emo.docs))

tdm = TermDocumentMatrix(corpus)
tdm = as.matrix(tdm)
colnames(tdm) = labels

# comparison word cloud
comparison.cloud(tdm, colors = brewer.pal(nemo, "Dark2"),scale = c(3,.5), random.order = FALSE, title.size = 1.5)



#######################################################################################################
###Method 2############

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

pos <- scan('C:\\Shiva\\Projects\\GS_fault_Text_analytics\\Medallia\\Data\\Positive words.csv', what='character', comment.char=';') #folder with positive dictionary
neg <- scan('C:\\Shiva\\Projects\\GS_fault_Text_analytics\\Medallia\\Data\\Negative words.csv', what='character', comment.char=';') #folder with negative dictionary

pos.words <- c(pos, 'upgrade')
neg.words <- c(neg, 'wtf', 'wait', 'waiting', 'epicfail')

Dataset <- senti
Dataset$Anything.Else.Comment <- as.character(Dataset$Anything.Else.Comment)

senti$Anything.Else.Comment=tolower(senti$Anything.Else.Comment)
wordcloud(senti$Anything.Else.Comment,min.freq=10)#develop word cloud #wordcloud package


#Dataset2=Dataset[1:3,]
scores <- score.sentiment(senti$Anything.Else.Comment, pos.words, neg.words, .progress='text')

#total score calculation: positive / negative / neutral
stat <- scores
stat <- mutate(stat,sentiment=ifelse(stat$score > 0, 'positive', ifelse(stat$score < 0, 'negative', 'neutral')))

sentiment_score_out <- cbind(senti,stat)

#########------------------------Word cloud-------------------------------------##########
#separate text by sentiment
sents = levels(factor(sentiment_score_out$sentiment))

# get the labels and percents
labels <- lapply(sents, function(x) paste(x,format(round((length((sentiment_score_out[sentiment_score_out$sentiment ==x,])$text)/length(sentiment_score_out$sentiment)*100),2),nsmall=2),"%"))
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
emo.docs=removeNumbers(emo.docs) # to remove numbers

stpwrds1=stopwords("english")
emo.docs = removeWords(emo.docs, stpwrds1)

corpus = Corpus(VectorSource(emo.docs))
tdm = TermDocumentMatrix(corpus)

tdm = as.matrix(tdm)
colnames(tdm) = labels

# comparison word cloud
set.seed(1213)
comparison.cloud(tdm, colors = c(negative="red",neutral="blue",positive="green"),max.words=200,
                 scale = c(3,.5), random.order = FALSE, title.size = 1.0)

####--------------------------------Bi gram sentiment Word cloud(Anything else comment)------------------------------------############
library(dplyr)
library(tidytext)
library(janeaustenr)
library(quanteda)

senti$Anything.Else.Comment <- as.character(senti$Anything.Else.Comment)
mydfm <- dfm(senti$Anything.Else.Comment,ngrams=2)

d <- data_frame(txt = senti$Anything.Else.Comment)

d %>%
  unnest_tokens(word, txt)

d %>%
  unnest_tokens(sentence, txt, token = "sentences")

bigram <- d %>%
  unnest_tokens(ngram, txt, token = "ngrams", n = 2)


bigram$ngram <- factor(bigram$ngram)

d %>%
  count(ngram, sort = TRUE)

bigram %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word) %>%
  count(word1, word2, sort = TRUE)


stpwrds=c("BT","GS","services","also","yet","years","see",
          "two","can","however","months","times","time","now","one",
          "support","good","still","btgs","csr","year","day",
          "gov","tui","get","wan","will","last","don","set","way","key")
d = removeWords(d, stpwrds)
d=removeNumbers(d) # to remove numbers
stpwrds1=stopwords("english")
d = removeWords(d, stpwrds1)
d <- removePunctuation(d)

inaugbigramsDfm <- dfm(d, ngrams = 2, ignoredFeatures = stopwords("SMART"))
inaugbigramsDfm = as.matrix(inaugbigramsDfm)

plot(inaugbigramsDfm, min.freq = 2, random.order = FALSE, 
     colors = sample(colors()[2:128]))

# comparison word cloud
set.seed(1213)
comparison.cloud(inaugbigramsDfm, colors = c(negative="red",neutral="blue",positive="green"),max.words=200,
                 scale = c(3,.5), random.order = FALSE, title.size = 1.0)



#library(RWeka)
#corpus = VCorpus(VectorSource(emo.docs))
#BigramTokenizer<-function(x){RWeka::NGramTokenizer(x,RWeka::Weka_control(min=2,max=2))}

#BigramTokenizer<-function(x){NGramTokenizer(x,Weka_control(min=2,max=2))}
#tdm.bigram = TermDocumentMatrix(corpus,
                                #control = list(tokenize = BigramTokenizer))
#inspect(tdm.bigram)

freq = sort(rowSums(as.matrix(tdm1)),decreasing = TRUE)
freq.df = data.frame(word=names(freq), freq=freq)
head(freq.df, 20)

pal=brewer.pal(8,"Blues")
pal=pal[-(1:3)]
wordcloud(freq.df$word,freq.df$freq,max.words=100,random.order = F, colors=pal)

ggplot(head(freq.df,15), aes(reorder(word,freq), freq)) +
  geom_bar(stat = "identity") + coord_flip() +
  xlab("Bigrams") + ylab("Frequency") +
  ggtitle("Most frequent bigrams")

# comparison word cloud
set.seed(121)
comparison.cloud(freq.df$word,freq.df$freq, colors = c(negative="red",neutral="blue",positive="green"),max.words=200,
                 scale = c(3,.5), random.order = FALSE, title.size = 1.0)


write.csv(sentiment_score_out, file='C:\\Shiva\\Projects\\GS_fault_Text_analytics\\Medallia\\Output\\sentiment_out_method2.csv', row.names=TRUE) #save evaluation results

#################################################################################################################################################################################
#################################Likelihood recommend comment#####################################

Dataset <- read.csv("C:\\Shiva\\Projects\\GS_fault_Text_analytics\\Medallia\\Data\\2016-18 All Medallia Surveys Aug 31_2.csv",stringsAsFactors = F)

pos <- scan('C:\\Shiva\\Projects\\GS_fault_Text_analytics\\Medallia\\Data\\Positive words.csv', what='character', comment.char=';') #folder with positive dictionary
neg <- scan('C:\\Shiva\\Projects\\GS_fault_Text_analytics\\Medallia\\Data\\Negative words.csv', what='character', comment.char=';') #folder with negative dictionary

pos.words <- c(pos, 'upgrade')
neg.words <- c(neg, 'wtf', 'wait', 'waiting', 'epicfail')

Dataset$Likelihood.to.Recommend.Comment <- as.character(Dataset$Likelihood.to.Recommend.Comment)
Dataset$Likelihood.to.Recommend.Comment=tolower(Dataset$Likelihood.to.Recommend.Comment)

#develop word cloud #wordcloud package
wordcloud(Dataset$Likelihood.to.Recommend.Comment,min.freq=10)

#Dataset2=Dataset[1:3,]
scores <- score.sentiment(Dataset$Likelihood.to.Recommend.Comment, pos.words, neg.words, .progress='text')

#total score calculation: positive / negative / neutral
stat <- scores
stat <- mutate(stat,sentiment=ifelse(stat$score > 0, 'positive', ifelse(stat$score < 0, 'negative', 'neutral')))

sentiment_score_out <- cbind(Dataset,stat)

#########------------------------Word cloud-------------------------------------##########
#separate text by sentiment
sents = levels(factor(sentiment_score_out$sentiment))

# get the labels and percents
labels <- lapply(sents, function(x) paste(x,format(round((length((sentiment_score_out[sentiment_score_out$sentiment ==x,])$text)/length(sentiment_score_out$sentiment)*100),2),nsmall=2),"%"))
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
emo.docs=removeNumbers(emo.docs) # to remove numbers

stpwrds1=stopwords("english")
emo.docs = removeWords(emo.docs, stpwrds1)

emo.docs <- removePunctuation(emo.docs)
corpus = Corpus(VectorSource(emo.docs))

tdm = TermDocumentMatrix(corpus)
tdm = as.matrix(tdm)

colnames(tdm) = labels

# comparison word cloud
set.seed(1213)
comparison.cloud(tdm, colors = c(negative="red",neutral="blue",positive="green"),max.words=300,
                 scale = c(3,.5), random.order = FALSE, title.size = 1.0)

#save evaluation results
write.csv(sentiment_score_out, file='C:\\Shiva\\Projects\\GS_fault_Text_analytics\\Medallia\\Output\\sentiment_out_Likelihood to recommend comment2.csv', row.names=TRUE) 

######################################################################################################################################################################################
##########POS tagging#######################

library(NLP)
library(openNLP)

txt<- c("BT Global Services is an extremely professional organization and I know my clients will be taken care of with them.")

extractPOS <- function(x, thisPOSregex) {
  x <- as.String(x)
  wordAnnotation <- annotate(x, list(Maxent_Sent_Token_Annotator(), Maxent_Word_Token_Annotator()))
  POSAnnotation <- annotate(x, Maxent_POS_Tag_Annotator(), wordAnnotation)
  POSwords <- subset(POSAnnotation, type == "word")
  tags <- sapply(POSwords$features, '[[', "POS")
  thisPOSindex <- grep(thisPOSregex, tags)
  tokenizedAndTagged <- sprintf("%s/%s", x[POSwords][thisPOSindex], tags[thisPOSindex])
  untokenizedAndTagged <- paste(tokenizedAndTagged, collapse = " ")
  untokenizedAndTagged
}

lapply(txt, extractPOS, "NN")   #for noun
lapply(txt, extractPOS, "JJ")       # for adjective
lapply(txt, extractPOS, "VB")     # for verb  

#########################################################################################
##Hierarical Clustering#####

pos_tag_out <- read.csv("C:\\Shiva\\Projects\\GS_fault_Text_analytics\\Medallia\\Data\\likelihood_to_recommand_POS.csv",stringsAsFactors = F)
#Create Corpus
docs <- Corpus(VectorSource(pos_tag_out$POS_tagging))

#inspect a particular document
writeLines(as.character(docs[[30]]))

#Transform to lower case
docs <- tm_map(docs,content_transformer(tolower))

#remove potentiallyy problematic symbols
toSpace <- content_transformer(function(x, pattern) { return (gsub(pattern, " ", x))})
docs <- tm_map(docs, toSpace, ",")

#remove punctuation
docs <- tm_map(docs, removePunctuation)

#Strip digits
docs <- tm_map(docs, removeNumbers)

stopwords <- c("BT", "Global")

docs <- tm_map(docs, removeWords,stopwords)

#remove whitespace
docs <- tm_map(docs, stripWhitespace)
writeLines(as.character(docs[[30]]))

docs <- tm_map(docs,stemDocument)

#DTM
dtm <- TermDocumentMatrix(docs, control = list(minWordLength = 1))

#print a summary
dtm

findFreqTerms(dtm, lowfreq=10)

#convert dtm to matrix
m <- as.matrix(dtm)


#shorten rownames for display purposes
rownames(m) <- paste(substring(rownames(m),1,3),rep("..",nrow(m)),
                                           substring(rownames(m), nchar(rownames(m))-12,nchar(rownames(m))-4))
#compute distance between document vectors
d <- dist(m,method="euclidean")

#run hierarchical clustering using Ward's method
groups <- hclust(d,method="ward.D")
groups

fit <- cutree(groups,k=25) # cut tree into 5 clusters


#plot dendogram, use hang to ensure that labels fall below tree
plot(groups, cex=0.5, hang=-1)

clusterCut <- cutree(groups, 25)

#cut into 2 subtrees - try 3 and 5
rect.hclust(groups,5)


#########################################################################################
#########K means clustering###########

library("RSiteCatalyst") 
library("RTextTools") #Loads many packages useful for text mining 


#Create document-term matrix, passing data cleaning options 
#Stem the words to avoid multiples of similar words 
#Need to set wordLength to minimum of 1 because "r" a likely term 
dtm <- create_matrix(pos_tag_out$POS_tagging,  
                                              stemWords=TRUE,  
                                              removeStopwords=FALSE,  
                                              minWordLength=1, 
                                              removePunctuation= TRUE)

dtm <- as.matrix(dtm)

#Inspect most popular words, minimum frequency of 20  
findFreqTerms(dtm, lowfreq=20) 

#scale
dtm <- scale(dtm)


#I think there are 5 main topics: Data Science, Web Analytics, R, Julia, Wordpress 
set.seed(124)
kmeans <-  kmeans(dtm,25)

saveRDS(kmeans,file = "kmeans_25.RDS")


#Merge cluster assignment back to keywords 
kw_with_cluster <- as.data.frame(cbind(pos_tag_out$POS_tagging, kmeans$cluster)) 
names(kw_with_cluster) <- c("keyword", "kmeans5")

clust_out <- cbind(pos_tag_out,kw_with_cluster)

write.csv(clust_out,"C:\\Shiva\\Projects\\GS_fault_Text_analytics\\Medallia\\Output\\likelihood_to_recommand_clust_25.csv")
 
#I think there are 5 main topics: Data Science, Web Analytics, R, Julia, Wordpress 
set.seed(124)
kmeans1 <-  kmeans(dtm,10)

saveRDS(kmeans1,file = "kmeans_10.RDS")


#Merge cluster assignment back to keywords 
kw_with_cluster <- as.data.frame(cbind(pos_tag_out$POS_tagging, kmeans$cluster)) 
names(kw_with_cluster) <- c("keyword", "kmeans5")

clust_out <- cbind(pos_tag_out,kw_with_cluster)

write.csv(clust_out,"C:\\Shiva\\Projects\\GS_fault_Text_analytics\\Medallia\\Output\\likelihood_to_recommand_clust_25.csv")




