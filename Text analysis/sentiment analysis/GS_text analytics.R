library(textmineR)
library(tm)
library(caret)
rm(list = ls())
setwd("C:\\Shiva\\Projects\\GS_fault_Text_analytics\\Medallia\\Output")
# Load some data into the workspace 

text_raw1= read.csv("C:\\Shiva\\Projects\\GS_fault_Text_analytics\\Medallia\\Data\\2016-18 All Medallia Surveys Aug 31_1.csv",header=TRUE)
#colnames(text_raw1) <- c("date", "note")
text_raw1$Anything.Else.Comment = as.character(text_raw1$Anything.Else.Comment)

#code to convert Redw log file date to a format needed.
#text_raw1$date = as.character(text_raw1$date)
#formated_Date <- sapply(text_raw1$date,format_date_function)

#format_date_function <- function(each_date)
#{
# split_list <- strsplit(each_date," ")[[1]]
# year <- strsplit(split_list[5],"_")[[1]][1]
#format_time <- paste(year,as.character(match(split_list[2],month.abb)),split_list[3],sep = "-")
#format_time <- paste(format_time,split_list[4],sep = " ")
# format_time
#}
#text_raw1$formated_Date <- formated_Date


#text_raw1$formated_Date <- as.character(text_raw1$formated_Date)
#write.csv(text_raw1,"automated_timestamp_format.csv",row.names = FALSE)

# Create a document term matrix

cosmos_dtm <- CreateDtm(text_raw1$Anything.Else.Comment, 
                        ngram_window = c(1, 2))

dim(cosmos_dtm)

# explore basic frequencies & curate vocabulary
term_frequency_cosmos <- TermDocFreq(dtm = cosmos_dtm)

# Eliminate words appearing less than 2 times or in more than half of the
# documents
cosmos_needed_Terms <- term_frequency_cosmos$term[ term_frequency_cosmos$term_freq > 2 & term_frequency_cosmos$doc_freq < nrow(cosmos_dtm) / 2 ]

cosmos_dtm <- cosmos_dtm[ , cosmos_needed_Terms]

dim(cosmos_dtm)


#fine tuning to find appropriate number of topics
topics_list <- seq(5, 150, by = 5)

model_lda_directory <- paste0("lda_models", digest::digest(cosmos_needed_Terms, algo = "sha1"))

if (!dir.exists(model_lda_directory)) dir.create(model_lda_directory)

tunable_lda_list <- TmParallelApply(X = topics_list, FUN = function(k){
  filename = file.path(model_lda_directory, paste0(k, "_lda_topics.rda"))
  
  if (!file.exists(filename)) {
    lda_model <- FitLdaModel(dtm = cosmos_dtm, k = k, iterations = 1000)
    lda_model$k <- k
    lda_model$topic_coherence <- CalcProbCoherence(phi = lda_model$phi, dtm = cosmos_dtm, M = 5)
    save(lda_model, file = filename)
  } else {
    load(filename)
  }
  lda_model
}, export=c("cosmos_dtm", "model_lda_directory")) 






max_coherence_param <- data.frame(k = sapply(tunable_lda_list, function(x) nrow(x$phi)), 
                                  topics_coherence = sapply(tunable_lda_list, function(x) mean(x$topic_coherence)), 
                                  stringsAsFactors = FALSE)




plot(max_coherence_param, type = "o")




tuned_lda_model <- tunable_lda_list[ which.max(max_coherence_param$topics_coherence) ][[ 1 ]]
tuned_lda_model <- FitLdaModel(dtm = cosmos_dtm, k = 10, iterations = 5000)

GetTopicTerms <- function(phi, req_topic_top_Terms){
  
  result_topterms <- apply(phi, 1, function(x){
    names(x)[ order(x, decreasing=TRUE) ][ 1:req_topic_top_Terms]
  })
  
  return(result_topterms)
}
tuned_lda_model$topic_terms <- GetTopicTerms(phi = tuned_lda_model$phi, req_topic_top_Terms = 20)

tuned_lda_model$class <-colnames(tuned_lda_model$theta)[max.col(tuned_lda_model$theta,ties.method="first")]

text_raw1$topic <- tuned_lda_model$class
write.csv(text_raw1,"Anything_else_comment_lda",row.names = FALSE)
# to extract names of top 2 topics 

top_2_topic_probs <- t(apply(tuned_lda_model$theta,1,function(xx)tail(sort(xx),2)))

#to extract probabilities of top 2 topics
top_2_topic_names <- t(apply(tuned_lda_model$theta,1,function(xx)tail(names(sort(xx)),2)))

# naming the top two topics with their probabilities in the output data frame
colnames(top_2_topic_names) <- c("top_2_topic","top_topic")

colnames(top_2_topic_probs) <- c("top_2_topic_probability","top_1_topic_probability")

text_raw1$top_2_topic_name <- top_2_topic_names[,"top_2_topic"]

text_raw1$top_2_topic_probability <- top_2_topic_probs[,"top_2_topic_probability"]

text_raw1$top_most_topic <- tuned_lda_model$class

text_raw1$top_most_topic_probability <- top_2_topic_probs[,"top_1_topic_probability"]

write.csv(text_raw1,"Anything_else_comment_top_2_topic.csv",row.names = FALSE)


write.csv(tuned_lda_model$topic_terms,"Anything_else_comment_top_terms_topics.csv",row.names = FALSE)
re_terms_df <- read.csv("Anything_else_comment_top_terms_topics.csv", header=TRUE)

#automatic naming with top 3 terms


concat_top_3_terms_function <- function(each_coloumn)
{
  name_of_topic <- paste(each_coloumn[[1]],each_coloumn[[2]],each_coloumn[[3]],sep="_")
  name_of_topic
  
}

names_mapping <- sapply(re_terms_df,concat_top_3_terms_function)

names_topics <- names(names_mapping)
names_mapping <- unname(names_mapping)

mapping_df = data.frame(topics=names_topics, name_from_top_3_terms = names_mapping)
dim(mapping_df)
write.csv(mapping_df,"Anythingelse_topic_mapping_from_top_3_terms.csv",row.names = FALSE)


################################################################################
##Word cloud
library(SnowballC)
library(wordcloud)

data_wc <- read.csv("C:\\Shiva\\Projects\\GS_fault_Text_analytics\\data_wc.csv",stringsAsFactors = F)

# Load the data as a corpus
docs <- Corpus(VectorSource(data_wc$lr1))

inspect(docs)

dtm <- TermDocumentMatrix(docs)

m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)


set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 4,
          max.words=128, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

###########################################################################################

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
# for stemming the words
library(SnowballC)
# libraries required by caret
library(klaR)
library(e1071)
# for the Naive Bayes modelling
library(caret)
# to process the text into a corpus
library(tm)

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
inspect(df_corpus_clean[1:10])

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

library(tm)
library(wordcloud)
library(syuzhet)
library(ggplot2)


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
comparison.cloud(tdm, colors = brewer.pal(nemo, "Dark2"),
                 scale = c(3,.5), random.order = FALSE, title.size = 1.5)



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
stpwrds1=stopwords("en")
emo.docs = removeWords(emo.docs, stpwrds1)
corpus = Corpus(VectorSource(emo.docs))
tdm = TermDocumentMatrix(corpus)
tdm = as.matrix(tdm)
colnames(tdm) = labels

# comparison word cloud
set.seed(1213)
comparison.cloud(tdm, colors = c(negative="red",neutral="blue",positive="green"),max.words=200,
                 scale = c(3,.5), random.order = FALSE, title.size = 1.0)

####--------------------------------Bi gram Word cloud------------------------------------############
library(RWeka)
#corpus = VCorpus(VectorSource(emo.docs))
#BigramTokenizer<-function(x){RWeka::NGramTokenizer(x,RWeka::Weka_control(min=2,max=2))}

BigramTokenizer<-function(x){NGramTokenizer(x,Weka_control(min=2,max=2))}
tdm.bigram = TermDocumentMatrix(corpus,
                                control = list(tokenize = BigramTokenizer))


Sys.setenv(JAVA_HOME='C:\\Program Files (x86)\\Java\\jre6')
#inspect(tdm.bigram)
freq = sort(rowSums(as.matrix(tdm.bigram)),decreasing = TRUE)
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
##Likelihood recommend comment#####

pos <- scan('C:\\Shiva\\Projects\\GS_fault_Text_analytics\\Medallia\\Data\\Positive words.csv', what='character', comment.char=';') #folder with positive dictionary
neg <- scan('C:\\Shiva\\Projects\\GS_fault_Text_analytics\\Medallia\\Data\\Negative words.csv', what='character', comment.char=';') #folder with negative dictionary

pos.words <- c(pos, 'upgrade')
neg.words <- c(neg, 'wtf', 'wait', 'waiting', 'epicfail')

Dataset <- read.csv("C:\\Shiva\\Projects\\GS_fault_Text_analytics\\Medallia\\Data\\2016-18 All Medallia Surveys Aug 31_2.csv",stringsAsFactors = F)

Dataset$Likelihood.to.Recommend.Comment <- as.character(Dataset$Likelihood.to.Recommend.Comment)

Dataset$Likelihood.to.Recommend.Comment=tolower(Dataset$Likelihood.to.Recommend.Comment)

wordcloud(Dataset$Likelihood.to.Recommend.Comment,min.freq=10)#develop word cloud #wordcloud package


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
stpwrds1=stopwords("en")
emo.docs = removeWords(emo.docs, stpwrds1)
corpus = Corpus(VectorSource(emo.docs))
tdm = TermDocumentMatrix(corpus)
tdm = as.matrix(tdm)
colnames(tdm) = labels

# comparison word cloud
set.seed(1213)
comparison.cloud(tdm, colors = c(negative="red",neutral="blue",positive="green"),max.words=300,
                 scale = c(3,.5), random.order = FALSE, title.size = 1.0)


write.csv(sentiment_score_out, file='C:\\Shiva\\Projects\\GS_fault_Text_analytics\\Medallia\\Output\\sentiment_out_Likelihood to recommend comment2.csv', row.names=TRUE) #save evaluation results


