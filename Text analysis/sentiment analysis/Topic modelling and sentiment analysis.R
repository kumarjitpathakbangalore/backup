###############################################################################################################
##Topic modelling and sentiment analysis##
################################################################################################################
rm(list = ls())
setwd("C:\\Shiva\\Projects\\GS_fault_Text_analytics\\Account Improvement\\R code")

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
library(textmineR)
library(caret)
###################################################################################################################
##LDA## 
####################################################################################################################

text_raw1= read.csv("C:\\Shiva\\Projects\\GS_fault_Text_analytics\\Account Improvement\\Data\\split_comments.csv",header=TRUE)

text_raw <- text_raw1
text_raw$Comments = as.character(text_raw$Comments)


# Create a document term matrix
cosmos_dtm <- CreateDtm(text_raw$Comments, 
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
tuned_lda_model <- FitLdaModel(dtm = cosmos_dtm, k = 75, iterations = 1000)

GetTopicTerms <- function(phi, req_topic_top_Terms){
  
  result_topterms <- apply(phi, 1, function(x){
    names(x)[ order(x, decreasing=TRUE) ][ 1:req_topic_top_Terms]
  })
  
  return(result_topterms)
}
tuned_lda_model$topic_terms <- GetTopicTerms(phi = tuned_lda_model$phi, req_topic_top_Terms = 20)

tuned_lda_model$class <-colnames(tuned_lda_model$theta)[max.col(tuned_lda_model$theta,ties.method="first")]

text_raw$topic <- tuned_lda_model$class

write.csv(text_raw,"C:\\Shiva\\Projects\\GS_fault_Text_analytics\\Account Improvement\\Output\\topic_mappings_account_summary.v1.csv",row.names = FALSE)

# to extract names of top 2 topics 
top_2_topic_probs <- t(apply(tuned_lda_model$theta,1,function(xx)tail(sort(xx),2)))

#to extract probabilities of top 2 topics
top_2_topic_names <- t(apply(tuned_lda_model$theta,1,function(xx)tail(names(sort(xx)),2)))

# naming the top two topics with their probabilities in the output data frame
colnames(top_2_topic_names) <- c("top_2_topic","top_topic")

colnames(top_2_topic_probs) <- c("top_2_topic_probability","top_1_topic_probability")

text_raw$top_2_topic_name <- top_2_topic_names[,"top_2_topic"]

text_raw$top_2_topic_probability <- top_2_topic_probs[,"top_2_topic_probability"]

text_raw$top_most_topic <- tuned_lda_model$class

text_raw$top_most_topic_probability <- top_2_topic_probs[,"top_1_topic_probability"]

write.csv(text_raw,"C:\\Shiva\\Projects\\GS_fault_Text_analytics\\Account Improvement\\Output\\top_2_topic_probabilty_account_summary.csv",row.names = FALSE)

write.csv(tuned_lda_model$topic_terms,"C:\\Shiva\\Projects\\GS_fault_Text_analytics\\Account Improvement\\Output\\top_terms_topics_account_summary.csv",row.names = FALSE)

tuned_lda_model$mapping[tuned_lda_model$class == "t_1"] = "Curcuit access"
tuned_lda_model$mapping[tuned_lda_model$class == "t_2"] = "Escalation"
tuned_lda_model$mapping[tuned_lda_model$class == "t_3"] = "Lack Resiliency"
tuned_lda_model$mapping[tuned_lda_model$class == "t_4"] = "Service desk"
tuned_lda_model$mapping[tuned_lda_model$class == "t_5"] = "Invoice"
tuned_lda_model$mapping[tuned_lda_model$class == "t_6"] = "Service Account"
tuned_lda_model$mapping[tuned_lda_model$class == "t_7"] = "Lead time response"
tuned_lda_model$mapping[tuned_lda_model$class == "t_8"] = "SLA performance"
tuned_lda_model$mapping[tuned_lda_model$class == "t_9"] = "Site installation"
tuned_lda_model$mapping[tuned_lda_model$class == "t_10"] = "SIP closure"
tuned_lda_model$mapping[tuned_lda_model$class == "t_11"] = "Migration"
tuned_lda_model$mapping[tuned_lda_model$class == "t_12"] = "routers"
tuned_lda_model$mapping[tuned_lda_model$class == "t_13"] = "backlog"
tuned_lda_model$mapping[tuned_lda_model$class == "t_14"] = "Client service"
tuned_lda_model$mapping[tuned_lda_model$class == "t_15"] = "fibre"
tuned_lda_model$mapping[tuned_lda_model$class == "t_16"] = "Circuit delivery"
tuned_lda_model$mapping[tuned_lda_model$class == "t_17"] = "Product quality"
tuned_lda_model$mapping[tuned_lda_model$class == "t_18"] = "Quality delivery"
tuned_lda_model$mapping[tuned_lda_model$class == "t_19"] = "Long support"
tuned_lda_model$mapping[tuned_lda_model$class == "t_20"] = "Migration"
tuned_lda_model$mapping[tuned_lda_model$class == "t_21"] = "Customer face improved"
tuned_lda_model$mapping[tuned_lda_model$class == "t_22"] = "Network Issues"
tuned_lda_model$mapping[tuned_lda_model$class == "t_23"] = "Weekly calls progress"
tuned_lda_model$mapping[tuned_lda_model$class == "t_24"] = "Technical teams"
tuned_lda_model$mapping[tuned_lda_model$class == "t_25"] = "others"
tuned_lda_model$mapping[tuned_lda_model$class == "t_26"] = "others"
tuned_lda_model$mapping[tuned_lda_model$class == "t_27"] = "others"
tuned_lda_model$mapping[tuned_lda_model$class == "t_28"] = "others"
tuned_lda_model$mapping[tuned_lda_model$class == "t_29"] = "Good progress"
tuned_lda_model$mapping[tuned_lda_model$class == "t_30"] = "tbc"
tuned_lda_model$mapping[tuned_lda_model$class == "t_31"] = "Group action"
tuned_lda_model$mapping[tuned_lda_model$class == "t_32"] = "Lead times delivery"
tuned_lda_model$mapping[tuned_lda_model$class == "t_33"] = "nps survey"
tuned_lda_model$mapping[tuned_lda_model$class == "t_34"] = "others"
tuned_lda_model$mapping[tuned_lda_model$class == "t_35"] = "noc update"
tuned_lda_model$mapping[tuned_lda_model$class == "t_36"] = "Site bound"
tuned_lda_model$mapping[tuned_lda_model$class == "t_37"] = "uk sites orders"
tuned_lda_model$mapping[tuned_lda_model$class == "t_38"] = "sev_green"
tuned_lda_model$mapping[tuned_lda_model$class == "t_39"] = "others"
tuned_lda_model$mapping[tuned_lda_model$class == "t_40"] = "corrective_actions"
tuned_lda_model$mapping[tuned_lda_model$class == "t_41"] = "customer_feedback"
tuned_lda_model$mapping[tuned_lda_model$class == "t_42"] = "others"
tuned_lda_model$mapping[tuned_lda_model$class == "t_43"] = "resiliency_plan"
tuned_lda_model$mapping[tuned_lda_model$class == "t_44"] = "Communication"
tuned_lda_model$mapping[tuned_lda_model$class == "t_45"] = "DM delays"
tuned_lda_model$mapping[tuned_lda_model$class == "t_46"] = "SIP"
tuned_lda_model$mapping[tuned_lda_model$class == "t_47"] = "WIP resolution"
tuned_lda_model$mapping[tuned_lda_model$class == "t_48"] = "internet_instability"
tuned_lda_model$mapping[tuned_lda_model$class == "t_49"] = "Team deliver"
tuned_lda_model$mapping[tuned_lda_model$class == "t_50"] = "Project delays"
tuned_lda_model$mapping[tuned_lda_model$class == "t_51"] = "Customer service review"
tuned_lda_model$mapping[tuned_lda_model$class == "t_52"] = "Account team"
tuned_lda_model$mapping[tuned_lda_model$class == "t_53"] = "Delivery"
tuned_lda_model$mapping[tuned_lda_model$class == "t_54"] = "Network management"
tuned_lda_model$mapping[tuned_lda_model$class == "t_55"] = "Delivery"
tuned_lda_model$mapping[tuned_lda_model$class == "t_56"] = "Team working"
tuned_lda_model$mapping[tuned_lda_model$class == "t_57"] = "Ongoing delayed"
tuned_lda_model$mapping[tuned_lda_model$class == "t_58"] = "others"
tuned_lda_model$mapping[tuned_lda_model$class == "t_59"] = "customer_feedback"
tuned_lda_model$mapping[tuned_lda_model$class == "t_60"] = "service_model"
tuned_lda_model$mapping[tuned_lda_model$class == "t_61"] = "others"
tuned_lda_model$mapping[tuned_lda_model$class == "t_62"] = "Billing"
tuned_lda_model$mapping[tuned_lda_model$class == "t_63"] = "Billing"
tuned_lda_model$mapping[tuned_lda_model$class == "t_64"] = "Customer review"
tuned_lda_model$mapping[tuned_lda_model$class == "t_65"] = "others"
tuned_lda_model$mapping[tuned_lda_model$class == "t_66"] = "call_summary"
tuned_lda_model$mapping[tuned_lda_model$class == "t_67"] = "router"
tuned_lda_model$mapping[tuned_lda_model$class == "t_68"] = "incident_management"
tuned_lda_model$mapping[tuned_lda_model$class == "t_69"] = "action_plan"
tuned_lda_model$mapping[tuned_lda_model$class == "t_70"] = "change_control"
tuned_lda_model$mapping[tuned_lda_model$class == "t_71"] = "others"
tuned_lda_model$mapping[tuned_lda_model$class == "t_72"] = "improvement_plan"
tuned_lda_model$mapping[tuned_lda_model$class == "t_73"] = "handbook_updated"
tuned_lda_model$mapping[tuned_lda_model$class == "t_74"] = "service_management"
tuned_lda_model$mapping[tuned_lda_model$class == "t_75"] = "Escalation"

tuned_lda_model$class
text_raw$class <- tuned_lda_model$mapping
text_raw$topics <- tuned_lda_model$class

write.csv(text_raw,"C:\\Shiva\\Projects\\GS_fault_Text_analytics\\Account Improvement\\Output\\topic_maping.csv",row.names = F)


#########################################################################################################################################################################################
##Hierarical Clustering#####
########################################################################################################################################################################################
pos_tag_out <- text_raw1

#Create Corpus
docs <- Corpus(VectorSource(pos_tag_out$Comments))
docs<-tm_map(docs,content_transformer(tolower))
docs<-tm_map(docs,content_transformer(removeNumbers))
docs<-tm_map(docs,content_transformer(removePunctuation))
docs<-tm_map(docs,removeWords,stopwords('english'))
docs<-tm_map(docs,stripWhitespace)
#docs <- tm_map(docs, stemDocument, language = "english")  

stpwrds=c("BT","GS","Global services","services","take","global","also","yet","years","see",
          "two","can","however","months","times","time","past","now","one","good","still",
          "btgs","csr","year","day","bt","solutions","long","well","need","th","new","due",
          "gov","tui","get","wan","will","last","don","set","way","key","cet","action","plan",
          "want","sev","put","want","first","say","case","gsip","even","good","customer")
docs<-tm_map(docs,removeWords,stpwrds)

#term doccument matrix
tdm <- TermDocumentMatrix(docs,
                          control = list(wordLengths = c(1, Inf)))
tdm

# inspect frequent words
(freq.terms <- findFreqTerms(tdm, lowfreq = 80))

term.freq <- rowSums(as.matrix(tdm))
term.freq <- subset(term.freq, term.freq >= 80)
df <- data.frame(term = names(term.freq), freq = term.freq)

#Frequency matrix
ggplot(df, aes(x = term, y = freq)) + geom_bar(stat = "identity") +
  xlab("Terms") + ylab("Count") + coord_flip()

#relationship plot
plot(tdm, term = freq.terms, corThreshold = 0.1, weighting = T,
     attrs=list(node=list(label="foo", fillcolor="lightblue", fontsize="50", shape="ellipse"), 
                edge=list(color="lightgreen"), graph=list(rankdir="LR")))


#convert dtm to matrix
m <- as.matrix(tdm)


#compute distance between document vectors
d <- dist(m,method="euclidean")

#run hierarchical clustering using Ward's method
set.seed(12)
groups <- hclust(d,method="ward.D")
groups

fit <- cutree(groups,k=50) # cut tree into 5 clusters

#plot dendogram, use hang to ensure that labels fall below tree
plot(groups, cex=0.1, hang=-1)

clusterCut <- cutree(groups, 50)

#cut into 2 subtrees - try 3 and 5
rect.hclust(groups,50)



##################################################################################################################################################
##K means clustering##
##################################################################################################################################################

#Inspect most popular words, minimum frequency of 20  
findFreqTerms(tdm, lowfreq=50) 

#25 cluster
set.seed(211)
fit.km = flexclust::kcca(d, 50, kccaFamily("kmeans"))
pred_train <- predict(fit.km)

#cluster plot
clusplot(m, pred_train, color=T, shade=T, labels=2, lines=0)


#saving model
saveRDS(fit.km,file = "account improvement summary_50.RDS")

#Merge cluster assignment back to keywords 
df <- data.frame(text = get("content", docs))
kw_with_cluster <- as.data.frame(cbind(df, pred_train)) 
names(kw_with_cluster) <- c("keyword", "kmeans")
clust_out <- cbind(pos_tag_out,pred_train)

#exporting 25 cluster output
write.csv(clust_out,"C:\\Shiva\\Projects\\GS_fault_Text_analytics\\Medallia\\Output\\likelihood_to_recommand_clust_25.csv")

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

########################################################################################################################################################
## Supervised learning models for topic modelling##
#########################################################################################################################################################

#Clean text
stpwrds=c("BT","2017","GS","new","now","services","also","yet","years","see","sev","seen","improve","norm","axtel","issue",
          "two","can","however","months","times","time","now","one","mar","april","review","plan",
          "support","good","still","btgs","csr","year","day","issue","abb","number","right","time",
          "gov","tui","get","wan","will","last","don","set","way","key","met","month","level","now")


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

####################################################################################################################################################
##Supervised learning topic prediction##
#####################################################################################################################################################

#Reading trained model
SVM <- readRDS("SVM.RDS")
RF <- readRDS("RF.RDS")


#Clean text
stpwrds=c("BT","2017","GS","new","now","services","also","yet","years","see","sev","seen","improve","norm","axtel","issue",
          "two","can","however","months","times","time","now","one","mar","april","review","plan",
          "support","good","still","btgs","csr","year","day","issue","abb","number","right","time",
          "gov","tui","get","wan","will","last","don","set","way","key","met","month","level","now")


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

# create a prediction document term matrix( trace("create_matrix", edit=T) "go to line 42 and change  Acronym  to  acronym .)
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

########################################################################################################################
## Sentiment Analysis##
#########################################################################################################################
#Import text file
senti <- read.csv("C:\\Shiva\\Projects\\GS_fault_Text_analytics\\Account Improvement\\Data\\Account Improvement Plans_topic_prob0.5 v1.csv",stringsAsFactors = F)

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
    stpwrds=c("BT","2017","GS","new","now","services","also","yet","years","see","sev","seen","improve","norm","axtel","issue",
              "two","can","however","months","times","time","now","one","mar","april","review","plan",
              "support","will","good","april","still","btgs","csr","year","day","issue","abb","number","right","time",
              "gov","tui","get","wan","will","end","last","don","set","way","key","met","month","level","now")
    
    myCorpus <- tm_map(myCorpus, removeWords, stpwrds)
    myStopwords <- c(stopwords('english'))
    myCorpus <- tm_map(myCorpus, removeWords, myStopwords)
    myCorpus <- tm_map(myCorpus, stemDocument)
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

#converting text to lower case
senti$Comments=tolower(senti$Comments)

#develop word cloud #wordcloud package
wordcloud(senti$Comments,min.freq=10)


#sentiment scoring
scores <- score.sentiment(senti$Comments, pos.words, neg.words, .progress='text')

#total score calculation: positive / negative / neutral
stat <- scores
stat <- mutate(stat,sentiment=ifelse(stat$score > 0, 'positive', ifelse(stat$score < 0, 'negative', 'neutral')))

sentiment_score_out <- cbind(senti,stat)

write.csv(sentiment_score_out,"C:\\Shiva\\Projects\\GS_fault_Text_analytics\\Account Improvement\\Output\\aacount_improvement_sentiment_score v1.csv")

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
stpwrds=c("BT","GS","services","also","yet","years","see","sev","seen","improve","norm","axtel",
          "two","can","however","months","times","time","now","one","mar","april","review","now","improvement",
          "support","good","still","btgs","csr","year","day","issue","abb","number","right","new",
          "gov","tui","get","wan","will","tat","last","don","set","way","key","met","month","level")

emo.docs = removeWords(emo.docs, stpwrds)

# to remove numbers
emo.docs=removeNumbers(emo.docs) 


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

#############################################################################################################
##Association between word:Service Delivery##
###########################################################################################################
association_Service_Delivery <- association[association$Sub_topic=='Service Delivery',]
#Create Corpus
docs <- Corpus(VectorSource(association_Service_Delivery$Likelihood.to.Recommend.Comment.English))
docs<-tm_map(docs,content_transformer(tolower))
docs<-tm_map(docs,content_transformer(removeNumbers))
docs<-tm_map(docs,content_transformer(removePunctuation))
docs<-tm_map(docs,removeWords,stopwords('english'))
docs<-tm_map(docs,stripWhitespace)
#docs <- tm_map(docs, stemDocument, language = "english")  

stpwrds=c("BT","GS","Global services","services","take","global","also","yet","years","see",
          "two","can","however","months","times","time","past","now","one","good","still",
          "btgs","csr","year","day","bt","solutions","long","well","need","always","new","teams","many",
          "gov","tui","get","wan","will","last","don","set","way","key","cet","use","provide","overall",
          "want","sev","put","want","first","say","case","gsip","even","good","customer","network")
docs<-tm_map(docs,removeWords,stpwrds)

#term doccument matrix
tdm <- TermDocumentMatrix(docs,
                          control = list(wordLengths = c(1, Inf)))
tdm

# inspect frequent words
(freq.terms <- findFreqTerms(tdm, lowfreq = 10))

term.freq <- rowSums(as.matrix(tdm))
term.freq <- subset(term.freq, term.freq >= 10)
df <- data.frame(term = names(term.freq), freq = term.freq)

#Frequency matrix
ggplot(df, aes(x = term, y = freq)) + geom_bar(stat = "identity") +
  xlab("Terms") + ylab("Count") + coord_flip()

#relationship plot
plot(tdm, term = freq.terms, corThreshold = 0.1, weighting = T,
     attrs=list(node=list(label="foo", fillcolor="lightblue", fontsize="50", shape="ellipse"), 
                edge=list(color="lightgreen"), graph=list(rankdir="LR")))


###################################################################################################################
