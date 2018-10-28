
####################################################################################################################################################
##Supervised learning topic prediction##
#####################################################################################################################################################

new_data = read.csv("C:\\Users\\eselvaraj\\Desktop\\Projects\\Text Analytics\\Medallia Download October 13th.csv")

new_data <- new_data[!(is.na(new_data$Likelihood.to.Recommend.Comment.English) | new_data$Likelihood.to.Recommend.Comment.English==""), ]

new_data$Likelihood.to.Recommend.Comment.English <- as.character(new_data$Likelihood.to.Recommend.Comment.English)

#Reading trained model
SVM <- readRDS("C:\\Users\\eselvaraj\\Desktop\\Projects\\Text Analytics\\SVM.RDS")
RF <- readRDS("C:\\Users\\eselvaraj\\Desktop\\Projects\\Text Analytics\\RF.RDS")


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

trace("create_matrix", edit=T)
# create a prediction document term matrix( trace("create_matrix", edit=T) "go to line 42 and change  Acronym  to  acronym .)
predMatrix <- create_matrix(k, originalMatrix=doc_matrix)

# create the corresponding container
predSize = length(k)
predictionContainer <- create_container(predMatrix, labels=rep(0,predSize), testSize=1:predSize, virgin=FALSE)

# predict class
results <- classify_model(predictionContainer, SVM)
results
write.csv("C:\\Users\\eselvaraj\\Desktop\\Projects\\Text Analytics\\svm_results.csv", results)
svm_text <- cbind(test,results)
round(prop.table(table(svm_text$Main_topic == svm_text$SVM_LABEL)), 3)

#exporting output
write.csv(svm_text,"C:\\Shiva\\Projects\\GS_fault_Text_analytics\\Medallia\\Output\\Supervised learning technique for classification\\svm_test.csv")

results <- classify_model(predictionContainer, RF)
########################################################################################################################
## Sentiment Analysis##
#########################################################################################################################
#Import text file
senti <- read.csv("C:\\Shiva\\Projects\\GS_fault_Text_analytics\\Account Improvement\\Data\\Account Improvement Plans_topic_prob0.5 v1.csv",stringsAsFactors = F)
senti <- new_data
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
pos <- scan('C:\\Users\\eselvaraj\\Desktop\\Projects\\Text Analytics\\pos words.csv', what='character', comment.char=';') #folder with positive dictionary
neg <- scan('C:\\Users\\eselvaraj\\Desktop\\Projects\\Text Analytics\\neg words.csv', what='character', comment.char=';') #folder with negative dictionary

pos.words <- c(pos, 'upgrade')
neg.words <- c(neg, 'wtf', 'wait', 'waiting', 'epicfail')

#converting text to lower case
senti$Likelihood.to.Recommend.Comment.English=tolower(senti$Likelihood.to.Recommend.Comment.English)

#develop word cloud #wordcloud package
wordcloud(senti$Likelihood.to.Recommend.Comment.English,min.freq=10)


#sentiment scoring
scores <- score.sentiment(senti$Likelihood.to.Recommend.Comment.English, pos.words, neg.words, .progress='text')

#total score calculation: positive / negative / neutral
stat <- scores
stat <- mutate(stat,sentiment=ifelse(stat$score > 0, 'positive', ifelse(stat$score < 0, 'negative', 'neutral')))

sentiment_score_out <- cbind(senti,stat)

write.csv(sentiment_score_out,"C:\\Users\\eselvaraj\\Desktop\\Projects\\Text Analytics\\medallia_sentiment_oct_13.csv")

