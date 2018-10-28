
install.packages("textmineR")
install.packages("tm")
install.packages("caret")
install.packages("ggplot2")

library(textmineR)
library(tm)
library(caret)

rm(list = ls())
setwd("E:\\sherlock")
# Load some data into the workspace 

text_raw1= read.csv('sherlock.csv',header=TRUE)
colnames(text_raw1) <- c("date", "note")
text_raw1$note = as.character(text_raw1$note)

#code to convert Redw log file date to a format needed.
text_raw1$date = as.character(text_raw1$date)
formated_Date <- sapply(text_raw1$date,format_date_function)

format_date_function <- function(each_date)
{
  split_list <- strsplit(each_date," ")[[1]]
  year <- strsplit(split_list[5],"_")[[1]][1]
  format_time <- paste(year,as.character(match(split_list[2],month.abb)),split_list[3],sep = "-")
  format_time <- paste(format_time,split_list[4],sep = " ")
  format_time
}
text_raw1$formated_Date <- formated_Date


text_raw1$formated_Date <- as.character(text_raw1$formated_Date)
write.csv(text_raw1,"automated_timestamp_format.csv",row.names = FALSE)

# Create a document term matrix
cosmos_dtm <- CreateDtm(text_raw1$note, 
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
topics_list <- seq(5, 50, by = 5)

model_lda_directory <- paste0("lda_models", digest::digest(cosmos_needed_Terms, algo = "sha1"))

if (!dir.exists(model_lda_directory)) dir.create(model_lda_directory)

tunable_lda_list <- TmParallelApply(X = topics_list, FUN = function(k){
  filename = file.path(model_lda_directory, paste0(k, "_lda_topics.rda"))
  
  if (!file.exists(filename)) {
    lda_model <- FitLdaModel(dtm = cosmos_dtm, k = k, iterations = 10)
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
tuned_lda_model <- FitLdaModel(dtm = cosmos_dtm, k = 25, iterations = 50)

GetTopicTerms <- function(phi, req_topic_top_Terms){
  
  result_topterms <- apply(phi, 1, function(x){
    names(x)[ order(x, decreasing=TRUE) ][ 1:req_topic_top_Terms]
  })
  
  return(result_topterms)
}
tuned_lda_model$topic_terms <- GetTopicTerms(phi = tuned_lda_model$phi, req_topic_top_Terms = 20)

tuned_lda_model$class <-colnames(tuned_lda_model$theta)[max.col(tuned_lda_model$theta,ties.method="first")]

text_raw1$topic <- tuned_lda_model$class
write.csv(text_raw1,"topic_mappings_new_log_8_2_2017.csv",row.names = FALSE)
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

write.csv(text_raw1,"top_2_topic_probabilty_sample_Data.csv",row.names = FALSE)


write.csv(tuned_lda_model$topic_terms,"top_terms_topics_latest_17_7.csv",row.names = FALSE)
re_terms_df <- read.csv("top_terms_topics_latest_17_7.csv", header=TRUE)

#automatic naming with top 3 terms
names_mapping <- sapply(re_terms_df,concat_top_3_terms_function)

concat_top_3_terms_function <- function(each_coloumn)
{
  name_of_topic <- paste(each_coloumn[[1]],each_coloumn[[2]],each_coloumn[[3]],sep="_")
  name_of_topic
  
}

names_topics <- names(names_mapping)
names_mapping <- unname(names_mapping)

mapping_df = data.frame(topics=names_topics, name_from_top_3_terms = names_mapping)
dim(mapping_df)
write.csv(mapping_df,"topic_mapping_from_top_3_terms.csv",row.names = FALSE)

#finetuning for different iteration numbers to eliminate  duplicate mappings
k <- tuned_lda_model$k
lda_model1 <- FitLdaModel(dtm = cosmos_dtm, k = k, iterations = 1500)
lda_model2 <- FitLdaModel(dtm = cosmos_dtm, k = k, iterations = 5000)
lda_model3 <- FitLdaModel(dtm = cosmos_dtm, k = k, iterations = 1000)
lda_model4 <- FitLdaModel(dtm = cosmos_dtm, k = k, iterations = 7500)
lda_model5 <- FitLdaModel(dtm = cosmos_dtm, k = k, iterations = 10000)



