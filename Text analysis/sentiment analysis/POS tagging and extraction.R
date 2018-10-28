
library(NLP)
library(openNLP)
library(stringr)

txt <- c("This is a short tagging example, by John Doe.",
         "Too bad OpenNLP is so slow on large texts.")
str(txt)
txt<- c("BT Global Services is an extremely professional organization and I know my clients will be taken care of with them.")

extractPOS <- function(x, thisPOSregex) {
  x <- as.String(x)
  wordAnnotation <- annotate(x, list(Maxent_Sent_Token_Annotator(), Maxent_Word_Token_Annotator()))
  POSAnnotation <- annotate(x, Maxent_POS_Tag_Annotator(), wordAnnotation)
  POSwords <- subset(POSAnnotation, type == "word")
  tags <- sapply(POSwords$features, '[[', "POS")
  thisPOSindex <- grep(thisPOSregex, tags)
  if(length(thisPOSindex)==0)
  {
    return(vector())
  }
  if(length(POSwords)==1&&thisPOSindex==1)
  {
    tokenizedAndTagged=x[[1]] 
  }
  else{
  tokenizedAndTagged <- sprintf("%s/%s", x[POSwords][thisPOSindex], tags[thisPOSindex])
  }
  untokenizedAndTagged <- paste(tokenizedAndTagged, collapse = " ")
  rm(wordAnnotation,POSAnnotation,POSwords,tags,thisPOSindex,tokenizedAndTagged,x)
  gc()
  return(untokenizedAndTagged)
  
}

noun <- lapply(txt, extractPOS, "NN")
adj <- lapply(txt, extractPOS, "JJ")

unlist(str_extract_all(unlist(noun), "\\w+(?=\\/)"))



setwd("C:\\Users\\inkpathak\\Desktop\\GS Sentiment")
# Load some data into the workspace 

text_raw1= read.csv("2016-18 All Medallia Surveys Aug 31_2.csv",header=TRUE)
m <- text_raw1$Likelihood.to.Recommend.Comment

noun <- lapply(m, extractPOS, "NN")
adj <- lapply(m, extractPOS, "JJ")

#colnames(text_raw1) <- c("date", "note")
str(text_raw1$Likelihood.to.Recommend.Comment.English)
colnames(text_raw1)
text_raw1$Likelihood.to.Recommend.Comment.English = as.factor(text_raw1$Likelihood.to.Recommend.Comment.English)

lapply(txt, extractPOS, "VB")

text_raw1$Likelihood.to.Recommend.Comment_noun<- capply(text_raw1$Likelihood.to.Recommend.Comment.English, extractPOS, "NN")


######################################################################################################################
m_text=as.character(m)
l=lapply(m_text,extractPOS,"NN")
#n=unlist(str_extract_all(unlist(l),"\\w+(?=\\/)"))
n=str_extract_all(unlist(l),"\\w+(?=\\/)")

### Extracting Nouns

text_nn=data.frame(matrix(NA,1,2))
colnames(text_nn)=c("text","Nouns")
m_text=as.character(m)
for(i in 1:length(m_text)){
  print(i)
nouns <- lapply(m_text[i], extractPOS, "NN")
l=unlist(str_extract_all(unlist(nouns), "\\w+(?=\\/)"))
#temp=c(temp,l)
if(length(l)==0)
l=as.character("")
l=paste(l,collapse =",")
temp2=as.data.frame(cbind(m_text[i],l))
colnames(temp2)=c("text","Nouns")
text_nn=rbind(text_nn,temp2)
}
text_nn=text_nn[-1,]

### Extracting adjectives

text_jj=data.frame(matrix(NA,1,2))
colnames(text_jj)=c("text","Adj")
m_text=as.character(m)
for(i in 1:length(m_text)){
  print(i)
  nouns <- lapply(m_text[i], extractPOS, "JJ")
  l=unlist(str_extract_all(unlist(nouns), "\\w+(?=\\/)"))
  #temp=c(temp,l)
  if(length(l)==0)
    l=as.character("")
  l=paste(l,collapse =",")
  temp2=as.data.frame(cbind(m_text[i],l))
  colnames(temp2)=c("text","Adj")
  text_jj=rbind(text_jj,temp2)
}
text_jj=text_jj[-1,]

POS_tagging <- cbind(text_nn,text_jj$Adj)

likelihood_to_recommand_POS <- cbind(text_raw1,POS_tagging)
likelihood_to_recommand_POS$POS_tagging <- paste(likelihood_to_recommand_POS$Nouns, likelihood_to_recommand_POS$`text_jj$Adj`, sep=' ')

write.csv(likelihood_to_recommand_POS,"C:\\Users\\inkpathak\\Desktop\\GS Sentiment\\likelihood_to_recommand_POS.csv")
