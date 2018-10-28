
# this require sentiment package which can be downloaded from CRAN Archive but before that you need to install "Rstem" Package

install.packages("Rstem")
library(Rstem)
library(tm)
library(sentiment)


##--------Load the data

getwd()
setwd("E:/Rate Gain")


boardtype<-read.csv("E:\\Hayatt\\teamwork\\social media kumarjit\\HYATTNY.csv",header=T)

boaedtype$Full_Review<-boaedtype$Full_Review
str(boardtype)

kp<-boardtype
str(kp)


--------------------------------------------------------------------------------
OUTPUT:  
data.frame':  427 obs. of  4 variables:
 $ SourceName : Factor w/ 6 levels "AppleiTunesStore",..: 1 2 3 3 3 3 3 3 3 3 ...
 $ ReviewID   : int  20909 40858 98997 99134 99142 99232 99271 99300 99344 99375 ...
 $ ReviewTitle: Factor w/ 340 levels "'File Download' Pop Up asking me to download 'chat...",..: 7 291 150 133 199 154 153 37 297 33 ...
 $ ReviewText : Factor w/ 424 levels "&nbsp;  &nbsp;  dwlotter wrote:  I am very unhappy that Skype will simply charge my account for another year of Premium with no"| __truncated__,..: 21 343 76 155 138 116 37 29 24 404 ..."

-------------------------------------------------------------------------------

# -----assigning the same to different data so that original is intact
test1<-kp[,7]
str(test1)

____________________________________________________________________________________________________
____________________________________________________________________________________________________
#__________________sentiment package is not available and hence trying other source

install.packages("tm.lexicon.GeneralInquirer", repos="http://datacube.wu.ac.at", type="source")
library(tm.lexicon.GeneralInquirer)
install.packages("tm.plugin.sentiment", repos="http://R-Forge.R-project.org")
library(tm.plugin.sentiment) # posted comments on SO about this not working
library(tm)


#__________________Again trying other ways
require(devtools)
install_url("http://cran.r-project.org/src/contrib/Archive/sentiment/sentiment_0.2.tar.gz")
require(sentiment)
ls("package:sentiment")

setwd("E:\\Hayatt\\teamwork\\social media kumarjit")

corpus <- Corpus(VectorSource(test1)) 
pos <- sum(sapply(corpus, tm_term_score, terms_in_General_Inquirer_categories("Positiv")))
neg <- sum(sapply(corpus, tm_term_score, terms_in_General_Inquirer_categories("Negativ")))
pos.score <- tm_term_score(TermDocumentMatrix(corpus, control = list(removePunctuation = TRUE)), 
                           terms_in_General_Inquirer_categories("Positiv")) # this lists each document with number below

neg.score <- tm_term_score(TermDocumentMatrix(corpus, control = list(removePunctuation = TRUE)), 
                           terms_in_General_Inquirer_categories("Negativ")) 

total.df <- data.frame(positive = pos.score, negative = neg.score)
total.df <- transform(total.df, net = positive - negative)
________________________________________________________________________________________________________
_______________________________________________________________________________________________________

dim(test1)
length(test1)
head(test1)


# ------------in case there are multiple file we can create a corpus with source of the files and file names togeather as shown below
# -------------for the current assignment it is not required
#cname <- file.path("D:","R Practice","texts","Symphony reviews","Working file_final","Advice")   
#cname   
#dir(cname)

#docs <- Corpus(DirSource(cname))
#summary(docs)

##-------------------------Data cleaning

# ------------remove punctuations
test1 = gsub('[[:punct:]]', '', test1)

#------------ remove numbers
test1 = gsub("[[:digit:]]", "", test1)


head(test1)
#------------- remove html links
test1 = gsub("http\\w+", "", test1)

# ------------remove unnecessary spaces
test1 = gsub("[ \t]{2,}", "", test1)     # please note that for each step of removing space it might be joining 2 words be careful

test1 = gsub("^\\s+|\\s+$", "", test1)

test1 = gsub('[[:cntrl:]]', '', test1)

test1 = gsub('\\d+', '', test1)



# -----------and convert to lower case:
test1 = tolower(test1)

head(test1)


# ----------define "tolower error handling" function 
try.error = function(x)
{
  # ------create missing value
  y = NA
  # -------tryCatch error
  try_error = tryCatch(tolower(x), error=function(e) e)
  # ------if not an error
  if (!inherits(try_error, "error"))
    y = tolower(x)
  # -----result
  return(y)
}

# ---------lower case using try.error with sapply 
test1 = sapply(test1, try.error)
length(test1)

# also remove names (column headings) from the text, as we do not want them in the sentiment analysis
names(test1) = NULL

# ------remove NAs in test1
test1 = test1[!is.na(test1)]
names(test1) = NULL
head(test1,11)
length(test1)                        # length is 427 for this example... it should remain intact


# ------classify emotion
class_emo = classify_emotion(test1, algorithm="bayes", prior=1.0)   # Baysian Believe network classification is used here

# ------get emotion best fit
emotion = class_emo[,7]

# ------substitute NA's by "unknown"
emotion[is.na(emotion)] = "unknown"

barplot(table(emotion), col = c("red", "mistyrose", "red","green","red", "lightgreen","lightblue"))

barplot(table(emotion), col = table(emotion))

??barplot

# ----classify polarity
class_pol = classify_polarity(test1, algorithm="bayes") # this polarity gives score of POS,NEG,POS/NEG, BEST_FIT

# ----get polarity best fit
polarity = class_pol[,4]
head(polarity)
kp<-count(polarity)
str(data_df)
ggplot(data_df, aes(x=polarity)) + geom_bar(aes(y=..count.., fill=polarity))




#hist(kp,labels = TRUE,ylim=c(0, 60),xlim=c(-4, 6),col="red",xlab = "Sentiment scores",main='Sentiment analysis')

#-----There are other method of doing the polarity score. which is more manual but gives better accuracy
#------------------------------------------------OTHER WAY OF SCORING--------------------------------
#Load sentiment word lists from existing positive and negetive word list
#-------Before working on this step please save the files as txt

hu.liu.pos=scan("F:/Practice R/text/Pos.txt",what='character',comment.char=';')
hu.liu.neg=scan("F:/Practice R/text/Neg.txt",what='character',comment.char=';')

#-------Add any industry specific terms we are just adding few more terms based on our knowledge
pos.words=c(hu.liu.pos,'intelligent','great','fine')
neg.words=c(hu.liu.neg,'wtf','wait','waiting','revised','mechanical','please','focus',
            'fire','inactive','lack','weak','try','increase','get','bring','improve',
            'retain','bring','serious','lack','suffering','uncertainty','accountable',
            'crap','enough','start','change','attention','hitlers','struggling','spoiling',
            'bad','takeover','cuttle','needs','fool','consider','create','retaining')

scores.dat = data.frame(id=numeric(427))    #it creates 427 rows with 0 values in the data frame .. equal to the no of comments we have in test1

#---------score the skype reviews
library(stringr)

for (i in 1:length(test1))
{
  data_1 <- test1[i]
  #your desired function
  
  
  # clean up data_1 with R's regex-driven global substitute, gsub():
  data_1 = gsub('[[:punct:]]', '', data_1)
  data_1 = gsub('[[:cntrl:]]', '', data_1)
  data_1 = gsub('\\d+', '', data_1)
  # and convert to lower case:
  data_1 = tolower(data_1)
  
  # split into words. str_split is in the stringr package
  word.list = str_split(data_1, '\\s+')
  # sometimes a list() is one level of hierarchy too much
  words = unlist(word.list)
  
  # compare our words to the dictionaries of positive & negative terms
  pos.matches = match(words, pos.words)
  neg.matches = match(words, neg.words)
  
  # match() returns the position of the matched term or NA
  # we just want a TRUE/FALSE:
  pos.matches = count(!is.na(pos.matches))
  neg.matches = count(!is.na(neg.matches))
  
  # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
  score = sum(pos.matches) - sum(neg.matches)
  scores.dat$id[i]<- score
  print(score)
  
}

df<- data.frame(test1,scores=scores.dat$id)
head(df)

write.csv(df, file="F:/Practice R/text/polarity_manual.csv")



score1 = read.csv("F:/Practice R/text/polarity_manual.csv")
names(score1)
head(score1)

library(plyr)
kumar<-count(score1$scores)
hist(score1$scores)
hist(score1$scores,labels = TRUE,ylim=c(0, 60),xlim=c(-4, 6),col="red",xlab = "Sentiment scores",main='Sentiment analysis')



#---------------------------------------------------------------------------------------------------




# -----data frame with results of emotion and polarity
data_df = data.frame(text=test1, emotion=emotion,polarity=polarity, stringsAsFactors=FALSE) #  this will throw a matrix of joined table
head(data_df)
str(data_df)


data_df1 = within(data_df, emotion<-factor(emotion, levels=names(sort(table(emotion),decreasing=T))))





















**********************************************
write.csv(data_df, file="E:\\Hayatt\\teamwork\\social media kumarjit\\emotion_polarityhyatt.csv")

# ----to get a colorful emotion graph
library(RColorBrewer)
library(ggplot2)

ggplot(data_df, aes(x=emotion)) + geom_bar(aes(y=..count.., fill=emotion))

# +ylab('Number of Reviews') + xlab('Emotion Categories')
#  ggtitle("Sentiment Analysis of SKYPE Reviews") +                  # these codes are not working
#  theme(legend.position='right') + 

 # to see the polarity bar diagram 
  ggplot(data_df, aes(x=polarity)) + geom_bar(aes(y=..count.., fill=polarity))

# ----separating text by emotion---- below is not giving good result hence next step tried with subset function
emos = levels(factor(data_df1$emotion))
nemo = length(emos)
emo.docs = rep("", nemo)
for (i in 1:nemo)
{
  tmp = data_df1[emotion == emos[i],]
  emo.docs[i] = paste(tmp, collapse=" ")
}

# using subset function to create a seperate file with only Negetive comments

negetive <- subset(data_df, polarity == "negative" | emotion == "disgust" | emotion == "anger" | emotion == "sadness" )  #select=c(var1, var2)  you can use this if you want only specific these values  
head(data_df)
head(negetive)


write.csv(negetive, file="E:\\Hayatt\\teamwork\\social media kumarjit\\negetive_polarityhyatt.csv")

# segmenting positive words

positive <- subset(data_df, polarity == "negative" | emotion == "joy" | emotion == "surprise" )  #select=c(var1, var2)  you can use this if you want only specific these values  
head(data_df)
head(positive)

write.csv(positive, file="E:\\Hayatt\\teamwork\\social media kumarjit\\positive_polarityhyatt.csv")

# remove stopwords

head(negetive$text)
emo.docs = removeWords(emo.docs, stopwords("english"))





___________________________________________WORD CLOUD ON NEGETIVE________________________________


# required pakacges
library(tm)
library(sentiment)
library(plyr)
library(ggplot2)
library(wordcloud)
library(RColorBrewer)
library(RWeka)
library(SnowballC)


#########################below code is not required for the current project######################
##Load the data
cname <- file.path("D:","R Practice","texts","Symphony reviews","Working file_final","Advice")   
cname   
dir(cname)

docs <- Corpus(DirSource(cname))
docs.copy <- docs
summary(docs)







##Data cleaning
docs <- tm_map(docs, removePunctuation)

toSpace <- content_transformer(function (x,pattern ) gsub(pattern," ",x))
docs <- tm_map(docs, toSpace, "/|@|\\|")



docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, tolower)
docs <- tm_map(docs, removeWords, stopwords("english"))
docs <- tm_map(docs, removeWords, c("etc","will"))
docs <- tm_map(docs, stripWhitespace)


#docs <- tm_map(docs, stemDocument)

#############################################################################################


# create corpus
corpus = Corpus(VectorSource(emo.docs))
tdm = TermDocumentMatrix(corpus)
tdm = as.matrix(tdm)
colnames(tdm) = emos   this is not working need to check
write.csv(tdm, file="E:\\Hayatt\\teamwork\\social media kumarjit\\terms_classification.csv")

docs<-corpus

docs <- tm_map(docs, PlainTextDocument)

docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, tolower)
docs <- tm_map(docs, removeWords, stopwords("english"))
docs <- tm_map(docs, removeWords, c("etc","will","positive","negative","neutral","joy","sadness","anger","disgust"))

writeLines(as.character(docs[[1]]))   # WHY we are doing this


# coparison word cloud

comparison.cloud(tdm,colors=brewer.pal(nemo,"Dark2"),scale=c(3,.5),random.order=FALSE, title.size = 1.5)


??comparison.cloud











##Stage the data
dtm <- DocumentTermMatrix(docs)   
dtm
inspect(dtm[1:5, 1:20])
dim(dtm)

tdm <- TermDocumentMatrix(docs)   
tdm

##Data Exploration
freq <- colSums(as.matrix(dtm))   
length(freq)
ord <- order(freq)

m <- as.matrix(dtm)
dim(m)
write.csv(m, file="E:\\Hayatt\\teamwork\\social media kumarjit\\dtm1.csv")

##Focus
dim(dtm)
dtms <- removeSparseTerms(dtm, 0.9) # This makes a matrix that is 10% empty space, maximum.   
dim(dtms) 
inspect(dtms)
*************************below to be performed******************
findAssocs(dtms, c("hyatt"), corlimit=0.1)
findAssocs(dtms, c("management"), corlimit=0.29)
findAssocs(dtms, c("manager"), corlimit=0.6)
findAssocs(dtms, c("company"), corlimit=0.6)
findAssocs(dtms, c("please"), corlimit=0.6)
findAssocs(dtms, c("project"), corlimit=0.6)
findAssocs(dtms, c("people"), corlimit=0.6)
findAssocs(dtms, c("onsite"), corlimit=0.4)

##Word Frequency

freq[head(ord)]
freq[tail(ord)]

head(table(freq), 20)

freq <- colSums(as.matrix(dtms))   
freq


freq <- sort(colSums(as.matrix(dtm)), decreasing=TRUE)   
head(freq, 14)

findFreqTerms(dtm, lowfreq=200)
wf <- data.frame(word=names(freq), freq=freq)
write.csv(wf, file="E:\\Hayatt\\teamwork\\social media kumarjit\\word count.csv")
head(wf) 

##Plot Word Frequencies
##Plot words that appear at least 50 times.
library(ggplot2)   
p <- ggplot(subset(wf, freq>1500), aes(word, freq))    
p <- p + geom_bar(stat="identity",col=freq)   
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))   
p

#ngram
SixgramTokenizer <- function(x) NGramTokenizer(x,Weka_control(min = 6, max = 6))

atdm <- TermDocumentMatrix(docs, control = list(tokenize = SixgramTokenizer))
m <- as.matrix(atdm)
dim(m)
write.csv(m, file="D:/R Practice/texts/Symphony reviews/Working file_final/Advice_Output/grams_66.csv")

##Plot the 100 most frequently occurring words.
set.seed(142)   
dark2 <- brewer.pal(6, "Dark2")   
wordcloud(names(freq), freq, min.words=600,min.freq=3, rot.per=0.2, colors=dark2) 

set.seed(142)   
dark2 <- brewer.pal(6, "Dark2")   
wordcloud(names(freq), freq, min.freq=2,max.words=600, colors=brewer.pal(6, "Dark2"),
          scale=c(2, .2),rot.per=0.2)

#Now lets try it with frequent words plotted first
set.seed(142)   
dark2 <- brewer.pal(6, "Dark2") 
wordcloud(names(freq), freq,c(2,.5),2,,FALSE,.1, colors=dark2)

# Word Cloud - BigramTokenizer #
BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
tdm.ng <- TermDocumentMatrix(docs, control = list(tokenize = BigramTokenizer))
tdm91.ng <- removeSparseTerms(tdm.ng, 0.9)
notsparse <- tdm91.ng
m = as.matrix(notsparse)
v = sort(rowSums(m),decreasing=TRUE)
d = data.frame(word = names(v),freq=v)

# Create the word cloud
pal = brewer.pal(9,"BuPu")
wordcloud(words = d$word,
          freq = d$freq,
          scale = c(1,.5),
          random.order = F,
          colors = pal,max.words=20)

#B-gram frequency
freq1 <- sort(colSums(as.matrix(notsparse)), decreasing=TRUE)
wof <- data.frame(word = names(v),freq=v)

pl <- ggplot(subset(wof, freq >1) ,aes(words, freq))
pl <- pl + geom_bar(stat="identity", fill="darkgreen", colour="blue")
pl + theme(axis.text.x=element_text(angle=45, hjust=1)) + ggtitle("Bi-Gram Frequency") 






#################################  EXTRA CODES  ########################################

toString <- content_transformer(function(x,from,to)gsub(from, to, x))
docs <- tm_map(docs, toString, "ceos", "ceo")
docs <- tm_map(docs, toString, "ceos", "ceo")
docs <- tm_map(docs, toString, "changes", "change")
docs <- tm_map(docs, toString, "clearly", "clear")
docs <- tm_map(docs, toString, "clients", "client")
docs <- tm_map(docs, toString, "companies", "company")
docs <- tm_map(docs, toString, "concentrated", "concentrate")
docs <- tm_map(docs, toString, "coupons", "coupon")
docs <- tm_map(docs, toString, "customers", "customer")
docs <- tm_map(docs, toString, "cut", "cutting")
docs <- tm_map(docs, toString, "certification", "certificate")
docs <- tm_map(docs, toString, "considering", "consider")
docs <- tm_map(docs, toString, "employee", "employees")
docs <- tm_map(docs, toString, "employeess", "employees")
docs <- tm_map(docs, toString, "engineers", "engineer")
docs <- tm_map(docs, toString, "executed", "execute")
docs <- tm_map(docs, toString, "experiences", "experience")
docs <- tm_map(docs, toString, "fired", "fire")
docs <- tm_map(docs, toString, "faces", "face")
docs <- tm_map(docs, toString, "feels", "feel")
docs <- tm_map(docs, toString, "gets", "get")
docs <- tm_map(docs, toString, "grow", "growth")
docs <- tm_map(docs, toString, "growthth", "growth")
docs <- tm_map(docs, toString, "hikes", "hike")
docs <- tm_map(docs, toString, "hiring", "hire")
docs <- tm_map(docs, toString, "increased", "increase")
docs <- tm_map(docs, toString, "issues", "issue")
docs <- tm_map(docs, toString, "issuess", "issue")
docs <- tm_map(docs, toString, "kept", "keep")
docs <- tm_map(docs, toString, "lacking", "lack")
docs <- tm_map(docs, toString, "lots", "lot")
docs <- tm_map(docs, toString, "looking", "look")
docs <- tm_map(docs, toString, "lets", "let")
docs <- tm_map(docs, toString, "making", "make")
docs <- tm_map(docs, toString, "managers", "manager")
docs <- tm_map(docs, toString, "managerss", "manager")
docs <- tm_map(docs, toString, "needed", "need")
docs <- tm_map(docs, toString, "needs", "need")
docs <- tm_map(docs, toString, "opportunity", "opportunities")
docs <- tm_map(docs, toString, "performances", "performance")
docs <- tm_map(docs, toString, "performing", "performance")
docs <- tm_map(docs, toString, "policies", "policy")
docs <- tm_map(docs, toString, "potentials", "potential")
docs <- tm_map(docs, toString, "processes", "process")
docs <- tm_map(docs, toString, "particularly", "particular")
docs <- tm_map(docs, toString, "projects", "project")
docs <- tm_map(docs, toString, "recognize", "recognition")
docs <- tm_map(docs, toString, "respected", "respect")
docs <- tm_map(docs, toString, "retaining", "retain")
docs <- tm_map(docs, toString, "retention", "retain")
docs <- tm_map(docs, toString, "reviews", "review")
docs <- tm_map(docs, toString, "revised", "revise")
docs <- tm_map(docs, toString, "running", "run")
docs <- tm_map(docs, toString, "ranges", "range")
docs <- tm_map(docs, toString, "salaries", "salary")
docs <- tm_map(docs, toString, "serious", "seriously")
docs <- tm_map(docs, toString, "seriouslyly", "seriously")
docs <- tm_map(docs, toString, "started", "start")
docs <- tm_map(docs, toString, "starting", "start")
docs <- tm_map(docs, toString, "taken", "take")
docs <- tm_map(docs, toString, "talents", "talent")
docs <- tm_map(docs, toString, "technolgoies", "technologies")
docs <- tm_map(docs, toString, "terms", "term")
docs <- tm_map(docs, toString, "training", "train")
docs <- tm_map(docs, toString, "trained", "train")
docs <- tm_map(docs, toString, "traininged", "train")
docs <- tm_map(docs, toString, "traininging", "train")
docs <- tm_map(docs, toString, "useful", "use")
docs <- tm_map(docs, toString, "valued", "value")
docs <- tm_map(docs, toString, "values", "value")
docs <- tm_map(docs, toString, "waits", "wait")
docs <- tm_map(docs, toString, "wanted", "want")
docs <- tm_map(docs, toString, "working", "work")
docs <- tm_map(docs, toString, "works", "work")
docs <- tm_map(docs, toString, "years", "year")


##ngram
tdmss <- removeSparseTerms(atdm, 0.7)
tdmss
atdm
inspect(tdmss[1:5,1:5])
dim(tdmss)

##Relationships Between Terms

#Dictionary
inspect(DocumentTermMatrix(docs,
                           list(dictionary = c("opportunities", "cutting"))))

findAssocs(dtm, c("project"), corlimit=0.6)
findAssocs(dtm, c("opportunities"), corlimit=0.6)

findAssocs(dtm, c("sales"), corlimit=0.6)
findAssocs(dtm, c("team"), corlimit=0.6)

findAssocs(dtm, c("cost"), corlimit=0.8)
findAssocs(dtm, c("cut"), corlimit=0.8)

findAssocs(dtm, c("opportunity"), corlimit=0.7)
findAssocs(dtm, c("nothing"), corlimit=0.55)


findAssocs(dtm, c("please","management"), corlimit=0.7)
findAssocs(dtm, c("management"), corlimit=0.6)
findAssocs(dtm, c("transparency"), corlimit=0.8)
findAssocs(dtm, c("improve"), corlimit=0.7)


findAssocs(dtm, c("keep"), corlimit=0.4)
findAssocs(dtm, c("work"), corlimit=0.6)
findAssocs(dtm, c("growth"), corlimit=0.7)


findAssocs(dtm, c("supportive"), corlimit=0.5)
findAssocs(dtm, c("organization"), corlimit=0.8)

##Clustering by Term Similarity
dtmss <- removeSparseTerms(dtm, 0.7) # This makes a matrix that is only 15% empty space, maximum.   
inspect(dtmss)

library(cluster)   
d <- dist(t(dtmss), method="euclidian")   
fit <- hclust(d=d, method="ward.D")   
fit 
plot(fit, hang=-1) 


plot.new()
plot(fit, hang=-1)
groups <- cutree(fit, k=6)   # "k=" defines the number of clusters you are using   
rect.hclust(fit, k=6, border="red") # draw dendogram with red borders around the 5 clusters 


library(fpc)   
d <- dist(t(dtmss), method="euclidian")   
kfit <- kmeans(d, 3)
clusplot(as.matrix(d), kfit$cluster, color=T, shade=T, labels=2, lines=0)

tx<-c("makes")

tx2  <- gsub(pattern = "makes", replace = "make", x = tx)
docs <- tm_map(tx, toString, "makes", "make")

#docs<-tm_map(docs,replaceWords,synonyms(dic,"employees"),by="employee")


# comparison word cloud
set.seed(142)
comparison.cloud(tdm, colors = brewer.pal(emos, "Dark2"),max.words=600,min.freq=9,
                 scale = c(1,.5), random.order = FALSE, title.size = 1.5,rot.per=0.2)

comparison.cloud(tdm,scale=c(.2,2),max.words=100,min.freq=9,random.order=FALSE,
                 rot.per=.1,colors = brewer.pal(nemo, "Dark2"),use.r.layout=FALSE,title.size=1.5)


# comparison cloud
comparison.cloud(tdm, random.order=FALSE, 
                 colors = c("#00B2FF", "red", "#FF0099", "#6600CC"),
                 title.size=1.5, max.words=500)

# commonality cloud
commonality.cloud(tdm, random.order=FALSE, 
                  colors = brewer.pal(8, "Dark2"),
                  title.size=1.5)


dfs<-read.csv(file="D:/R Practice/texts/Symphony reviews/Working file_final/Advice_Output/sentiments2.csv",header = TRUE)
temp<-subset(dfs,polarity=="negative")
write.csv(temp,file="D:/R Practice/texts/Symphony reviews/Working file_final/Advice_Output/negativereviews.csv")
















