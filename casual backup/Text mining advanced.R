setwd("F:/Practice R")
library(rvest)
setInternet2()

require(httr)
require(XML)


 setInternet2() 


htmlpage<-html("https://www.trustpilot.com/review/www.skype.com")
htmlpage<-htmlpage
##Extarct comments
proshtml <- html_nodes(htmlpage, ".review-body")
comments <- html_text(proshtml)
comments=as.data.frame(comments)
#pros=pros[-1,]
comments

write.table(comments,"F:/Practice R/text/mycrawl5.csv",row.names=FALSE,sep=",",append=TRUE)

___________________________________________

htmlpage<-html("https://www.trustpilot.com/review/www.skype.com")
htmlpage<-htmlpage
##Extarct comments
proshtml1 <- html_nodes(htmlpage, "#reviews-container .clearfix span")
author <- html_text(proshtml1)
author=as.data.frame(author)
#pros=pros[-1,]
author

data=data.frame(comments,author)
write.table(author,"F:/Practice R/text/mycrawl5.csv",row.names=FALSE,sep=",",append=TRUE)













trim <- function (x) gsub("^\\s+|\\s+$", "   ", x)

comments <- trim(comments)

comments

comments <- gsub("(?<=[\\s])\\s*|^\\s+$", "", comments, perl=TRUE)



??html_nodes
______________________________________
lego_movie<-htmlpage
lego_movie %>% 
  html_nodes(".review-body") %>%
  html_text()
html_table()
  as.numeric()


___________________________________________________________________________
library(XML)
url = "http://www.consumeraffairs.com/cell_phones/skype.html"
doc = htmlTreeParse(url, useInternalNodes=T)

date <- xpathSApply(doc, "//*[contains(concat( " ", @class, " " ), concat( " ", "post-date", " " ))]", xmlValue)
author <- xpathSApply(doc, "//*[contains(concat( " ", @class, " " ), concat( " ", "author-name", " " ))]", xmlValue)
comments <- xpathSApply(doc, "//*[(@id = "campaign-reviews")]//p", xmlValue)

doc

??xpathSApply


#_____________________another code for web crawl_________________

library(XML)
library(httr)
url <- "http://www.wikiart.org/en/claude-monet/mode/all-paintings-by-alphabet/"
hrefs <- list()
for (i in 1:23) {
  response <- GET(paste0(url,i))
  doc      <- content(response,type="text/html")
  hrefs    <- c(hrefs,doc["//p[@class='pb5']/a/@href"])
}
url      <- "http://www.wikiart.org"
xPath    <- c(pictureName = "//h1[@itemprop='name']",
              date        = "//span[@itemprop='dateCreated']",
              author      = "//a[@itemprop='author']",
              style       = "//span[@itemprop='style']",
              genre       = "//span[@itemprop='genre']")
get.picture <- function(href) {
  response <- GET(paste0(url,href))
  doc      <- content(response,type="text/html")
  info     <- sapply(xPath,function(xp)ifelse(length(doc[xp])==0,NA,xmlValue(doc[xp][[1]])))
}
pictures <- do.call(rbind,lapply(hrefs,get.picture))
head(pictures)
#      pictureName                           date     author         style           genre           
# [1,] "A Corner of the Garden at Montgeron" "1877"   "Claude Monet" "Impressionism" "landscape"     
# [2,] "A Corner of the Studio"              "1861"   "Claude Monet" "Realism"       "self-portrait" 
# [3,] "A Farmyard in Normandy"              "c.1863" "Claude Monet" "Realism"       "landscape"     
# [4,] "A Windmill near Zaandam"             NA       "Claude Monet" "Impressionism" "landscape"     
# [5,] "A Woman Reading"                     "1872"   "Claude Monet" "Impressionism" "genre painting"
# [6,] "Adolphe Monet Reading in the Garden" "1866"   "Claude Monet" "Impressionism" "genre painting


____________________________________________ANALYSIS OF TEXT


##Load the data
cname <- file.path("F:/Practice R/Text")   
??file.path

cname   
dir(cname)

docs <- Corpus(DirSource(cname))
summary(docs)

##Data cleaning
docs <- tm_map(docs, removePunctuation)

toSpace <- content_transformer(function (x,pattern ) gsub(pattern," ",x))
docs <- tm_map(docs, toSpace, "/|@|\\|")



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











docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, tolower)
docs <- tm_map(docs, removeWords, stopwords("english"))

docs <- tm_map(docs, removeWords, c("etc"))
docs <- tm_map(docs, stripWhitespace)
docs <- tm_map(docs, stemDocument)
docs <- tm_map(docs, PlainTextDocument)

docs <- tm_map(docs, removeWords, c("skype"))

docs <- tm_map(docs, removeWords, c("nbsp"))
writeLines(as.character(docs[[1]]))

##Stage the data
dtm <- DocumentTermMatrix(docs)   
dtm
inspect(dtm)
dim(dtm)

tdm <- TermDocumentMatrix(docs)   
tdm

##Data Exploration
freq <- colSums(as.matrix(dtm))   
length(freq)
ord <- order(freq)

m <- as.matrix(dtm)
dim(m)
write.csv(m, file="F:/Practice R/Text/dtm.csv")

##Focus
dim(dtm)
dtms <- removeSparseTerms(dtm, 0.4) # This makes a matrix that is 10% empty space, maximum.   
dim(dtms) 
inspect(dtms)

##Word Frequency

freq[head(ord)]
freq[tail(ord)]

head(table(freq), 20)

freq <- colSums(as.matrix(dtms))   
freq

freq <- sort(colSums(as.matrix(dtm)), decreasing=TRUE)   
head(freq, 14)

findFreqTerms(dtm, lowfreq=10)
wf <- data.frame(word=names(freq), freq=freq)   
head(wf) 

##Plot Word Frequencies
##Plot words that appear at least 50 times.
library(ggplot2)   
p <- ggplot(subset(wf, freq>9), aes(word, freq))    
p <- p + geom_bar(stat="identity")   
p <- p + theme(axis.text.x=element_text(angle=90, hjust=1))   
p

#ngram
TrigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 4, max = 4))

atdm <- TermDocumentMatrix(docs, control = list(tokenize = TrigramTokenizer))

tdmss <- removeSparseTerms(atdm, 0.1)
tdmss
inspect(tdmss[1:5,1:5])
dim(tdmss)
m <- as.matrix(atdm)
dim(m)
write.csv(m, file="D:/R Practice/texts/Symphony reviews/Working file_final/Advice_Output/grams_44.csv")




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


findAssocs(dtm, c("please"), corlimit=0.6)
findAssocs(dtm, c("management"), corlimit=0.6)
findAssocs(dtm, c("transparency"), corlimit=0.8)
findAssocs(dtm, c("improve"), corlimit=0.7)


findAssocs(dtm, c("keep"), corlimit=0.4)
findAssocs(dtm, c("work"), corlimit=0.6)
findAssocs(dtm, c("growth"), corlimit=0.7)


findAssocs(dtm, c("supportive"), corlimit=0.5)
findAssocs(dtm, c("organization"), corlimit=0.8)

library(wordcloud)
##Plot the 100 most frequently occurring words.
set.seed(142)   
dark2 <- brewer.pal(6, "Dark2")   
wordcloud(names(freq), freq, max.words=100, rot.per=0.2, colors=dark2) 


##Clustering by Term Similarity
dtmss <- removeSparseTerms(dtm, 0.7) # This makes a matrix that is only 15% empty space, maximum.   
inspect(dtmss)





###############################  DATA COLLECTION CODE FROM RONAK#######################

setInternet2(TRUE)

library("rvest")
htmlpage <- html("http://www.glassdoor.com/GD/Reviews/Symphony-Teleca-Reviews-E28614_P20.htm?sort.sortType=RD&sort.ascending=false&filter.employmentStatus=REGULAR&filter.employmentStatus=PART_TIME&filter.employmentStatus=UNKNOWN")

##Extarct Pros
proshtml <- html_nodes(htmlpage, ".pros")
pros <- html_text(proshtml)
pros=data.frame(pros)
#pros=pros[-1,]
pros

##Extract Cons
conshtml <- html_nodes(htmlpage, ".cons")
cons <- html_text(conshtml)
cons=data.frame(cons)
#cons=cons[-1,]
cons

##Job Title
jobhtml <- html_nodes(htmlpage, ".authorJobTitle")
job <- html_text(jobhtml)
job=data.frame(job)

##To remove the every second row
Nth.delete<-function(dataframe, n)dataframe[-(seq(n,to=nrow(dataframe),by=n)),]
jobtitle=Nth.delete(job,2)

##Date
datehtml <- html_nodes(htmlpage, "#EmployerReviews .subtle.small")
date <- html_text(datehtml)
date=data.frame(date)

result=data.frame(pros,cons,jobtitle,date)

write.table(result,"D:/R Practice/texts/Symphony reviews/Reviews/mydata20.csv",row.names=FALSE,sep=",")

##Advice to management
advicehtml <- html_nodes(htmlpage, ".adviceMgmt")
advice <- html_text(advicehtml)
advice=data.frame(advice)
write.table(advice,"D:/R Practice/texts/Symphony reviews/Advice/advice20.csv",row.names=FALSE,sep=",")










