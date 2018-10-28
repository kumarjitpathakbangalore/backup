getwd()
setwd("F:/Practice R/New folder")


logistic <- read.csv(  file = "Logistic_raw_fnl1.csv"  , header = F  , stringsAsFactors=TRUE)

str(logistic)
head(logistic)
summary(logistic)


logistic <- read.csv(  file = "Logistic_raw_fnl1.csv"  , header = F  , col.names = c("HOUSEHOLD_ID","CHURN", "RECENCY", "VISIT_DEC", "VISIT_SEP_OCT", "VISIT_JAN_AUG", "PUR_DEC", "PUR_SEP_OCT") stringsAsFactors=TRUE)
??read.csv

colnames(logistic)[1] <- "HOUSEHOLD_ID"
colnames(logistic)[2] <- "CHURN"
colnames(logistic)[3] <- "RECENCY"

colnames(logistic)[4] <- "VISIT_DEC"
colnames(logistic)[5] <- "VISIT_SEP_OCT"
colnames(logistic)[6] <- "VISIT_JAN_AUG"
colnames(logistic)[7] <- "PUR_DEC"
colnames(logistic)[8] <- "PUR_SEP_OCT"
colnames(logistic)[9] <- "PUR_JAN_AUG"



# as the sum is been taken for the quarter and the 8 months figure we need to take the average of the same

logistic$VISIT_SEP_OCT=logistic$VISIT_SEP_OCT/3
logistic$VISIT_JAN_AUG=logistic$VISIT_JAN_AUG/8

logistic$PUR_SEP_OCT=logistic$PUR_SEP_OCT/3
logistic$PUR_JAN_AUG=logistic$PUR_JAN_AUG/8


# ________________creating a different dataset so that original data is intact

chtest1<-logistic
chtest2<-logistic







#________________________________________Logistic Regression____________________________

# EDA----------

summary(logistic)

#outliers observed also NA's ... so need to clear both before taking up logistic
# doing another analysis of outliers using BOXPLOT

boxplot(logistic$VISIT_SEP_OCT)

boxplot(logistic$VISIT_JAN_AUG)

boxplot(logistic$PUR_SEP_OCT)


boxplot(logistic$PUR_JAN_AUG)

boxplot(logistic$VISIT_DEC)

boxplot(logistic$PUR_DEC)

#_____________________________OBSERVING HOW THE log transformed DATA fits in----------------------

boxplot(log(logistic$VISIT_SEP_OCT))

boxplot(log(logistic$VISIT_JAN_AUG))

boxplot(log(logistic$PUR_SEP_OCT))


boxplot(log(logistic$PUR_JAN_AUG))

boxplot(log(logistic$VISIT_DEC))

boxplot(log(logistic$PUR_DEC))

str(logistic)
summary(logistic)

#__________________________________________REMOVING OUTLIERS_________________________________________________

boxplot.stats(logistic$VISIT_JAN_AUG)$out

hboutlier <- function(x,r){
  x <- x[is.finite(x)]
  stopifnot(
    length(x) > 0
    , all(x>0)
  )
  xref <- median(x)
  if (xref <= sqrt(.Machine$double.eps))
    warning("Reference value close to zero: results may be inaccurate")
  pmax(x/xref, xref/x) > r
}

#____________________ANOTHER WAY____________________________________________________________________

remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}

#- Next step_------------------------------------

set.seed(1)
x <- rnorm(100)
x <- c(-10, x, 10)
y <- remove_outliers(x)
## png()
par(mfrow = c(1, 2))
boxplot(x)
boxplot(y)
## dev.off()




#___________________________________________Using subset function to have dataset without outliers-----------

#newdata <- subset(chtest1, VISIT_DEC >= 100 | VISIT_SEP_OCT >= 100 | VISIT_JAN_AUG >= 100 | PUR_DEC >=2000 | PUR_SEP_OCT >=2000 | PUR_JAN_AUG >= 2000)  #select=c(var1, var2)  you can use this if you want only specific these values  
newdata <- subset(chtest1,  PUR_DEC >=4000 | PUR_SEP_OCT >=4000 | PUR_JAN_AUG >= 4000)  #select=c(var1, var2)  you can use this if you want only specific these values  

#-----------------sending these for checking

write.csv(data.frame(newdata), file = "F:/Practice R/outliers_churn.csv")



# using subset function (part 2)
newdata <- subset(mydata, sex=="m" & age > 25,
                  select=weight:income)



#____________________MISSING VALUE TREATMENT

# list rows of data that have missing values 
mydata[!complete.cases(mydata),]

# create new dataset without missing data 
newdata <- na.omit(mydata)

# Excluding Missing values from Analysis
x <- c(1,2,NA,3)
mean(x) # returns NA
mean(x, na.rm=TRUE) # returns 2

# recode 99 to missing for variable v1
# select rows where v1 is 99 and recode column v1 
mydata$v1[mydata$v1==99] <- NA

# recoding NA's to any other number of choice
logistic$VISIT_DEC[is.na(logistic$VISIT_DEC)] <- 0

is.na(x) # returns TRUE of x is missing
y <- c(1,2,3,NA)
is.na(y) # returns a vector (F F F T)

Deleting unnessessary columns bu column no derived from variable name

logistic <- logistic[,-(which(names(logistic)=="PUR_SEP_OCT"))]     # this functions returns the column number and I am reducing this from the data frame


# -----------Here in this case all NA's will be coded as '0'


logistic$PUR_DEC[is.na(logistic$PUR_DEC) ] <- 0
logistic$VISIT_SEP_OCT[is.na(logistic$VISIT_SEP_OCT) ] <- 0
logistic$VISIT_JAN_AUG[is.na(logistic$VISIT_JAN_AUG) ] <- 0
logistic$PUR_SEP_OCT[is.na(logistic$PUR_SEP_OCT) ] <- 0
logistic$PUR_JAN_AUG[is.na(logistic$PUR_JAN_AUG) ] <- 0
logistic$VISIT_DEC[is.na(logistic$VISIT_DEC)] <- 0


summary(logistic)

colnum(logistic$PUR_SEP_OCTC)

logistic <- logistic[,-(which(names(logistic)=="PUR_SEP_OCT"))]     # this functions returns the column number and I am reducing this from the data frame


which(names(logistic)=="PUR_JAN_AUG")

??getColumnNumber







#_____________________________________Selecting variables from dataset and creating a data set for further analysis_________________

dat1<-churn[,c("dep","VISITS","TOTAL_ITEMS_PURCHASED","TOTAL_PURCHASE_AMT","FACTS_SEG","SHOPSTYLES_SEG","TRUPRICE_SEG")]


install.packages("riv")
library("riv")

library("ROCR")


#______________________________________________________________________________________________
#  set.seed(1000)

#________________________________________ dividing the training and testing set________________

dat1<-logistic[,2:6]

dat1$CHURN<- as.factor(dat1$CHURN)

set.seed(100)
str(dat1)
sub <- sample(nrow(dat1), floor(nrow(dat1) * 0.7))
training <- dat1[sub, ]
testing <- dat1[-sub, ]
#___________________________________________Checking wheather training and testing has similar 0 & 1
summary(training)
summary(training)
str(training)

#____________________________________changing categorical IV in to factor class here to put as dummy. If dependent is not factor then change it to factor


#training$TRUPRICE_SEG <- as.factor(training$TRUPRICE_SEG)

#training$SHOPSTYLES_SEG <- as.factor(training$SHOPSTYLES_SEG)

#training$FACTS_SEG <- as.factor(training$FACTS_SEG)

#training$TOTAL_ITEMS_PURCHASED <- as.numeric(training$TOTAL_ITEMS_PURCHASED)



#____________________________________________________________________________________________________________
#coarse bining

#___________________________________________________Logistic Model___________________________________________

mylogit <- glm(dep ~ VISITS + TOTAL_ITEMS_PURCHASED + TOTAL_PURCHASE_AMT + as.factor(FACTS_SEG), data = training , family = "binomial")

summary(mylogit)
summary(lg)
mylogit


help(glm)



# another way
#_________________________to include dummy variable puth the variable as .. factor__________________________
#lg<-glm(CHURN ~ RECENCY + VISIT_DEC + VISIT_SEP_OCT + factor(SHOPSTYLES_SEG),family=binomial(),data=training)

lg<-glm(CHURN ~ RECENCY + VISIT_DEC + VISIT_SEP_OCT + VISIT_JAN_AUG,family=binomial(),data=training)



summary(lg)


#_____________________________________Checking syep AIC to see if variable inclusion is giving lower AIC_____________

trail_lg<-stepAIC(lg)
vif(lg)



#_______________________________________To get the ROC____________________

prob=predict(lg,type=c("response"))
training$prob=prob
library(pROC)
g<-roc(CHURN~prob,data=training)
plot(g)
summary(g)
##_____________________________________to get the AUC value_______________
g

# ______________________________Calculating p value for each observation in testing set

summary(trail_lg)

training$pred<-predict(trail_lg,type="response")
testing$pred<-predict(trail_lg,type="response",newdata=testing)
#__________________________________________________________________________________________________________________





#______________________________________________another way to create ROC curve__________________
str(training)
Florence$pred<-predict(log_model,type="response")

testing$score<-predict(mylogit,type="response",testing)

help(predict)

pred1<-prediction(testing$pred,testing$dep)
perf<-performance(pred1,"tpr","fpr")
plot(perf)

# _______________________________________________ AIC BIC

AIC(lg)
BIC(lg)

#__________________________________Multi-collinearity(now we have 5 variables)


library(car)
install.packages(MASS)
help(glm)

summary(lg)
vif(lg)

vif(lg)


_________________________________________________________________________________________________

#changing categorical IV in to factor class here to put as dummy. If dependent is not factor then change it to factor
testing$TRUPRICE_SEG <- as.factor(testing$TRUPRICE_SEG)

testing$SHOPSTYLES_SEG <- as.factor(testing$SHOPSTYLES_SEG)

testing$FACTS_SEG <- as.factor(testing$FACTS_SEG)

testing$TOTAL_ITEMS_PURCHASED <- as.numeric(testing$TOTAL_ITEMS_PURCHASED)

















library(aod)
install.packages("aod")

# Confusion Matrix


setInternet2()
library(e1071)
library("caret")
testing$act<-factor(testing$dep)
str(testing)
str(training)
testing<-testing[,-7]
testp4<-as.data.frame(predict(lg,testing,type='response'))
testp5<- as.data.frame(ifelse(testp4$predicted > 0.589742,1,0))

names(testp5)<-"predicted"
actual<-as.data.frame(testing$act)
names(actual)<-"Actual"
ac_pred<-cbind(actual,testp5)


tab<-table(ac_pred$Actual,ac_pred$predicted)

str(tab)
confusionMatrix(tab)
tab
actual

testp4
testp5

library("rattle")
yes

install.packages("e1071")
