###########################################################################
#                       Data Sciecne Exploration                          #
#         Exploration of Data from a Data Scientist Prespective           #
#                        Creator : Jitin Kapila                           #
###########################################################################

#### Libraries Required
# library(Hmisc)
library(ggplot2)
library(caret)
library(party)
library(partykit)
library(class)
library(e1071)
library(rpart)
library(rpart.plot)
library(psych)
library(randomForest)
library(nnet)
library(neuralnet)
library(xgboost)

#### Setting seed for reproducibility
set.seed(101)

#### Setting Path
# setwd("/media/jitink/dsa/r-package/")
# setwd("D:/Learn/Data Set/r-package/")
setwd("C:/Users/609794065/Documents/Backup Data/Learn/r-package/")

### Getting the data ##################

# data can be downloaded freely from :
# http://wwwn.cdc.gov/Nchs/Nhanes/Search/DataPage.aspx

### demographic, questionare and examination data
demoG <- Hmisc::sasxport.get("data/Demographic-Data.XPT")
occupation  <- Hmisc::sasxport.get("data/Occupation.XPT")
medi_condition <- Hmisc::sasxport.get("data/Medical Conditions.XPT")
blood_press <- Hmisc::sasxport.get("data/Blood-Pressure-Data.XPT")
blood_measure <- Hmisc::sasxport.get("data/Blood-Measure-Data.XPT")
diabetese <- Hmisc::sasxport.get("data/Diabetes.XPT")
physical_activity <- Hmisc::sasxport.get("data/Physical Activity.XPT")
weigth_hist <- Hmisc::sasxport.get("data/Weight History.XPT")
weigth_hist_young <- Hmisc::sasxport.get("data/Weight History - Youth.XPT")
current_health <- Hmisc::sasxport.get("data/Current Health Status.XPT")

## merging demographic, questionare and examination data

length(intersect(demoG$seqn,blood_press$seqn))
length(intersect(demoG$seqn,blood_measure$seqn))
length(intersect(demoG$seqn,diabetese$seqn))
length(intersect(demoG$seqn,medi_condition$seqn))
length(intersect(demoG$seqn,physical_activity$seqn))
length(intersect(demoG$seqn,weigth_hist$seqn))
length(intersect(demoG$seqn,weigth_hist_young$seqn))
length(intersect(demoG$seqn,occupation$seqn))
length(intersect(demoG$seqn,current_health$seqn))


### merging 1
# since all are mapped in demographic data hence merging on
# demographic as main data

merge1 <- merge(x=demoG,y=blood_press,all.x = TRUE,by="seqn")
merge1 <- merge(x=merge1,y=diabetese,all.x = TRUE,by="seqn")
merge1 <- merge(x=merge1,y=medi_condition,all.x = TRUE,by="seqn")
merge1 <- merge(x=merge1,y=physical_activity,all.x = TRUE,by="seqn")
merge1 <- merge(x=merge1,y=weigth_hist,all.x = TRUE,by="seqn")
merge1 <- merge(x=merge1,y=weigth_hist_young,all.x = TRUE,by="seqn")
merge1 <- merge(x=merge1,y=current_health,all.x = TRUE,by="seqn")
merge1 <- merge(x=merge1,y=occupation,all.x = TRUE,by="seqn")
merge1 <- merge(x=merge1,y=blood_measure,all.x = TRUE,by="seqn")

## defining the obese criteria
table(ifelse(merge1$bmxbmi>30,"Obese","Not-Obese"))
sum(is.na(ifelse(merge1$bmxbmi>30,"Obese","Not-Obese")))

merge1$is_obese <- as.factor(ifelse(merge1$bmxbmi>30,"Obese","Not-Obese"))
# describe(merge1)

rm(diabetese,demoG,blood_press,blood_measure,
   weigth_hist_young,weigth_hist,physical_activity,occupation,
   medi_condition,current_health)

data_original <- merge1
rm(merge1)
summary(data_original)


# write.csv(data_original,file="demo-data.csv",row.names = FALSE)
# data_original <- read.csv("demo-data.csv",header = TRUE)

### Data Cleaning and Preprocessing #############

## Iteration 1
## observing and cleaning the dependent variable
table(data_original$is_obese)
sum(is.na(data_original$is_obese))

data_ <- data_original[which(data_original$is_obese != "NA"),]

## removing columns which contatins more than 50 % data as 
## NA or Missing

to_remove <- c()
for (col_ in colnames(data_)){
  if (sum(is.na(data_[,col_]))> (nrow(data_) * 0.5)){
    print(col_)
    to_remove <- c(to_remove,col_)
  }
}

data_ <- data_[,setdiff(colnames(data_),to_remove)]

## imputing NA in rest of data as -1
data_[is.na(data_)] <- -1

## converting is_obese to binary for modelling
data_$is_obese <- ifelse(data_$is_obese == "Obese",1,0)

# randomising the data
data_ <- data_[sample(nrow(data_),nrow(data_)),]

## End of Iteration 1
## We might need to do more changes if model does not behave well

########################################################################

#### Plotting ####################
boxplot(data_[,-c(1,135)])

# from boxplot we see that some variable has a very high range
# selecting sucg variabls for diffenrent processing

for (col_ in colnames(data_)){
  # if (max(as.numeric(data_[,col_])) > 100){
  #   print(col_)
  # }
  print(paste(col_,"has max value at ",max(data_[,col_]),
              collapse = " "))
}

# We found
# "wtint2yr has max value at  152162.421584"
# "wtmec2yr has max value at  156152.180687"

boxplot(subset(data_,select = -c(seqn,wtint2yr,wtmec2yr)))

## Plotting histogram and density for each Variable
pr_orig <- par()
par <- par(mfrow = c(2,1))
for (col_ in colnames(data_[,-c(1)])){
  
  hist(data_[,col_], xlab = col_,breaks = 50,
       main = paste("Histogram for",col_,collapse = " "),cex = 0.5)
  plot(density(data_[,col_],na.rm = TRUE),
       main = paste("Density for",col_,collapse = " "))
}

## Cross plots
par <- pr_orig
par <- par(mfrow = c(2,2))
for (col_ in colnames(data_)){
  plot(data_[,col_],data_[,"is_obese"],
       xlab = col_ , ylab = "Is Obese ?",
       main = paste("Cross plot between",col_,"is_obese",
                    collapse = " "))
}

## Matrix Plots
# to be made

###########################################################################

### Splitting into Test and Train ###############
# we are making 80 - 20 random split

trainIndex <- sample(1:nrow(data_),floor(nrow(data_)*.8))

train <- data_[trainIndex,]
test <- data_[-trainIndex,]

table(train$is_obese)
table(test$is_obese)

### Modelling  Iteration 1 ###############################

## first Cut Model

mod1A <- glm( is_obese ~ . , data = train[,-1])
summary(mod1A)
pred_1A <- predict(mod1A,test[,-c(1,658)])
confusionMatrix(ifelse(pred_1A>0.8,1,0),test[,"is_obese"])

#####

mod1B <- glm( is_obese ~ . , data = train[,-1],family = binomial)
summary(mod1B)
pred_1B <- predict(mod1B,test[,-c(1,658)])
confusionMatrix(ifelse(pred_1B>0,1,0),test[,"is_obese"])

#####

mod1C <- glm( is_obese ~ . , data = train[,-1],
              control = glm.control(epsilon = 1e-10,
                                    maxit = 50,
                                    trace = TRUE) )
summary(mod1C)
pred_1C <- predict(mod1C,test[,-c(1,658)])
confusionMatrix(ifelse(pred_1C>0.5,1,0),test[,"is_obese"])

#####

mod1D <- glm( is_obese ~ . , data = train[,-1],family = binomial,
              control = glm.control(epsilon = 1e-10,
                                    maxit = 50,
                                    trace = TRUE))
summary(mod1D)
pred_1D <- predict(mod1D,test[,-c(1,658)])
confusionMatrix(ifelse(pred_1D>0.5,1,0),test[,"is_obese"])

#####

mod1E <- glm( is_obese ~ . , data = train[,-1],
              control = glm.control(epsilon = 1e-6,
                                    maxit = 50,
                                    trace = TRUE) )
summary(mod1E)
pred_1E <- predict(mod1E,test[,-c(1,658)])
confusionMatrix(ifelse(pred_1E>0.5,1,0),test[,"is_obese"])

#####

mod1F <- glm( is_obese ~ . , data = train[,-1],family = binomial,
              control = glm.control(epsilon = 1e-6,
                                    maxit = 70,
                                    trace = TRUE))
summary(mod1F)
pred_1F <- predict(mod1F,test[,-c(1,658)])
confusionMatrix(ifelse(pred_1F>0.5,1,0),test[,"is_obese"])

#####

## As we can Clerarly see the Model is not able to converge which means
## the data needs more modification

rm(mod1A,mod1B,mod1C,mod1D,mod1E,mod1F,
   pred_1A,pred_1B,pred_1C,pred_1D,pred_1E,pred_1F)

#######################################################################

### Data Processing Iteration 2##############

data_num <- data_
## finding varables which are likely to be categorical bt are numeric

apply(data_,2,function(x) length(unique(x)))

for (col_ in colnames(data_[,-135])){
  l_u <- length(unique(data_[,col_]))
  if ( l_u < 10){
    print(paste(col_,"has",l_u,"no. of Unique values",collapse = " "))
  }
}

# from the output we can see that there are quite many variables which 
# can be considered as categories rather than numerics

# converting such variable excluding "is_obese" and also printing there 
# distribution

for (col_ in colnames(data_[,-135])){
  l_u <- length(unique(data_[,col_]))
  if ( l_u < 10){
    print(paste(col_,"has",l_u,"no. of Unique values",collapse = " "))
    # print(as.data.frame(table(data_[,col_])))
    print(table(data_[,col_]))
    data_[,col_] <- as.factor(data_[,col_])
  }
}

## dropping variable with only one value

to_remove <- c()
for (col_ in colnames(data_[,-135])){
  l_u <- length(unique(data_[,col_]))
  if ( l_u == 1){
    print(paste(col_,"has",l_u,"no. of Unique values",collapse = " "))
    to_remove <- c(to_remove,col_)
  
  }
}


# again splitting data without variables with only one values
train <- data_[trainIndex,setdiff(colnames(data_),to_remove)]
test <- data_[-trainIndex,setdiff(colnames(data_),to_remove)]

## Model testing

mod_test <- glm( is_obese ~ . , data = train[,-1])
summary(mod_test)
pred_test <- predict(mod_test,test[,])

# since we found that while splitting we lost some information 
# redoing the split in another way and re training

table(data_$pad200)
# -1    1    2    3    9 
# 2149 2979 3620  200    1 

# This tells us that in the data we might have to split 
# considering this distribution

# finding such situations
data_tp <- data_[0,]

for (col_ in colnames(data_)){
  if (is.factor(data_[,col_])){
    tbl_dat <- data.frame(table(data_[,col_]))
    tbl_dat <- tbl_dat[with(tbl_dat,order(Freq)),]
    val_ <- tbl_dat[which(tbl_dat$Freq <= 5),"Var1"]
    print(col_)
    # print(head(tbl_dat))
    # print(tbl_dat[which(tbl_dat$Freq <= 1),])
    print(val_)
    print(length(val_))
    if(length(val_) >0){
      data_tp <- rbind(data_tp,data_[which(data_[,col_] %in% val_),])
    }
  }
}

# This data has to be in training data for model to be capture complte 
# behavior

# removing data from train and test set
sp_indexes <- rownames(data_tp)
train <- train[setdiff(rownames(train),sp_indexes),]
test <- test[setdiff(rownames(test),sp_indexes),]

## adding this data only to training set
train <- rbind(train,data_tp[,setdiff(colnames(data_tp),to_remove)])

#removing extra variables
rm(mod_test,pred_test,data_tp,tbl_dat,val_,col_,l_u)

# since dependent variable is derived from bmxbmi we will drop this 
# variable for this analysis

to_keep <- setdiff(colnames(train),c("seqn","bmxbmi"))

#updating training and testing indexes
trainIndex <- rownames(train)

#### Iteration 2 for Data Processing ends ###

### Modelling Iteration 2 #####################

mod2B <- glm( is_obese ~ . , data = train[,-1])
summary(mod2B)
pred_2B <- predict(mod2B,test)
confusionMatrix(ifelse(pred_2B>0.5,1,0),test[,"is_obese"])

####

mod2C <- glm( is_obese ~ . , data = train[,-1], family = binomial)
summary(mod2C)
pred_2C <- predict(mod2C,test)
confusionMatrix(ifelse(pred_2C>0.5,1,0),test[,"is_obese"])

####

mod2D <- glm( is_obese ~ . , data = train[,-1],
              control = glm.control(epsilon = 1e-6,
                                    maxit = 50,
                                    trace = TRUE) )
summary(mod2D)
pred_2D <- predict(mod2D,test[,-c(1,658)])
confusionMatrix(ifelse(pred_2D>0.5,1,0),test[,"is_obese"])

####

mod2E <- glm( is_obese ~ . , data = train[,-1],
              family = binomial,
              control = glm.control(epsilon = 1e-6,
                                    maxit = 50,
                                    trace = TRUE) )
summary(mod2E)
pred_2E <- predict(mod2E,test[,-c(1,658)])
confusionMatrix(ifelse(pred_2E>0.5,1,0),test[,"is_obese"])

## cleaning up
rm(mod2A,mod2B,mod2C,mod2D,mod2E,pred_2A,pred_2B,pred_2C,pred_2D,
   pred_2E)

## Since none of model are converging we will not trust the outputs and
## will now use other models

## CART Model

## making cart models
modCart1 <- rpart(is_obese ~ . ,
                   data = train[,to_keep])

rpart.plot(modCart1)
predict_cart1 <- predict(modCart1,test)

confusionMatrix(ifelse(predict_cart1>0.5,1,0),test[,"is_obese"])
confusionMatrix(ifelse(predict_cart1>0.4,1,0),test[,"is_obese"])
confusionMatrix(ifelse(predict_cart1>0.3,1,0),test[,"is_obese"])
confusionMatrix(ifelse(predict_cart1>0.2,1,0),test[,"is_obese"])

####

modCart2 <- rpart(is_obese ~ . ,
                  data = train[,to_keep],
                  control = rpart.control(cp = 0.001,minsplit = 18,
                                          minbucket = 6,maxcompete = 4,
                                          maxdepth = 30,surrogatestyle = 0))

rpart.plot(modCart2)

predict_cart2 <- predict(modCart2,test)

confusionMatrix(ifelse(predict_cart2>0.5,1,0),test[,"is_obese"])
confusionMatrix(ifelse(predict_cart2>0.4,1,0),test[,"is_obese"])
confusionMatrix(ifelse(predict_cart2>0.3,1,0),test[,"is_obese"])

####

modCart3 <- rpart(is_obese ~ . ,
                  data = train[,to_keep],
                  method = "class",
                  control = rpart.control(cp = 0.001,minsplit = 18,
                                          minbucket = 6,maxcompete = 4,
                                          maxdepth = 30,surrogatestyle = 0))

rpart.plot(modCart3)

predict_cart3 <- predict(modCart3,test)

head(predict_cart3)
class(predict_cart3)
predict_cart3 <- as.data.frame(predict_cart3)

confusionMatrix(ifelse(predict_cart3[,"1"]>predict_cart3[,"0"],1,0),test[,"is_obese"])

####

modCart4 <- rpart(is_obese ~ . ,
                  data = train[,to_keep],
                  method = "class",
                  control = rpart.control(cp = 0.0001,minsplit = 18,
                                          minbucket = 6,maxcompete = 10,
                                          maxdepth = 30,surrogatestyle = 0))

rpart.plot(modCart4)

predict_cart4 <- predict(modCart4,test)
predict_cart4 <- as.data.frame(predict_cart4)

confusionMatrix(ifelse(predict_cart4[,"1"]>predict_cart4[,"0"],1,0),test[,"is_obese"])

####

modCart5 <- rpart(is_obese ~ . ,
                  data = train[,to_keep],
                  method = "class",
                  control = rpart.control(cp = 0.005,minsplit = 18,
                                          minbucket = 6,maxcompete = 5,
                                          maxdepth = 30,surrogatestyle = 0))

rpart.plot(modCart5)

predict_cart5 <- predict(modCart5,test)
predict_cart5 <- as.data.frame(predict_cart5)

confusionMatrix(ifelse(predict_cart5[,"1"]>predict_cart5[,"0"],1,0),test[,"is_obese"])

####

modCart6 <- rpart(is_obese ~ . ,
                  data = train[,to_keep],
                  method = "class",
                  control = rpart.control(cp = 0.0005,minsplit = 18,
                                          minbucket = 6,maxcompete = 5,
                                          maxdepth = 30,surrogatestyle = 0))

rpart.plot(modCart6)

predict_cart6 <- predict(modCart6,test)
predict_cart6 <- as.data.frame(predict_cart6)

confusionMatrix(ifelse(predict_cart6[,"1"]>predict_cart6[,"0"],1,0),test[,"is_obese"])

####

modCart7 <- rpart(is_obese ~ . ,
                  data = train[,to_keep],
                  method = "anova",
                  control = rpart.control(cp = 0.0005,minsplit = 18,
                                          minbucket = 6,maxcompete = 5,
                                          maxdepth = 30,surrogatestyle = 1))

rpart.plot(modCart7)

predict_cart7 <- predict(modCart7,test)

confusionMatrix(ifelse(predict_cart7>0.6,1,0),test[,"is_obese"])
confusionMatrix(ifelse(predict_cart7>0.5,1,0),test[,"is_obese"])
confusionMatrix(ifelse(predict_cart7>0.4,1,0),test[,"is_obese"])
confusionMatrix(ifelse(predict_cart7>0.3,1,0),test[,"is_obese"])
confusionMatrix(ifelse(predict_cart7>0.45,1,0),test[,"is_obese"])

## the last is best prediction from above model

#cleaning else
rm(modCart1,modCart2,modCart3,modCart4,modCart5,modCar6,predict_cart1,
   predict_cart2,predict_cart3,predict_cart4,predict_cart5,predict_cart6)


## Decision tree Model

modDT1 <- ctree(is_obese ~ ., data=train[,to_keep])

plot(modDT1)

pred_dt1 <- predict(modDT1,test)

confusionMatrix(ifelse(pred_dt1>0.5,1,0),test[,"is_obese"])
confusionMatrix(ifelse(pred_dt1>0.6,1,0),test[,"is_obese"])
confusionMatrix(ifelse(pred_dt1>0.7,1,0),test[,"is_obese"])
confusionMatrix(ifelse(pred_dt1>0.56,1,0),test[,"is_obese"])

# This is by far best model we have. Now we'll tune it get better results

modDT2 <- ctree(is_obese ~ ., data=train[,to_keep],
                controls = ctree_control(teststat = "quad",
                                         testtype = "Bonferroni",
                                         mincriterion = 0.95,
                                         minsplit = 20,
                                         minbucket = 7,
                                         # nresample = 100,
                                         mtry = 0,
                                         maxdepth = 0))

plot(modDT2)

pred_dt2 <- predict(modDT2,test)

confusionMatrix(ifelse(pred_dt2>0.5,1,0),test[,"is_obese"])
confusionMatrix(ifelse(pred_dt2>0.57,1,0),test[,"is_obese"])
# this was model built without any control argument passed

####

modDT3 <- ctree(is_obese ~ ., data=train[,to_keep],
                controls = ctree_control(teststat = "max",
                                         testtype = "Bonferroni",
                                         mincriterion = 0.95,
                                         minsplit = 20,
                                         minbucket = 7,
                                         # nresample = 100,
                                         mtry = 0,
                                         maxdepth = 0))

plot(modDT3)

pred_dt3 <- predict(modDT3,test)

confusionMatrix(ifelse(pred_dt3>0.5,1,0),test[,"is_obese"])
confusionMatrix(ifelse(pred_dt3>0.6,1,0),test[,"is_obese"])
confusionMatrix(ifelse(pred_dt3>0.56,1,0),test[,"is_obese"])

####

modDT4 <- ctree(is_obese ~ ., data=train[,to_keep],
                controls = ctree_control(teststat = "quad",
                                         testtype = "Bonferroni",
                                         mincriterion = 0.95,
                                         minsplit = 20,
                                         minbucket = 7,
                                         # nresample = 100,
                                         mtry = 0,
                                         maxdepth = 0))

plot(modDT4)

pred_dt4 <- predict(modDT4,test)

confusionMatrix(ifelse(pred_dt4>0.5,1,0),test[,"is_obese"])

####

modDT5 <- ctree(is_obese ~ ., data=train[,to_keep],
                controls = ctree_control(teststat = "max",
                                         testtype = "Univariate",
                                         mincriterion = 0.95,
                                         minsplit = 20,
                                         minbucket = 7,
                                         # nresample = 100,
                                         mtry = 0,
                                         maxdepth = 0))

plot(modDT5)

pred_dt5 <- predict(modDT5,test)

confusionMatrix(ifelse(pred_dt5>0.5,1,0),test[,"is_obese"])
confusionMatrix(ifelse(pred_dt5>0.6,1,0),test[,"is_obese"])
confusionMatrix(ifelse(pred_dt5>0.4,1,0),test[,"is_obese"])
confusionMatrix(ifelse(pred_dt5>0.35,1,0),test[,"is_obese"])

####

modDT6 <- ctree(is_obese ~ ., data=train[,to_keep],
                controls = ctree_control(teststat = "quad",
                                         testtype = "Univariate",
                                         mincriterion = 0.95,
                                         minsplit = 20,
                                         minbucket = 7,
                                         # nresample = 100,
                                         mtry = 0,
                                         maxdepth = 0))

plot(modDT6)

pred_dt6 <- predict(modDT6,test)

confusionMatrix(ifelse(pred_dt6>0.5,1,0),test[,"is_obese"])
confusionMatrix(ifelse(pred_dt6>0.6,1,0),test[,"is_obese"])
confusionMatrix(ifelse(pred_dt6>0.4,1,0),test[,"is_obese"])
confusionMatrix(ifelse(pred_dt6>0.35,1,0),test[,"is_obese"])

# teststat "qaud" is not as good as "max" for this data
# hence we will iterarrte with "max" for future iterations

# we will consider modDT5 as our best tree model

## KNN Model

library(class)


modknn1 <- knn(train = subset(train[,to_keep],select = -c(is_obese)),
               test = subset(test[,to_keep],select = -c(is_obese)),
               cl = train[,"is_obese"],k = 5 , l = 0 , prob = TRUE)

confusionMatrix(modknn1,test[,"is_obese"])

####

modknn2 <- knn(train = subset(train[,to_keep],select = -c(is_obese)),
               test = subset(test[,to_keep],select = -c(is_obese)),
               cl = train[,"is_obese"],
               k = 10 , l = 0 , prob = TRUE)

confusionMatrix(modknn2,test[,"is_obese"])

####

modknn3 <- knn(train = subset(train[,to_keep],select = -c(is_obese)),
               test = subset(test[,to_keep],select = -c(is_obese)),
               cl = train[,"is_obese"],
               k = 10 , l = 1 , prob = TRUE)

confusionMatrix(modknn3,test[,"is_obese"])

####

modknn4 <- knn(train = subset(train[,to_keep],select = -c(is_obese)),
               test = subset(test[,to_keep],select = -c(is_obese)),
               cl = train[,"is_obese"],
               k = 3 , l = 0 , prob = TRUE)

confusionMatrix(modknn4,test[,"is_obese"])

####

modknn5 <- knn(train = subset(train[,to_keep],select = -c(is_obese)),
               test = subset(test[,to_keep],select = -c(is_obese)),
               cl = train[,"is_obese"],
               k = 3 , l = 1 , prob = TRUE)

confusionMatrix(modknn5,test[,"is_obese"])

####

modknn6 <- knn(train = subset(train[,to_keep],select = -c(is_obese)),
               test = subset(test[,to_keep],select = -c(is_obese)),
               cl = train[,"is_obese"],
               k = 2 , l = 0 , prob = TRUE)

confusionMatrix(modknn6,test[,"is_obese"])

## since we can see that KNN is not at all behaving well,
## this essentially indicates noise and mulitcolinearity in
## data, which we might need to address


### Naive bayes model
library(e1071)

modNB1 <- naiveBayes(is_obese ~. , data = train[,to_keep])
summary(modNB1)

pred_nb1 <- as.data.frame(predict(modNB1,test,type = "raw"))

confusionMatrix(ifelse(pred_nb1[,"1"]>pred_nb1[,"0"],1,0),test[,"is_obese"])
confusionMatrix(ifelse(pred_nb1[,"1"]>0.5,1,0),test[,"is_obese"])

####

modNB2 <- naiveBayes(is_obese ~. , data = train[,to_keep],laplace = 1)
summary(modNB1)

pred_nb2 <- as.data.frame(predict(modNB2,test,type = "raw"))

confusionMatrix(ifelse(pred_nb1[,"1"]>0.5,1,0),test[,"is_obese"])
confusionMatrix(ifelse(pred_nb2[,"1"]>pred_nb2[,"0"],1,0),test[,"is_obese"])

####

modNB3 <- naiveBayes(is_obese ~. , data = train[,to_keep],laplace = 3)
summary(modNB3)

pred_nb3 <- as.data.frame(predict(modNB3,test,type = "raw"))

confusionMatrix(ifelse(pred_nb3[,"1"]>pred_nb3[,"0"],1,0),test[,"is_obese"])
confusionMatrix(ifelse(pred_nb1[,"1"]>0.5,1,0),test[,"is_obese"])

####

modNB4 <- naiveBayes(is_obese ~. , data = train[,to_keep],laplace = 1)
summary(modNB4)

pred_nb4 <- as.data.frame(predict(modNB4,test,type = "raw"))

confusionMatrix(ifelse(pred_nb4[,"1"]>pred_nb4[,"0"],1,0),test[,"is_obese"])
confusionMatrix(ifelse(pred_nb1[,"1"]>0.5,1,0),test[,"is_obese"])

### essentially the model is performing same

rm(modNB1,modNB2,modNB3,pred_nb1,pred_nb2,pred_nb3,pred_nb4)

######## Modelling Iteration 2 ends

#### Data Preparation Iteration 3 #########################

## since we saw that we cannot improve model performance by
## merely changing parameters, we now need to dig deeper into 
## data.

# Checking correlation
cor_ <- corr.test(data_num[,setdiff(colnames(data_num),
                                    c(to_remove,"seqn","is_obese"))],
                  use = "pairwise",method="pearson",
                  adjust="holm", alpha=.05,ci=TRUE)

names(cor_)
head(as.data.frame(cor_$r))

# write.csv(as.data.frame(cor_$r),file = "correlation-mat.csv",
#           row.names = F)

# # ++++++++++++++++++++++++++++
# # flattenCorrMatrix
# # ++++++++++++++++++++++++++++
# # cormat : matrix of the correlation coefficients
# # pmat : matrix of the correlation p-values
# flattenCorrMatrix <- function(cormat, pmat) {
#   ut <- upper.tri(cormat)
#   data.frame(
#     row = rownames(cormat)[row(cormat)[ut]],
#     column = rownames(cormat)[col(cormat)[ut]],
#     cor  =(cormat)[ut],
#     p = pmat[ut]
#   )
# }
# 
# 
# cor_flat <- flattenCorrMatrix(cor_$r,cor_$P)
# length(unique(cor_flat$column))
# length(unique(cor_flat$row))
# 
# head(cor_flat)
# 
# nrow(cor_flat[which(cor_flat$p <= 0.05 & (cor_flat$cor >= 0.6 | cor_flat$cor <= -0.6)),])
# cor_flat_req <- cor_flat[which(cor_flat$p <= 0.05 & (cor_flat$cor >= 0.6 | cor_flat$cor <= -0.6)),]
# table(cor_flat_req$row)
# 
# length(unique(cor_flat_req$row))
# length(unique(cor_flat_req$column))
# max(cor_flat_req$p)
# min(cor_flat_req$p)
# 
# length(intersect(unique(cor_flat_req$row),unique(cor_flat_req$column)))
# setdiff(unique(cor_flat_req$row),unique(cor_flat_req$column))

to_remov_cor <- c()

to_keep_cor <- setdiff(to_keep,to_remov_cor)

modcor <- glm(is_obese ~ . , data = train[,to_keep_cor])
summary(modcor)
plot(modcor)
pred_modcor <- predict(modcor,test)

confusionMatrix(ifelse(pred_modcor>0.3,1,0),test[,"is_obese"])

# Information Value
iv_init <- Information::create_infotables(data_,y="is_obese")
iv_init_table <- data.frame(iv_init$Summary)
iv_init_table$IV <- round(iv_init_table$IV,3)
iv_init_table

to_keep_iv <- iv_init_table[which(iv_init_table$IV >= 0.2), "Variable"]

to_keep_iv <- c(to_keep_iv,"is_obese")


modiv <- glm(is_obese ~ . , data = train[,to_keep_iv])
summary(modiv)
plot(modiv)
pred_modiv <- predict(modiv,test)

confusionMatrix(ifelse(pred_modiv>0.5,1,0),test[,"is_obese"])
confusionMatrix(ifelse(pred_modiv>0.4,1,0),test[,"is_obese"])
confusionMatrix(ifelse(pred_modiv>0.3,1,0),test[,"is_obese"])
confusionMatrix(ifelse(pred_modiv>0.35,1,0),test[,"is_obese"])

to_keep <- to_keep_iv
# to_keep <- intersect(to_keep_cor,to_keep_iv)


# this is stable outpu hence going further modelling with these
# list of variable

## cleaning up
rm(modiv,modcor,pred_modiv,pred_modcor)

#### Data preparation Iteration 3 ends

### Modelling Iteration 3 #####################

## GLM Model 

mod3B <- glm( is_obese ~ . , data = train[,to_keep])
summary(mod3B)
pred_2B <- predict(mod3B,test)
confusionMatrix(ifelse(pred_2B>0.5,1,0),test[,"is_obese"])

####

mod3C <- glm( is_obese ~ . , data = train[,to_keep], family = binomial)
summary(mod3C)
pred_2C <- predict(mod3C,test)
confusionMatrix(ifelse(pred_2C>0.5,1,0),test[,"is_obese"])

####

mod3D <- glm( is_obese ~ . , data = train[,to_keep],
              control = glm.control(epsilon = 1e-6,
                                    maxit = 50,
                                    trace = TRUE) )
summary(mod3D)
pred_2D <- predict(mod3D,test[,-c(1,658)])
confusionMatrix(ifelse(pred_2D>0.5,1,0),test[,"is_obese"])

####

mod3E <- glm( is_obese ~ . , data = train[,to_keep],
              family = binomial,
              control = glm.control(epsilon = 1e-6,
                                    maxit = 50,
                                    trace = TRUE) )
summary(mod3E)
pred_2E <- predict(mod3E,test[,-c(1,658)])
confusionMatrix(ifelse(pred_2E>0.5,1,0),test[,"is_obese"])

## cleaning up
rm(mod3A,mod3B,mod3C,mod3D,mod3E,pred_2A,pred_2B,pred_2C,pred_2D,
   pred_2E)

## Since none of model are converging we will now use other 
## models

## CART Model

## making cart models
modCart1 <- rpart(is_obese ~ . ,
                  data = train[,to_keep])

rpart.plot(modCart1)
predict_cart1 <- predict(modCart1,test)

confusionMatrix(ifelse(predict_cart1>0.5,1,0),test[,"is_obese"])
confusionMatrix(ifelse(predict_cart1>0.4,1,0),test[,"is_obese"])
confusionMatrix(ifelse(predict_cart1>0.3,1,0),test[,"is_obese"])
confusionMatrix(ifelse(predict_cart1>0.2,1,0),test[,"is_obese"])

####

modCart2 <- rpart(is_obese ~ . ,
                  data = train[,to_keep],
                  control = rpart.control(cp = 0.001,minsplit = 18,
                                          minbucket = 6,maxcompete = 4,
                                          maxdepth = 30,surrogatestyle = 0))

rpart.plot(modCart2)

predict_cart2 <- predict(modCart2,test)

confusionMatrix(ifelse(predict_cart2>0.5,1,0),test[,"is_obese"])
confusionMatrix(ifelse(predict_cart2>0.4,1,0),test[,"is_obese"])
confusionMatrix(ifelse(predict_cart2>0.3,1,0),test[,"is_obese"])

####

modCart3 <- rpart(is_obese ~ . ,
                  data = train[,to_keep],
                  method = "class",
                  control = rpart.control(cp = 0.001,minsplit = 18,
                                          minbucket = 6,maxcompete = 4,
                                          maxdepth = 30,surrogatestyle = 0))

rpart.plot(modCart3)

predict_cart3 <- predict(modCart3,test)

head(predict_cart3)
class(predict_cart3)
predict_cart3 <- as.data.frame(predict_cart3)

confusionMatrix(ifelse(predict_cart3[,"1"]>predict_cart3[,"0"],1,0),test[,"is_obese"])

####

modCart4 <- rpart(is_obese ~ . ,
                  data = train[,to_keep],
                  method = "class",
                  control = rpart.control(cp = 0.0001,minsplit = 18,
                                          minbucket = 6,maxcompete = 10,
                                          maxdepth = 30,surrogatestyle = 0))

rpart.plot(modCart4)

predict_cart4 <- predict(modCart4,test)
predict_cart4 <- as.data.frame(predict_cart4)

confusionMatrix(ifelse(predict_cart4[,"1"]>predict_cart4[,"0"],1,0),test[,"is_obese"])

####

modCart5 <- rpart(is_obese ~ . ,
                  data = train[,to_keep],
                  method = "class",
                  control = rpart.control(cp = 0.005,minsplit = 18,
                                          minbucket = 6,maxcompete = 5,
                                          maxdepth = 30,surrogatestyle = 0))

rpart.plot(modCart5)

predict_cart5 <- predict(modCart5,test)
predict_cart5 <- as.data.frame(predict_cart5)

confusionMatrix(ifelse(predict_cart5[,"1"]>predict_cart5[,"0"],1,0),test[,"is_obese"])

####

modCart6 <- rpart(is_obese ~ . ,
                  data = train[,to_keep],
                  method = "class",
                  control = rpart.control(cp = 0.0005,minsplit = 18,
                                          minbucket = 6,maxcompete = 5,
                                          maxdepth = 30,surrogatestyle = 0))

rpart.plot(modCart6)

predict_cart6 <- predict(modCart6,test)
predict_cart6 <- as.data.frame(predict_cart6)

confusionMatrix(ifelse(predict_cart6[,"1"]>predict_cart6[,"0"],1,0),test[,"is_obese"])

####

modCart7 <- rpart(is_obese ~ . ,
                  data = train[,to_keep],
                  method = "anova",
                  control = rpart.control(cp = 0.0005,minsplit = 18,
                                          minbucket = 6,maxcompete = 5,
                                          maxdepth = 30,surrogatestyle = 1))

rpart.plot(modCart7)

predict_cart7 <- predict(modCart7,test)

confusionMatrix(ifelse(predict_cart7>0.6,1,0),test[,"is_obese"])
confusionMatrix(ifelse(predict_cart7>0.5,1,0),test[,"is_obese"])
confusionMatrix(ifelse(predict_cart7>0.4,1,0),test[,"is_obese"])
confusionMatrix(ifelse(predict_cart7>0.3,1,0),test[,"is_obese"])
confusionMatrix(ifelse(predict_cart7>0.45,1,0),test[,"is_obese"])

## the last is best prediction from above model

#cleaning else
rm(modCart1,modCart2,modCart3,modCart4,modCart5,modCar6,predict_cart1,
   predict_cart2,predict_cart3,predict_cart4,predict_cart5,predict_cart6)


## Decision tree Model

modDT1 <- ctree(is_obese ~ ., data=train[,to_keep])

plot(modDT1)

pred_dt1 <- predict(modDT1,test)

confusionMatrix(ifelse(pred_dt1>0.5,1,0),test[,"is_obese"])
confusionMatrix(ifelse(pred_dt1>0.6,1,0),test[,"is_obese"])
confusionMatrix(ifelse(pred_dt1>0.7,1,0),test[,"is_obese"])
confusionMatrix(ifelse(pred_dt1>0.56,1,0),test[,"is_obese"])

# This is by far best model we have. Now we'll tune it get better results

modDT2 <- ctree(is_obese ~ ., data=train[,to_keep],
                controls = ctree_control(teststat = "quad",
                                         testtype = "Bonferroni",
                                         mincriterion = 0.95,
                                         minsplit = 20,
                                         minbucket = 7,
                                         # nresample = 100,
                                         mtry = 0,
                                         maxdepth = 0))

plot(modDT2)

pred_dt2 <- predict(modDT2,test)

confusionMatrix(ifelse(pred_dt2>0.5,1,0),test[,"is_obese"])
confusionMatrix(ifelse(pred_dt2>0.57,1,0),test[,"is_obese"])
# this was model built without any control argument passed

####

modDT3 <- ctree(is_obese ~ ., data=train[,to_keep],
                controls = ctree_control(teststat = "max",
                                         testtype = "Bonferroni",
                                         mincriterion = 0.95,
                                         minsplit = 20,
                                         minbucket = 7,
                                         # nresample = 100,
                                         mtry = 0,
                                         maxdepth = 0))

plot(modDT3)

pred_dt3 <- predict(modDT3,test)

confusionMatrix(ifelse(pred_dt3>0.5,1,0),test[,"is_obese"])
confusionMatrix(ifelse(pred_dt3>0.6,1,0),test[,"is_obese"])
confusionMatrix(ifelse(pred_dt3>0.56,1,0),test[,"is_obese"])

####

modDT4 <- ctree(is_obese ~ ., data=train[,to_keep],
                controls = ctree_control(teststat = "quad",
                                         testtype = "Bonferroni",
                                         mincriterion = 0.95,
                                         minsplit = 20,
                                         minbucket = 7,
                                         # nresample = 100,
                                         mtry = 0,
                                         maxdepth = 0))

plot(modDT4)

pred_dt4 <- predict(modDT4,test)

confusionMatrix(ifelse(pred_dt4>0.5,1,0),test[,"is_obese"])

####

modDT5 <- ctree(is_obese ~ ., data=train[,to_keep],
                controls = ctree_control(teststat = "max",
                                         testtype = "Univariate",
                                         mincriterion = 0.95,
                                         minsplit = 20,
                                         minbucket = 7,
                                         # nresample = 100,
                                         mtry = 0,
                                         maxdepth = 0))

plot(modDT5)

pred_dt5 <- predict(modDT5,test)

confusionMatrix(ifelse(pred_dt5>0.5,1,0),test[,"is_obese"])
confusionMatrix(ifelse(pred_dt5>0.6,1,0),test[,"is_obese"])
confusionMatrix(ifelse(pred_dt5>0.4,1,0),test[,"is_obese"])
confusionMatrix(ifelse(pred_dt5>0.35,1,0),test[,"is_obese"])

####

modDT6 <- ctree(is_obese ~ ., data=train[,to_keep],
                controls = ctree_control(teststat = "quad",
                                         testtype = "Univariate",
                                         mincriterion = 0.95,
                                         minsplit = 20,
                                         minbucket = 7,
                                         # nresample = 100,
                                         mtry = 0,
                                         maxdepth = 0))

plot(modDT6)

pred_dt6 <- predict(modDT6,test)

confusionMatrix(ifelse(pred_dt6>0.5,1,0),test[,"is_obese"])
confusionMatrix(ifelse(pred_dt6>0.6,1,0),test[,"is_obese"])
confusionMatrix(ifelse(pred_dt6>0.4,1,0),test[,"is_obese"])
confusionMatrix(ifelse(pred_dt6>0.35,1,0),test[,"is_obese"])

# teststat "qaud" is not as good as "max" for this data
# hence we will iterarrte with "max" for future iterations

# we will consider modDT5 as our best tree model

rm(modDT1,modDT2,modDT3,modDT4,modDT6,pred_dt1,pred_dt2,pred_dt3,
   pred_dt4,pred_dt6,pred_dt5)

## KNN Model

modknn1 <- knn(train = subset(train[,to_keep],select = -c(is_obese)),
               test = subset(test[,to_keep],select = -c(is_obese)),
               cl = train[,"is_obese"],k = 5 , l = 0 , prob = TRUE)

confusionMatrix(modknn1,test[,"is_obese"])

####

modknn2 <- knn(train = subset(train[,to_keep],select = -c(is_obese)),
               test = subset(test[,to_keep],select = -c(is_obese)),
               cl = train[,"is_obese"],
               k = 10 , l = 0 , prob = TRUE)

confusionMatrix(modknn2,test[,"is_obese"])

####

modknn3 <- knn(train = subset(train[,to_keep],select = -c(is_obese)),
               test = subset(test[,to_keep],select = -c(is_obese)),
               cl = train[,"is_obese"],
               k = 10 , l = 1 , prob = TRUE)

confusionMatrix(modknn3,test[,"is_obese"])

####

modknn4 <- knn(train = subset(train[,to_keep],select = -c(is_obese)),
               test = subset(test[,to_keep],select = -c(is_obese)),
               cl = train[,"is_obese"],
               k = 3 , l = 0 , prob = TRUE)

confusionMatrix(modknn4,test[,"is_obese"])

####

modknn5 <- knn(train = subset(train[,to_keep],select = -c(is_obese)),
               test = subset(test[,to_keep],select = -c(is_obese)),
               cl = train[,"is_obese"],
               k = 3 , l = 1 , prob = TRUE)

confusionMatrix(modknn5,test[,"is_obese"])

####

modknn6 <- knn(train = subset(train[,to_keep],select = -c(is_obese)),
               test = subset(test[,to_keep],select = -c(is_obese)),
               cl = train[,"is_obese"],
               k = 2 , l = 0 , prob = TRUE)

confusionMatrix(modknn6,test[,"is_obese"])

## since we can see that KNN is not at all behaving well,
## this essentially indicates noise and mulitcolinearity in
## data, which we might need to address

## cleaning up
rm(modknn1,modknn1,modknn2,modknn3,modknn4,modknn5,modknn6)

### Naive bayes model

modNB1 <- naiveBayes(is_obese ~. , data = train[,to_keep])
summary(modNB1)

pred_nb1 <- as.data.frame(predict(modNB1,test,type = "raw"))

confusionMatrix(ifelse(pred_nb1[,"1"]>pred_nb1[,"0"],1,0),test[,"is_obese"])
confusionMatrix(ifelse(pred_nb1[,"1"]>0.5,1,0),test[,"is_obese"])

####

modNB2 <- naiveBayes(is_obese ~. , data = train[,to_keep],laplace = 1)
summary(modNB1)

pred_nb2 <- as.data.frame(predict(modNB2,test,type = "raw"))

confusionMatrix(ifelse(pred_nb1[,"1"]>0.5,1,0),test[,"is_obese"])
confusionMatrix(ifelse(pred_nb2[,"1"]>pred_nb2[,"0"],1,0),test[,"is_obese"])

####

modNB3 <- naiveBayes(is_obese ~. , data = train[,to_keep],laplace = 3)
summary(modNB3)

pred_nb3 <- as.data.frame(predict(modNB3,test,type = "raw"))

confusionMatrix(ifelse(pred_nb3[,"1"]>pred_nb3[,"0"],1,0),test[,"is_obese"])
confusionMatrix(ifelse(pred_nb1[,"1"]>0.5,1,0),test[,"is_obese"])

####

modNB4 <- naiveBayes(is_obese ~. , data = train[,to_keep],laplace = 1)
summary(modNB4)

pred_nb4 <- as.data.frame(predict(modNB4,test,type = "raw"))

confusionMatrix(ifelse(pred_nb4[,"1"]>pred_nb4[,"0"],1,0),test[,"is_obese"])
confusionMatrix(ifelse(pred_nb1[,"1"]>0.5,1,0),test[,"is_obese"])

### essentially the model is performing same

rm(modNB1,modNB2,modNB3,pred_nb1,pred_nb2,pred_nb3,pred_nb4)

# Random Forest Model

modrf1 <- randomForest(is_obese ~ . , data = train[,to_keep],
                       importance = TRUE,proximity = TRUE,
                       ntree = 100,
                       # sampsize = .75, nodesize = 2,
                       # maxnodes = 50, nPerm = 1.2,
                       do.trace= TRUE)
summary(modrf1)
plot(modrf1)

pred_rf1 <- predict(modrf1,test[,to_keep])

confusionMatrix(ifelse(pred_rf1>0.5,1,0),test[,"is_obese"])
confusionMatrix(ifelse(pred_rf1>0.6,1,0),test[,"is_obese"])
confusionMatrix(ifelse(pred_rf1>0.55,1,0),test[,"is_obese"])
confusionMatrix(ifelse(pred_rf1>0.45,1,0),test[,"is_obese"])
confusionMatrix(ifelse(pred_rf1>0.4,1,0),test[,"is_obese"])

####

modrf2 <- randomForest(as.factor(is_obese) ~ . , data = train[,to_keep],
                       importance = TRUE,proximity = TRUE,
                       ntree = 120,
                       # maxnodes = 50, nPerm = 1.2,
                       do.trace= TRUE)
summary(modrf2)
plot(modrf2)

pred_rf2 <- predict(modrf2,test[,to_keep])

confusionMatrix(pred_rf2,test[,"is_obese"])

####

modrf3 <- randomForest(as.factor(is_obese) ~ . , data = train[,to_keep],
                       importance = TRUE,proximity = TRUE,
                       ntree = 200,
                       # maxnodes = 50, nPerm = 1.2,
                       do.trace= TRUE)
summary(modrf3)
plot(modrf3)

pred_rf3 <- predict(modrf3,test[,to_keep])

confusionMatrix(pred_rf3,test[,"is_obese"])

####

modrf4 <- randomForest(as.factor(is_obese) ~ . , data = train[,to_keep],
                       importance = TRUE,proximity = TRUE,
                       ntree = 150,
                       # maxnodes = 50,
                       do.trace= TRUE)
summary(modrf4)
plot(modrf4)

pred_rf4 <- predict(modrf4,test[,to_keep])

confusionMatrix(pred_rf4,test[,"is_obese"])

####

modrf5 <- randomForest(is_obese ~ . , data = train[,to_keep],
                       importance = TRUE,proximity = TRUE,
                       ntree = 150, nPerm = 1.2,
                       # maxnodes = 50,
                       do.trace= TRUE)
summary(modrf5)
plot(modrf5)

pred_rf5 <- predict(modrf5,test[,to_keep])

confusionMatrix(ifelse(pred_rf5>0.5,1,0),test[,"is_obese"])

####

modrf6 <- randomForest(as.factor(is_obese) ~ . , data = train[,to_keep],
                       importance = TRUE,proximity = TRUE,
                       ntree = 150,mtry = 15,
                       # maxnodes = 500,
                       do.trace= TRUE)
summary(modrf6)
plot(modrf6)

pred_rf6 <- predict(modrf6,test[,to_keep])

confusionMatrix(pred_rf6,test[,"is_obese"])

####

modrf7 <- randomForest(as.factor(is_obese) ~ . , data = train[,to_keep],
                       importance = TRUE,proximity = TRUE,
                       ntree = 150,mtry = 15,
                       maxnodes = 500,
                       do.trace= TRUE)
summary(modrf7)
plot(modrf7)

pred_rf7 <- predict(modrf7,test[,to_keep])

confusionMatrix(pred_rf7,test[,"is_obese"])

# best model is the 6 th mode keeping it for reference

# as we can see model performance is still not improving even while 
# tuning all this tells we need more data preparation

# cleaning up
rm(modrf1,modrf2,modrf3,modrf4,modrf5,modrf7,pred_rf1,
   pred_rf2,pred_rf3,pred_rf4,pred_rf5,pred_rf7)

### Data Preparation Iteration 4 ##########################

## since we saw that we cannot improve model performance by
## reducing the number variables, we have enough proof to believe  
## that data has interations between to each other

## Doing PCA on data

# since pca is done only on numeric data
# checking numeric data

str(data_num)

##
pca_ <- prcomp(~.,data = data_num[,-1])
names(pca_)
pca_$rotation

screeplot(pca_)
scree(cor_$r)

# since scree plot is showing only one major bar this tells data 
# has too much colinearity

# Using IV columns previously and redoing pca

pca_1 <- prcomp(~.,data = data_num[,to_keep])
pca_1$rotation

screeplot(pca_1)
scree(cor_$r)

# cor_exp <- rcorr(x = as.matrix(data_num[,to_keep])) to remove dependency in Hmisc

cor_exp <- corr.test(data_num[,setdiff(colnames(data_num),
                                       c(to_remove,"seqn","is_obese"))],
                     use = "pairwise",method="pearson",
                     adjust="holm", alpha=.05,ci=TRUE)


pca_2 <- principal( cor_exp$r, nfactors = 10, 
                    rotate="promax",scores = T)

summary(pca_2)
print.psych(pca_2)
scree(pca_2$Structure)

pca_3 <- principal( cor_exp$r, nfactors = 15, 
                    method = "classification",
                    rotate="promax",scores = T)

summary(pca_3)
print.psych(pca_3)
scree(pca_3$Structure)

pca_4 <- principal( cor_exp$r, nfactors = 30, 
                    method="classification",
                    rotate="quartimax",scores = T)
names(pca_4)
summary(pca_4)
print.psych(pca_4)
scree(pca_4$Structure)

pca_5 <- principal( cor_exp$r, nfactors = 30, 
                    method="classification",
                    rotate="biquartimin",scores = T)
names(pca_5)
summary(pca_5)
print.psych(pca_5)
scree(pca_5$Structure)

pca_6 <- principal( cor_exp$r, nfactors = 30, 
                    method="classification",
                    rotate="simplimax",scores = T)
names(pca_6)
summary(pca_6)
print.psych(pca_6)
scree(pca_6$Structure)

pca_7 <- principal( cor_exp$r, nfactors = 15, 
                    method="classification",
                    rotate="oblimin",scores = T)
names(pca_7)
summary(pca_7)
print.psych(pca_7)
scree(pca_7$Structure)

pca_8 <- principal( cor_exp$r, nfactors = 15, 
                    method="classification",
                    rotate="cluster",scores = T)
names(pca_8)
summary(pca_8)
print.psych(pca_8)
scree(pca_8$Structure)

## since pca_5 explains the data properly we'll take this for 
## our next level of variable seletion

write.csv(pca_5$Structure,file = "pca-evaluation.csv",row.names = TRUE)

## also trying to sele the no of factors for data
# library(nFactors)
# ev <- eigen(cor(data_num[,setdiff(colnames(data_num),
#                                   c(to_remove,"seqn","is_obese"))])) # get eigenvalues
# ap <- parallel(subject=nrow(data_num[,setdiff(colnames(data_num),
#                                               c(to_remove,"seqn"))]),var=ncol(data_num[,setdiff(colnames(data_num),
#                                                                c(to_remove,"seqn"))]),
#                rep=100,cent=.05)
# nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
# plotnScree(nS)

# we took 15 variables and as suggested by this method is 16 
# variables .. We will stick with 15 variables and do further 
# modelling

#getting maximum value for each variable in structure matrix
# var_name <- setdiff(rownames(pca_5$Structure),"is_obese")
# colnames(pca_5$Structure)[apply(pca_5$Structure,1,which.max)]
# apply(pca_5$Structure,1,max)

pca_col <- data.frame(Variable = rownames(pca_5$Structure),
                      field_var = colnames(pca_5$Structure)[apply(pca_5$Structure,1,which.max)],
                      value = apply(pca_5$Structure,1,max))


# Generating data form pca evaluation

mat_dat <- as.matrix(data_num[,setdiff(colnames(data_num),
                                       c(to_remove,"seqn","is_obese"))]) %*% as.matrix(pca_5$Structure)

class(mat_dat)
mat_dat <- as.data.frame(mat_dat)
mat_dat <- cbind(mat_dat,data_num[,"is_obese"])
colnames(mat_dat)[16] <- "is_obese" 

### new data for testing
train <- mat_dat[trainIndex,]
test <- mat_dat[rownames(test),]

mat_dat[is.na(mat_dat$is_obese),]
train[is.na(train$is_obese),]

if (anyNA(train$is_obese)){
  train <- train[which(train$is_obese != "NA" ),]
}

train[is.na(train$is_obese),]
test[is.na(test$is_obese),]

#cleaning up
rm(cor_exp,pca_,pca_1,pca_2,pca_3,pca_4,pca_5,pca_6,pca_7,ev,nS,ap)

#### Data Preparation Iteration 4 Ends

##### Modelling Iteration 4 ####################

## GLM Model 

modglm1 <- glm( is_obese ~ . , data = train,
              control = glm.control(epsilon = 1e-6,
                                    maxit = 50,
                                    trace = TRUE) )
summary(modglm1)
pred_glm1 <- predict(modglm1,test)
confusionMatrix(ifelse(pred_glm1>0.5,1,0),test[,"is_obese"])

####

modglm2 <- glm( is_obese ~ . , data = train,
                family = binomial,
                control = glm.control(epsilon = 1e-6,
                                      maxit = 50,
                                      trace = TRUE) )
summary(modglm2)
pred_glm2 <- predict(modglm2,test)
confusionMatrix(ifelse(pred_glm2>0.5,1,0),test[,"is_obese"])

## cleaning up
rm(modglm1,modglm2,pred_glm1,pred_glm2)

## not Even close to our previous attempts

## CART Model

## making cart models
modCart1 <- rpart(is_obese ~ . ,
                  data = train)

rpart.plot(modCart1)
predict_cart1 <- predict(modCart1,test)

confusionMatrix(ifelse(predict_cart1>0.5,1,0),test[,"is_obese"])
confusionMatrix(ifelse(predict_cart1>0.4,1,0),test[,"is_obese"])
confusionMatrix(ifelse(predict_cart1>0.3,1,0),test[,"is_obese"])
confusionMatrix(ifelse(predict_cart1>0.2,1,0),test[,"is_obese"])

#cleaning else
rm(modCart1,predict_cart1)


## Decision tree Model

modDT1 <- ctree(is_obese ~ ., data=train)

plot(modDT1)

pred_dt1 <- predict(modDT1,test)

confusionMatrix(ifelse(pred_dt1>0.5,1,0),test[,"is_obese"])
confusionMatrix(ifelse(pred_dt1>0.6,1,0),test[,"is_obese"])
confusionMatrix(ifelse(pred_dt1>0.7,1,0),test[,"is_obese"])
confusionMatrix(ifelse(pred_dt1>0.56,1,0),test[,"is_obese"])

####

modDT2 <- ctree(as.factor(is_obese) ~ ., data=train,
                controls = ctree_control(teststat = "max",
                                         testtype = "Bonferroni",
                                         mincriterion = 0.95,
                                         minsplit = 20,
                                         minbucket = 7,
                                         # nresample = 100,
                                         mtry = 0,
                                         maxdepth = 0))

plot(modDT2)

pred_dt2 <- predict(modDT2,test)

confusionMatrix(ifelse(pred_dt2>0.5,1,0),test[,"is_obese"])
confusionMatrix(ifelse(pred_dt2>0.6,1,0),test[,"is_obese"])
confusionMatrix(ifelse(pred_dt2>0.56,1,0),test[,"is_obese"])

####

modDT3 <- ctree(as.factor(is_obese) ~ ., data=train,
                controls = ctree_control(teststat = "max",
                                         testtype = "Univariate",
                                         mincriterion = 0.95,
                                         minsplit = 20,
                                         minbucket = 7,
                                         # nresample = 100,
                                         mtry = 0,
                                         maxdepth = 0))

plot(modDT3)

pred_dt3 <- predict(modDT3,test)

confusionMatrix(ifelse(pred_dt3>0.5,1,0),test[,"is_obese"])
confusionMatrix(ifelse(pred_dt3>0.6,1,0),test[,"is_obese"])
confusionMatrix(ifelse(pred_dt3>0.4,1,0),test[,"is_obese"])
confusionMatrix(ifelse(pred_dt3>0.35,1,0),test[,"is_obese"])

# we will consider modDT5 as our best tree model

rm(modDT1,modDT2,modDT3pred_dt1,pred_dt2,pred_dt3)

## KNN Model

modknn1 <- knn(train = subset(train,select = -c(is_obese)),
               test = subset(test,select = -c(is_obese)),
               cl = train[,"is_obese"],k = 5 , l = 0 , prob = TRUE)

confusionMatrix(modknn1,test[,"is_obese"])

####

modknn2 <- knn(train = subset(train,select = -c(is_obese)),
               test = subset(test,select = -c(is_obese)),
               cl = train[,"is_obese"],
               k = 10 , l = 0 , prob = TRUE)

confusionMatrix(modknn2,test[,"is_obese"])


## cleaning up
rm(modknn1,modknn2)

### Naive bayes model

modNB1 <- naiveBayes(is_obese ~. , data = train)
summary(modNB1)

pred_nb1 <- as.data.frame(predict(modNB1,test,type = "raw"))

confusionMatrix(ifelse(pred_nb1[,"1"]>pred_nb1[,"0"],1,0),test[,"is_obese"])
confusionMatrix(ifelse(pred_nb1[,"1"]>0.5,1,0),test[,"is_obese"])

### essentially the model is pathetic

rm(modNB1,pred_nb1)

# Random Forest Model

modrf1 <- randomForest(is_obese ~ . , data = train,
                       importance = TRUE,proximity = TRUE,
                       ntree = 100,
                       do.trace= TRUE)
summary(modrf1)
plot(modrf1)

pred_rf1 <- predict(modrf1,test)

confusionMatrix(ifelse(pred_rf1>0.5,1,0),test[,"is_obese"])
confusionMatrix(ifelse(pred_rf1>0.6,1,0),test[,"is_obese"])
confusionMatrix(ifelse(pred_rf1>0.55,1,0),test[,"is_obese"])
confusionMatrix(ifelse(pred_rf1>0.45,1,0),test[,"is_obese"])
confusionMatrix(ifelse(pred_rf1>0.4,1,0),test[,"is_obese"])

####

modrf2 <- randomForest(as.factor(is_obese) ~ . , data = train,
                       importance = TRUE,proximity = TRUE,
                       ntree = 120,
                       # maxnodes = 50, nPerm = 1.2,
                       do.trace= TRUE)
summary(modrf2)
plot(modrf2)

pred_rf2 <- predict(modrf2,test)

confusionMatrix(pred_rf2,test[,"is_obese"])

# cleaning up
rm(modrf1,modrf2,pred_rf1,pred_rf2)

### Neural Network Model

modnnet1 <- nnet(is_obese~., data = train,
                 size = 7 ,linout = FALSE, entropy = FALSE, 
                 softmax = FALSE,censored = FALSE, skip = FALSE, 
                 rang = 0.7, decay = 0, maxit = 100, Hess = FALSE, 
                 trace = TRUE, MaxNWts = 1000,
                 abstol = 1.0e-8, reltol = 1.0e-8)

summary(modnnet1)
pred_nn1 <- predict(modnnet1,test)

confusionMatrix(ifelse(pred_nn1>.3,1,0),test[,"is_obese"])
table(pred_nn1,test[,"is_obese"])

### As we can see that all models are performing bad even much 
### infrerior than all. This means that data has not ben properly
### We need do some modulation in how we get data.

### Modelling Iteration 4 Ends

#### Data Preparation Iteration 5 #########################

## Doing PCA again
cor_exp <- corr.test(data_num[,setdiff(colnames(data_num),
                                       c(to_remove,"seqn","is_obese"))],
                     use = "pairwise",method="pearson",
                     adjust="holm", alpha=.05,ci=TRUE)

pca_ <- principal( cor_exp$r, nfactors = 30, 
                   method="classification",
                   rotate="biquartimin",scores = T)

names(pca_)
summary(pca_)
print.psych(pca_)
scree(pca_$Structure)

mat_dat <- pca_$Structure
mat_dat <- ifelse(mat_dat==apply(mat_dat,1,max),apply(mat_dat,1,max),0)

## keeping this matrix for reference
mat_dat_ref <- as.matrix(mat_dat)

apply(mat_dat_ref,2,table)

### Getting Information
pca_col <- data.frame(Variable = rownames(pca_$Structure),
                      field_var = colnames(pca_$Structure)[apply(pca_$Structure,1,which.max)],
                      value = apply(pca_$Structure,1,max))

mat_dat <- as.matrix(data_num[,setdiff(colnames(data_num),
                                       c(to_remove,"seqn","is_obese"))]) %*% mat_dat_ref

class(mat_dat)
mat_dat <- as.data.frame(mat_dat)
mat_dat <- cbind(mat_dat,data_num[,"is_obese"])
colnames(mat_dat)[16] <- "is_obese" 

### new data for testing
train <- mat_dat[trainIndex,]
test <- mat_dat[rownames(test),]

mat_dat[is.na(mat_dat$is_obese),]
train[is.na(train$is_obese),]

if (anyNA(train$is_obese)){
  train <- train[which(train$is_obese != "NA" ),]
}

train[is.na(train$is_obese),]
test[is.na(test$is_obese),]

### as Refrenced to make another dataset 
# again looking at scree plot
scree(pca_$Structure)

# also looking at unique variables by each column
apply(mat_dat_ref,2,function(x) length(unique(x))-1)

# we see that most variables are explained by first 3 PCs
# hence selecting only these variables

# var_pca <- rownames(mat_dat_ref)[apply(mat_dat_ref[,c("RC1","RC2","RC3","RC4","RC5")],
#                                        2,which.max)]

var_pca <- rownames(mat_dat_ref)[which(mat_dat_ref[,"RC1"]>0)]
var_pca <- c(var_pca,rownames(mat_dat_ref)[which(mat_dat_ref[,"RC2"]>0)])
var_pca <- c(var_pca,rownames(mat_dat_ref)[which(mat_dat_ref[,"RC3"]>0)])

# keeping depedent and unique columns only
var_pca <- unique(c(var_pca,"is_obese"))

# also we'll consider same analysis for with selected PCs
pca_var_sel <- c("RC1","RC2","RC3","RC4","RC5","is_obese")

# So now we have Three data sets to model
# 1) Containing all PCS
# 2) Variables based on Selected PCs
# 3) Selected PCs


######### Data Preparation Iteration 5 Ends 

##### Modelling Iteration 5 ####################

## GLM Model 

# Modelling with all PCs
modglm1 <- glm( is_obese ~ . , data = train,
                control = glm.control(epsilon = 1e-6,
                                      maxit = 50,
                                      trace = TRUE) )
summary(modglm1)
pred_glm1 <- predict(modglm1,test)
confusionMatrix(ifelse(pred_glm1>0.5,1,0),test[,"is_obese"])
confusionMatrix(ifelse(pred_glm1>0.3,1,0),test[,"is_obese"])
confusionMatrix(ifelse(pred_glm1>0.2,1,0),test[,"is_obese"])

####

modglm2 <- glm( is_obese ~ . -1, data = train,
                family = binomial,
                control = glm.control(epsilon = 1e-6,
                                      maxit = 50,
                                      trace = TRUE) )
summary(modglm2)
pred_glm2 <- predict(modglm2,test)
confusionMatrix(ifelse(pred_glm2>0.5,1,0),test[,"is_obese"])

# Modelling with all selected Variables

modglm1 <- glm( is_obese ~ . , data = data_num[trainIndex,var_pca],
                control = glm.control(epsilon = 1e-6,
                                      maxit = 50,
                                      trace = TRUE) )
summary(modglm1)
pred_glm1 <- predict(modglm1,data_num[-trainIndex,var_pca])
confusionMatrix(ifelse(pred_glm1>0.5,1,0),data_num[-trainIndex,"is_obese"])
confusionMatrix(ifelse(pred_glm1>0.3,1,0),data_num[-trainIndex,"is_obese"])
confusionMatrix(ifelse(pred_glm1>0.2,1,0),data_num[-trainIndex,"is_obese"])

####

modglm2 <- glm( is_obese ~ . -1, data = data_num[trainIndex,var_pca],
                family = binomial,
                control = glm.control(epsilon = 1e-6,
                                      maxit = 50,
                                      trace = TRUE) )
summary(modglm2)
pred_glm2 <- predict(modglm2,data_num[-trainIndex,var_pca])
confusionMatrix(ifelse(pred_glm2>0.5,1,0),data_num[-trainIndex,"is_obese"])
confusionMatrix(ifelse(pred_glm2>0.3,1,0),data_num[-trainIndex,"is_obese"])
confusionMatrix(ifelse(pred_glm2>0.2,1,0),data_num[-trainIndex,"is_obese"])

# Modelling with selected PCs
modglm1 <- glm( is_obese ~ . , data = train[,pca_var_sel],
                control = glm.control(epsilon = 1e-6,
                                      maxit = 50,
                                      trace = TRUE) )
summary(modglm1)
pred_glm1 <- predict(modglm1,test)
confusionMatrix(ifelse(pred_glm1>0.5,1,0),test[,"is_obese"])
confusionMatrix(ifelse(pred_glm1>0.3,1,0),test[,"is_obese"])
confusionMatrix(ifelse(pred_glm1>0.2,1,0),test[,"is_obese"])

####

modglm2 <- glm( is_obese ~ . -1, data = train[,pca_var_sel],
                family = binomial,
                control = glm.control(epsilon = 1e-6,
                                      maxit = 50,
                                      trace = TRUE) )
summary(modglm2)
pred_glm2 <- predict(modglm2,test)
confusionMatrix(ifelse(pred_glm2>0.5,1,0),test[,"is_obese"])

#### Iterating to reduce noice
to_remove_pca <- c("RC11","RC6","RC8","RC3","RC7","RC4","RC13")
to_keep <- setdiff(colnames(train),to_remove_pca)
to_keep <- c("RC1","RC4","RC9","RC12","is_obese")

modglm2 <- glm( is_obese ~ ., data = train[,to_keep],
                family = binomial,
                control = glm.control(epsilon = 1e-6,
                                      maxit = 50,
                                      trace = TRUE) )
summary(modglm2)
plot(modglm2)
pred_glm2 <- predict(modglm2,test)
confusionMatrix(ifelse(pred_glm2>0.5,1,0),test[,"is_obese"])
confusionMatrix(ifelse(pred_glm2>0.3,1,0),test[,"is_obese"])
confusionMatrix(ifelse(pred_glm2>-1,1,0),test[,"is_obese"])

## cleaning up
rm(modglm1,modglm2,pred_glm1,pred_glm2)

## not Even close to our previous attempts

## CART Model

## making cart models
modCart1 <- rpart(is_obese ~ . ,
                  data = train[,to_keep])

rpart.plot(modCart1)
predict_cart1 <- predict(modCart1,test)

confusionMatrix(ifelse(predict_cart1>0.5,1,0),test[,"is_obese"])
confusionMatrix(ifelse(predict_cart1>0.4,1,0),test[,"is_obese"])
confusionMatrix(ifelse(predict_cart1>0.3,1,0),test[,"is_obese"])
confusionMatrix(ifelse(predict_cart1>0.2,1,0),test[,"is_obese"])

#cleaning else
rm(modCart1,predict_cart1)


## Decision tree Model

modDT1 <- ctree(is_obese ~ ., data=train[,to_keep])

plot(modDT1)

pred_dt1 <- predict(modDT1,test)

confusionMatrix(ifelse(pred_dt1>0.5,1,0),test[,"is_obese"])
confusionMatrix(ifelse(pred_dt1>0.6,1,0),test[,"is_obese"])
confusionMatrix(ifelse(pred_dt1>0.7,1,0),test[,"is_obese"])
confusionMatrix(ifelse(pred_dt1>0.4,1,0),test[,"is_obese"])

####

modDT2 <- ctree(as.factor(is_obese) ~ ., data=train[,to_keep],
                controls = ctree_control(teststat = "max",
                                         testtype = "Bonferroni",
                                         mincriterion = 0.95,
                                         minsplit = 20,
                                         minbucket = 7,
                                         # nresample = 100,
                                         mtry = 0,
                                         maxdepth = 0))

plot(modDT2)

pred_dt2 <- predict(modDT2,test)

confusionMatrix(pred_dt2,test[,"is_obese"])

####

modDT3 <- ctree(as.factor(is_obese) ~ ., data=train[,to_keep],
                controls = ctree_control(teststat = "max",
                                         testtype = "Univariate",
                                         mincriterion = 0.95,
                                         minsplit = 20,
                                         minbucket = 7,
                                         # nresample = 100,
                                         mtry = 0,
                                         maxdepth = 0))

plot(modDT3)

pred_dt3 <- predict(modDT3,test)

confusionMatrix(pred_dt3,test[,"is_obese"])

# we will consider modDT5 as our best tree model

rm(modDT1,modDT2,modDT3pred_dt1,pred_dt2,pred_dt3)

## KNN Model

modknn1 <- knn(train = subset(train,select = -c(is_obese)),
               test = subset(test,select = -c(is_obese)),
               cl = train[,"is_obese"],k = 5 , l = 0 , prob = TRUE)

confusionMatrix(modknn1,test[,"is_obese"])

####

modknn2 <- knn(train = subset(train,select = -c(is_obese)),
               test = subset(test,select = -c(is_obese)),
               cl = train[,"is_obese"],
               k = 10 , l = 0 , prob = TRUE)

confusionMatrix(modknn2,test[,"is_obese"])


## cleaning up
rm(modknn1,modknn2)

### Naive bayes model

modNB1 <- naiveBayes(is_obese ~. , data = train)
summary(modNB1)

pred_nb1 <- as.data.frame(predict(modNB1,test,type = "raw"))

confusionMatrix(ifelse(pred_nb1[,"1"]>pred_nb1[,"0"],1,0),test[,"is_obese"])
confusionMatrix(ifelse(pred_nb1[,"1"]>0.5,1,0),test[,"is_obese"])

### essentially the model is pathetic

rm(modNB1,pred_nb1)

# Random Forest Model

modrf1 <- randomForest(is_obese ~ . , data = train,
                       importance = TRUE,proximity = TRUE,
                       ntree = 100,
                       do.trace= TRUE)
summary(modrf1)
plot(modrf1)

pred_rf1 <- predict(modrf1,test)

confusionMatrix(ifelse(pred_rf1>0.5,1,0),test[,"is_obese"])
confusionMatrix(ifelse(pred_rf1>0.6,1,0),test[,"is_obese"])
confusionMatrix(ifelse(pred_rf1>0.55,1,0),test[,"is_obese"])
confusionMatrix(ifelse(pred_rf1>0.45,1,0),test[,"is_obese"])
confusionMatrix(ifelse(pred_rf1>0.4,1,0),test[,"is_obese"])

####

modrf2 <- randomForest(as.factor(is_obese) ~ . , data = train,
                       importance = TRUE,proximity = TRUE,
                       ntree = 120,
                       # maxnodes = 50, nPerm = 1.2,
                       do.trace= TRUE)
summary(modrf2)
plot(modrf2)

pred_rf2 <- predict(modrf2,test)

confusionMatrix(pred_rf2,test[,"is_obese"])

# cleaning up
rm(modrf1,modrf2,pred_rf1,pred_rf2)

### Neural Network Model

modnnet1 <- nnet(is_obese~., data = train,
                 size = 7 ,linout = FALSE, entropy = FALSE, 
                 softmax = FALSE,censored = FALSE, skip = FALSE, 
                 rang = 0.7, decay = 0, maxit = 100, Hess = FALSE, 
                 trace = TRUE, MaxNWts = 1000,
                 abstol = 1.0e-8, reltol = 1.0e-8)

summary(modnnet1)
pred_nn1 <- predict(modnnet1,test)

confusionMatrix(ifelse(pred_nn1>.3,1,0),test[,"is_obese"])
table(pred_nn1,test[,"is_obese"])

### As we can see that all models are performing bad even much 
### infrerior than all. This means that data has not ben properly
### We need do some modulation in how we get data.

### Modelling Iteration 5 Ends

#### Data Preparation Iteration 6 ###########################

## After doing so many Iteration and even testing on neural 
## network, We find that model performance is still the same.

## This is one main problem with PCA and missing values.
## If data has missing values and you impute them with some 
## constant this tends to degrade PCAs performance as it dosent
## understand why and what is done..

## This problem is even bigger when columns are categories and 
## you perform PCA on that data.( This is Our case)

## To resolve suche issues we do Factor Analysis, which is 
## essentially same type of analysis but advantage of Factor Analysis 
## lies that it can handle factors i.e categorical Variables.

## So in this step we will do Factor Analysis on data and see how 
## far things can go.

# Factor Analysis


## for this we now require orderd factors 
## using same logic as earlier for getting factors but tweaking it 
## for order based on numeriuc values
data_fa <- data_num

for (col_ in colnames(data_fa[,-135])){
  l_u <- length(unique(data_fa[,col_]))
  if ( l_u < 10){
    print(paste(col_,"has",l_u,"no. of Unique values",collapse = " "))
    # print(as.data.frame(table(data_[,col_])))
    print(table(data_[,col_]))
    data_fa[,col_] <- factor(data_fa[,col_],ordered = TRUE)
  }
}
str(data_fa)

apply(data_fa,1,function(x) ifelse(is.factor(x),table(x),print(paste(x,"is not factor!"))))

# library(polycor)
# pc <- hetcor(data_fa[,setdiff(colnames(data_fa),
#                               c(to_remove,"seqn","is_obese"))], 
#              ML=FALSE,bins = 10,std.err = TRUE)
# 
## since it is showing error as

pc <- FactoMineR::FAMD(data_[,setdiff(colnames(data_),
                                        c(to_remove,"seqn","is_obese"))],
                       ncp = 50)
pc
names(pc$var)
scree(pc$var$coord)

# hence from plot we can see that we get an elbow at around 20 factors

## exploring other to see for better results

fa <- FactoMineR::MFA(base = data_[,setdiff(colnames(data_),
                                            c(to_remove,"seqn","is_obese"))],
                      ncp = 30)

# generating Correlation Matrix with factors

# cor_exp_fa <- corr.test(data_[,setdiff(colnames(data_),
#                                     c(to_remove,"seqn","is_obese"))],
#                                    use = "pairwise",method="pearson",
#                                    adjust="holm", alpha=.05,ci=TRUE)
# 
# fa_score <- fa(pc$var$coord,nfactors=5,n.obs = 40, n.iter=1, 
#                rotate="oblimin",fm="minres",scores="regression", 
#                residuals=TRUE, SMC=TRUE,min.err = 0.001,max.iter = 50,
#                symmetric=TRUE, warnings=TRUE, 
#                alpha=.1,p=.05,oblique.scores=FALSE,
#                use="pairwise", cor="cor")
# 
# fa_score
# names(fa_score)
# scree(fa_score$Structure)
# 
# fa_score1 <- fa(cor_exp$r,nfactors=10,n.obs = 40, n.iter=1, 
#                rotate="oblimin",fm="minres",scores="regression", 
#                residuals=TRUE, SMC=TRUE,min.err = 0.001,max.iter = 50,
#                symmetric=TRUE, warnings=TRUE, 
#                alpha=.1,p=.05,oblique.scores=FALSE,
#                use="pairwise", cor="cor")
# 
# fa_score1
# names(fa_score1)
# scree(fa_score1$Structure)
# 
# fa_score2 <- fa(cor_exp$r,nfactors=10,n.obs = 40, n.iter=1, 
#                rotate="quartimax",fm="minres",scores="tenBerge", 
#                residuals=TRUE, SMC=TRUE,min.err = 0.001,max.iter = 50,
#                symmetric=TRUE, warnings=TRUE, 
#                alpha=.1,p=.05,oblique.scores=FALSE,
#                use="pairwise", cor="cor")
# 
# fa_score2
# names(fa_score2)
# scree(fa_score2$Structure)
# 
# rt <- "biquartimin"
# scr <- "Harman"
# fms <- "ml"
# cort <- "cor"
# 
# fa_score3 <- fa(cor_exp$r,nfactors=50,n.obs = 50, n.iter=1, 
#                 rotate=rt,fm=fms,scores=scr, 
#                 residuals=TRUE, min.err = 0.001,max.iter = 100,
#                 symmetric=TRUE, warnings=TRUE, 
#                 alpha=.1,p=.05,oblique.scores=FALSE,cor=cort)
# 
# # fa_score3
# # names(fa_score3)
# scree(fa_score3$Structure,main = paste("Scree Plot for ",
#                                        paste(rt,scr,fms,cort,sep="-"),
#                                        sep = ":"))
# 
colnames(pc$var$coord)

mat_dat_fa <- pc$var$coord
mat_dat_fa <- ifelse(mat_dat_fa==apply(mat_dat_fa,1,max),apply(mat_dat_fa,1,max),0)

## keeping this matrix for reference
mat_dat_ref_fa <- as.matrix(mat_dat_fa)

apply(mat_dat_ref_fa,2,table)

### Getting Information
fa_col <- data.frame(Variable = rownames(fa_score3$Structure),
                      field_var = colnames(fa_score3$Structure)[apply(fa_score3$Structure,1,which.max)],
                      value = apply(fa_score3$Structure,1,max))

mat_dat_fa <- as.matrix(data_num[,setdiff(colnames(data_num),
                                       c(to_remove,"seqn","is_obese"))]) %*% mat_dat_ref_fa

class(mat_dat_fa)
mat_dat_fa <- as.data.frame(mat_dat_fa)
mat_dat_fa <- cbind(mat_dat_fa,is_obese=data_[,"is_obese"])
colnames(mat_dat_fa)

empty_fa <- c()
for(col_ in colnames(mat_dat_fa)){
  if (length(unique(mat_dat_fa[,col_]))==1){
    print(col_)
    empty_fa <- c(empty_fa,col_)
  }
}

non_empty_fa <- setdiff(colnames(mat_dat_fa),empty_fa)
### new data for testing
train_fa <- mat_dat_fa[trainIndex,non_empty_fa]
test_fa <- mat_dat_fa[rownames(test),non_empty_fa]

mat_dat_fa[is.na(mat_dat_fa$is_obese),]
train_fa[is.na(train_fa$is_obese),]

if (anyNA(train_fa$is_obese)){
  train_fa <- train[which(train_fa$is_obese != "NA" ),]
}

train_fa[is.na(train_fa$is_obese),]
test_fa[is.na(test_fa$is_obese),]

### as Refrenced to make another dataset 
# again looking at scree plot
scree(pc$var$coord)

# also looking at unique variables by each column
apply(mat_dat_ref_fa,2,function(x) length(unique(x))-1)

# we see that most variables are explained by first 3 PCs
# hence selecting only these variables

# var_pca <- rownames(mat_dat_ref)[apply(mat_dat_ref[,c("RC1","RC2","RC3","RC4","RC5")],
#                                        2,which.max)]

var_fa <- rownames(mat_dat_ref_fa)[which(mat_dat_ref_fa[,"Dim.1"]>0)]
var_fa <- c(var_fa,rownames(mat_dat_ref_fa)[which(mat_dat_ref_fa[,"Dim.2"]>0)])
var_fa <- c(var_fa,rownames(mat_dat_ref_fa)[which(mat_dat_ref_fa[,"Dim.3"]>0)])

# keeping depedent and unique columns only
var_fa <- unique(c(var_fa,"is_obese"))

# also we'll consider same analysis for with selected PCs
fa_var_sel <- c("Dim.1","Dim.2","Dim.3","Dim.9","Dim.17","is_obese")

# So now we have Three data sets to model
# 1) Containing all PCS
# 2) Variables based on Selected PCs
# 3) Selected PCs

##### Modelling Iteration 6 ####################

## GLM Model 

# Modelling with all PCs
modglm1 <- glm( is_obese ~ . , data = train,
                control = glm.control(epsilon = 1e-6,
                                      maxit = 50,
                                      trace = TRUE) )
summary(modglm1)
pred_glm1 <- predict(modglm1,test)
confusionMatrix(ifelse(pred_glm1>0.5,1,0),test[,"is_obese"])
confusionMatrix(ifelse(pred_glm1>0.3,1,0),test[,"is_obese"])
confusionMatrix(ifelse(pred_glm1>0.2,1,0),test[,"is_obese"])

####

modglm2 <- glm( is_obese ~ . -1, data = train,
                family = binomial,
                control = glm.control(epsilon = 1e-6,
                                      maxit = 50,
                                      trace = TRUE) )
summary(modglm2)
pred_glm2 <- predict(modglm2,test)
confusionMatrix(ifelse(pred_glm2>0.5,1,0),test[,"is_obese"])

# Modelling with all selected Variables

modglm1 <- glm( is_obese ~ . , data = data_num[trainIndex,var_pca],
                control = glm.control(epsilon = 1e-6,
                                      maxit = 50,
                                      trace = TRUE) )
summary(modglm1)
pred_glm1 <- predict(modglm1,data_num[-trainIndex,var_pca])
confusionMatrix(ifelse(pred_glm1>0.5,1,0),data_num[-trainIndex,"is_obese"])
confusionMatrix(ifelse(pred_glm1>0.3,1,0),data_num[-trainIndex,"is_obese"])
confusionMatrix(ifelse(pred_glm1>0.2,1,0),data_num[-trainIndex,"is_obese"])

####

modglm2 <- glm( is_obese ~ . -1, data = data_num[trainIndex,var_pca],
                family = binomial,
                control = glm.control(epsilon = 1e-6,
                                      maxit = 50,
                                      trace = TRUE) )
summary(modglm2)
pred_glm2 <- predict(modglm2,data_num[-trainIndex,var_pca])
confusionMatrix(ifelse(pred_glm2>0.5,1,0),data_num[-trainIndex,"is_obese"])
confusionMatrix(ifelse(pred_glm2>0.3,1,0),data_num[-trainIndex,"is_obese"])
confusionMatrix(ifelse(pred_glm2>0.2,1,0),data_num[-trainIndex,"is_obese"])

# Modelling with selected PCs
modglm1 <- glm( is_obese ~ . , data = train[,pca_var_sel],
                control = glm.control(epsilon = 1e-6,
                                      maxit = 50,
                                      trace = TRUE) )
summary(modglm1)
pred_glm1 <- predict(modglm1,test)
confusionMatrix(ifelse(pred_glm1>0.5,1,0),test[,"is_obese"])
confusionMatrix(ifelse(pred_glm1>0.3,1,0),test[,"is_obese"])
confusionMatrix(ifelse(pred_glm1>0.2,1,0),test[,"is_obese"])

####

modglm2 <- glm( is_obese ~ . -1, data = train[,pca_var_sel],
                family = binomial,
                control = glm.control(epsilon = 1e-6,
                                      maxit = 50,
                                      trace = TRUE) )
summary(modglm2)
pred_glm2 <- predict(modglm2,test)
confusionMatrix(ifelse(pred_glm2>0.5,1,0),test[,"is_obese"])

#### Iterating to reduce noice
to_remove_pca <- c("RC11","RC6","RC8","RC3","RC7","RC4","RC13")
to_keep <- setdiff(colnames(train),to_remove_pca)
to_keep <- c("RC1","RC4","RC9","RC12","is_obese")

modglm2 <- glm( is_obese ~ ., data = train[,to_keep],
                family = binomial,
                control = glm.control(epsilon = 1e-6,
                                      maxit = 50,
                                      trace = TRUE) )
summary(modglm2)
plot(modglm2)
pred_glm2 <- predict(modglm2,test)
confusionMatrix(ifelse(pred_glm2>0.5,1,0),test[,"is_obese"])
confusionMatrix(ifelse(pred_glm2>0.3,1,0),test[,"is_obese"])
confusionMatrix(ifelse(pred_glm2>-1,1,0),test[,"is_obese"])

## cleaning up
rm(modglm1,modglm2,pred_glm1,pred_glm2)

## not Even close to our previous attempts

## CART Model

## making cart models
modCart1 <- rpart(is_obese ~ . ,
                  data = train[,to_keep])

rpart.plot(modCart1)
predict_cart1 <- predict(modCart1,test)

confusionMatrix(ifelse(predict_cart1>0.5,1,0),test[,"is_obese"])
confusionMatrix(ifelse(predict_cart1>0.4,1,0),test[,"is_obese"])
confusionMatrix(ifelse(predict_cart1>0.3,1,0),test[,"is_obese"])
confusionMatrix(ifelse(predict_cart1>0.2,1,0),test[,"is_obese"])

#cleaning else
rm(modCart1,predict_cart1)


## Decision tree Model

modDT1 <- ctree(is_obese ~ ., data=train[,to_keep])

plot(modDT1)

pred_dt1 <- predict(modDT1,test)

confusionMatrix(ifelse(pred_dt1>0.5,1,0),test[,"is_obese"])
confusionMatrix(ifelse(pred_dt1>0.6,1,0),test[,"is_obese"])
confusionMatrix(ifelse(pred_dt1>0.7,1,0),test[,"is_obese"])
confusionMatrix(ifelse(pred_dt1>0.4,1,0),test[,"is_obese"])

####

modDT2 <- ctree(as.factor(is_obese) ~ ., data=train[,to_keep],
                controls = ctree_control(teststat = "max",
                                         testtype = "Bonferroni",
                                         mincriterion = 0.95,
                                         minsplit = 20,
                                         minbucket = 7,
                                         # nresample = 100,
                                         mtry = 0,
                                         maxdepth = 0))

plot(modDT2)

pred_dt2 <- predict(modDT2,test)

confusionMatrix(pred_dt2,test[,"is_obese"])

####

modDT3 <- ctree(as.factor(is_obese) ~ ., data=train[,to_keep],
                controls = ctree_control(teststat = "max",
                                         testtype = "Univariate",
                                         mincriterion = 0.95,
                                         minsplit = 20,
                                         minbucket = 7,
                                         # nresample = 100,
                                         mtry = 0,
                                         maxdepth = 0))

plot(modDT3)

pred_dt3 <- predict(modDT3,test)

confusionMatrix(pred_dt3,test[,"is_obese"])

# we will consider modDT5 as our best tree model

rm(modDT1,modDT2,modDT3pred_dt1,pred_dt2,pred_dt3)

## KNN Model

modknn1 <- knn(train = subset(train,select = -c(is_obese)),
               test = subset(test,select = -c(is_obese)),
               cl = train[,"is_obese"],k = 5 , l = 0 , prob = TRUE)

confusionMatrix(modknn1,test[,"is_obese"])

####

modknn2 <- knn(train = subset(train,select = -c(is_obese)),
               test = subset(test,select = -c(is_obese)),
               cl = train[,"is_obese"],
               k = 10 , l = 0 , prob = TRUE)

confusionMatrix(modknn2,test[,"is_obese"])


## cleaning up
rm(modknn1,modknn2)

### Naive bayes model

modNB1 <- naiveBayes(is_obese ~. , data = train)
summary(modNB1)

pred_nb1 <- as.data.frame(predict(modNB1,test,type = "raw"))

confusionMatrix(ifelse(pred_nb1[,"1"]>pred_nb1[,"0"],1,0),test[,"is_obese"])
confusionMatrix(ifelse(pred_nb1[,"1"]>0.5,1,0),test[,"is_obese"])

### essentially the model is pathetic

rm(modNB1,pred_nb1)

# Random Forest Model

modrf1 <- randomForest(is_obese ~ . , data = train,
                       importance = TRUE,proximity = TRUE,
                       ntree = 100,
                       do.trace= TRUE)
summary(modrf1)
plot(modrf1)

pred_rf1 <- predict(modrf1,test)

confusionMatrix(ifelse(pred_rf1>0.5,1,0),test[,"is_obese"])
confusionMatrix(ifelse(pred_rf1>0.6,1,0),test[,"is_obese"])
confusionMatrix(ifelse(pred_rf1>0.55,1,0),test[,"is_obese"])
confusionMatrix(ifelse(pred_rf1>0.45,1,0),test[,"is_obese"])
confusionMatrix(ifelse(pred_rf1>0.4,1,0),test[,"is_obese"])

####

modrf2 <- randomForest(as.factor(is_obese) ~ . , data = train,
                       importance = TRUE,proximity = TRUE,
                       ntree = 120,
                       # maxnodes = 50, nPerm = 1.2,
                       do.trace= TRUE)
summary(modrf2)
plot(modrf2)

pred_rf2 <- predict(modrf2,test)

confusionMatrix(pred_rf2,test[,"is_obese"])

# cleaning up
rm(modrf1,modrf2,pred_rf1,pred_rf2)

### Neural Network Model

modnnet1 <- nnet(is_obese~., data = train,
                 size = 7 ,linout = FALSE, entropy = FALSE, 
                 softmax = FALSE,censored = FALSE, skip = FALSE, 
                 rang = 0.7, decay = 0, maxit = 100, Hess = FALSE, 
                 trace = TRUE, MaxNWts = 1000,
                 abstol = 1.0e-8, reltol = 1.0e-8)

summary(modnnet1)
pred_nn1 <- predict(modnnet1,test)

confusionMatrix(ifelse(pred_nn1>.3,1,0),test[,"is_obese"])
table(pred_nn1,test[,"is_obese"])

### As we can see that all models are performing bad even much 
### infrerior than all. This means that data has not ben properly
### We need do some modulation in how we get data.

### Modelling Iteration 6 Ends

#### Data Preparation Iteration 7 ##################

## As all the above work is not improving the results much
## we go back to main data fo reprocessing everyting.
## But this time we will take data which has atleast 80% of feilds
## filled and impute "NAs" with proper methods.

data_ <- data_original[which(data_original$is_obese != "NA"),]

## removing columns which contatins more than 80 % data as 
## NA or Missing

to_remove <- c("bmxbmi","seqn")
for (col_ in colnames(data_)){
  if (sum(is.na(data_[,col_]))> (nrow(data_) * 0.2)){
    print(col_)
    to_remove <- c(to_remove,col_)
  }
}

# againg finding variables with only one values or one value with nas,  
# dropping them right away

for (col_ in setdiff(colnames(data_),"is_obese")){
  l_u <- length(unique(data_[,col_]))
  
  if ( l_u == 2 && NROW(data.frame(table(data_[,col_]))) == 1){
    print(paste(col_,"has",l_u,"no. of Unique values",
                "but has only 1 value with NA"))
    print(unique(data_[col_]))
    to_remove <- c(to_remove,col_)
    
  } else if ( l_u == 1){
    print(paste(col_,"has",l_u,"no. of Unique values",collapse = " "))
    to_remove <- c(to_remove,col_)
    
  }
  
}

data_ <- data_[,setdiff(colnames(data_),to_remove)]

str(data_)
summary(data_)


# hence we are left with only 68 variables

# looking at distribution of this data
apply(data_,2,function(x) length(unique(x)))

apply(data_,2,function(x) if(length(unique(x))<=10) {
  table(x)}else{
    summary(x)
  })

## converting is_obese to binary for modelling
data_$is_obese <- ifelse(data_$is_obese == "Obese",1,0)


## converting numeric to factors
for (col_ in colnames(data_)){
  l_u <- length(unique(data_[,col_]))
  if ( l_u < 10){
    print(paste(col_,"has",l_u,"no. of Unique values",collapse = " "))
    # print(as.data.frame(table(data_[,col_])))
    print(table(data_[,col_]))
    data_[,col_] <- factor(data_[,col_])
  }
}

## imputing values in NA
library(mice)
library(VIM)

md.pattern(data_)

aggr_plot <- aggr(data_, col=c('navyblue','red'), numbers=TRUE,
                  sortVars=TRUE, labels=names(data), cex.axis=.7,
                  gap=3, ylab=c("Histogram of missing data","Pattern"))

# tempData <- mice(data_,m=5,maxit=5,meth='cart',seed=500)
# summary(tempData)
# 
# # xyplot(tempData,is_obese ~ .,pch=18,cex=1)
# # densityplot(tempData)
# 
# stripplot(tempData, pch = 20, cex = 1.2)
# 
# Imputing with median for continuous and mode for factors

tempData2 <- data_
str(tempData2)

for( col_ in colnames(tempData2)){
    df_vec <- as.data.frame(tempData2[,col_]) 
  if (anyNA(df_vec) && !is.factor(df_vec) ){

    print(paste(col_,"has",sum(is.na(df_vec)),"NAs and is not factor!"))
    df_vec <- apply(df_vec, 2, function(z) {
      z[is.na(z)] <- median(as.numeric(z), na.rm = TRUE)
      z
    })
    print(paste(col_,"now has NAs. replaced with",median(df_vec),"!"))
    tempData2[,col_] <- df_vec

  }else if(is.factor(tempData2[,col_])){
    # print(paste(col_,"is factor with unique values as: ",unique(df_vec)))
    if( sum(is.na(df_vec)) > 1 && is.factor(df_vec)){
        
      print(paste(col_,"has ",sum(is.na(df_vec)),"NAs and is factor!"))
      
      tb_vec <- as.data.frame(table(df_vec))
      
      val_ <- tb_vec[tb_vec$Freq==max(tb_vec$Freq),"df_vec"]
      
      df_vec <- apply(df_vec, 2, function(z) {
        z[is.na(z)] <- val_
        z
      })
      print(paste(col_,"now has NAs. replaced with",val," !"))
      tempData2[,col_] <- df_vec
    }
  }
}

apply(tempData2,2,function(x) {
  if(anyNA(x)){
    head(x, 10)
  }
})


## imputing using knn

tempData3 <- DMwR::knnImputation(data_,k=7)

apply(tempData3,2,function(x) {
  if(anyNA(x)){
    head(x, 10)
  }
})


# making intelligent data splitting by considering all oblique 
# cases in data
data_tp <- data_[0,]

for (col_ in colnames(data_)){
  if (is.factor(data_[,col_])){
    tbl_dat <- data.frame(table(data_[,col_]))
    tbl_dat <- tbl_dat[with(tbl_dat,order(Freq)),]
    val_ <- tbl_dat[which(tbl_dat$Freq <= 5),"Var1"]
    print(col_)
    # print(head(tbl_dat))
    # print(tbl_dat[which(tbl_dat$Freq <= 1),])
    print(val_)
    print(length(val_))
    if(length(val_) >0){
      data_tp <- rbind(data_tp,data_[which(data_[,col_] %in% val_),])
    }
  }
}

# This data has to be in training data for model to be capture complte 
# behavior

# removing data from train and test set
sp_indexes <- rownames(data_tp)

## new training indexes
trainIndex <- sample(rownames(data_)[!rownames(data_)%in%sp_indexes] ,(nrow(data_)*.8))
base::intersect(trainIndex,sp_indexes)
trainIndex <- c(trainIndex,sp_indexes)
trainIndex
base::intersect(trainIndex,rownames(data_))
setdiff(trainIndex,rownames(data_))

trainindex <- base::intersect(trainIndex,rownames(data_))

# ratio of split
length(trainIndex)/nrow(data_)

# again checking for singular values in data
apply(data_,2,function(x) length(unique(x)))
apply(tempData,2,function(x) length(unique(x)))
apply(tempData2,2,function(x) length(unique(x)))
apply(tempData3,2,function(x) length(unique(x)))

# splitting the data
train <- data_[trainIndex,]
rownames(train)
test <- data_[setdiff(rownames(data_),rownames(train)),]

## smimilarly
train_1 <- tempData[trainIndex,]
test_1 <- tempData[setdiff(rownames(tempData),rownames(train_1)),]

train_2 <- tempData2[trainIndex,]
test_2 <- tempData2[setdiff(rownames(tempData2),rownames(train_2)),]

train_3 <- tempData3[trainIndex,]
test_3 <- tempData3[setdiff(rownames(tempData3),rownames(train_3)),]

#Checking singularity for train and test set
apply(train,2,function(x) length(unique(x)))
apply(train_1,2,function(x) length(unique(x)))
apply(train_2,2,function(x) length(unique(x)))
apply(train_3,2,function(x) length(unique(x)))


## checking for na rows
train[is.na(train$is_obese),]
test[is.na(test$is_obese),]
train_1[is.na(train$is_obese),]
test_1[is.na(test$is_obese),]
train_2[is.na(train$is_obese),]
test_2[is.na(test$is_obese),]
train_3[is.na(train$is_obese),]
test_3[is.na(test$is_obese),]

## removing NA rows
if (anyNA(train$is_obese)){
  train <- train[which(train$is_obese != "NA" ),]
}
if (anyNA(train_1$is_obese)){
  train_1 <- train_1[which(train_1$is_obese != "NA" ),]
}
if (anyNA(train_2$is_obese)){
  train_2 <- train_2[which(train_2$is_obese != "NA" ),]
}
if (anyNA(train_3$is_obese)){
  train_3 <- train_3[which(train_3$is_obese != "NA" ),]
}

# rechecking na rows
train[is.na(train$is_obese),]
test[is.na(test$is_obese),]
train_1[is.na(train$is_obese),]
test_1[is.na(test$is_obese),]
train_2[is.na(train$is_obese),]
test_2[is.na(test$is_obese),]
train_3[is.na(train$is_obese),]
test_3[is.na(test$is_obese),]

#rechecking Checking singularity for train set
apply(train,2,function(x) length(unique(x)) == 1)
apply(train_1,2,function(x) length(unique(x)) == 1)
apply(train_2,2,function(x) length(unique(x)) == 1)
apply(train_3,2,function(x) length(unique(x)) == 1)



# cleaning up
rm(data_tp,tbl_dat,col_,l_u,val_,df_vec)

#### Data preparation Iteration 7 Ends

#### Modelling Iteration 7 #######################


#GLM

# Modelling with basic data
modglm1 <- glm( as.integer(is_obese) ~ . , data = train,
                control = glm.control(epsilon = 1e-6,
                                      maxit = 50,
                                      trace = TRUE) )
summary(modglm1)
pred_glm1 <- predict(modglm1,test)
confusionMatrix(ifelse(pred_glm1>0.5,1,0),test[,"is_obese"])
confusionMatrix(ifelse(pred_glm1>0.3,1,0),test[,"is_obese"])
confusionMatrix(ifelse(pred_glm1>0.2,1,0),test[,"is_obese"])

####

modglm2 <- glm( is_obese ~ . -1, data = train,
                family = binomial,
                control = glm.control(epsilon = 1e-6,
                                      maxit = 50,
                                      trace = TRUE) )
summary(modglm2)
pred_glm2 <- predict(modglm2,test)
confusionMatrix(ifelse(pred_glm2>0.5,1,0),test[,"is_obese"])

# Modelling with all selected Variables

modglm1 <- glm( is_obese ~ . , data = data_num[trainIndex,var_pca],
                control = glm.control(epsilon = 1e-6,
                                      maxit = 50,
                                      trace = TRUE) )
summary(modglm1)
pred_glm1 <- predict(modglm1,data_num[-trainIndex,var_pca])
confusionMatrix(ifelse(pred_glm1>0.5,1,0),data_num[-trainIndex,"is_obese"])
confusionMatrix(ifelse(pred_glm1>0.3,1,0),data_num[-trainIndex,"is_obese"])
confusionMatrix(ifelse(pred_glm1>0.2,1,0),data_num[-trainIndex,"is_obese"])

####

modglm2 <- glm( is_obese ~ . -1, data = data_num[trainIndex,var_pca],
                family = binomial,
                control = glm.control(epsilon = 1e-6,
                                      maxit = 50,
                                      trace = TRUE) )
summary(modglm2)
pred_glm2 <- predict(modglm2,data_num[-trainIndex,var_pca])
confusionMatrix(ifelse(pred_glm2>0.5,1,0),data_num[-trainIndex,"is_obese"])
confusionMatrix(ifelse(pred_glm2>0.3,1,0),data_num[-trainIndex,"is_obese"])
confusionMatrix(ifelse(pred_glm2>0.2,1,0),data_num[-trainIndex,"is_obese"])

# Modelling with selected PCs
modglm1 <- glm( is_obese ~ . , data = train[,pca_var_sel],
                control = glm.control(epsilon = 1e-6,
                                      maxit = 50,
                                      trace = TRUE) )
summary(modglm1)
pred_glm1 <- predict(modglm1,test)
confusionMatrix(ifelse(pred_glm1>0.5,1,0),test[,"is_obese"])
confusionMatrix(ifelse(pred_glm1>0.3,1,0),test[,"is_obese"])
confusionMatrix(ifelse(pred_glm1>0.2,1,0),test[,"is_obese"])

####

modglm2 <- glm( is_obese ~ . -1, data = train[,pca_var_sel],
                family = binomial,
                control = glm.control(epsilon = 1e-6,
                                      maxit = 50,
                                      trace = TRUE) )
summary(modglm2)
pred_glm2 <- predict(modglm2,test)
confusionMatrix(ifelse(pred_glm2>0.5,1,0),test[,"is_obese"])

#### Iterating to reduce noice
to_remove_pca <- c("RC11","RC6","RC8","RC3","RC7","RC4","RC13")
to_keep <- setdiff(colnames(train),to_remove_pca)
to_keep <- c("RC1","RC4","RC9","RC12","is_obese")

modglm2 <- glm( is_obese ~ ., data = train[,to_keep],
                family = binomial,
                control = glm.control(epsilon = 1e-6,
                                      maxit = 50,
                                      trace = TRUE) )
summary(modglm2)
plot(modglm2)
pred_glm2 <- predict(modglm2,test)
confusionMatrix(ifelse(pred_glm2>0.5,1,0),test[,"is_obese"])
confusionMatrix(ifelse(pred_glm2>0.3,1,0),test[,"is_obese"])
confusionMatrix(ifelse(pred_glm2>-1,1,0),test[,"is_obese"])

## cleaning up
rm(modglm1,modglm2,pred_glm1,pred_glm2)

## not Even close to our previous attempts

## CART Model

## making cart models
modCart1 <- rpart(is_obese ~ . ,
                  data = train)

rpart.plot(modCart1)
predict_cart1 <- predict(modCart1,test)
head(predict_cart1,5)

confusionMatrix(ifelse(predict_cart1[,"0"]>0.8,1,0),test[,"is_obese"])

####
modCart2 <- rpart(is_obese ~ . ,
                  data = train_2)

rpart.plot(modCart2)
predict_cart2 <- predict(modCart2,test_2)
head(predict_cart2,5)

confusionMatrix(ifelse(predict_cart2[,"0"]>0.8,1,0),test[,"is_obese"])

#####

modCart3 <- rpart(is_obese ~ . ,
                  data = train_3)

rpart.plot(modCart3)
predict_cart3 <- predict(modCart3,test_3)
head(predict_cart3,5)

confusionMatrix(ifelse(predict_cart3[,"1"]>0.01,1,0),test[,"is_obese"])


#cleaning else
rm(modCart1,predict_cart1)


## Decision tree Model

modDT1 <- ctree(is_obese ~ ., data=train)

plot(modDT1)

pred_dt1 <- predict(modDT1,test)

confusionMatrix(pred_dt1,test[,"is_obese"])

####

modDT2 <- ctree(is_obese ~ ., data=train,
                controls = ctree_control(teststat = "max",
                                         testtype = "Bonferroni",
                                         mincriterion = 0.95,
                                         minsplit = 20,
                                         minbucket = 7,
                                         # nresample = 100,
                                         mtry = 0,
                                         maxdepth = 0))

plot(modDT2)

pred_dt2 <- predict(modDT2,test)

confusionMatrix(pred_dt2,test[,"is_obese"])

####

modDT3 <- ctree(as.factor(is_obese) ~ ., data=train[,to_keep],
                controls = ctree_control(teststat = "max",
                                         testtype = "Univariate",
                                         mincriterion = 0.95,
                                         minsplit = 20,
                                         minbucket = 7,
                                         # nresample = 100,
                                         mtry = 0,
                                         maxdepth = 0))

plot(modDT3)

pred_dt3 <- predict(modDT3,test)

confusionMatrix(pred_dt3,test[,"is_obese"])

# we will consider modDT5 as our best tree model

rm(modDT1,modDT2,modDT3pred_dt1,pred_dt2,pred_dt3)

## KNN Model

modknn1 <- knn(train = subset(train,select = -c(is_obese)),
               test = subset(test,select = -c(is_obese)),
               cl = train[,"is_obese"],k = 5 , l = 0 , prob = TRUE)

confusionMatrix(modknn1,test[,"is_obese"])

####

modknn2 <- knn(train = subset(train,select = -c(is_obese)),
               test = subset(test,select = -c(is_obese)),
               cl = train[,"is_obese"],
               k = 10 , l = 0 , prob = TRUE)

confusionMatrix(modknn2,test[,"is_obese"])


## cleaning up
rm(modknn1,modknn2)

### Naive bayes model

modNB1 <- naiveBayes(is_obese ~. , data = train)
summary(modNB1)

pred_nb1 <- as.data.frame(predict(modNB1,test,type = "raw"))

confusionMatrix(ifelse(pred_nb1[,"1"]>pred_nb1[,"0"],1,0),test[,"is_obese"])
confusionMatrix(ifelse(pred_nb1[,"1"]>0.5,1,0),test[,"is_obese"])

### essentially the model is pathetic

rm(modNB1,pred_nb1)

# Random Forest Model

modrf1 <- randomForest(is_obese ~ . , data = train,
                       importance = TRUE,proximity = TRUE,
                       ntree = 100,
                       do.trace= TRUE)
summary(modrf1)
plot(modrf1)

pred_rf1 <- predict(modrf1,test)

confusionMatrix(ifelse(pred_rf1>0.5,1,0),test[,"is_obese"])
confusionMatrix(ifelse(pred_rf1>0.6,1,0),test[,"is_obese"])
confusionMatrix(ifelse(pred_rf1>0.55,1,0),test[,"is_obese"])
confusionMatrix(ifelse(pred_rf1>0.45,1,0),test[,"is_obese"])
confusionMatrix(ifelse(pred_rf1>0.4,1,0),test[,"is_obese"])

####

modrf2 <- randomForest(as.factor(is_obese) ~ . , data = train,
                       importance = TRUE,proximity = TRUE,
                       ntree = 120,
                       # maxnodes = 50, nPerm = 1.2,
                       do.trace= TRUE)
summary(modrf2)
plot(modrf2)

pred_rf2 <- predict(modrf2,test)

confusionMatrix(pred_rf2,test[,"is_obese"])

# cleaning up
rm(modrf1,modrf2,pred_rf1,pred_rf2)

### Neural Network Model

modnnet1 <- nnet(is_obese~., data = train,
                 size = 7 ,linout = FALSE, entropy = FALSE, 
                 softmax = FALSE,censored = FALSE, skip = FALSE, 
                 rang = 0.7, decay = 0, maxit = 100, Hess = FALSE, 
                 trace = TRUE, MaxNWts = 1000,
                 abstol = 1.0e-8, reltol = 1.0e-8)

summary(modnnet1)
pred_nn1 <- predict(modnnet1,test)

confusionMatrix(ifelse(pred_nn1>.3,1,0),test[,"is_obese"])
table(pred_nn1,test[,"is_obese"])

## XGBoost Model
d_train <- xgb.DMatrix(data = data.matrix(train),
                       label = data.matrix(train[,"is_obese"]),
                       missing = 0)
d_test <- xgb.DMatrix(data = data.matrix(test),
                      label = data.matrix(test[,"is_obese"]),
                      missing = 0)

par  <-  list(booster = "gblinear",
              objective = "reg:logistic",
              # eta = 0.2,
              # min_child_weight = 6,
              gamma = 2,
              subsample = 0.85,
              colsample_bytree = 0.75,
              # max_depth = 10,
              verbose = 1,
              alpha = 0.5,
              scale_pos_weight = 1)
#selecting number of Rounds
n_rounds= 200

#modeling
xgb_mod_1  <- xgboost(params = par, 
                           data = d_train,
                           # data = sparse_matrix_tr,
                           # label = op_vec ,
                           nrounds = n_rounds)

pred  <- predict(xgb_mod_1,d_test)
as.data.frame(pred)

pred_xg_1 <- ifelse(pred>0.24,1,0)
confusionMatrix(pred_xg_1,test_[,"tnd_cc_flag"])

### Modelling Iteration 8 Ends

#### Data Preparation Iteration 8 ##################

# Till Now we have been doing some basic analysis of variables 
# We havent yet considered (Except Information Value) any statistics 
# that tell us about the 
# influence and explanation of depedent variable.

# cleaning up some space
rm(train_3,train_2,train_1,train,test_3,test_2,test_1,test)

## Before Going further lets check for one of most irking things
## HETROSACARDISITY

# for testing this we need all variables to be numeric
# generating numeric data
data_num <- data_

for(col_ in colnames(data_num)){
  if (is.factor(data_num[,col_]))
  {
    data_num[,col_] <- as.numeric(levels(data_num[,col_]))[data_num[,col_]]
  }
}
str(data_num)
data_num1 <- data_num

# similarly for imputed datas
data_num <- tempData2
for(col_ in colnames(data_num)){
  if (is.factor(data_num[,col_]))
  {
    data_num[,col_] <- as.numeric(levels(data_num[,col_]))[data_num[,col_]]
  }
}
str(data_num)
data_num2 <- data_num


data_num <- tempData3
for(col_ in colnames(data_num)){
  if (is.factor(data_num[,col_]))
  {
    data_num[,col_] <- as.numeric(levels(data_num[,col_]))[data_num[,col_]]
  }
}
str(data_num)
data_num3 <- data_num

rm(data_num)

# including library
# library(lmtest)
# library(sandwich)
# library(car)

gen_lm <- lm(is_obese ~. -1, data = data_num1,na.action = na.omit)
plot(gen_lm)
#testing hetroscardisity
lmtest::bptest(gen_lm)

gen_lm1 <- lm(is_obese ~. -1, data = data_num2)
plot(gen_lm1)
#testing hetroscardisity
lmtest::bptest(gen_lm1)

gen_lm2 <- lm(is_obese ~. -1, data = data_num3)
plot(gen_lm2)
#testing hetroscardisity
lmtest::bptest(gen_lm2)


sandwich::vcovHAC(gen_lm)
car::hccm(gen_lm)
coeftest(gen_lm,vcov.=car::hccm(gen_lm))

sandwich::vcovHAC(gen_lm1)
car::hccm(gen_lm1)
coeftest(gen_lm1,vcov.=car::hccm(gen_lm1))

sandwich::vcovHAC(gen_lm2)
car::hccm(gen_lm2)
coeftest(gen_lm2,vcov.=car::hccm(gen_lm2))

coefweight <- coeftest(gen_lm2,vcov.=car::hccm(gen_lm2))

names(coefweight)
dim(coefweight)
data.frame(est = coefweight[,1],
           stder = coefweight[,2])

## Considering only the variables which appears significant here

var_coef <- coefweight

### Data Preparation Iteration 8 Ends

#### Modelling Iteration 8 #######################


#GLM

# Modelling with basic data
modglm1 <- glm( as.integer(is_obese) ~ . , data = train,
                control = glm.control(epsilon = 1e-6,
                                      maxit = 50,
                                      trace = TRUE) )
summary(modglm1)
pred_glm1 <- predict(modglm1,test)
confusionMatrix(ifelse(pred_glm1>0.5,1,0),test[,"is_obese"])
confusionMatrix(ifelse(pred_glm1>0.3,1,0),test[,"is_obese"])
confusionMatrix(ifelse(pred_glm1>0.2,1,0),test[,"is_obese"])

####

modglm2 <- glm( is_obese ~ . -1, data = train,
                family = binomial,
                control = glm.control(epsilon = 1e-6,
                                      maxit = 50,
                                      trace = TRUE) )
summary(modglm2)
pred_glm2 <- predict(modglm2,test)
confusionMatrix(ifelse(pred_glm2>0.5,1,0),test[,"is_obese"])

# Modelling with all selected Variables

modglm1 <- glm( is_obese ~ . , data = data_num[trainIndex,var_pca],
                control = glm.control(epsilon = 1e-6,
                                      maxit = 50,
                                      trace = TRUE) )
summary(modglm1)
pred_glm1 <- predict(modglm1,data_num[-trainIndex,var_pca])
confusionMatrix(ifelse(pred_glm1>0.5,1,0),data_num[-trainIndex,"is_obese"])
confusionMatrix(ifelse(pred_glm1>0.3,1,0),data_num[-trainIndex,"is_obese"])
confusionMatrix(ifelse(pred_glm1>0.2,1,0),data_num[-trainIndex,"is_obese"])

####

modglm2 <- glm( is_obese ~ . -1, data = data_num[trainIndex,var_pca],
                family = binomial,
                control = glm.control(epsilon = 1e-6,
                                      maxit = 50,
                                      trace = TRUE) )
summary(modglm2)
pred_glm2 <- predict(modglm2,data_num[-trainIndex,var_pca])
confusionMatrix(ifelse(pred_glm2>0.5,1,0),data_num[-trainIndex,"is_obese"])
confusionMatrix(ifelse(pred_glm2>0.3,1,0),data_num[-trainIndex,"is_obese"])
confusionMatrix(ifelse(pred_glm2>0.2,1,0),data_num[-trainIndex,"is_obese"])

# Modelling with selected PCs
modglm1 <- glm( is_obese ~ . , data = train[,pca_var_sel],
                control = glm.control(epsilon = 1e-6,
                                      maxit = 50,
                                      trace = TRUE) )
summary(modglm1)
pred_glm1 <- predict(modglm1,test)
confusionMatrix(ifelse(pred_glm1>0.5,1,0),test[,"is_obese"])
confusionMatrix(ifelse(pred_glm1>0.3,1,0),test[,"is_obese"])
confusionMatrix(ifelse(pred_glm1>0.2,1,0),test[,"is_obese"])

####

modglm2 <- glm( is_obese ~ . -1, data = train[,pca_var_sel],
                family = binomial,
                control = glm.control(epsilon = 1e-6,
                                      maxit = 50,
                                      trace = TRUE) )
summary(modglm2)
pred_glm2 <- predict(modglm2,test)
confusionMatrix(ifelse(pred_glm2>0.5,1,0),test[,"is_obese"])

#### Iterating to reduce noice
to_remove_pca <- c("RC11","RC6","RC8","RC3","RC7","RC4","RC13")
to_keep <- setdiff(colnames(train),to_remove_pca)
to_keep <- c("RC1","RC4","RC9","RC12","is_obese")

modglm2 <- glm( is_obese ~ ., data = train[,to_keep],
                family = binomial,
                control = glm.control(epsilon = 1e-6,
                                      maxit = 50,
                                      trace = TRUE) )
summary(modglm2)
plot(modglm2)
pred_glm2 <- predict(modglm2,test)
confusionMatrix(ifelse(pred_glm2>0.5,1,0),test[,"is_obese"])
confusionMatrix(ifelse(pred_glm2>0.3,1,0),test[,"is_obese"])
confusionMatrix(ifelse(pred_glm2>-1,1,0),test[,"is_obese"])

## cleaning up
rm(modglm1,modglm2,pred_glm1,pred_glm2)

## not Even close to our previous attempts

## CART Model

## making cart models
modCart1 <- rpart(is_obese ~ . ,
                  data = train)

rpart.plot(modCart1)
predict_cart1 <- predict(modCart1,test)
head(predict_cart1,5)

confusionMatrix(ifelse(predict_cart1[,"0"]>0.8,1,0),test[,"is_obese"])

####
modCart2 <- rpart(is_obese ~ . ,
                  data = train_2)

rpart.plot(modCart2)
predict_cart2 <- predict(modCart2,test_2)
head(predict_cart2,5)

confusionMatrix(ifelse(predict_cart2[,"0"]>0.8,1,0),test[,"is_obese"])

#####

modCart3 <- rpart(is_obese ~ . ,
                  data = train_3)

rpart.plot(modCart3)
predict_cart3 <- predict(modCart3,test_3)
head(predict_cart3,5)

confusionMatrix(ifelse(predict_cart3[,"1"]>0.01,1,0),test[,"is_obese"])


#cleaning else
rm(modCart1,predict_cart1)


## Decision tree Model

modDT1 <- ctree(is_obese ~ ., data=train)

plot(modDT1)

pred_dt1 <- predict(modDT1,test)

confusionMatrix(pred_dt1,test[,"is_obese"])

####

modDT2 <- ctree(is_obese ~ ., data=train,
                controls = ctree_control(teststat = "max",
                                         testtype = "Bonferroni",
                                         mincriterion = 0.95,
                                         minsplit = 20,
                                         minbucket = 7,
                                         # nresample = 100,
                                         mtry = 0,
                                         maxdepth = 0))

plot(modDT2)

pred_dt2 <- predict(modDT2,test)

confusionMatrix(pred_dt2,test[,"is_obese"])

####

modDT3 <- ctree(as.factor(is_obese) ~ ., data=train[,to_keep],
                controls = ctree_control(teststat = "max",
                                         testtype = "Univariate",
                                         mincriterion = 0.95,
                                         minsplit = 20,
                                         minbucket = 7,
                                         # nresample = 100,
                                         mtry = 0,
                                         maxdepth = 0))

plot(modDT3)

pred_dt3 <- predict(modDT3,test)

confusionMatrix(pred_dt3,test[,"is_obese"])

# we will consider modDT5 as our best tree model

rm(modDT1,modDT2,modDT3pred_dt1,pred_dt2,pred_dt3)

## KNN Model

modknn1 <- knn(train = subset(train,select = -c(is_obese)),
               test = subset(test,select = -c(is_obese)),
               cl = train[,"is_obese"],k = 5 , l = 0 , prob = TRUE)

confusionMatrix(modknn1,test[,"is_obese"])

####

modknn2 <- knn(train = subset(train,select = -c(is_obese)),
               test = subset(test,select = -c(is_obese)),
               cl = train[,"is_obese"],
               k = 10 , l = 0 , prob = TRUE)

confusionMatrix(modknn2,test[,"is_obese"])


## cleaning up
rm(modknn1,modknn2)

### Naive bayes model

modNB1 <- naiveBayes(is_obese ~. , data = train)
summary(modNB1)

pred_nb1 <- as.data.frame(predict(modNB1,test,type = "raw"))

confusionMatrix(ifelse(pred_nb1[,"1"]>pred_nb1[,"0"],1,0),test[,"is_obese"])
confusionMatrix(ifelse(pred_nb1[,"1"]>0.5,1,0),test[,"is_obese"])

### essentially the model is pathetic

rm(modNB1,pred_nb1)

# Random Forest Model

modrf1 <- randomForest(is_obese ~ . , data = train,
                       importance = TRUE,proximity = TRUE,
                       ntree = 100,
                       do.trace= TRUE)
summary(modrf1)
plot(modrf1)

pred_rf1 <- predict(modrf1,test)

confusionMatrix(ifelse(pred_rf1>0.5,1,0),test[,"is_obese"])
confusionMatrix(ifelse(pred_rf1>0.6,1,0),test[,"is_obese"])
confusionMatrix(ifelse(pred_rf1>0.55,1,0),test[,"is_obese"])
confusionMatrix(ifelse(pred_rf1>0.45,1,0),test[,"is_obese"])
confusionMatrix(ifelse(pred_rf1>0.4,1,0),test[,"is_obese"])

####

modrf2 <- randomForest(as.factor(is_obese) ~ . , data = train,
                       importance = TRUE,proximity = TRUE,
                       ntree = 120,
                       # maxnodes = 50, nPerm = 1.2,
                       do.trace= TRUE)
summary(modrf2)
plot(modrf2)

pred_rf2 <- predict(modrf2,test)

confusionMatrix(pred_rf2,test[,"is_obese"])

# cleaning up
rm(modrf1,modrf2,pred_rf1,pred_rf2)

### Neural Network Model

modnnet1 <- nnet(is_obese~., data = train,
                 size = 7 ,linout = FALSE, entropy = FALSE, 
                 softmax = FALSE,censored = FALSE, skip = FALSE, 
                 rang = 0.7, decay = 0, maxit = 100, Hess = FALSE, 
                 trace = TRUE, MaxNWts = 1000,
                 abstol = 1.0e-8, reltol = 1.0e-8)

summary(modnnet1)
pred_nn1 <- predict(modnnet1,test)

confusionMatrix(ifelse(pred_nn1>.3,1,0),test[,"is_obese"])
table(pred_nn1,test[,"is_obese"])

## XGBoost Model
d_train <- xgb.DMatrix(data = data.matrix(train),
                       label = data.matrix(train[,"is_obese"]),
                       missing = 0)
d_test <- xgb.DMatrix(data = data.matrix(test),
                      label = data.matrix(test[,"is_obese"]),
                      missing = 0)

par  <-  list(booster = "gblinear",
              objective = "reg:logistic",
              # eta = 0.2,
              # min_child_weight = 6,
              gamma = 2,
              subsample = 0.85,
              colsample_bytree = 0.75,
              # max_depth = 10,
              verbose = 1,
              alpha = 0.5,
              scale_pos_weight = 1)
#selecting number of Rounds
n_rounds= 200

#modeling
xgb_mod_1  <- xgboost(params = par, 
                      data = d_train,
                      # data = sparse_matrix_tr,
                      # label = op_vec ,
                      nrounds = n_rounds)

pred  <- predict(xgb_mod_1,d_test)
as.data.frame(pred)

pred_xg_1 <- ifelse(pred>0.24,1,0)
confusionMatrix(pred_xg_1,test_[,"tnd_cc_flag"])

### Modelling Iteration 8 Ends

#### Data Prearation Iteration 9 ##########################

## In previous Step We saw hetrosarcadisity in data!
## Now how to tackle this...

## Note: This is a hot and trial Method and there is no absolute 
## answer to this

# As we know the data contains units in all kgs, cms,time etc 
# so first we will try to make numeric data unit less
# we will do this by follwing methods

# 1. Feature Scaling
feature.scale <- function(x){
  min_ <- min(x,na.rm = T)
  max_ <- max(x,na.rm = T)
  
  y <- ((x-min_)/(max_-min_))
  return(y)
}

# 2. Standard Scoring
std.score <- function(x){
  mean_ <- mean(x,na.rm = T)
  sd_ <- sd(x,na.rm = T)
  
  y <- ((x-mean_)/sd_)
  return(y)
}

# 3. Postional Standard Scoring
pos.std.score <- function(x){
  median_ <- median(x,na.rm = T)
  sd_ <- sd(x,na.rm = T)
  
  y <- ((x-median_)/sd_)
  return(y)
}

# we use mostly above 3 as of now , but there are many more ways to 
# do this

data_num <- data_

for (col_ in setdiff(colnames(data_num),"is_obese")){
  if (is.factor(data_num[,col_])){
    x <- as.numeric(levels(data_num[,col_]))[data_num[,col_]]
    data_num[col_] <- feature.scale(x)
  }else if(!is.factor(data_num[,col_])){
    x <- as.numeric(data_num[,col_])
    data_num[col_] <- feature.scale(x)
  }
}

data_1 <- data_num

data_num <- tempData2

for (col_ in setdiff(colnames(data_num),"is_obese")){
  if (is.factor(data_num[,col_])){
    x <- as.numeric(levels(data_num[,col_]))[data_num[,col_]]
    data_num[col_] <- feature.scale(x)
  }else if(!is.factor(data_num[,col_])){
    x <- as.numeric(data_num[,col_])
    data_num[col_] <- feature.scale(x)
  }
}

data_2 <- data_num

data_num <- tempData3

for (col_ in setdiff(colnames(data_num),"is_obese")){
  if (is.factor(data_num[,col_])){
    x <- as.numeric(levels(data_num[,col_]))[data_num[,col_]]
    data_num[col_] <- feature.scale(x)
  }else if(!is.factor(data_num[,col_])){
    x <- as.numeric(data_num[,col_])
    data_num[col_] <- feature.scale(x)
  }
}

data_3 <- data_num

class(data_1$is_obese)
## since is_obese is factor converting to numeric
data_1$is_obese <- ifelse(data_1$is_obese == "1",1,0)
data_2$is_obese <- ifelse(data_2$is_obese == "1",1,0)
data_3$is_obese <- ifelse(data_3$is_obese == "1",1,0)

# again checking for hetroscardisity
gen_lm <- lm(is_obese ~. -1, data = data_1,na.action = na.omit)
plot(gen_lm)
#testing hetroscardisity
lmtest::bptest(gen_lm)

gen_lm1 <- lm(is_obese ~. -1, data = data_2)
plot(gen_lm1)
#testing hetroscardisity
lmtest::bptest(gen_lm1)

gen_lm2 <- lm(is_obese ~. -1, data = data_3)
plot(gen_lm2)
#testing hetroscardisity
lmtest::bptest(gen_lm2)

## no effect of feature scaling

# trying std scaling
data_num <- data_

for (col_ in setdiff(colnames(data_num),"is_obese")){
  if (is.factor(data_num[,col_])){
    x <- as.numeric(levels(data_num[,col_]))[data_num[,col_]]
    data_num[col_] <- std.score(x)
  }else if(!is.factor(data_num[,col_])){
    x <- as.numeric(data_num[,col_])
    data_num[col_] <- std.score(x)
  }
}

data_1 <- data_num

data_num <- tempData2

for (col_ in setdiff(colnames(data_num),"is_obese")){
  if (is.factor(data_num[,col_])){
    x <- as.numeric(levels(data_num[,col_]))[data_num[,col_]]
    data_num[col_] <- std.score(x)
  }else if(!is.factor(data_num[,col_])){
    x <- as.numeric(data_num[,col_])
    data_num[col_] <- std.score(x)
  }
}

data_2 <- data_num

data_num <- tempData3

for (col_ in setdiff(colnames(data_num),"is_obese")){
  if (is.factor(data_num[,col_])){
    x <- as.numeric(levels(data_num[,col_]))[data_num[,col_]]
    data_num[col_] <- std.score(x)
  }else if(!is.factor(data_num[,col_])){
    x <- as.numeric(data_num[,col_])
    data_num[col_] <- std.score(x)
  }
}

data_3 <- data_num

class(data_1$is_obese)
## since is_obese is factor converting to numeric
data_1$is_obese <- ifelse(data_1$is_obese == "1",1,0)
data_2$is_obese <- ifelse(data_2$is_obese == "1",1,0)
data_3$is_obese <- ifelse(data_3$is_obese == "1",1,0)

# again checking for hetroscardisity
gen_lm <- lm(is_obese ~. -1, data = data_1,na.action = na.omit)
plot(gen_lm)
#testing hetroscardisity
lmtest::bptest(gen_lm)

gen_lm1 <- lm(is_obese ~. -1, data = data_2)
plot(gen_lm1)
#testing hetroscardisity
lmtest::bptest(gen_lm1)

gen_lm2 <- lm(is_obese ~. -1, data = data_3)
plot(gen_lm2)
#testing hetroscardisity
lmtest::bptest(gen_lm2)

## this means essentially there may be two things
## 1) either data is less
## 2) either data is much more

## Dealing with 2 case we try to find variable importance and 
## then make a decision on which variable are important for modelling.

# cleaning up
rm(data_1,data_2,data_3,data_num,gen_lm,gen_lm1,gen_lm2)

## Before this we will do scaling of data by feature scaling for
## continuous variable

data_num <- tempData2

for (col_ in setdiff(colnames(data_num),"is_obese")){
  if(!is.factor(data_num[,col_])){
    x <- as.numeric(data_num[,col_])
    data_num[col_] <- feature.scale(x)
  }
}

data_wrk <- data_num


variableImp <- data.frame(variable = colnames(data_wrk),
                          is.factor=rep(0,ncol(data_wrk)),
                         loglik=rep(0,ncol(data_wrk)),
                         giniImpurity=rep(0,ncol(data_wrk)),
                         missclass=rep(0,ncol(data_wrk)),
                         entropy=rep(0,ncol(data_wrk)),
                         concordance=rep(0,ncol(data_wrk)),
                         ginicoeff=rep(0,ncol(data_wrk)),
                         informationValue=rep(0,ncol(data_wrk)))

for (col_ in colnames(data_wrk)){
  variableImp[which(variableImp$variable == col_),"is.factor"] <- ifelse(
    is.factor(data_wrk[,col_],1,0))
}

## Log Likelihood

for(col_ in colnames(data_wrk)){
  data_sm <- data.frame(variable=data_wrk[,col_],
                        target = ifelse(data_wrk[,"is_obese"] == "1",1,0))
  colnames(data_sm) <- c("variable","target")
  print(paste("Making Model for",col_,"and is_obese"))
  print(head(data_sm))
  # table(data_sm$target)
  if (col_ != "is_obese"){
    gen_mod <- glm(target ~ variable, data = data_sm,na.action = na.omit)
    variableImp[variableImp$variable == col_,"loglik"] <- logLik(gen_mod)
  }
}

## Gini Impurity

for(col_ in colnames(data_wrk)){
  data_sm <- data.frame(variable=data_wrk[,col_],
                        target = ifelse(data_wrk[,"is_obese"] == "1",1,0))
  colnames(data_sm) <- c("variable","target")
  print(paste("Making Model for",col_,"and is_obese"))
  print(head(data_sm))
  # table(data_sm$target)
  if(is.factor(data_sm$variable)){
    tot_tab <- data.frame(table(data_sm$variable,data_sm$target))
    tot_tab
    gini_variable <- 0
    for (i in unique(tot_tab$Var1)){
      sub_tab <- tot_tab[which(tot_tab$Var1 == i),c("Var2","Freq")]
      # print(sub_tab)
      g_sub <-  1 - sum((sub_tab$Freq/sum(sub_tab$Freq)) ^ 2)
      # print(g_sub)
      p_sub <- (sum(sub_tab$Freq)/sum(tot_tab$Freq))
      # print(p_sub)
      # print(p_sub*g_sub)
      gini_variable <- gini_variable + (p_sub*g_sub)
      # print(gini_variable)
    }
    variableImp[variableImp$variable == col_,"giniImpurity"] <- gini_variable
  }else{
    
    variableImp[variableImp$variable == col_,"giniImpurity"] <- 0
    
  }
}

## Misclassification

for(col_ in colnames(data_wrk)){
  data_sm <- data.frame(variable=data_wrk[,col_],
                        target = ifelse(data_wrk[,"is_obese"] == "1",1,0))
  colnames(data_sm) <- c("variable","target")
  print(paste("Making Model for",col_,"and is_obese"))
  print(head(data_sm))
  # table(data_sm$target)
  if(is.factor(data_sm$variable)){
    tot_tab <- data.frame(table(data_sm$variable,data_sm$target))
    tot_tab
    missclass <- 0
    p_sub <- c()
    for (i in unique(tot_tab$Var1)){
      sub_tab <- tot_tab[which(tot_tab$Var1 == i),c("Var2","Freq")]
      p_sub <- c(p_sub,(sum(sub_tab$Freq)/sum(tot_tab$Freq)))
    }
    variableImp[variableImp$variable == col_,"missclass"] <- (1 - max(p_sub,
                                                                      na.rm = T))
  }else{
    
    variableImp[variableImp$variable == col_,"missclass"] <- 0
  }
}

## Entropy Gain

for(col_ in colnames(data_wrk)){
  data_sm <- data.frame(variable=data_wrk[,col_],
                        target = ifelse(data_wrk[,"is_obese"] == "1",1,0))
  colnames(data_sm) <- c("variable","target")
  print(paste("Making Model for",col_,"and is_obese"))
  # print(head(data_sm))
  # table(data_sm$target)
  pred_parent <- data.frame(table(data_sm$target))
  pred_parent$pred <- pred_parent$Freq/sum(pred_parent$Freq)
  # print(pred_parent)
  entropy_parent <- -sum(pred_parent$pred*log2(pred_parent$pred))
  print(entropy_parent)
  if(is.factor(data_sm$variable)){
    
    tot_tab <- data.frame(table(data_sm$variable,data_sm$target))
    
    entropy_variable <- 0
    for (i in unique(tot_tab$Var1)){
      sub_tab <- tot_tab[which(tot_tab$Var1 == i),]
      sub_tab$pred <- sub_tab$Freq/sum(sub_tab$Freq)
      # print(sub_tab)
      g_sub <- -sum(sub_tab$pred*log2(sub_tab$pred))
      # print(g_sub)
      p_sub <- (sum(sub_tab$Freq)/sum(tot_tab$Freq))
      # print(p_sub)
      if(!is.nan(g_sub) && !is.nan(p_sub)){
        entropy_variable <- entropy_variable + (p_sub*g_sub)
      }
    }
    print(entropy_variable)
    print(entropy_parent - entropy_variable)
    if(entropy_variable != 0 ){
      variableImp[variableImp$variable == col_,"entropy"] <- entropy_parent - entropy_variable
    }
  }else{
    
      variableImp[variableImp$variable == col_,"entropy"] <- 0
    
  }
}

## Concordance
for(col_ in colnames(data_wrk)){
  data_sm <- data.frame(variable=data_wrk[,col_],
                        target = ifelse(data_wrk[,"is_obese"] == "1",1,0))
  colnames(data_sm) <- c("variable","target")
  print(paste("Making Model for",col_,"and is_obese"))
  print(head(data_sm))
  # table(data_sm$target)
  if (col_ != "is_obese"){
    gen_mod <- glm(target ~ variable, data = data_sm,na.action = na.omit)
    pred <- predict(gen_mod,data_sm)
    ( tab=as.matrix(table(data_sm$target, pred)) )
    # print(tab)
    ( tot=colSums(tab) )                            # Number of patients w/ each test result
    ( truepos=unname(rev(cumsum(rev(tab[2,])))) )   # Number of true positives
    ( falsepos=unname(rev(cumsum(rev(tab[1,])))) )  # Number of false positives
    ( totpos=sum(tab[2,]) )                         # The total number of positives (one number)
    ( totneg=sum(tab[1,]) )                         # The total number of negatives (one number)
    (sens=truepos/totpos)                           # Sensitivity (fraction true positives)
    (omspec=falsepos/totneg)                        # 1 − specificity (false positives)
    sens=c(sens,0); omspec=c(omspec,0)
    height = (sens[-1]+sens[-length(sens)])/2
    width = -diff(omspec) # = diff(rev(omspec))
    concordance <- abs(sum(height*width) - 0.5)
    print(concordance)
    variableImp[variableImp$variable == col_,"concordance"] <- concordance
  }
}

## Gini Coefficient
for(col_ in colnames(data_wrk)){
  data_sm <- data.frame(variable=data_wrk[,col_],
                        target = ifelse(data_wrk[,"is_obese"] == "1",1,0))
  colnames(data_sm) <- c("variable","target")
  print(paste("Making Model for",col_,"and is_obese"))
  print(head(data_sm))
  # table(data_sm$target)
  if (col_ != "is_obese"){
    gen_mod <- glm(target ~ variable, data = data_sm,na.action = na.omit)
    pred <- predict(gen_mod,data_sm)
    ( tab=as.matrix(table(data_sm$target, pred)) )
    # print(tab)
    ( tot=colSums(tab) )                            # Number of patients w/ each test result
    ( truepos=unname(rev(cumsum(rev(tab[2,])))) )   # Number of true positives
    ( falsepos=unname(rev(cumsum(rev(tab[1,])))) )  # Number of false positives
    ( totpos=sum(tab[2,]) )                         # The total number of positives (one number)
    ( totneg=sum(tab[1,]) )                         # The total number of negatives (one number)
    (sens=truepos/totpos)                           # Sensitivity (fraction true positives)
    (omspec=falsepos/totneg)                        # 1 − specificity (false positives)
    sens=c(sens,0); omspec=c(omspec,0)
    height = (sens[-1]+sens[-length(sens)])/2
    width = -diff(omspec) # = diff(rev(omspec))
    gini <- 2*sum(height*width) - 1
    print(gini)
    variableImp[variableImp$variable == col_,"ginicoeff"] <- gini
  }
}

## Information Valule

for(col_ in colnames(data_wrk)){
  data_sm <- data.frame(variable=data_wrk[,col_],
                        target = ifelse(data_wrk[,"is_obese"] == "1",1,0))
  colnames(data_sm) <- c("variable","target")
  print(paste("Making Model for",col_,"and is_obese"))
  print(head(data_sm))
  # table(data_sm$target)
  if(is.factor(data_sm$variable)){
    variableImp[variableImp$variable == col_,"informationValue"] <- 0
  }else{
    data_sm$binned <- binner(x=data_sm$variable,
                             y=data_sm$target)
    if (identical(data_sm$binned,data_sm$variable)){
      variableImp[variableImp$variable == col_,"informationValue"] <- 0
    }
    tot_tab <- data.frame(table(data_sm$binned,data_sm$target))
    tot_tab
    woe <- 0; iv <- 0
    for (i in unique(tot_tab$Var1)){
      tot_good <- sum(tot_tab[which(tot_tab$Var2 == good),"Freq"])
      tot_bad <- sum(tot_tab[which(tot_tab$Var2 != good),"Freq"])
      
      sub_tab <- tot_tab[which(tot_tab$Var1 == i),c("Var2","Freq")]
      # print(sub_tab)
      (dist_good <- (sub_tab[sub_tab$Var2 == good,"Freq"]/tot_good))
      (dist_bad <- (sub_tab[sub_tab$Var2 != good,"Freq"]/tot_bad))
      (woe <- log((dist_good/dist_bad),base = exp(1)))
      print(paste("woe:",woe))
      if(is.finite(woe)){
        (iv <- iv + (dist_good - dist_bad)*woe)
      }
      # print(paste("iv:",iv))
    }
    variableImp[variableImp$variable == col_,"informationValue"] <- iv
    
  }
}

#cleaning up
rm(sub_tab,data_sm,pred_parent,tot_tab,concordance,dist_good,dist_bad,
   entropy_parent,entropy_variable,falsepos,g_sub,p_sub,gini,gini_variable,
   height,width,i,iv,missclass,omspec,pred,sens,tab,tot,tot_bad,tot_good,
   totneg,totpos,truepos,woe,x,col_,good)

## ranking variables on each measures
variableImp$rankval_cont <- rowSums(variableImp[variableImp$is.factor == 0,2:8])/7
variableImp$rankval_cat <- rowSums(variableImp[variableImp$is.factor == 0,2:8])/7

variableImp$rankorder_cont <- rank(variableImp$variableImp$rankval_cont)
variableImp$rankorder_cat <- rank(variableImp$variableImp$rankval_cat)
  
variableImp_mat <- variableImp

# selction of variables

var_iv <- variableImp_mat[variableImp_mat$informationValue.x > 0.02,"variable"]
var_iv
# adding depedent variable
var_iv <- c(as.character(var_iv),"is_obese")

var_ranked <- c(variableImp_mat[variableImp_mat$rankorder_cont <=10 ,
                                "variable"],
                variableImp_mat[variableImp_mat$rankorder_cat <=10 ,
                                                            "variable"])
var_ranked
# adding depedent variable
var_ranked <- c(as.character(var_ranked),"is_obese")
          
setdiff(var_iv,var_ranked)           
# since we have only 6 more values these sound reasonable to proceed with


## now checking hetrosacardisity with only these variables
data_num <- data_

for (col_ in setdiff(colnames(data_num),"is_obese")){
  if (is.factor(data_num[,col_])){
    x <- as.numeric(levels(data_num[,col_]))[data_num[,col_]]
    data_num[col_] <- feature.scale(x)
  }else if(!is.factor(data_num[,col_])){
    x <- as.numeric(data_num[,col_])
    data_num[col_] <- feature.scale(x)
  }
}

data_1 <- data_num

data_num <- tempData2

for (col_ in setdiff(colnames(data_num),"is_obese")){
  if (is.factor(data_num[,col_])){
    x <- as.numeric(levels(data_num[,col_]))[data_num[,col_]]
    data_num[col_] <- feature.scale(x)
  }else if(!is.factor(data_num[,col_])){
    x <- as.numeric(data_num[,col_])
    data_num[col_] <- feature.scale(x)
  }
}

data_2 <- data_num

data_num <- tempData3

for (col_ in setdiff(colnames(data_num),"is_obese")){
  if (is.factor(data_num[,col_])){
    x <- as.numeric(levels(data_num[,col_]))[data_num[,col_]]
    data_num[col_] <- feature.scale(x)
  }else if(!is.factor(data_num[,col_])){
    x <- as.numeric(data_num[,col_])
    data_num[col_] <- feature.scale(x)
  }
}

data_3 <- data_num

class(data_1$is_obese)
## since is_obese is factor converting to numeric
data_1$is_obese <- ifelse(data_1$is_obese == "1",1,0)
data_2$is_obese <- ifelse(data_2$is_obese == "1",1,0)
data_3$is_obese <- ifelse(data_3$is_obese == "1",1,0)

# checking for hetroscardisity
gen_lm <- lm(is_obese ~. -1, data = data_1[,var_ranked],na.action = na.omit)
plot(gen_lm)
lmtest::bptest(gen_lm)

gen_lm1 <- lm(is_obese ~. -1, data = data_2[,var_ranked])
plot(gen_lm1)
lmtest::bptest(gen_lm1)

gen_lm2 <- lm(is_obese ~. -1, data = data_3[,var_ranked])
plot(gen_lm2)
lmtest::bptest(gen_lm2)

gen_lm <- lm(is_obese ~. -1, data = data_1[,var_iv],na.action = na.omit)
plot(gen_lm)
lmtest::bptest(gen_lm)

gen_lm1 <- lm(is_obese ~. -1, data = data_2[,var_iv])
plot(gen_lm1)
lmtest::bptest(gen_lm1)

gen_lm2 <- lm(is_obese ~. -1, data = data_3[,var_iv])
plot(gen_lm2)
lmtest::bptest(gen_lm2)

# one other way to address this is to make dummy variables
# categorical variables and normalising the continuos variables

data_num <- data_

for (col_ in setdiff(colnames(data_num),"is_obese")){
  if (is.factor(data_num[,col_])){
    name <- col_
    data_sm <- data.frame(data_num[, name])
    colnames(data_sm) <- name
    data_num[, name] <- NULL
    for (t in unique(data_sm[, name])) {
      new_col <- paste(name, t, sep = "_")
      data_sm[, new_col] <- ifelse(data_sm[, name] == t, 1, 0)
    }
    data_sm[,name] <- NULL
    data_num <- cbind(data_num, data_sm)
    rm(data_sm,name)
  }else if(!is.factor(data_num[,col_])){
    x <- as.numeric(data_num[,col_])
    data_num[col_] <- feature.scale(x)
  }
}

data_1 <- data_num

data_num <- tempData2

for (col_ in setdiff(colnames(data_num),"is_obese")){
  if (is.factor(data_num[,col_])){
    name <- col_
    data_sm <- data.frame(data_num[, name])
    colnames(data_sm)<- name
    data_num[, name] <- NULL
    for (t in unique(data_[, name])) {
      new_col <- paste(name, t, sep = "_")
      data_[, new_col] <- ifelse(data_[, name] == t, 1, 0)
    }
    data_sm[,name] <- NULL
    data_num <- cbind(data_num, data_sm)
    rm(data_,name)
  }else if(!is.factor(data_num[,col_])){
    x <- as.numeric(data_num[,col_])
    data_num[col_] <- feature.scale(x)
  }
}

data_2 <- data_num

data_num <- tempData3

for (col_ in setdiff(colnames(data_num),"is_obese")){
  if (is.factor(data_num[,col_])){
    name <- col_
    data_sm <- data.frame(data_num[, name])
    colnames(data_sm)<- name
    data_num[, name] <- NULL
    for (t in unique(data_[, name])) {
      new_col <- paste(name, t, sep = "_")
      data_[, new_col] <- ifelse(data_[, name] == t, 1, 0)
    }
    data_sm[,name] <- NULL
    data_num <- cbind(data_num, data_sm)
    rm(data_,name)
  }else if(!is.factor(data_num[,col_])){
    x <- as.numeric(data_num[,col_])
    data_num[col_] <- feature.scale(x)
  }
}

data_3 <- data_num
rm(data_num)

## since is_obese is factor converting to numeric
data_1$is_obese <- ifelse(data_1$is_obese == "1",1,0)
data_2$is_obese <- ifelse(data_2$is_obese == "1",1,0)
data_3$is_obese <- ifelse(data_3$is_obese == "1",1,0)

# checking for hetroscardisity
gen_lm <- lm(is_obese ~. -1, data = data_1,na.action = na.omit)
plot(gen_lm)
lmtest::bptest(gen_lm)

gen_lm1 <- lm(is_obese ~. -1, data = data_2)
plot(gen_lm1)
lmtest::bptest(gen_lm1)

gen_lm2 <- lm(is_obese ~. -1, data = data_3)
plot(gen_lm2)
lmtest::bptest(gen_lm2)

# now considering variables selected above

data_num <- data_[,var_ranked]

for (col_ in setdiff(colnames(data_num),"is_obese")){
  if (is.factor(data_num[,col_])){
    name <- col_
    data_sm <- data.frame(data_num[, name])
    colnames(data_sm)<- name
    data_num[, name] <- NULL
    for (t in unique(data_[, name])) {
      new_col <- paste(name, t, sep = "_")
      data_[, new_col] <- ifelse(data_[, name] == t, 1, 0)
    }
    data_sm[,name] <- NULL
    data_num <- cbind(data_num, data_sm)
    rm(data_,name)
  }else if(!is.factor(data_num[,col_])){
    x <- as.numeric(data_num[,col_])
    data_num[col_] <- feature.scale(x)
  }
}

data_1 <- data_num

data_num <- tempData2[,var_ranked]

for (col_ in setdiff(colnames(data_num),"is_obese")){
  if (is.factor(data_num[,col_])){
    name <- col_
    data_sm <- data.frame(data_num[, name])
    colnames(data_sm)<- name
    data_num[, name] <- NULL
    for (t in unique(data_[, name])) {
      new_col <- paste(name, t, sep = "_")
      data_[, new_col] <- ifelse(data_[, name] == t, 1, 0)
    }
    data_sm[,name] <- NULL
    data_num <- cbind(data_num, data_sm)
    rm(data_,name)
  }else if(!is.factor(data_num[,col_])){
    x <- as.numeric(data_num[,col_])
    data_num[col_] <- feature.scale(x)
  }
}

data_2 <- data_num

data_num <- tempData3[,var_ranked]

for (col_ in setdiff(colnames(data_num),"is_obese")){
  if (is.factor(data_num[,col_])){
    name <- col_
    data_sm <- data.frame(data_num[, name])
    colnames(data_sm)<- name
    data_num[, name] <- NULL
    for (t in unique(data_[, name])) {
      new_col <- paste(name, t, sep = "_")
      data_[, new_col] <- ifelse(data_[, name] == t, 1, 0)
    }
    data_sm[,name] <- NULL
    data_num <- cbind(data_num, data_sm)
    rm(data_,name)
  }else if(!is.factor(data_num[,col_])){
    x <- as.numeric(data_num[,col_])
    data_num[col_] <- feature.scale(x)
  }
}

data_3 <- data_num
rm(data_num)

## since is_obese is factor converting to numeric
data_1$is_obese <- ifelse(data_1$is_obese == "1",1,0)
data_2$is_obese <- ifelse(data_2$is_obese == "1",1,0)
data_3$is_obese <- ifelse(data_3$is_obese == "1",1,0)

# checking for hetroscardisity
gen_lm <- lm(is_obese ~. -1, data = data_1,na.action = na.omit)
plot(gen_lm)
lmtest::bptest(gen_lm)

gen_lm1 <- lm(is_obese ~. -1, data = data_2)
plot(gen_lm1)
lmtest::bptest(gen_lm1)

gen_lm2 <- lm(is_obese ~. -1, data = data_3)
plot(gen_lm2)
lmtest::bptest(gen_lm2)

## checking with iv based variables
data_num <- data_[,var_iv]

for (col_ in setdiff(colnames(data_num),"is_obese")){
  if (is.factor(data_num[,col_])){
    name <- col_
    data_sm <- data.frame(data_num[, name])
    colnames(data_sm)<- name
    data_num[, name] <- NULL
    for (t in unique(data_[, name])) {
      new_col <- paste(name, t, sep = "_")
      data_[, new_col] <- ifelse(data_[, name] == t, 1, 0)
    }
    data_sm[,name] <- NULL
    data_num <- cbind(data_num, data_sm)
    rm(data_,name)
  }else if(!is.factor(data_num[,col_])){
    x <- as.numeric(data_num[,col_])
    data_num[col_] <- feature.scale(x)
  }
}

data_1 <- data_num

data_num <- tempData2[,var_iv]

for (col_ in setdiff(colnames(data_num),"is_obese")){
  if (is.factor(data_num[,col_])){
    name <- col_
    data_sm <- data.frame(data_num[, name])
    colnames(data_sm)<- name
    data_num[, name] <- NULL
    for (t in unique(data_[, name])) {
      new_col <- paste(name, t, sep = "_")
      data_[, new_col] <- ifelse(data_[, name] == t, 1, 0)
    }
    data_sm[,name] <- NULL
    data_num <- cbind(data_num, data_sm)
    rm(data_,name)
  }else if(!is.factor(data_num[,col_])){
    x <- as.numeric(data_num[,col_])
    data_num[col_] <- feature.scale(x)
  }
}

data_2 <- data_num

data_num <- tempData3[,var_iv]

for (col_ in setdiff(colnames(data_num),"is_obese")){
  if (is.factor(data_num[,col_])){
    name <- col_
    data_sm <- data.frame(data_num[, name])
    colnames(data_sm)<- name
    data_num[, name] <- NULL
    for (t in unique(data_[, name])) {
      new_col <- paste(name, t, sep = "_")
      data_[, new_col] <- ifelse(data_[, name] == t, 1, 0)
    }
    data_sm[,name] <- NULL
    data_num <- cbind(data_num, data_sm)
    rm(data_,name)
  }else if(!is.factor(data_num[,col_])){
    x <- as.numeric(data_num[,col_])
    data_num[col_] <- feature.scale(x)
  }
}

data_3 <- data_num
rm(data_num)

## since is_obese is factor converting to numeric
data_1$is_obese <- ifelse(data_1$is_obese == "1",1,0)
data_2$is_obese <- ifelse(data_2$is_obese == "1",1,0)
data_3$is_obese <- ifelse(data_3$is_obese == "1",1,0)

# checking for hetroscardisity
gen_lm <- lm(is_obese ~. -1, data = data_1,na.action = na.omit)
plot(gen_lm)
lmtest::bptest(gen_lm)

gen_lm1 <- lm(is_obese ~. -1, data = data_2)
plot(gen_lm1)
lmtest::bptest(gen_lm1)

gen_lm2 <- lm(is_obese ~. -1, data = data_3)
plot(gen_lm2)
lmtest::bptest(gen_lm2)

### checking with some more variables
var_ranked <- variableImp_mat[variableImp_mat$rank.order <=30 ,"variable"]
var_ranked
# adding depedent variable
var_ranked <- c(as.character(var_ranked),"is_obese")


data_num <- data_[,var_ranked]

for (col_ in setdiff(colnames(data_num),"is_obese")){
  if (is.factor(data_num[,col_])){
    name <- col_
    data_sm <- data.frame(data_num[, name])
    colnames(data_sm)<- name
    data_num[, name] <- NULL
    for (t in unique(data_[, name])) {
      new_col <- paste(name, t, sep = "_")
      data_[, new_col] <- ifelse(data_[, name] == t, 1, 0)
    }
    data_sm[,name] <- NULL
    data_num <- cbind(data_num, data_sm)
    rm(data_,name)
  }else if(!is.factor(data_num[,col_])){
    x <- as.numeric(data_num[,col_])
    data_num[col_] <- feature.scale(x)
  }
}

data_1 <- data_num

data_num <- tempData2[,var_ranked]

for (col_ in setdiff(colnames(data_num),"is_obese")){
  if (is.factor(data_num[,col_])){
    name <- col_
    data_sm <- data.frame(data_num[, name])
    colnames(data_sm)<- name
    data_num[, name] <- NULL
    for (t in unique(data_[, name])) {
      new_col <- paste(name, t, sep = "_")
      data_[, new_col] <- ifelse(data_[, name] == t, 1, 0)
    }
    data_sm[,name] <- NULL
    data_num <- cbind(data_num, data_sm)
    rm(data_,name)
  }else if(!is.factor(data_num[,col_])){
    x <- as.numeric(data_num[,col_])
    data_num[col_] <- feature.scale(x)
  }
}

data_2 <- data_num

data_num <- tempData3[,var_ranked]

for (col_ in setdiff(colnames(data_num),"is_obese")){
  if (is.factor(data_num[,col_])){
    name <- col_
    data_sm <- data.frame(data_num[, name])
    colnames(data_sm)<- name
    data_num[, name] <- NULL
    for (t in unique(data_[, name])) {
      new_col <- paste(name, t, sep = "_")
      data_[, new_col] <- ifelse(data_[, name] == t, 1, 0)
    }
    data_sm[,name] <- NULL
    data_num <- cbind(data_num, data_sm)
    rm(data_,name)
  }else if(!is.factor(data_num[,col_])){
    x <- as.numeric(data_num[,col_])
    data_num[col_] <- feature.scale(x)
  }
}

data_3 <- data_num
rm(data_num)

## since is_obese is factor converting to numeric
data_1$is_obese <- ifelse(data_1$is_obese == "1",1,0)
data_2$is_obese <- ifelse(data_2$is_obese == "1",1,0)
data_3$is_obese <- ifelse(data_3$is_obese == "1",1,0)

# checking for hetroscardisity
gen_lm <- lm(is_obese ~. -1, data = data_1,na.action = na.omit)
plot(gen_lm)
lmtest::bptest(gen_lm)

gen_lm1 <- lm(is_obese ~. -1, data = data_2)
plot(gen_lm1)
lmtest::bptest(gen_lm1)

gen_lm2 <- lm(is_obese ~. -1, data = data_3)
plot(gen_lm2)
lmtest::bptest(gen_lm2)

## now this tells us something that model might not working 
## properly with continuous variable
## so hence we try to bin the contineous and the do dummy 
## generation

data_num <- data_

for (col_ in setdiff(colnames(data_num),"is_obese")){
  if (is.factor(data_num[,col_])){
    name <- col_
    data_sm <- data.frame(data_num[, name])
    colnames(data_sm)<- name
    data_num[, name] <- NULL
    for (t in unique(data_[, name])) {
      new_col <- paste(name, t, sep = "_")
      data_[, new_col] <- ifelse(data_[, name] == t, 1, 0)
    }
    data_sm[,name] <- NULL
    data_num <- cbind(data_num, data_sm)
    rm(data_,name)
  }else if(!is.factor(data_num[,col_])){
    name <- col_
    data_sm <- binner(data.frame(data_num[, name]))
    colnames(data_sm)<- name
    data_num[, name] <- NULL
    for (t in unique(data_[, name])) {
      new_col <- paste(name, t, sep = "_")
      data_[, new_col] <- ifelse(data_[, name] == t, 1, 0)
    }
    data_sm[,name] <- NULL
    data_num <- cbind(data_num, data_sm)
    rm(data_,name)
  }
}

data_1 <- data_num

data_num <- tempData2

for (col_ in setdiff(colnames(data_num),"is_obese")){
  if (is.factor(data_num[,col_])){
    x <- as.numeric(levels(data_num[,col_]))[data_num[,col_]]
    data_num[col_] <- feature.scale(x)
  }else if(!is.factor(data_num[,col_])){
    x <- as.numeric(data_num[,col_])
    data_num[col_] <- feature.scale(x)
  }
}

data_2 <- data_num

data_num <- tempData3

for (col_ in setdiff(colnames(data_num),"is_obese")){
  if (is.factor(data_num[,col_])){
    x <- as.numeric(levels(data_num[,col_]))[data_num[,col_]]
    data_num[col_] <- feature.scale(x)
  }else if(!is.factor(data_num[,col_])){
    x <- as.numeric(data_num[,col_])
    data_num[col_] <- feature.scale(x)
  }
}

data_3 <- data_num

class(data_1$is_obese)
## since is_obese is factor converting to numeric
data_1$is_obese <- ifelse(data_1$is_obese == "1",1,0)
data_2$is_obese <- ifelse(data_2$is_obese == "1",1,0)
data_3$is_obese <- ifelse(data_3$is_obese == "1",1,0)

# checking for hetroscardisity
gen_lm <- lm(is_obese ~. -1, data = data_1,na.action = na.omit)
plot(gen_lm)
lmtest::bptest(gen_lm)

gen_lm1 <- lm(is_obese ~. -1, data = data_2)
plot(gen_lm1)
lmtest::bptest(gen_lm1)

gen_lm2 <- lm(is_obese ~. -1, data = data_3)
plot(gen_lm2)
lmtest::bptest(gen_lm2)

#### Modelling Iteration 9 #######################


#GLM

# Modelling with basic data
modglm1 <- glm( as.integer(is_obese) ~ . , data = train,
                control = glm.control(epsilon = 1e-6,
                                      maxit = 50,
                                      trace = TRUE) )
summary(modglm1)
pred_glm1 <- predict(modglm1,test)
confusionMatrix(ifelse(pred_glm1>0.5,1,0),test[,"is_obese"])
confusionMatrix(ifelse(pred_glm1>0.3,1,0),test[,"is_obese"])
confusionMatrix(ifelse(pred_glm1>0.2,1,0),test[,"is_obese"])

####

modglm2 <- glm( is_obese ~ . -1, data = train,
                family = binomial,
                control = glm.control(epsilon = 1e-6,
                                      maxit = 50,
                                      trace = TRUE) )
summary(modglm2)
pred_glm2 <- predict(modglm2,test)
confusionMatrix(ifelse(pred_glm2>0.5,1,0),test[,"is_obese"])

# Modelling with all selected Variables

modglm1 <- glm( is_obese ~ . , data = data_num[trainIndex,var_pca],
                control = glm.control(epsilon = 1e-6,
                                      maxit = 50,
                                      trace = TRUE) )
summary(modglm1)
pred_glm1 <- predict(modglm1,data_num[-trainIndex,var_pca])
confusionMatrix(ifelse(pred_glm1>0.5,1,0),data_num[-trainIndex,"is_obese"])
confusionMatrix(ifelse(pred_glm1>0.3,1,0),data_num[-trainIndex,"is_obese"])
confusionMatrix(ifelse(pred_glm1>0.2,1,0),data_num[-trainIndex,"is_obese"])

####

modglm2 <- glm( is_obese ~ . -1, data = data_num[trainIndex,var_pca],
                family = binomial,
                control = glm.control(epsilon = 1e-6,
                                      maxit = 50,
                                      trace = TRUE) )
summary(modglm2)
pred_glm2 <- predict(modglm2,data_num[-trainIndex,var_pca])
confusionMatrix(ifelse(pred_glm2>0.5,1,0),data_num[-trainIndex,"is_obese"])
confusionMatrix(ifelse(pred_glm2>0.3,1,0),data_num[-trainIndex,"is_obese"])
confusionMatrix(ifelse(pred_glm2>0.2,1,0),data_num[-trainIndex,"is_obese"])

# Modelling with selected PCs
modglm1 <- glm( is_obese ~ . , data = train[,pca_var_sel],
                control = glm.control(epsilon = 1e-6,
                                      maxit = 50,
                                      trace = TRUE) )
summary(modglm1)
pred_glm1 <- predict(modglm1,test)
confusionMatrix(ifelse(pred_glm1>0.5,1,0),test[,"is_obese"])
confusionMatrix(ifelse(pred_glm1>0.3,1,0),test[,"is_obese"])
confusionMatrix(ifelse(pred_glm1>0.2,1,0),test[,"is_obese"])

####

modglm2 <- glm( is_obese ~ . -1, data = train[,pca_var_sel],
                family = binomial,
                control = glm.control(epsilon = 1e-6,
                                      maxit = 50,
                                      trace = TRUE) )
summary(modglm2)
pred_glm2 <- predict(modglm2,test)
confusionMatrix(ifelse(pred_glm2>0.5,1,0),test[,"is_obese"])

#### Iterating to reduce noice
to_remove_pca <- c("RC11","RC6","RC8","RC3","RC7","RC4","RC13")
to_keep <- setdiff(colnames(train),to_remove_pca)
to_keep <- c("RC1","RC4","RC9","RC12","is_obese")

modglm2 <- glm( is_obese ~ ., data = train[,to_keep],
                family = binomial,
                control = glm.control(epsilon = 1e-6,
                                      maxit = 50,
                                      trace = TRUE) )
summary(modglm2)
plot(modglm2)
pred_glm2 <- predict(modglm2,test)
confusionMatrix(ifelse(pred_glm2>0.5,1,0),test[,"is_obese"])
confusionMatrix(ifelse(pred_glm2>0.3,1,0),test[,"is_obese"])
confusionMatrix(ifelse(pred_glm2>-1,1,0),test[,"is_obese"])

## cleaning up
rm(modglm1,modglm2,pred_glm1,pred_glm2)

## not Even close to our previous attempts

## CART Model

## making cart models
modCart1 <- rpart(is_obese ~ . ,
                  data = train)

rpart.plot(modCart1)
predict_cart1 <- predict(modCart1,test)
head(predict_cart1,5)

confusionMatrix(ifelse(predict_cart1[,"0"]>0.8,1,0),test[,"is_obese"])

####
modCart2 <- rpart(is_obese ~ . ,
                  data = train_2)

rpart.plot(modCart2)
predict_cart2 <- predict(modCart2,test_2)
head(predict_cart2,5)

confusionMatrix(ifelse(predict_cart2[,"0"]>0.8,1,0),test[,"is_obese"])

#####

modCart3 <- rpart(is_obese ~ . ,
                  data = train_3)

rpart.plot(modCart3)
predict_cart3 <- predict(modCart3,test_3)
head(predict_cart3,5)

confusionMatrix(ifelse(predict_cart3[,"1"]>0.01,1,0),test[,"is_obese"])


#cleaning else
rm(modCart1,predict_cart1)


## Decision tree Model

modDT1 <- ctree(is_obese ~ ., data=train)

plot(modDT1)

pred_dt1 <- predict(modDT1,test)

confusionMatrix(pred_dt1,test[,"is_obese"])

####

modDT2 <- ctree(is_obese ~ ., data=train,
                controls = ctree_control(teststat = "max",
                                         testtype = "Bonferroni",
                                         mincriterion = 0.95,
                                         minsplit = 20,
                                         minbucket = 7,
                                         # nresample = 100,
                                         mtry = 0,
                                         maxdepth = 0))

plot(modDT2)

pred_dt2 <- predict(modDT2,test)

confusionMatrix(pred_dt2,test[,"is_obese"])

####

modDT3 <- ctree(as.factor(is_obese) ~ ., data=train[,to_keep],
                controls = ctree_control(teststat = "max",
                                         testtype = "Univariate",
                                         mincriterion = 0.95,
                                         minsplit = 20,
                                         minbucket = 7,
                                         # nresample = 100,
                                         mtry = 0,
                                         maxdepth = 0))

plot(modDT3)

pred_dt3 <- predict(modDT3,test)

confusionMatrix(pred_dt3,test[,"is_obese"])

# we will consider modDT5 as our best tree model

rm(modDT1,modDT2,modDT3pred_dt1,pred_dt2,pred_dt3)

## KNN Model

modknn1 <- knn(train = subset(train,select = -c(is_obese)),
               test = subset(test,select = -c(is_obese)),
               cl = train[,"is_obese"],k = 5 , l = 0 , prob = TRUE)

confusionMatrix(modknn1,test[,"is_obese"])

####

modknn2 <- knn(train = subset(train,select = -c(is_obese)),
               test = subset(test,select = -c(is_obese)),
               cl = train[,"is_obese"],
               k = 10 , l = 0 , prob = TRUE)

confusionMatrix(modknn2,test[,"is_obese"])


## cleaning up
rm(modknn1,modknn2)

### Naive bayes model

modNB1 <- naiveBayes(is_obese ~. , data = train)
summary(modNB1)

pred_nb1 <- as.data.frame(predict(modNB1,test,type = "raw"))

confusionMatrix(ifelse(pred_nb1[,"1"]>pred_nb1[,"0"],1,0),test[,"is_obese"])
confusionMatrix(ifelse(pred_nb1[,"1"]>0.5,1,0),test[,"is_obese"])

### essentially the model is pathetic

rm(modNB1,pred_nb1)

# Random Forest Model

modrf1 <- randomForest(is_obese ~ . , data = train,
                       importance = TRUE,proximity = TRUE,
                       ntree = 100,
                       do.trace= TRUE)
summary(modrf1)
plot(modrf1)

pred_rf1 <- predict(modrf1,test)

confusionMatrix(ifelse(pred_rf1>0.5,1,0),test[,"is_obese"])
confusionMatrix(ifelse(pred_rf1>0.6,1,0),test[,"is_obese"])
confusionMatrix(ifelse(pred_rf1>0.55,1,0),test[,"is_obese"])
confusionMatrix(ifelse(pred_rf1>0.45,1,0),test[,"is_obese"])
confusionMatrix(ifelse(pred_rf1>0.4,1,0),test[,"is_obese"])

####

modrf2 <- randomForest(as.factor(is_obese) ~ . , data = train,
                       importance = TRUE,proximity = TRUE,
                       ntree = 120,
                       # maxnodes = 50, nPerm = 1.2,
                       do.trace= TRUE)
summary(modrf2)
plot(modrf2)

pred_rf2 <- predict(modrf2,test)

confusionMatrix(pred_rf2,test[,"is_obese"])

# cleaning up
rm(modrf1,modrf2,pred_rf1,pred_rf2)

### Neural Network Model

modnnet1 <- nnet(is_obese~., data = train,
                 size = 7 ,linout = FALSE, entropy = FALSE, 
                 softmax = FALSE,censored = FALSE, skip = FALSE, 
                 rang = 0.7, decay = 0, maxit = 100, Hess = FALSE, 
                 trace = TRUE, MaxNWts = 1000,
                 abstol = 1.0e-8, reltol = 1.0e-8)

summary(modnnet1)
pred_nn1 <- predict(modnnet1,test)

confusionMatrix(ifelse(pred_nn1>.3,1,0),test[,"is_obese"])
table(pred_nn1,test[,"is_obese"])

## XGBoost Model
d_train <- xgb.DMatrix(data = data.matrix(train),
                       label = data.matrix(train[,"is_obese"]),
                       missing = 0)
d_test <- xgb.DMatrix(data = data.matrix(test),
                      label = data.matrix(test[,"is_obese"]),
                      missing = 0)

par  <-  list(booster = "gblinear",
              objective = "reg:logistic",
              # eta = 0.2,
              # min_child_weight = 6,
              gamma = 2,
              subsample = 0.85,
              colsample_bytree = 0.75,
              # max_depth = 10,
              verbose = 1,
              alpha = 0.5,
              scale_pos_weight = 1)
#selecting number of Rounds
n_rounds= 200

#modeling
xgb_mod_1  <- xgboost(params = par, 
                      data = d_train,
                      # data = sparse_matrix_tr,
                      # label = op_vec ,
                      nrounds = n_rounds)

pred  <- predict(xgb_mod_1,d_test)
as.data.frame(pred)

pred_xg_1 <- ifelse(pred>0.24,1,0)
confusionMatrix(pred_xg_1,test_[,"tnd_cc_flag"])

### Modelling Iteration 9 Ends

