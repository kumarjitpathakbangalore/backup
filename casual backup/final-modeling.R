### Smarts Prediction...

# Setting seed for Reproducebility
set.seed(1234)



# Setting working Directory

setwd("F:/ofc/fault-pred")
setwd("/media/jitink/dsa/ofc/fault-pred/")

#On server
set.seed(1610)
setwd("/apps/data/models/")	
.libPaths(c("/apps/R",.libPaths()))
load("/apps/data/smarts-data.RData")
results <- read.csv("/apps/data/results-all.csv",header = T,stringsAsFactors = F)

# getting smoted data for 30 days
apr25 <- read.csv("smoted/apr25_all.csv",header = T,stringsAsFactors = F)
apr26 <- read.csv("smoted/apr26_all.csv",header = T,stringsAsFactors = F)
apr27 <- read.csv("smoted/apr27_all.csv",header = T,stringsAsFactors = F)
apr28 <- read.csv("smoted/apr28_all.csv",header = T,stringsAsFactors = F)
apr29 <- read.csv("smoted/apr29_all.csv",header = T,stringsAsFactors = F)
apr30 <- read.csv("smoted/apr30_all.csv",header = T,stringsAsFactors = F)
jun01 <- read.csv("smoted/june1_all.csv",header = T,stringsAsFactors = F)
jun03 <- read.csv("smoted/june3_all.csv",header = T,stringsAsFactors = F)
jun04 <- read.csv("smoted/june4_all.csv",header = T,stringsAsFactors = F)
jun05 <- read.csv("smoted/june5_all.csv",header = T,stringsAsFactors = F)
jun06 <- read.csv("smoted/june6_all.csv",header = T,stringsAsFactors = F)
jun07 <- read.csv("smoted/june7_all.csv",header = T,stringsAsFactors = F)
jun08 <- read.csv("smoted/june8_all.csv",header = T,stringsAsFactors = F)
jun09 <- read.csv("smoted/june9_all.csv",header = T,stringsAsFactors = F)
may01 <- read.csv("smoted/may01_all.csv",header = T,stringsAsFactors = F)
may02 <- read.csv("smoted/may02_all.csv",header = T,stringsAsFactors = F)
may03 <- read.csv("smoted/may03_all.csv",header = T,stringsAsFactors = F)
may04 <- read.csv("smoted/may04_all.csv",header = T,stringsAsFactors = F)
may06 <- read.csv("smoted/may06_all.csv",header = T,stringsAsFactors = F)
may09 <- read.csv("smoted/may09_all.csv",header = T,stringsAsFactors = F)
may10 <- read.csv("smoted/may10_all.csv",header = T,stringsAsFactors = F)
may13 <- read.csv("smoted/may13_all.csv",header = T,stringsAsFactors = F)
may14 <- read.csv("smoted/may14_all.csv",header = T,stringsAsFactors = F)
may15 <- read.csv("smoted/may15_all.csv",header = T,stringsAsFactors = F)
may24 <- read.csv("smoted/may24_all.csv",header = T,stringsAsFactors = F)
may26 <- read.csv("smoted/may26_all.csv",header = T,stringsAsFactors = F)
may27 <- read.csv("smoted/apr27_all.csv",header = T,stringsAsFactors = F)
may28 <- read.csv("smoted/may28_all.csv",header = T,stringsAsFactors = F)
may29 <- read.csv("smoted/may29_all.csv",header = T,stringsAsFactors = F)
may30 <- read.csv("smoted/may30_all.csv",header = T,stringsAsFactors = F)

### merging all data
smarts_all <- rbind(apr26,apr27,apr28,apr29,jun01,jun03,jun04,jun05,jun06,jun07,jun09,
                    may01,may02,may03,may04,may06,may09,may10,may13,may14,may15,may24,
                    apr25,apr30,jun08,may26,may27,may28,may29,may30)

# cleaning up the space
rm(apr26,apr27,apr28,apr29,jun01,jun03,jun04,jun05,jun06,jun07,jun09,
   may01,may02,may03,may04,may06,may09,may10,may13,may14,may15,may24,
   apr25,apr30,jun08,may26,may27,may28,may29,may30)

# getting original columns
orig_var <- c("pmp_line_attn_us", "pmp_lols_fer_ds",
              "pmp_crc_ec_ner_us", "pmp_loss_fer_ds", "pmp_ses_ner_us", "pmp_r_us",
              "pmp_uas_ner_us","pmp_es_ner_us","pmp_pwr_stt", "pmp_loss_ner_us",
              "pmp_aatp_us", "pmp_act_inp_ds", "pmp_s_ds",  "pmp_line_attn_ds",
              "pmp_uas_ner_ds", "pmp_suc_init", "pmp_act_inp_us", "pmp_lops_fer_ds",
              "pmp_n_us", "rtx_used_ds",  "pmp_s_us", "pmp_snr_us", "pmp_up_time",
              "pmp_fail_full_init", "pmp_inmme_us", "pmp_ses_fer_ds",
              "rtx_used_us",  "pmp_d_ds", "pmp_snr_ds", "pmp_line_rate_us",
              "pmp_r_ds", "pmp_crc_ec_fer_ds",  "pmp_max_atbl_dr_ds",
              "pmp_inmiat_ds", "pmp_trfc_ds", "pmp_lofs_fer_ds",  "pmp_aatp_ds",
              "pmp_nte_pwr_stt", "pmp_fecs_fer_ds", "pmp_n_ds", "pmp_max_atbl_dr_us",
              "pmp_inmme_ds", "pmp_trfc_us", "pmp_fec_evt_ner_us",  "pmp_d_us",
              "pmp_line_rate_ds", "pmp_inminpeq_ds", "pmp_es_fer_ds",
              "pmp_fec_evt_fer_ds", "pmp_fecs_ner_us")

##Trying to remove duplicates
smarts <- smarts_all[!duplicated(smarts_all[orig_var]),]
str(smarts)

# Saving the file
# write.csv(smarts,file = "smarts-final.csv",row.names = F)
smarts <- read.csv("smarts-final.csv",header = T,stringsAsFactors = F)

# On server
load("/apps/data/smarts-data.RData")

table(smarts$tnd_cc_flag)
# 0      1
# 327789 100189

# final data ratio
table(smarts$tnd_cc_flag)/nrow(smarts)
# 0         1
# 0.7659015 0.2340985



rm(smarts_all)



# validation data
jun11 <- read.csv("phase-validation/jun11-0.95.csv",header = T,stringsAsFactors = F)
jun12 <- read.csv("phase-validation/jun12-0.95.csv",header = T,stringsAsFactors = F)
jun13 <- read.csv("phase-validation/jun13-0.95.csv",header = T,stringsAsFactors = F)
jun14 <- read.csv("phase-validation/jun14-0.95.csv",header = T,stringsAsFactors = F)
jun15 <- read.csv("phase-validation/jun15-0.95.csv",header = T,stringsAsFactors = F)
jun16 <- read.csv("phase-validation/jun16-0.95.csv",header = T,stringsAsFactors = F)

attribute <-read.csv("/apps/data/attrib.csv",header=T)
jun11 <- read.csv("/apps/data/mapped/jun11/jun11.csv",header = T,stringsAsFactors = F)
jun12 <- read.csv("/apps/data/mapped/jun12/jun12.csv",header = T,stringsAsFactors = F)
jun13 <- read.csv("/apps/data/mapped/jun13/jun13.csv",header = T,stringsAsFactors = F)
jun14 <- read.csv("/apps/data/mapped/jun14/jun14.csv",header = T,stringsAsFactors = F)
jun15 <- read.csv("/apps/data/mapped/jun15/jun15.csv",header = T,stringsAsFactors = F)
jun16 <- read.csv("/apps/data/mapped/jun16/jun16.csv",header = T,stringsAsFactors = F)
jun17 <- read.csv("/apps/data/mapped/jun17/jun17.csv",header = T,stringsAsFactors = F)
jun18 <- read.csv("/apps/data/mapped/jun18/jun18.csv",header = T,stringsAsFactors = F)
jun19 <- read.csv("/apps/data/mapped/jun19/jun19.csv",header = T,stringsAsFactors = F)
jun20 <- read.csv("/apps/data/mapped/jun20/jun20.csv",header = T,stringsAsFactors = F)


colnames(jun11) <- colnames(attribute)
colnames(jun12) <- colnames(attribute)
colnames(jun13) <- colnames(attribute)
colnames(jun14) <- colnames(attribute)
colnames(jun15) <- colnames(attribute)
validate <- rbind(jun11,jun12,jun13,jun14,jun15)
validate$new_flag <- ifelse(((validate$tnd_cc_flag==1)|(validate$smart_cc_flag==1)),1,0)
validate$tnd_cc_flag <- validate$new_flag

validate$b.neid <- NULL
validate$b.day_tslot <- NULL
validate$b.mask_neid <- NULL
validate$smart_cc_flag <- NULL
validate$new_flag <- NULL



rm(jun11,jun12,jun13,jun14,jun15,jun16)

table(validate$tnd_cc_flag)/nrow(validate)
# 0         1
# 0.9743582 0.0256418

table(validate$day_tslot)

#write.csv(validate,file = "phase-validation/6-days-validate.csv",row.names = F)
validate <- read.csv("phase-validation/6-days-validate.csv",header = T,stringAsFactors = F)

#Loading previous mrmr results
results <- read.csv("results-all.csv",header = T,stringsAsFactors = F)

var_mrmr_all <- results[1:515,1]
var_mrmr_100 <- results[1:100,1]
var_mrmr_50 <- results[1:50,1]
var_c <- c('neid','model','day_tslot','tnd_cc_flag')
# Variables dropped from Bivariate analysis 
var_dropped1 <- c("pmp_loss_ner_us_10", 
                  "pmp_loss_fer_ds", 
                  "pmp_lofs_fer_ds", 
                  "pmp_loss_fer_ds_9", 
                  "pmp_loss_ner_us_0", 
                  "pmp_loss_ner_us_4", 
                  "pmp_ses_ner_us_7", 
                  "rtx_used_ds_5", 
                  "pmp_loss_ner_us_8", 
                  "pmp_loss_ner_us_2", 
                  "pmp_loss_ner_us_7") 

#converting all data to numeric
for (col_ in setdiff(colnames(smarts),var_c)){
  smarts[,col_] <- as.numeric(smarts[,col_])
}
# Getting Null Varibales greater that 0.1 %
var_null <- c()
for (col_ in colnames(smarts)){
  x <- sum(is.na(smarts[,col_]))
  rat <- x/nrow(smarts)
  if (x > 0 ){
    cat(col_," has ",x," NAs and ", rat," precentage of na \n")
    if (rat>.1){
      var_null <- c(var_null,col_)
    }
  }
}


################################################################################
# Defining target variable
labelName <- "tnd_cc_flag"

# Running MRMR on smoted data
library(mRMRe)

data_mrmr <- mRMR.data(data =
                         data.frame(target=smarts[,labelName],
                                    smarts[,setdiff(colnames(smarts),labelName)]))

fs_all <- mRMR.ensemble(data = data_mrmr, target_indices = 1,
                        feature_count = 739, solution_count = 1)


## Doing Ensembling with caret
c
registerDoMC(cores = 2)
 
## Other type of feature Selection
library(fscaret)
data("funcRegPred")
print(funcRegPred)
# install.packages("fscaret", dependencies = c("Depends","Suggests"))

# Subsetting data for with Less Null values and replacing Nulls with zero
traindata <- smarts[,setdiff(colnames(smarts),var_null)]
traindata <- smarts[,c(var_mrmr_all,'tnd_cc_flag')]
traindata <- smarts[,c(var_mrmr_100,'tnd_cc_flag')]
traindata <- smarts[,c(var_mrmr_50,'tnd_cc_flag')]
traindata <- smarts[,c(orig_var,'tnd_cc_flag')]
traindata <- smarts[,c(var_mrm_all,'tnd_cc_flag')]

traindata[is.na(traindata)] <- 0

#On Server
predictors_org <- setdiff(orig_var,var_null)
traindata <- smarts[,c(predictors_org,labelName)]
traindata[is.na(traindata)] <- 0
traindata <- traindata[sample(nrow(traindata)),]
split <- floor(nrow(traindata)/3)
ensembleData <- traindata[0:split,]
blenderData <- traindata[(split+1):(split*2),]
testingData <- traindata[(split*2+1):nrow(traindata),]
rm(traindata)
dim(ensembleData)
# looking at distributions of data
table(ensembleData$tnd_cc_flag)
table(blenderData$tnd_cc_flag)
table(testingData$tnd_cc_flag)



#Shuffling the data
traindata <- traindata[sample(nrow(traindata)),]

# splitIndex<-createDataPartition(traindata$tnd_cc_flag,p=0.75,list=F,times=1)
#
# trainDF <- smarts[ splitIndex,]
# testDF  <- smarts[-splitIndex,]

## Splitting data into therr parts as data is huge.
split <- floor(nrow(traindata)/3)
ensembleData <- traindata[0:split,]
blenderData <- traindata[(split+1):(split*2),]
testingData <- traindata[(split*2+1):nrow(traindata),]

rm(traindata)
dim(ensembleData)

# fsModels<-c("glm","gbm","treebag","ridge","lasso","earth","rf","svmRadial") # these many moddels we are going to run on the feature selection
#
# myFS<-fscaret::fscaret(trainDF = ensembleData,testDF = testingData,
#               myTimeLimit = 40,
#               preprocessData = T,
#               Used.funcRegPred = fsModels,
#               with.labels = T,
#               supress.output = F,
#               no.cores = 4)
#
# names(myFS)
#
# myFS$VarImp
# myFS$PPlabels

# rm(trainDF,testDF)

# getting madel available
# names(getModelInfo())

# The ensembling apporach

# #again splitting the data
# splitIndex<-createDataPartition(traindata$tnd_cc_flag,p=0.75,list=F,times=1)
#
# ensembleData <- traindata[splitIndex,]
# testingData <- traindata[-splitIndex,]

# rm(traindata,ensembleData,ensembleDataEarth,testingData,testingDataEarth)

# looking at distributions of data
table(ensembleData$tnd_cc_flag)
table(blenderData$tnd_cc_flag)
table(testingData$tnd_cc_flag)

# Defining the variables to be seleted
# Considering topp 100 mrmr results
labelName <- 'tnd_cc_flag'
predictors <- setdiff(colnames(ensembleData),labelName)

# create a caret control object to control the number of cross-validations performed
myControl <- trainControl(method='cv', number=3, returnResamp='none')

# quick benchmark model
test_model <- train(blenderData[,predictors], blenderData[,labelName], method='gbm', trControl=myControl)
preds <- predict(object=test_model, testingData[,predictors])

auc <- roc(testingData[,labelName], preds)
print(auc$auc)
# Area under the curve: 7649

# converting data to factor (this loses the accuray)
# ensembleData[,labelName] <- as.factor(ensembleData[,labelName])
# train all the ensemble models with ensembleData
model_gbm <- train(ensembleData[,predictors], ensembleData[,labelName],
                   method='gbm', trControl=myControl)
model_rpart <- train(ensembleData[,predictors], ensembleData[,labelName],
                     method='rpart', trControl=myControl)
model_treebag <- train(ensembleData[,predictors], ensembleData[,labelName],
                       method='treebag', trControl=myControl)
model_ctree <- train(ensembleData[,predictors], ensembleData[,labelName],
                     method='ctree', trControl=myControl)
model_earth <- train(ensembleData[,predictors], ensembleData[,labelName],
                     method='earth', trControl=myControl)
# model_xgbLinear <- train(ensembleData[,predictors], ensembleData[,labelName],
#                       method='xgbLinear', trControl=myControl)
# model_xgbTree <- train(ensembleData[,predictors], ensembleData[,labelName],
#                        method='xgbTree', trControl=myControl)

# Making predictions
blenderData$gbm_PROB <- predict(object=model_gbm, blenderData[,predictors])
blenderData$rf_PROB <- predict(object=model_rpart, blenderData[,predictors])
blenderData$treebag_PROB <- predict(object=model_treebag, blenderData[,predictors])
blenderData$ctree_PROB <- predict(object=model_ctree, blenderData[,predictors])
blenderData$earth_PROB <- predict(object=model_earth, blenderData[,predictors])
# blenderData$xgblinear_PROB <- predict(object=model_xgbLinear, blenderData[,predictors])
# blenderData$xgbtree_PROB <- predict(object=model_xgbTree, blenderData[,predictors])

testingData$gbm_PROB <- predict(object=model_gbm, testingData[,predictors])
testingData$rf_PROB <- predict(object=model_rpart, testingData[,predictors])
testingData$treebag_PROB <- predict(object=model_treebag, testingData[,predictors])
testingData$ctree_PROB <- predict(object=model_ctree, testingData[,predictors])
testingData$earth_PROB <- predict(object=model_earth, testingData[,predictors])
# testingData$xgblinear_PROB <- predict(object=model_xgbLinear, testingData[,predictors])
# testingData$xgbtree_PROB <- predict(object=model_xgbTree, testingData[,predictors])

# see how each individual model performed on its own
# auc <- roc(testingData[,labelName], as.numeric(testingData$gbm_PROB))
# print(auc$auc)
# # Area under the curve:
#
# auc <- roc(testingData[,labelName], as.numeric(testingData$rf_PROB) )
# print(auc$auc)
# # Area under the curve:
#
# auc <- roc(testingData[,labelName], as.numeric(testingData$treebag_PROB ))
# print(auc$auc)
# # Area under the curve:
#
# auc <- roc(testingData[,labelName], as.numeric(testingData$ctree_PROB ))
# print(auc$auc)
# # Area under the curve:
#
# auc <- roc(testingData[,labelName], as.numeric(testingData$earth_PROB ))
# print(auc$auc)
# # Area under the curve:

confusionMatrix(ifelse(testingData$gbm_PROB>0.2,1,0),testingData[,labelName])
confusionMatrix(ifelse(testingData$rf_PROB >0.2,1,0),testingData[,labelName])
confusionMatrix(ifelse(testingData$treebag_PROB>0.18,1,0),testingData[,labelName])
confusionMatrix(ifelse(testingData$ctree_PROB>0.2,1,0),testingData[,labelName])
confusionMatrix(ifelse(testingData$earth_PROB>0.19,1,0),testingData[,labelName])

# auc <- roc(testingData[,labelName], as.numeric(testingData$xgblinear_PROB ))
# print(auc$auc)
# # Area under the curve:
#
# auc <- roc(testingData[,labelName], as.numeric(testingData$xgbtree_PROB ))
# print(auc$auc)
# # Area under the curve:



# run a final model to blend all the probabilities together
probabilities <- c("gbm_PROB","rf_PROB","treebag_PROB","ctree_PROB","earth_PROB")
                   # ,"xgblinear_PROB","xgbtree_PROB")
predictors1 <- c(predictors,probabilities)
final_blender_model1 <- train(blenderData[,predictors1], blenderData[,labelName],
                              method='gbm', trControl=myControl)

predictors2 <- probabilities
final_blender_model2 <- train(blenderData[,predictors2], blenderData[,labelName],
                              method='gbm', trControl=myControl)

# See final prediction and AUC of blended ensemble
preds <- predict(object=final_blender_model1, testingData[,predictors])
auc1 <- roc(testingData[,labelName], preds)
print(auc1$auc)
# Area under the curve:

preds1 <- predict(object=final_blender_model2, testingData[,predictors2])
auc2 <- roc(testingData[,labelName], preds)
print(auc2$auc)
# Area under the curve:

confusionMatrix(ifelse(preds>0.2,1,0),testingData[,labelName])
confusionMatrix(ifelse(preds1>0.2,1,0),testingData[,labelName])

# Testing on validation data
validate <- read.csv("phase-validation/6-days-validate.csv",
                     header = T,stringsAsFactors = F)
var_c <- c('neid','model','day_tslot','tnd_cc_flag')
valid_vars <- c(var_c,predictors)

load("/apps/data/models/validate2.RData")
validationData <- validate[,valid_vars]
rm(validate)
for (col_ in predictors){
  validationData[,col_] <- as.numeric(validationData[,col_])
}

str(validationData)


validate$new_flag <- ifelse(((validate$tnd_cc_flag==1)|(validate$smart_cc_flag==1)),1,0)


> validationData$earth_PROB <- predict(object=model_earth, validationData[,intersect(colnames(validationData),predictors_org)])

validationData$gbm_PROB <- predict(object=model_gbm, validationData[,intersect(colnames(validationData),predictors_org)])
validationData$rf_PROB <- predict(object=model_rpart, validationData[,intersect(colnames(validationData),predictors_org)])
validationData$treebag_PROB <- predict(object=model_treebag, validationData[,intersect(colnames(validationData),predictors_org)])
validationData$ctree_PROB <- predict(object=model_ctree, validationData[,intersect(colnames(validationData),predictors_org)])
validationData$earth_PROB <- predict(object=model_earth, validationData[,intersect(colnames(validationData),predictors_org)])


pred_validate1 <- predict(object=final_blender_model1, validationData[,predictors1])
pred_validate2 <- predict(object=final_blender_model2, validationData[,predictors2])

matrix_valid <- validationData[,var_c]
matrix_valid$gbm_PROB <- predict(object=model_gbm, validationData[,predictors])

table(ifelse(matrix_valid$gbm_PROB>0.18,1,0),matrix_valid[,labelName])
length(unique(matrix_valid[ifelse(matrix_valid$gbm_PROB>0.18,1,0)==1,"neid"]))

confusionMatrix(ifelse(gbm_PROB>0.18,1,0),validationData[,labelName])
confusionMatrix(ifelse(pred_validate1>0.18,1,0),validationData[,labelName])
confusionMatrix(ifelse(pred_validate2>0.17,1,0),validationData[,labelName])

validate_matrix <- validationData[,c('neid','day_tslot','tnd_cc_flag')]
# validate_matrix$pred1 <-pred_validate1
# validate_matrix$pred2 <-pred_validate2
validate_matrix$gbm_PROB <- validationData$gbm_PROB
validate_matrix$rf_PROB <- validationData$rf_PROB
validate_matrix$treebag_PROB <- validationData$treebag_PROB
validate_matrix$ctree_PROB <- validationData$ctree_PROB
validate_matrix$earth_PROB <- validationData$earth_PROB
write.csv(validate_matrix,file = "orig_46/Validation-results-orig_46.csv",row.names = F)


validate_matrix$pred1_20 <-ifelse(pred_validate1>0.20,1,0)
validate_matrix$pred2_20 <-ifelse(pred_validate2>0.20,1,0)
validate_matrix$pred1_18 <-ifelse(pred_validate1>0.18,1,0)
validate_matrix$pred2_18 <-ifelse(pred_validate2>0.18,1,0)
validate_matrix$pred1_17 <-ifelse(pred_validate1>0.17,1,0)
validate_matrix$pred2_17 <-ifelse(pred_validate2>0.17,1,0)
validate_matrix$pred1_15 <-ifelse(pred_validate1>0.15,1,0)
validate_matrix$pred2_15 <-ifelse(pred_validate2>0.15,1,0)



saveRDS(final_blender_model1,file = "orig_46/model_blender_46_orig.rds")
saveRDS(final_blender_model2,file = "orig_46/model_probabilities_46_orig.rds")
saveRDS(model_gbm,file = "orig_46/model_gbm_46_orig.rds")
saveRDS(model_rpart,file = "orig_46/model_rpart_46_orig.rds")
saveRDS(model_treebag,file = "orig_46/model_treebag_46_orig.rds")
saveRDS(model_ctree,file = "orig_46/model_ctree_46_orig.rds")
saveRDS(model_earth,file = "orig_46/model_earth_46_orig.rds")

rm(final_blender_model1,final_blender_model2,model_gbm,model_rpart,
   model_treebag,model_ctree,model_earth,test_model)
rm(preds,preds1,pred_validate2,pred_validate1,myControl,auc)

write.csv(ensembleData,file = "orig_46/ensembeldData.csv",row.names = F)
write.csv(blenderData,file = "orig_46/blenderData.csv",row.names = F)
write.csv(testingData,file = "orig_46/testingData.csv",row.names = F)
write.csv(validationData,file = "orig_46/validationData.csv",row.names = F)

rm(ensembleData,blenderData,testingData,validationData,validate_matrix)

validationData <- read.csv("mrmr_100/validationData.csv",
                           header = T,stringsAsFactors = F)

model_ctree <- readRDS("orig_46/model_ctree_46_orig.rds")
model_gbm <- readRDS("orig_46/model_gbm_46_orig.rds")
model_rpart <- readRDS("orig_46/model_rpart_46_orig.rds")
model_earth <- readRDS("orig_46/model_earth_46_orig.rds")
model_treebag <- readRDS("orig_46/model_treebag_46_orig.rds")
