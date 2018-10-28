### Smarts Prediction...

# Setting seed for Reproducebility
set.seed(1234)
set.seed(1610)
setwd("/apps/data/models/")	
.libPaths(c("/apps/R",.libPaths()))
load("/apps/data/smarts-data.RData")
results <- read.csv("/apps/data/results-all.csv",header = T,stringsAsFactors = F)

## Doing Ensembling with caret
library(caret)
library(caretEnsemble)
library(doMC)
registerDoMC(cores = 4)

#loading models
model_ctree <- readRDS("orig_46/model_ctree_46_orig.rds")
model_gbm <- readRDS("orig_46/model_gbm_46_orig.rds")
model_rpart <- readRDS("orig_46/model_rpart_46_orig.rds")
model_earth <- readRDS("orig_46/model_earth_46_orig.rds")
model_treebag <- readRDS("orig_46/model_treebag_46_orig.rds")

print("smarts data and saved models loaded")

# Original varaibles
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

var_c <- c('neid','model','day_tslot','tnd_cc_flag')
#Data preparation
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

# Defining target variable
labelName <- "tnd_cc_flag"

# Defining Predictors
predictors_org <- setdiff(orig_var,var_null)
print(predictors_org)

#Data splitting
print("Splitting data")
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

#Printing predictors
predictors <- setdiff(colnames(ensembleData),labelName)
print(predictors)
valid_vars <- c(var_c,predictors)

#Modeling
model_xgbLinear <- train(ensembleData[,predictors], ensembleData[,labelName],
                       method='xgbLinear', trControl=myControl)
model_xgbTree <- train(ensembleData[,predictors], ensembleData[,labelName],
                        method='xgbTree', trControl=myControl)

# Making predictions
print("Making probabilities for blenderData")
blenderData$gbm_PROB <- predict(object=model_gbm, blenderData[,predictors])
blenderData$rf_PROB <- predict(object=model_rpart, blenderData[,predictors])
blenderData$treebag_PROB <- predict(object=model_treebag, blenderData[,predictors])
blenderData$ctree_PROB <- predict(object=model_ctree, blenderData[,predictors])
blenderData$earth_PROB <- predict(object=model_earth, blenderData[,predictors])
blenderData$xgblinear_PROB <- predict(object=model_xgbLinear, blenderData[,predictors])
blenderData$xgbtree_PROB <- predict(object=model_xgbTree, blenderData[,predictors])

print("Making probabilities for testingData")

testingData$gbm_PROB <- predict(object=model_gbm, testingData[,predictors])
testingData$rf_PROB <- predict(object=model_rpart, testingData[,predictors])
testingData$treebag_PROB <- predict(object=model_treebag, testingData[,predictors])
testingData$ctree_PROB <- predict(object=model_ctree, testingData[,predictors])
testingData$earth_PROB <- predict(object=model_earth, testingData[,predictors])
testingData$xgblinear_PROB <- predict(object=model_xgbLinear, testingData[,predictors])
testingData$xgbtree_PROB <- predict(object=model_xgbTree, testingData[,predictors])

#Confusion Matrices
confusionMatrix(ifelse(testingData$gbm_PROB>0.2,1,0),testingData[,labelName])
confusionMatrix(ifelse(testingData$rf_PROB >0.2,1,0),testingData[,labelName])
confusionMatrix(ifelse(testingData$treebag_PROB>0.2,1,0),testingData[,labelName])
confusionMatrix(ifelse(testingData$ctree_PROB>0.2,1,0),testingData[,labelName])
confusionMatrix(ifelse(testingData$earth_PROB>0.2,1,0),testingData[,labelName])
confusionMatrix(ifelse(testingData$xgbLinear_PROB>0.2,1,0),testingData[,labelName])
confusionMatrix(ifelse(testingData$xgbTree_PROB>0.2,1,0),testingData[,labelName])

# run a final model to blend all the probabilities together
print("Making final ensemble model")

probabilities <- c("gbm_PROB","rf_PROB","treebag_PROB","ctree_PROB","earth_PROB",
                   "xgblinear_PROB","xgbtree_PROB")
predictors1 <- c(predictors,probabilities)
final_blender_model1 <- train(blenderData[,predictors1], blenderData[,labelName],
                              method='gbm', trControl=myControl)

predictors2 <- probabilities
final_blender_model2 <- train(blenderData[,predictors2], blenderData[,labelName],
                              method='gbm', trControl=myControl)

# See final prediction and AUC of blended ensemble
preds <- predict(object=final_blender_model1, testingData[,predictors])
preds1 <- predict(object=final_blender_model2, testingData[,predictors2])
confusionMatrix(ifelse(preds>0.2,1,0),testingData[,labelName])
confusionMatrix(ifelse(preds1>0.2,1,0),testingData[,labelName])

#saving models before validation
saveRDS(final_blender_model1,file = "orig_46/model_blender_46_orig.rds")
saveRDS(final_blender_model2,file = "orig_46/model_probabilities_46_orig.rds")
saveRDS(model_xgbTree,file = "orig_46/model_xgbTree_46_orig.rds")
saveRDS(model_xgbLinear,file = "orig_46/model_xgbLinear_46_orig.rds")

# validation process
var_c <- c('neid','model','day_tslot','tnd_cc_flag')
valid_vars <- c(var_c,predictors)

load("/apps/data/models/validate2.RData")
print(colnames(validate))
if ("tnd_cc_flag.1" %in% colnames(validate)){
	colnames(validate)[which(colnames(validate)=="tnd_cc_flag.1"] <- labelName
}
print(colnames(validate))

validationData <- validate[,valid_vars]
rm(validate)
for (col_ in predictors){
  validationData[,col_] <- as.numeric(validationData[,col_])
}
str(validationData)

validationData$gbm_PROB <- predict(object=model_gbm, validationData[,intersect(colnames(validationData),predictors)])
validationData$rf_PROB <- predict(object=model_rpart, validationData[,intersect(colnames(validationData),predictors)])
validationData$treebag_PROB <- predict(object=model_treebag, validationData[,intersect(colnames(validationData),predictors)])
validationData$ctree_PROB <- predict(object=model_ctree, validationData[,intersect(colnames(validationData),predictors)])
validationData$earth_PROB <- predict(object=model_earth, validationData[,intersect(colnames(validationData),predictors)])
validationData$xgbLinear_PROB <- predict(object=model_xgbLinear, validationData[,intersect(colnames(validationData),predictors)])
validationData$xgbTree_PROB <- predict(object=model_xgbTree, validationData[,intersect(colnames(validationData),predictors)])

#Model Behaviour
pred_validate1 <- predict(object=final_blender_model1, validationData[,predictors1])
pred_validate2 <- predict(object=final_blender_model2, validationData[,predictors2])
confusionMatrix(ifelse(pred_validate1>0.2,1,0),validationData[,labelName])
confusionMatrix(ifelse(pred_validate2>0.2,1,0),validationData[,labelName])

#Actual validation matrix
validate_matrix <- validationData[,c('neid','day_tslot','tnd_cc_flag')]
validate_matrix$pred_ensemble1 <-pred_validate1
validate_matrix$pred_ensemble2 <-pred_validate2
validate_matrix$gbm_PROB <- round(validationData$gbm_PROB,4)
validate_matrix$rf_PROB <- round(validationData$rf_PROB,4)
validate_matrix$treebag_PROB <- round(validationData$treebag_PROB,4)
validate_matrix$ctree_PROB <- round(validationData$ctree_PROB,4)
validate_matrix$earth_PROB <- round(validationData$earth_PROB,4)
validate_matrix$xgbTree_PROB <- round(validationData$xgbTree_PROB,4)
validate_matrix$xgbLinear_PROB <- round(validationData$xgbLinear_PROB,4)

# saving all outputs
write.csv(validate_matrix,file = "orig_46/Validation-results-orig_46-all.csv",row.names = F)

# printing Confusion Matrices
p_threshold <- c(.05,.1,.15,.2,.25,.3,.35,.4,.45,.5,.55,.6,.65,.7,.75,.8,.85,.9,.95,.99) 
for (i in p_threshold){
	cat("confusion matrix of xgbtree_PROB at cutoff : ",i)
	print(table(ifelse(validate_matrix$xgbTree_PROB>i,1,0),validate_matrix$tnd_cc_flag))
	cat("\n ")
}
for (i in p_threshold){
	cat("confusion matrix of xgbLinear_PROB at cutoff : ",i)
	print(table(ifelse(validate_matrix$xgbLinear_PROB>i,1,0),validate_matrix$tnd_cc_flag))
	cat("\n ")
}
for (i in p_threshold){
	cat("confusion matrix of ensemble1_PROB at cutoff : ",i)
	print(table(ifelse(validate_matrix$pred_ensemble1>i,1,0),validate_matrix$tnd_cc_flag))
	cat("\n ")
}
for (i in p_threshold){
	cat("confusion matrix of ensemble2_PROB at cutoff : ",i)
	print(table(ifelse(validate_matrix$pred_ensemble2>i,1,0),validate_matrix$tnd_cc_flag))
	cat("\n ")
}
