
library(dplyr)
library(effects)
library(InformationValue)
library(DMwR)
library(reshape2)
library(quantmod)
library(glmnet)



################ ####################Lag1 LDA class data############################

EDW_LDA_Class <- read.csv("C:\\Shiva\\Projects\\GS\\GS_fault\\EDW\\LDA_Class_out.csv",stringsAsFactors = F)
dim(EDW_LDA_Class)
str(EDW_LDA_Class)

#converting date format##
EDW_LDA_Class$date <- as.Date(EDW_LDA_Class$date, "%d/%m/%Y")

##Filling missing dates##
g <- data.frame(date=seq(min(EDW_LDA_Class$date),max(EDW_LDA_Class$date),1))
EDW_LDA_Class1 <- merge(g,EDW_LDA_Class,by="date",all.x=TRUE)


## Creating LDA class table##

table <- data.frame(table(EDW_LDA_Class1$date,EDW_LDA_Class1$class))
table1 <- reshape(table,idvar = "Var1",timevar = "Var2",direction = "wide")
colnames(table1)=c("Date",as.character(unique(table$Var2)))
table1[is.na(table1)] <- 0
table2 <- table1


col=2:dim(table1)[2]
j=20
for(i in col)
{
  print(i)
  table3 <- as.data.frame(table1 %>% 
                            mutate(lag_1=as.numeric(Lag(table1[,i],1)),
                                   lag_2=as.numeric(Lag(table1[,i],2)),
                                   lag_3=as.numeric(Lag(table1[,i],3)),
                                   lag_4=as.numeric(Lag(table1[,i],4)),
                                   lag_5=as.numeric(Lag(table1[,i],5))))
  
  colnames(table3)=c(colnames(table1),paste(colnames(table1)[i],"_lag",1:5,sep=""))
  vec=j+4
  table1=cbind(table1,table3[,j:vec])
  rm(table3)
  j=j+5
}

table1$Date <- NULL

################################################Lag2 LDA class data#################################################

col=2:dim(table2)[2]
j=20
for(i in col)
{
  print(i)
  table3 <- as.data.frame(table2 %>% 
                            mutate(lag_2=as.numeric(Lag(table2[,i],2)),
                                   lag_3=as.numeric(Lag(table2[,i],3)),
                                   lag_4=as.numeric(Lag(table2[,i],4)),
                                   lag_5=as.numeric(Lag(table2[,i],5)),
                                   lag_6=as.numeric(Lag(table2[,i],6))))
  
  colnames(table3)=c(colnames(table2),paste(colnames(table2)[i],"_lag",2:6,sep=""))
  vec=j+4
  table2=cbind(table2,table3[,j:vec])
  rm(table3)
  j=j+5
}

table2$Date <- NULL

##################################################################################################################################################

EDW_fault <- read.csv("C:\\Shiva\\Projects\\GS\\GS_fault\\EDW\\EDW_fault_majorcase.csv",stringsAsFactors = F)
dim(EDW_fault)
str(EDW_fault)


#converting date format##
EDW_fault$date <- as.Date(EDW_fault$date, "%d/%m/%Y")

##Filling missing dates##
EDW_fault1 <- merge(g,EDW_fault,by="date",all.x=TRUE)


## Creating LDA class table##
library(reshape2)
fault_table <- data.frame(table(EDW_fault1$date,EDW_fault1$Major.Case.Type))
table3 <- reshape(fault_table,idvar = "Var1",timevar = "Var2",direction = "wide")
colnames(table3)=c("Date",as.character(unique(fault_table$Var2)))
table3[is.na(table3)] <- 0



#######################################################################################################################################

##Lag1 data set##

lag1_data <- cbind(table3,table1)
lag1_data<- tail(lag1_data,-5)

## Lag2 data set ##

lag2_data <- cbind(table3,table2)
lag2_data<- tail(lag2_data,-6)



##--------------------------------------------------ORA-600--------------------------------------------------------------##

##Lag1##

names <- c("Date","ORA-600",	
           "ARCHIVE LOG_lag1",	"ARCHIVE LOG_lag3",	"ARCHIVE LOG_lag5",	"Archived_thread_error_lag2",	"Archived_thread_error_lag4",	"ASMB_lag1",	"ASMB_lag3",	"ASMB_lag5",	"Auto Tuning_lag2",	"Auto Tuning_lag4",	"Dbora Files_lag1",	"Dbora Files_lag3",	"Dbora Files_lag5",	"Diagnostic error_lag2",			
           "ARCHIVE LOG_lag2",	"ARCHIVE LOG_lag4",	"Archived_thread_error_lag1",	"Archived_thread_error_lag3",	"Archived_thread_error_lag5",	"ASMB_lag2",	"ASMB_lag4",	"Auto Tuning_lag1",	"Auto Tuning_lag3",	"Auto Tuning_lag5",	"Dbora Files_lag2",	"Dbora Files_lag4",	"Diagnostic error_lag1",	"Diagnostic error_lag3",			
           "Diagnostic error_lag4",	"Diagonal Adl error_lag1",	"Diagonal Adl error_lag3",	"Diagonal Adl error_lag5",	"Fatal Ni connections_lag2",	"Fatal Ni connections_lag4",	"GCS remote-cache_lag1",	"GCS remote-cache_lag3",	"GCS remote-cache_lag5",	"Inbound Error_lag2",	"Inbound Error_lag4",	"Killing Session note_lag1",	"Killing Session note_lag3",	"Killing Session note_lag5",	"KSFV_lag2",			
           "Diagnostic error_lag5",	"Diagonal Adl error_lag2",	"Diagonal Adl error_lag4",	"Fatal Ni connections_lag1",	"Fatal Ni connections_lag3",	"Fatal Ni connections_lag5",	"GCS remote-cache_lag2",	"GCS remote-cache_lag4",	"Inbound Error_lag1",	"Inbound Error_lag3",	"Inbound Error_lag5",	"Killing Session note_lag2",	"Killing Session note_lag4",	"KSFV_lag1",	"KSFV_lag3",			
           "KSFV_lag4",	"LGWR switch_lag1",	"LGWR switch_lag3",	"LGWR switch_lag5",	"NFS Error_lag2",	"NFS Error_lag4",	"Others_lag1",	"Others_lag3",	"Others_lag5",	"resize operation_lag2",	"resize operation_lag4",	"Roles Exceeded_lag1",	"Roles Exceeded_lag3",	"Roles Exceeded_lag5",	"Tns Error_lag2",	"Tns Error_lag4",		
           "KSFV_lag5",	"LGWR switch_lag2",	"LGWR switch_lag4",	"NFS Error_lag1",	"NFS Error_lag3",	"NFS Error_lag5",	"Others_lag2",	"Others_lag4",	"resize operation_lag1",	"resize operation_lag3",	"resize operation_lag5",	"Roles Exceeded_lag2",	"Roles Exceeded_lag4",	"Tns Error_lag1",	"Tns Error_lag3",	"Tns Error_lag5")

fault_Day1 <- lag1_data[names]


dim(fault_Day1)

str(fault_Day1)

summary(fault_Day1)

names(fault_Day1)

fault_Day1$Date <- NULL
fault_Day1$`ORA-600` <- replace(fault_Day1$`ORA-600`,fault_Day1$`ORA-600`>1,1)

fault_Day1$`ORA-600` <- as.factor(fault_Day1$`ORA-600`)

library(DMwR)
set.seed(122)
smoted_day1 <- SMOTE(`ORA-600`~., fault_Day1, perc.over=6000, perc.under=100, k=10, learner=NULL)
table(smoted_day1$`ORA-600`)

#x <- as.matrix(smoted_day1[,-1]) # Removes class
#y <- as.double(as.matrix(smoted_day2[, 1])) # Only class
#set.seed(999)
#cv.ridge <- cv.glmnet(x, y,  family='binomial', alpha=0, parallel=TRUE, standardize=TRUE, type.measure='auc')


model1 <- glm(`ORA-600` ~ .,family=binomial,data=smoted_day1)

summary(model1)

#slm2 <- step(model13)

#summary(slm2)

#model13 <- 


#summary(model13)


#anova(model1, test="Chisq")

#plot(allEffects(model1))
#plot(model1)

predicted1 <- predict(model1, fault_Day1, type="response")  # predicted scores

predicted_file1 <- data.frame(cbind(fault_Day1,predicted1))

predicted_file1 <- data.frame(cbind(lag1_data$Date,predicted_file1))

write.csv(predicted_file1,"C:\\Shiva\\Projects\\GS\\GS_fault\\EDW\\EDW_fault_prediction\\predicted_ORA-600_1.csv")


library(InformationValue)
optCutOff <- optimalCutoff(fault_Day1$`ORA-600`, predicted1)[1]
#optCutOff <- 0.4


misClassError(fault_Day1$`ORA-600`, predicted1, threshold = optCutOff)

plotROC(fault_Day1$`ORA-600`, predicted1)

Concordance(fault_Day1$`ORA-600`, predicted1)

sensitivity(fault_Day1$`ORA-600`, predicted1, threshold = optCutOff)

specificity(fault_Day1$`ORA-600`, predicted1, threshold = optCutOff)

confusionMatrix(fault_Day1$`ORA-600`, predicted1, threshold = optCutOff)



##################################################################################################################################################################
##Lag2####
names <- c("Date","ORA-600","ARCHIVE LOG_lag2",	"ARCHIVE LOG_lag4",	"ARCHIVE LOG_lag6",	"Archived_thread_error_lag3",	"Archived_thread_error_lag5",	"ASMB_lag2",	"ASMB_lag4",	"ASMB_lag6",	"Auto Tuning_lag3",	"Auto Tuning_lag5",	"Dbora Files_lag2",	"Dbora Files_lag4",	"Dbora Files_lag6",	"Diagnostic error_lag3",			
           "ARCHIVE LOG_lag3",	"ARCHIVE LOG_lag5",	"Archived_thread_error_lag2",	"Archived_thread_error_lag4",	"Archived_thread_error_lag6",	"ASMB_lag3",	"ASMB_lag5",	"Auto Tuning_lag2",	"Auto Tuning_lag4",	"Auto Tuning_lag6",	"Dbora Files_lag3",	"Dbora Files_lag5",	"Diagnostic error_lag2",	"Diagnostic error_lag4",			
           "Diagnostic error_lag5",	"Diagonal Adl error_lag2",	"Diagonal Adl error_lag4",	"Diagonal Adl error_lag6",	"Fatal Ni connections_lag3",	"Fatal Ni connections_lag5",	"GCS remote-cache_lag2",	"GCS remote-cache_lag4",	"GCS remote-cache_lag6",	"Inbound Error_lag3",	"Inbound Error_lag5",	"Killing Session note_lag2",	"Killing Session note_lag4",	"Killing Session note_lag6",	"KSFV_lag3",			
           "Diagnostic error_lag6",	"Diagonal Adl error_lag3",	"Diagonal Adl error_lag5",	"Fatal Ni connections_lag2",	"Fatal Ni connections_lag4",	"Fatal Ni connections_lag6",	"GCS remote-cache_lag3",	"GCS remote-cache_lag5",	"Inbound Error_lag2",	"Inbound Error_lag4",	"Inbound Error_lag6",	"Killing Session note_lag3",	"Killing Session note_lag5",	"KSFV_lag2",	"KSFV_lag4",			
           "KSFV_lag5",	"LGWR switch_lag2",	"LGWR switch_lag4",	"LGWR switch_lag6",	"NFS Error_lag3",	"NFS Error_lag5",	"Others_lag2",	"Others_lag4",	"Others_lag6",	"resize operation_lag3",	"resize operation_lag5",	"Roles Exceeded_lag2",	"Roles Exceeded_lag4",	"Roles Exceeded_lag6",	"Tns Error_lag3",	"Tns Error_lag5",		
           "KSFV_lag6",	"LGWR switch_lag3",	"LGWR switch_lag5",	"NFS Error_lag2",	"NFS Error_lag4",	"NFS Error_lag6",	"Others_lag3",	"Others_lag5",	"resize operation_lag2",	"resize operation_lag4",	"resize operation_lag6",	"Roles Exceeded_lag3",	"Roles Exceeded_lag5",	"Tns Error_lag2",	"Tns Error_lag4",	"Tns Error_lag6")



fault_Day2 <- lag2_data[names]


dim(fault_Day2)

str(fault_Day2)

summary(fault_Day2)

names(fault_Day2)

fault_Day2$`ORA-600` <- replace(fault_Day2$`ORA-600`,fault_Day2$`ORA-600`>1,1)

fault_Day2$`ORA-600` <- as.factor(fault_Day2$`ORA-600`)
fault_Day2$Date <- NULL

library(DMwR)
set.seed(123)
smoted_day2 <- SMOTE(`ORA-600`~., fault_Day2, perc.over=6000, perc.under=100, k=10, learner=NULL)
table(smoted_day2$`ORA-600`)

model2 <- glm(`ORA-600` ~ .,family=binomial,data=smoted_day2)
summary(model2)

#slm2 <- step(model2)
#summary(slm2)
#model13 <- glm
#summary(model2)
#anova(model2, test="Chisq")

#plot(allEffects(model2))
#plot(model2)

predicted2 <- predict(model2, fault_Day2, type="response")  # predicted scores

predicted_file2 <- data.frame(cbind(fault_Day2,predicted2))

write.csv(predicted_file2,"C:\\Shiva\\Projects\\GS\\GS_fault\\EDW\\EDW_fault_prediction\\predicted_ORA-600_2.csv")

library(InformationValue)

optCutOff <- optimalCutoff(fault_Day2$`ORA-600`, predicted2)[1]
#optCutOff <- 0.4

misClassError(fault_Day2$`ORA-600`, predicted2, threshold = optCutOff)

plotROC(fault_Day2$`ORA-600`, predicted2)

Concordance(fault_Day2$`ORA-600`, predicted2)

sensitivity(fault_Day2$`ORA-600`, predicted2, threshold = optCutOff)

specificity(fault_Day2$`ORA-600`, predicted2, threshold = optCutOff)

confusionMatrix(fault_Day2$`ORA-600`, predicted2, threshold = optCutOff)



#######################################################################################################################################

##--------------------------------------------------Process--------------------------------------------------------------##

##Lag1##

names <- c("Date","Process",	
           "ARCHIVE LOG_lag1",	"ARCHIVE LOG_lag3",	"ARCHIVE LOG_lag5",	"Archived_thread_error_lag2",	"Archived_thread_error_lag4",	"ASMB_lag1",	"ASMB_lag3",	"ASMB_lag5",	"Auto Tuning_lag2",	"Auto Tuning_lag4",	"Dbora Files_lag1",	"Dbora Files_lag3",	"Dbora Files_lag5",	"Diagnostic error_lag2",			
           "ARCHIVE LOG_lag2",	"ARCHIVE LOG_lag4",	"Archived_thread_error_lag1",	"Archived_thread_error_lag3",	"Archived_thread_error_lag5",	"ASMB_lag2",	"ASMB_lag4",	"Auto Tuning_lag1",	"Auto Tuning_lag3",	"Auto Tuning_lag5",	"Dbora Files_lag2",	"Dbora Files_lag4",	"Diagnostic error_lag1",	"Diagnostic error_lag3",			
           "Diagnostic error_lag4",	"Diagonal Adl error_lag1",	"Diagonal Adl error_lag3",	"Diagonal Adl error_lag5",	"Fatal Ni connections_lag2",	"Fatal Ni connections_lag4",	"GCS remote-cache_lag1",	"GCS remote-cache_lag3",	"GCS remote-cache_lag5",	"Inbound Error_lag2",	"Inbound Error_lag4",	"Killing Session note_lag1",	"Killing Session note_lag3",	"Killing Session note_lag5",	"KSFV_lag2",			
           "Diagnostic error_lag5",	"Diagonal Adl error_lag2",	"Diagonal Adl error_lag4",	"Fatal Ni connections_lag1",	"Fatal Ni connections_lag3",	"Fatal Ni connections_lag5",	"GCS remote-cache_lag2",	"GCS remote-cache_lag4",	"Inbound Error_lag1",	"Inbound Error_lag3",	"Inbound Error_lag5",	"Killing Session note_lag2",	"Killing Session note_lag4",	"KSFV_lag1",	"KSFV_lag3",			
           "KSFV_lag4",	"LGWR switch_lag1",	"LGWR switch_lag3",	"LGWR switch_lag5",	"NFS Error_lag2",	"NFS Error_lag4",	"Others_lag1",	"Others_lag3",	"Others_lag5",	"resize operation_lag2",	"resize operation_lag4",	"Roles Exceeded_lag1",	"Roles Exceeded_lag3",	"Roles Exceeded_lag5",	"Tns Error_lag2",	"Tns Error_lag4",		
           "KSFV_lag5",	"LGWR switch_lag2",	"LGWR switch_lag4",	"NFS Error_lag1",	"NFS Error_lag3",	"NFS Error_lag5",	"Others_lag2",	"Others_lag4",	"resize operation_lag1",	"resize operation_lag3",	"resize operation_lag5",	"Roles Exceeded_lag2",	"Roles Exceeded_lag4",	"Tns Error_lag1",	"Tns Error_lag3",	"Tns Error_lag5")

fault_Day1 <- lag1_data[names]


dim(fault_Day1)

str(fault_Day1)

summary(fault_Day1)

names(fault_Day1)

fault_Day1$Process <- replace(fault_Day1$Process,fault_Day1$Process>1,1)

fault_Day1$Process <- as.factor(fault_Day1$Process)
fault_Day1$Date <- NULL

library(DMwR)
set.seed(122)
smoted_day1 <- SMOTE(Process~., fault_Day1, perc.over=6000, perc.under=100, k=10, learner=NULL)
table(smoted_day1$Process)



model1 <- glm(Process ~ .,family=binomial,data=smoted_day1)
summary(model1)

#slm2 <- step(model13)
#summary(slm2)
#model13 <- 
#summary(model13)
#anova(model1, test="Chisq")
#plot(allEffects(model1))
#plot(model1)

predicted1 <- predict(model1, fault_Day1, type="response")  # predicted scores

predicted_file1 <- data.frame(cbind(fault_Day1,predicted1))

predicted_file1 <- data.frame(cbind(lag1_data$Date,predicted_file1))

write.csv(predicted_file1,"C:\\Shiva\\Projects\\GS\\GS_fault\\EDW\\EDW_fault_prediction\\predicted_Process_1.csv")

library(InformationValue)

optCutOff <- optimalCutoff(fault_Day1$Process, predicted1)[1]
#optCutOff <- 0.4


misClassError(fault_Day1$Process, predicted1, threshold = optCutOff)

plotROC(fault_Day1$Process, predicted1)

Concordance(fault_Day1$Process, predicted1)

sensitivity(fault_Day1$Process, predicted1, threshold = optCutOff)

specificity(fault_Day1$Process, predicted1, threshold = optCutOff)

confusionMatrix(fault_Day1$Process, predicted1, threshold = optCutOff)



##################################################################################################################################

names <- c("Date","Process","ARCHIVE LOG_lag2",	"ARCHIVE LOG_lag4",	"ARCHIVE LOG_lag6",	"Archived_thread_error_lag3",	"Archived_thread_error_lag5",	"ASMB_lag2",	"ASMB_lag4",	"ASMB_lag6",	"Auto Tuning_lag3",	"Auto Tuning_lag5",	"Dbora Files_lag2",	"Dbora Files_lag4",	"Dbora Files_lag6",	"Diagnostic error_lag3",			
           "ARCHIVE LOG_lag3",	"ARCHIVE LOG_lag5",	"Archived_thread_error_lag2",	"Archived_thread_error_lag4",	"Archived_thread_error_lag6",	"ASMB_lag3",	"ASMB_lag5",	"Auto Tuning_lag2",	"Auto Tuning_lag4",	"Auto Tuning_lag6",	"Dbora Files_lag3",	"Dbora Files_lag5",	"Diagnostic error_lag2",	"Diagnostic error_lag4",			
           "Diagnostic error_lag5",	"Diagonal Adl error_lag2",	"Diagonal Adl error_lag4",	"Diagonal Adl error_lag6",	"Fatal Ni connections_lag3",	"Fatal Ni connections_lag5",	"GCS remote-cache_lag2",	"GCS remote-cache_lag4",	"GCS remote-cache_lag6",	"Inbound Error_lag3",	"Inbound Error_lag5",	"Killing Session note_lag2",	"Killing Session note_lag4",	"Killing Session note_lag6",	"KSFV_lag3",			
           "Diagnostic error_lag6",	"Diagonal Adl error_lag3",	"Diagonal Adl error_lag5",	"Fatal Ni connections_lag2",	"Fatal Ni connections_lag4",	"Fatal Ni connections_lag6",	"GCS remote-cache_lag3",	"GCS remote-cache_lag5",	"Inbound Error_lag2",	"Inbound Error_lag4",	"Inbound Error_lag6",	"Killing Session note_lag3",	"Killing Session note_lag5",	"KSFV_lag2",	"KSFV_lag4",			
           "KSFV_lag5",	"LGWR switch_lag2",	"LGWR switch_lag4",	"LGWR switch_lag6",	"NFS Error_lag3",	"NFS Error_lag5",	"Others_lag2",	"Others_lag4",	"Others_lag6",	"resize operation_lag3",	"resize operation_lag5",	"Roles Exceeded_lag2",	"Roles Exceeded_lag4",	"Roles Exceeded_lag6",	"Tns Error_lag3",	"Tns Error_lag5",		
           "KSFV_lag6",	"LGWR switch_lag3",	"LGWR switch_lag5",	"NFS Error_lag2",	"NFS Error_lag4",	"NFS Error_lag6",	"Others_lag3",	"Others_lag5",	"resize operation_lag2",	"resize operation_lag4",	"resize operation_lag6",	"Roles Exceeded_lag3",	"Roles Exceeded_lag5",	"Tns Error_lag2",	"Tns Error_lag4",	"Tns Error_lag6")

fault_Day2 <- lag2_data[names]


dim(fault_Day2)

str(fault_Day2)

summary(fault_Day2)

names(fault_Day2)

fault_Day2$Process <- replace(fault_Day2$Process,fault_Day2$Process>1,1)
fault_Day2$Process <- as.factor(fault_Day2$Process)
fault_Day2$Date <- NULL

library(DMwR)
set.seed(123)
smoted_day2 <- SMOTE(Process~., fault_Day2, perc.over=6000, perc.under=100, k=10, learner=NULL)
table(smoted_day2$Process)



model2 <- glm(Process ~ .,family=binomial,data=smoted_day2)
summary(model2)

#slm2 <- step(model2)
#summary(slm2)
#model13 <- glm
#summary(model2)
#anova(model2, test="Chisq")
#plot(allEffects(model2))
#plot(model2)

predicted2 <- predict(model2, fault_Day2, type="response")  # predicted scores

predicted_file2 <- data.frame(cbind(fault_Day2,predicted2))

predicted_file2 <- data.frame(lag2_data$Date,predicted_file2)

write.csv(predicted_file2,"C:\\Shiva\\Projects\\GS\\GS_fault\\EDW\\EDW_fault_prediction\\predicted_Process_2.csv")

library(InformationValue)

optCutOff <- optimalCutoff(fault_Day2$Process, predicted2)[1]
optCutOff <- 0.4


misClassError(fault_Day2$Process, predicted2, threshold = optCutOff)

plotROC(fault_Day2$Process, predicted2)

Concordance(fault_Day2$Process, predicted2)

sensitivity(fault_Day2$Process, predicted2, threshold = optCutOff)

specificity(fault_Day2$Process, predicted2, threshold = optCutOff)

confusionMatrix(fault_Day2$Process, predicted2, threshold = optCutOff)



#######################################################################################################################

##--------------------------------------------------ASM--------------------------------------------------------------##

##Lag1##

names <- c("Date","ASM",	
           "ARCHIVE LOG_lag1",	"ARCHIVE LOG_lag3",	"ARCHIVE LOG_lag5",	"Archived_thread_error_lag2",	"Archived_thread_error_lag4",	"ASMB_lag1",	"ASMB_lag3",	"ASMB_lag5",	"Auto Tuning_lag2",	"Auto Tuning_lag4",	"Dbora Files_lag1",	"Dbora Files_lag3",	"Dbora Files_lag5",	"Diagnostic error_lag2",			
           "ARCHIVE LOG_lag2",	"ARCHIVE LOG_lag4",	"Archived_thread_error_lag1",	"Archived_thread_error_lag3",	"Archived_thread_error_lag5",	"ASMB_lag2",	"ASMB_lag4",	"Auto Tuning_lag1",	"Auto Tuning_lag3",	"Auto Tuning_lag5",	"Dbora Files_lag2",	"Dbora Files_lag4",	"Diagnostic error_lag1",	"Diagnostic error_lag3",			
           "Diagnostic error_lag4",	"Diagonal Adl error_lag1",	"Diagonal Adl error_lag3",	"Diagonal Adl error_lag5",	"Fatal Ni connections_lag2",	"Fatal Ni connections_lag4",	"GCS remote-cache_lag1",	"GCS remote-cache_lag3",	"GCS remote-cache_lag5",	"Inbound Error_lag2",	"Inbound Error_lag4",	"Killing Session note_lag1",	"Killing Session note_lag3",	"Killing Session note_lag5",	"KSFV_lag2",			
           "Diagnostic error_lag5",	"Diagonal Adl error_lag2",	"Diagonal Adl error_lag4",	"Fatal Ni connections_lag1",	"Fatal Ni connections_lag3",	"Fatal Ni connections_lag5",	"GCS remote-cache_lag2",	"GCS remote-cache_lag4",	"Inbound Error_lag1",	"Inbound Error_lag3",	"Inbound Error_lag5",	"Killing Session note_lag2",	"Killing Session note_lag4",	"KSFV_lag1",	"KSFV_lag3",			
           "KSFV_lag4",	"LGWR switch_lag1",	"LGWR switch_lag3",	"LGWR switch_lag5",	"NFS Error_lag2",	"NFS Error_lag4",	"Others_lag1",	"Others_lag3",	"Others_lag5",	"resize operation_lag2",	"resize operation_lag4",	"Roles Exceeded_lag1",	"Roles Exceeded_lag3",	"Roles Exceeded_lag5",	"Tns Error_lag2",	"Tns Error_lag4",		
           "KSFV_lag5",	"LGWR switch_lag2",	"LGWR switch_lag4",	"NFS Error_lag1",	"NFS Error_lag3",	"NFS Error_lag5",	"Others_lag2",	"Others_lag4",	"resize operation_lag1",	"resize operation_lag3",	"resize operation_lag5",	"Roles Exceeded_lag2",	"Roles Exceeded_lag4",	"Tns Error_lag1",	"Tns Error_lag3",	"Tns Error_lag5")

fault_Day1 <- lag1_data[names]


dim(fault_Day1)

str(fault_Day1)

summary(fault_Day1)

names(fault_Day1)

fault_Day1$ASM <- replace(fault_Day1$ASM,fault_Day1$ASM>1,1)
fault_Day1$ASM <- as.factor(fault_Day1$ASM)
fault_Day1$Date <- NULL

library(DMwR)
set.seed(122)
smoted_day1 <- SMOTE(ASM~., fault_Day1, perc.over=6000, perc.under=100, k=10, learner=NULL)
table(smoted_day1$ASM)



model1 <- glm(ASM ~ .,family=binomial,data=smoted_day1)
summary(model1)

#slm2 <- step(model13)
#summary(slm2)
#model13 <- 
#summary(model13)
#anova(model1, test="Chisq")
#plot(allEffects(model1))
#plot(model1)

predicted1 <- predict(model1, fault_Day1, type="response")  # predicted scores

predicted_file1 <- data.frame(cbind(fault_Day1,predicted1))

predicted_file1 <- data.frame(lag1_data$Date,predicted_file1)

write.csv(predicted_file1,"C:\\Shiva\\Projects\\GS\\GS_fault\\EDW\\EDW_fault_prediction\\predicted_ASM_1.csv")

library(InformationValue)

optCutOff <- optimalCutoff(fault_Day1$ASM, predicted1)[1]
#optCutOff <- 0.4


misClassError(fault_Day1$ASM, predicted1, threshold = optCutOff)

plotROC(fault_Day1$ASM, predicted1)

Concordance(fault_Day1$ASM, predicted1)

sensitivity(fault_Day1$ASM, predicted1, threshold = optCutOff)

specificity(fault_Day1$ASM, predicted1, threshold = optCutOff)

confusionMatrix(fault_Day1$ASM, predicted1, threshold = optCutOff)


##################################################################################################################################

names <- c("Date","ASM","ARCHIVE LOG_lag2",	"ARCHIVE LOG_lag4",	"ARCHIVE LOG_lag6",	"Archived_thread_error_lag3",	"Archived_thread_error_lag5",	"ASMB_lag2",	"ASMB_lag4",	"ASMB_lag6",	"Auto Tuning_lag3",	"Auto Tuning_lag5",	"Dbora Files_lag2",	"Dbora Files_lag4",	"Dbora Files_lag6",	"Diagnostic error_lag3",			
           "ARCHIVE LOG_lag3",	"ARCHIVE LOG_lag5",	"Archived_thread_error_lag2",	"Archived_thread_error_lag4",	"Archived_thread_error_lag6",	"ASMB_lag3",	"ASMB_lag5",	"Auto Tuning_lag2",	"Auto Tuning_lag4",	"Auto Tuning_lag6",	"Dbora Files_lag3",	"Dbora Files_lag5",	"Diagnostic error_lag2",	"Diagnostic error_lag4",			
           "Diagnostic error_lag5",	"Diagonal Adl error_lag2",	"Diagonal Adl error_lag4",	"Diagonal Adl error_lag6",	"Fatal Ni connections_lag3",	"Fatal Ni connections_lag5",	"GCS remote-cache_lag2",	"GCS remote-cache_lag4",	"GCS remote-cache_lag6",	"Inbound Error_lag3",	"Inbound Error_lag5",	"Killing Session note_lag2",	"Killing Session note_lag4",	"Killing Session note_lag6",	"KSFV_lag3",			
           "Diagnostic error_lag6",	"Diagonal Adl error_lag3",	"Diagonal Adl error_lag5",	"Fatal Ni connections_lag2",	"Fatal Ni connections_lag4",	"Fatal Ni connections_lag6",	"GCS remote-cache_lag3",	"GCS remote-cache_lag5",	"Inbound Error_lag2",	"Inbound Error_lag4",	"Inbound Error_lag6",	"Killing Session note_lag3",	"Killing Session note_lag5",	"KSFV_lag2",	"KSFV_lag4",			
           "KSFV_lag5",	"LGWR switch_lag2",	"LGWR switch_lag4",	"LGWR switch_lag6",	"NFS Error_lag3",	"NFS Error_lag5",	"Others_lag2",	"Others_lag4",	"Others_lag6",	"resize operation_lag3",	"resize operation_lag5",	"Roles Exceeded_lag2",	"Roles Exceeded_lag4",	"Roles Exceeded_lag6",	"Tns Error_lag3",	"Tns Error_lag5",		
           "KSFV_lag6",	"LGWR switch_lag3",	"LGWR switch_lag5",	"NFS Error_lag2",	"NFS Error_lag4",	"NFS Error_lag6",	"Others_lag3",	"Others_lag5",	"resize operation_lag2",	"resize operation_lag4",	"resize operation_lag6",	"Roles Exceeded_lag3",	"Roles Exceeded_lag5",	"Tns Error_lag2",	"Tns Error_lag4",	"Tns Error_lag6")

fault_Day2 <- lag2_data[names]


dim(fault_Day2)

str(fault_Day2)

summary(fault_Day2)

names(fault_Day2)

fault_Day2$ASM <- replace(fault_Day2$ASM,fault_Day2$ASM>1,1)
fault_Day2$ASM <- as.factor(fault_Day2$ASM)
fault_Day2$Date <- NULL

library(DMwR)
set.seed(123)
smoted_day2 <- SMOTE(ASM~., fault_Day2, perc.over=6000, perc.under=100, k=10, learner=NULL)
table(smoted_day2$ASM)



model2 <- glm(ASM ~ .,family=binomial,data=smoted_day2)
summary(model2)

#slm2 <- step(model2)
#summary(slm2)
#model13 <- glm
#summary(model2)
#anova(model2, test="Chisq")
#plot(allEffects(model2))
#plot(model2)

predicted2 <- predict(model2, fault_Day2, type="response")  # predicted scores

predicted_file2 <- data.frame(cbind(fault_Day2,predicted2))

predicted_file2 <- data.frame(cbind(lag2_data$Date,predicted_file2))

write.csv(predicted_file2,"C:\\Shiva\\Projects\\GS\\GS_fault\\EDW\\EDW_fault_prediction\\predicted_ASM_2.csv")

library(InformationValue)

optCutOff <- optimalCutoff(fault_Day2$ASM, predicted2)[1]
#optCutOff <- 0.4


misClassError(fault_Day2$ASM, predicted2, threshold = optCutOff)

plotROC(fault_Day2$ASM, predicted2)

Concordance(fault_Day2$ASM, predicted2)

sensitivity(fault_Day2$ASM, predicted2, threshold = optCutOff)

specificity(fault_Day2$ASM, predicted2, threshold = optCutOff)

confusionMatrix(fault_Day2$ASM, predicted2, threshold = optCutOff)


#######################################################################################################################

##--------------------------------------------------Dest_Breach--------------------------------------------------------------##

##Lag1##

names <- c("Date","Dest_Breach",	
           "ARCHIVE LOG_lag1",	"ARCHIVE LOG_lag3",	"ARCHIVE LOG_lag5",	"Archived_thread_error_lag2",	"Archived_thread_error_lag4",	"ASMB_lag1",	"ASMB_lag3",	"ASMB_lag5",	"Auto Tuning_lag2",	"Auto Tuning_lag4",	"Dbora Files_lag1",	"Dbora Files_lag3",	"Dbora Files_lag5",	"Diagnostic error_lag2",			
           "ARCHIVE LOG_lag2",	"ARCHIVE LOG_lag4",	"Archived_thread_error_lag1",	"Archived_thread_error_lag3",	"Archived_thread_error_lag5",	"ASMB_lag2",	"ASMB_lag4",	"Auto Tuning_lag1",	"Auto Tuning_lag3",	"Auto Tuning_lag5",	"Dbora Files_lag2",	"Dbora Files_lag4",	"Diagnostic error_lag1",	"Diagnostic error_lag3",			
           "Diagnostic error_lag4",	"Diagonal Adl error_lag1",	"Diagonal Adl error_lag3",	"Diagonal Adl error_lag5",	"Fatal Ni connections_lag2",	"Fatal Ni connections_lag4",	"GCS remote-cache_lag1",	"GCS remote-cache_lag3",	"GCS remote-cache_lag5",	"Inbound Error_lag2",	"Inbound Error_lag4",	"Killing Session note_lag1",	"Killing Session note_lag3",	"Killing Session note_lag5",	"KSFV_lag2",			
           "Diagnostic error_lag5",	"Diagonal Adl error_lag2",	"Diagonal Adl error_lag4",	"Fatal Ni connections_lag1",	"Fatal Ni connections_lag3",	"Fatal Ni connections_lag5",	"GCS remote-cache_lag2",	"GCS remote-cache_lag4",	"Inbound Error_lag1",	"Inbound Error_lag3",	"Inbound Error_lag5",	"Killing Session note_lag2",	"Killing Session note_lag4",	"KSFV_lag1",	"KSFV_lag3",			
           "KSFV_lag4",	"LGWR switch_lag1",	"LGWR switch_lag3",	"LGWR switch_lag5",	"NFS Error_lag2",	"NFS Error_lag4",	"Others_lag1",	"Others_lag3",	"Others_lag5",	"resize operation_lag2",	"resize operation_lag4",	"Roles Exceeded_lag1",	"Roles Exceeded_lag3",	"Roles Exceeded_lag5",	"Tns Error_lag2",	"Tns Error_lag4",		
           "KSFV_lag5",	"LGWR switch_lag2",	"LGWR switch_lag4",	"NFS Error_lag1",	"NFS Error_lag3",	"NFS Error_lag5",	"Others_lag2",	"Others_lag4",	"resize operation_lag1",	"resize operation_lag3",	"resize operation_lag5",	"Roles Exceeded_lag2",	"Roles Exceeded_lag4",	"Tns Error_lag1",	"Tns Error_lag3",	"Tns Error_lag5")

fault_Day1 <- lag1_data[names]


dim(fault_Day1)

str(fault_Day1)

summary(fault_Day1)

names(fault_Day1)

fault_Day1$Dest_Breach <- replace(fault_Day1$Dest_Breach,fault_Day1$Dest_Breach>1,1)
fault_Day1$Dest_Breach <- as.factor(fault_Day1$Dest_Breach)
fault_Day1$Date <- NULL

library(DMwR)
set.seed(122)
smoted_day1 <- SMOTE(Dest_Breach~., fault_Day1, perc.over=6000, perc.under=100, k=10, learner=NULL)
table(smoted_day1$Dest_Breach)



model1 <- glm(Dest_Breach ~ .,family=binomial,data=smoted_day1)
summary(model1)

#slm2 <- step(model13)
#summary(slm2)
#model13 <- 
#summary(model13)
#anova(model1, test="Chisq")
#plot(allEffects(model1))
#plot(model1)

predicted1 <- predict(model1, fault_Day1, type="response")  # predicted scores

predicted_file1 <- data.frame(cbind(fault_Day1,predicted1))

predicted_file1 <- data.frame(cbind(lag1_data$Date,predicted_file1))
write.csv(predicted_file1,"C:\\Shiva\\Projects\\GS\\GS_fault\\EDW\\EDW_fault_prediction\\predicted_Dest_Breach_1.csv")

library(InformationValue)

optCutOff <- optimalCutoff(fault_Day1$Dest_Breach, predicted1)[1]
#optCutOff <- 0.4


misClassError(fault_Day1$Dest_Breach, predicted1, threshold = optCutOff)

plotROC(fault_Day1$Dest_Breach, predicted1)

Concordance(fault_Day1$Dest_Breach, predicted1)

sensitivity(fault_Day1$Dest_Breach, predicted1, threshold = optCutOff)

specificity(fault_Day1$Dest_Breach, predicted1, threshold = optCutOff)

confusionMatrix(fault_Day1$Dest_Breach, predicted1, threshold = optCutOff)


##################################################################################################################################

names <- c("Date","Dest_Breach","ARCHIVE LOG_lag2",	"ARCHIVE LOG_lag4",	"ARCHIVE LOG_lag6",	"Archived_thread_error_lag3",	"Archived_thread_error_lag5",	"ASMB_lag2",	"ASMB_lag4",	"ASMB_lag6",	"Auto Tuning_lag3",	"Auto Tuning_lag5",	"Dbora Files_lag2",	"Dbora Files_lag4",	"Dbora Files_lag6",	"Diagnostic error_lag3",			
           "ARCHIVE LOG_lag3",	"ARCHIVE LOG_lag5",	"Archived_thread_error_lag2",	"Archived_thread_error_lag4",	"Archived_thread_error_lag6",	"ASMB_lag3",	"ASMB_lag5",	"Auto Tuning_lag2",	"Auto Tuning_lag4",	"Auto Tuning_lag6",	"Dbora Files_lag3",	"Dbora Files_lag5",	"Diagnostic error_lag2",	"Diagnostic error_lag4",			
           "Diagnostic error_lag5",	"Diagonal Adl error_lag2",	"Diagonal Adl error_lag4",	"Diagonal Adl error_lag6",	"Fatal Ni connections_lag3",	"Fatal Ni connections_lag5",	"GCS remote-cache_lag2",	"GCS remote-cache_lag4",	"GCS remote-cache_lag6",	"Inbound Error_lag3",	"Inbound Error_lag5",	"Killing Session note_lag2",	"Killing Session note_lag4",	"Killing Session note_lag6",	"KSFV_lag3",			
           "Diagnostic error_lag6",	"Diagonal Adl error_lag3",	"Diagonal Adl error_lag5",	"Fatal Ni connections_lag2",	"Fatal Ni connections_lag4",	"Fatal Ni connections_lag6",	"GCS remote-cache_lag3",	"GCS remote-cache_lag5",	"Inbound Error_lag2",	"Inbound Error_lag4",	"Inbound Error_lag6",	"Killing Session note_lag3",	"Killing Session note_lag5",	"KSFV_lag2",	"KSFV_lag4",			
           "KSFV_lag5",	"LGWR switch_lag2",	"LGWR switch_lag4",	"LGWR switch_lag6",	"NFS Error_lag3",	"NFS Error_lag5",	"Others_lag2",	"Others_lag4",	"Others_lag6",	"resize operation_lag3",	"resize operation_lag5",	"Roles Exceeded_lag2",	"Roles Exceeded_lag4",	"Roles Exceeded_lag6",	"Tns Error_lag3",	"Tns Error_lag5",		
           "KSFV_lag6",	"LGWR switch_lag3",	"LGWR switch_lag5",	"NFS Error_lag2",	"NFS Error_lag4",	"NFS Error_lag6",	"Others_lag3",	"Others_lag5",	"resize operation_lag2",	"resize operation_lag4",	"resize operation_lag6",	"Roles Exceeded_lag3",	"Roles Exceeded_lag5",	"Tns Error_lag2",	"Tns Error_lag4",	"Tns Error_lag6")

fault_Day2 <- lag2_data[names]


dim(fault_Day2)

str(fault_Day2)

summary(fault_Day2)

names(fault_Day2)

fault_Day2$Dest_Breach <- replace(fault_Day2$Dest_Breach,fault_Day2$Dest_Breach>1,1)
fault_Day2$Dest_Breach <- as.factor(fault_Day2$Dest_Breach)
fault_Day2$Date <- NULL

library(DMwR)
set.seed(123)
smoted_day2 <- SMOTE(Dest_Breach~., fault_Day2, perc.over=6000, perc.under=100, k=10, learner=NULL)
table(smoted_day2$Dest_Breach)



model2 <- glm(Dest_Breach ~ .,family=binomial,data=smoted_day2)
summary(model2)

#slm2 <- step(model2)
#summary(slm2)
#model13 <- glm
#summary(model2)
#anova(model2, test="Chisq")
#plot(allEffects(model2))
#plot(model2)

predicted2 <- predict(model2, fault_Day2, type="response")  # predicted scores

predicted_file2 <- data.frame(cbind(fault_Day2,predicted2))

predicted_file2 <- data.frame(cbind(lag2_data$Date,predicted_file2))

write.csv(predicted_file2,"C:\\Shiva\\Projects\\GS\\GS_fault\\EDW\\EDW_fault_prediction\\predicted_Dest_Breach_2.csv")

library(InformationValue)

optCutOff <- optimalCutoff(fault_Day2$Dest_Breach, predicted2)[1]
#optCutOff <- 0.4


misClassError(fault_Day2$Dest_Breach, predicted2, threshold = optCutOff)

plotROC(fault_Day2$Dest_Breach, predicted2)

Concordance(fault_Day2$Dest_Breach, predicted2)

sensitivity(fault_Day2$Dest_Breach, predicted2, threshold = optCutOff)

specificity(fault_Day2$Dest_Breach, predicted2, threshold = optCutOff)

confusionMatrix(fault_Day2$Dest_Breach, predicted2, threshold = optCutOff)


#######################################################################################################################

##--------------------------------------------------Misc--------------------------------------------------------------##

##Lag1##

names <- c("Date","Misc",	
           "ARCHIVE LOG_lag1",	"ARCHIVE LOG_lag3",	"ARCHIVE LOG_lag5",	"Archived_thread_error_lag2",	"Archived_thread_error_lag4",	"ASMB_lag1",	"ASMB_lag3",	"ASMB_lag5",	"Auto Tuning_lag2",	"Auto Tuning_lag4",	"Dbora Files_lag1",	"Dbora Files_lag3",	"Dbora Files_lag5",	"Diagnostic error_lag2",			
           "ARCHIVE LOG_lag2",	"ARCHIVE LOG_lag4",	"Archived_thread_error_lag1",	"Archived_thread_error_lag3",	"Archived_thread_error_lag5",	"ASMB_lag2",	"ASMB_lag4",	"Auto Tuning_lag1",	"Auto Tuning_lag3",	"Auto Tuning_lag5",	"Dbora Files_lag2",	"Dbora Files_lag4",	"Diagnostic error_lag1",	"Diagnostic error_lag3",			
           "Diagnostic error_lag4",	"Diagonal Adl error_lag1",	"Diagonal Adl error_lag3",	"Diagonal Adl error_lag5",	"Fatal Ni connections_lag2",	"Fatal Ni connections_lag4",	"GCS remote-cache_lag1",	"GCS remote-cache_lag3",	"GCS remote-cache_lag5",	"Inbound Error_lag2",	"Inbound Error_lag4",	"Killing Session note_lag1",	"Killing Session note_lag3",	"Killing Session note_lag5",	"KSFV_lag2",			
           "Diagnostic error_lag5",	"Diagonal Adl error_lag2",	"Diagonal Adl error_lag4",	"Fatal Ni connections_lag1",	"Fatal Ni connections_lag3",	"Fatal Ni connections_lag5",	"GCS remote-cache_lag2",	"GCS remote-cache_lag4",	"Inbound Error_lag1",	"Inbound Error_lag3",	"Inbound Error_lag5",	"Killing Session note_lag2",	"Killing Session note_lag4",	"KSFV_lag1",	"KSFV_lag3",			
           "KSFV_lag4",	"LGWR switch_lag1",	"LGWR switch_lag3",	"LGWR switch_lag5",	"NFS Error_lag2",	"NFS Error_lag4",	"Others_lag1",	"Others_lag3",	"Others_lag5",	"resize operation_lag2",	"resize operation_lag4",	"Roles Exceeded_lag1",	"Roles Exceeded_lag3",	"Roles Exceeded_lag5",	"Tns Error_lag2",	"Tns Error_lag4",		
           "KSFV_lag5",	"LGWR switch_lag2",	"LGWR switch_lag4",	"NFS Error_lag1",	"NFS Error_lag3",	"NFS Error_lag5",	"Others_lag2",	"Others_lag4",	"resize operation_lag1",	"resize operation_lag3",	"resize operation_lag5",	"Roles Exceeded_lag2",	"Roles Exceeded_lag4",	"Tns Error_lag1",	"Tns Error_lag3",	"Tns Error_lag5")

fault_Day1 <- lag1_data[names]


dim(fault_Day1)

str(fault_Day1)

summary(fault_Day1)

names(fault_Day1)
fault_Day1$Misc <- replace(fault_Day1$Misc,fault_Day1$Misc>1,1)
fault_Day1$Misc <- as.factor(fault_Day1$Misc)
fault_Day1$Date <- NULL

library(DMwR)
set.seed(122)
smoted_day1 <- SMOTE(Misc~., fault_Day1, perc.over=6000, perc.under=100, k=10, learner=NULL)
table(smoted_day1$Misc)



model1 <- glm(Misc ~ .,family=binomial,data=smoted_day1)
summary(model1)

#slm2 <- step(model13)
#summary(slm2)
#model13 <- 
#summary(model13)
#anova(model1, test="Chisq")
#plot(allEffects(model1))
#plot(model1)

predicted1 <- predict(model1, fault_Day1, type="response")  # predicted scores

predicted_file1 <- data.frame(cbind(fault_Day1,predicted1))

predicted_file1 <- data.frame(cbind(lag1_data$Date,predicted_file1))

write.csv(predicted_file1,"C:\\Shiva\\Projects\\GS\\GS_fault\\EDW\\EDW_fault_prediction\\predicted_Misc_1.csv")

library(InformationValue)

optCutOff <- optimalCutoff(fault_Day1$Misc, predicted1)[1]
#optCutOff <- 0.4


misClassError(fault_Day1$Misc, predicted1, threshold = optCutOff)

plotROC(fault_Day1$Misc, predicted1)

Concordance(fault_Day1$Misc, predicted1)

sensitivity(fault_Day1$Misc, predicted1, threshold = optCutOff)

specificity(fault_Day1$Misc, predicted1, threshold = optCutOff)

confusionMatrix(fault_Day1$Misc, predicted1, threshold = optCutOff)


##################################################################################################################################

names <- c("Date","Misc","ARCHIVE LOG_lag2",	"ARCHIVE LOG_lag4",	"ARCHIVE LOG_lag6",	"Archived_thread_error_lag3",	"Archived_thread_error_lag5",	"ASMB_lag2",	"ASMB_lag4",	"ASMB_lag6",	"Auto Tuning_lag3",	"Auto Tuning_lag5",	"Dbora Files_lag2",	"Dbora Files_lag4",	"Dbora Files_lag6",	"Diagnostic error_lag3",			
           "ARCHIVE LOG_lag3",	"ARCHIVE LOG_lag5",	"Archived_thread_error_lag2",	"Archived_thread_error_lag4",	"Archived_thread_error_lag6",	"ASMB_lag3",	"ASMB_lag5",	"Auto Tuning_lag2",	"Auto Tuning_lag4",	"Auto Tuning_lag6",	"Dbora Files_lag3",	"Dbora Files_lag5",	"Diagnostic error_lag2",	"Diagnostic error_lag4",			
           "Diagnostic error_lag5",	"Diagonal Adl error_lag2",	"Diagonal Adl error_lag4",	"Diagonal Adl error_lag6",	"Fatal Ni connections_lag3",	"Fatal Ni connections_lag5",	"GCS remote-cache_lag2",	"GCS remote-cache_lag4",	"GCS remote-cache_lag6",	"Inbound Error_lag3",	"Inbound Error_lag5",	"Killing Session note_lag2",	"Killing Session note_lag4",	"Killing Session note_lag6",	"KSFV_lag3",			
           "Diagnostic error_lag6",	"Diagonal Adl error_lag3",	"Diagonal Adl error_lag5",	"Fatal Ni connections_lag2",	"Fatal Ni connections_lag4",	"Fatal Ni connections_lag6",	"GCS remote-cache_lag3",	"GCS remote-cache_lag5",	"Inbound Error_lag2",	"Inbound Error_lag4",	"Inbound Error_lag6",	"Killing Session note_lag3",	"Killing Session note_lag5",	"KSFV_lag2",	"KSFV_lag4",			
           "KSFV_lag5",	"LGWR switch_lag2",	"LGWR switch_lag4",	"LGWR switch_lag6",	"NFS Error_lag3",	"NFS Error_lag5",	"Others_lag2",	"Others_lag4",	"Others_lag6",	"resize operation_lag3",	"resize operation_lag5",	"Roles Exceeded_lag2",	"Roles Exceeded_lag4",	"Roles Exceeded_lag6",	"Tns Error_lag3",	"Tns Error_lag5",		
           "KSFV_lag6",	"LGWR switch_lag3",	"LGWR switch_lag5",	"NFS Error_lag2",	"NFS Error_lag4",	"NFS Error_lag6",	"Others_lag3",	"Others_lag5",	"resize operation_lag2",	"resize operation_lag4",	"resize operation_lag6",	"Roles Exceeded_lag3",	"Roles Exceeded_lag5",	"Tns Error_lag2",	"Tns Error_lag4",	"Tns Error_lag6")

fault_Day2 <- lag2_data[names]


dim(fault_Day2)

str(fault_Day2)

summary(fault_Day2)

names(fault_Day2)
fault_Day2$Misc <- replace(fault_Day2$Misc,fault_Day2$Misc>1,1)
fault_Day2$Misc <- as.factor(fault_Day2$Misc)
fault_Day2$Date <- NULL

library(DMwR)
set.seed(123)
smoted_day2 <- SMOTE(Misc~., fault_Day2, perc.over=6000, perc.under=100, k=10, learner=NULL)
table(smoted_day2$Misc)



model2 <- glm(Misc ~ .,family=binomial,data=smoted_day2)
summary(model2)

#slm2 <- step(model2)
#summary(slm2)
#model13 <- glm
#summary(model2)
#anova(model2, test="Chisq")
#plot(allEffects(model2))
#plot(model2)

predicted2 <- predict(model2, fault_Day2, type="response")  # predicted scores

predicted_file2 <- data.frame(cbind(fault_Day2,predicted2))

predicted_file2 <- data.frame(cbind(lag2_data$Date,predicted_file2))

write.csv(predicted_file2,"C:\\Shiva\\Projects\\GS\\GS_fault\\EDW\\EDW_fault_prediction\\predicted_Misc_2.csv")

library(InformationValue)

optCutOff <- optimalCutoff(fault_Day2$Misc, predicted2)[1]
#optCutOff <- 0.4


misClassError(fault_Day2$Misc, predicted2, threshold = optCutOff)

plotROC(fault_Day2$Misc, predicted2)

Concordance(fault_Day2$Misc, predicted2)

sensitivity(fault_Day2$Misc, predicted2, threshold = optCutOff)

specificity(fault_Day2$Misc, predicted2, threshold = optCutOff)

confusionMatrix(fault_Day2$Misc, predicted2, threshold = optCutOff)



#######################################################################################################################

##--------------------------------------------------DB_Down--------------------------------------------------------------##

##Lag1##

names <- c("Date","DB_Down",	
           "ARCHIVE LOG_lag1",	"ARCHIVE LOG_lag3",	"ARCHIVE LOG_lag5",	"Archived_thread_error_lag2",	"Archived_thread_error_lag4",	"ASMB_lag1",	"ASMB_lag3",	"ASMB_lag5",	"Auto Tuning_lag2",	"Auto Tuning_lag4",	"Dbora Files_lag1",	"Dbora Files_lag3",	"Dbora Files_lag5",	"Diagnostic error_lag2",			
           "ARCHIVE LOG_lag2",	"ARCHIVE LOG_lag4",	"Archived_thread_error_lag1",	"Archived_thread_error_lag3",	"Archived_thread_error_lag5",	"ASMB_lag2",	"ASMB_lag4",	"Auto Tuning_lag1",	"Auto Tuning_lag3",	"Auto Tuning_lag5",	"Dbora Files_lag2",	"Dbora Files_lag4",	"Diagnostic error_lag1",	"Diagnostic error_lag3",			
           "Diagnostic error_lag4",	"Diagonal Adl error_lag1",	"Diagonal Adl error_lag3",	"Diagonal Adl error_lag5",	"Fatal Ni connections_lag2",	"Fatal Ni connections_lag4",	"GCS remote-cache_lag1",	"GCS remote-cache_lag3",	"GCS remote-cache_lag5",	"Inbound Error_lag2",	"Inbound Error_lag4",	"Killing Session note_lag1",	"Killing Session note_lag3",	"Killing Session note_lag5",	"KSFV_lag2",			
           "Diagnostic error_lag5",	"Diagonal Adl error_lag2",	"Diagonal Adl error_lag4",	"Fatal Ni connections_lag1",	"Fatal Ni connections_lag3",	"Fatal Ni connections_lag5",	"GCS remote-cache_lag2",	"GCS remote-cache_lag4",	"Inbound Error_lag1",	"Inbound Error_lag3",	"Inbound Error_lag5",	"Killing Session note_lag2",	"Killing Session note_lag4",	"KSFV_lag1",	"KSFV_lag3",			
           "KSFV_lag4",	"LGWR switch_lag1",	"LGWR switch_lag3",	"LGWR switch_lag5",	"NFS Error_lag2",	"NFS Error_lag4",	"Others_lag1",	"Others_lag3",	"Others_lag5",	"resize operation_lag2",	"resize operation_lag4",	"Roles Exceeded_lag1",	"Roles Exceeded_lag3",	"Roles Exceeded_lag5",	"Tns Error_lag2",	"Tns Error_lag4",		
           "KSFV_lag5",	"LGWR switch_lag2",	"LGWR switch_lag4",	"NFS Error_lag1",	"NFS Error_lag3",	"NFS Error_lag5",	"Others_lag2",	"Others_lag4",	"resize operation_lag1",	"resize operation_lag3",	"resize operation_lag5",	"Roles Exceeded_lag2",	"Roles Exceeded_lag4",	"Tns Error_lag1",	"Tns Error_lag3",	"Tns Error_lag5")

fault_Day1 <- lag1_data[names]


dim(fault_Day1)

str(fault_Day1)

summary(fault_Day1)

names(fault_Day1)
fault_Day1$DB_Down <- replace(fault_Day1$DB_Down,fault_Day1$DB_Down>1,1)
fault_Day1$DB_Down <- as.factor(fault_Day1$DB_Down)
fault_Day1$Date <- NULL

library(DMwR)
smoted_day1 <- SMOTE(DB_Down~., fault_Day1, perc.over=6000, perc.under=100, k=10, learner=NULL)
table(smoted_day1$DB_Down)



model1 <- glm(DB_Down ~ .,family=binomial,data=smoted_day1)
summary(model1)

#slm2 <- step(model13)
#summary(slm2)
#model13 <- 
#summary(model13)
#anova(model1, test="Chisq")
#plot(allEffects(model1))
#plot(model1)

predicted1 <- predict(model1, fault_Day1, type="response")  # predicted scores

predicted_file1 <- data.frame(cbind(fault_Day1,predicted1))

predicted_file1 <- data.frame(cbind(lag1_data$Date,predicted_file1))

write.csv(predicted_file1,"C:\\Shiva\\Projects\\GS\\GS_fault\\EDW\\EDW_fault_prediction\\predicted_DB_Down_1.csv")

library(InformationValue)

optCutOff <- optimalCutoff(fault_Day1$DB_Down, predicted1)[1]
#optCutOff <- 0.4


DB_DownlassError(fault_Day1$DB_Down, predicted1, threshold = optCutOff)

plotROC(fault_Day1$DB_Down, predicted1)

Concordance(fault_Day1$DB_Down, predicted1)

sensitivity(fault_Day1$DB_Down, predicted1, threshold = optCutOff)

specificity(fault_Day1$DB_Down, predicted1, threshold = optCutOff)

confusionMatrix(fault_Day1$DB_Down, predicted1, threshold = optCutOff)


##################################################################################################################################

names <- c("Date","DB_Down","ARCHIVE LOG_lag2",	"ARCHIVE LOG_lag4",	"ARCHIVE LOG_lag6",	"Archived_thread_error_lag3",	"Archived_thread_error_lag5",	"ASMB_lag2",	"ASMB_lag4",	"ASMB_lag6",	"Auto Tuning_lag3",	"Auto Tuning_lag5",	"Dbora Files_lag2",	"Dbora Files_lag4",	"Dbora Files_lag6",	"Diagnostic error_lag3",			
           "ARCHIVE LOG_lag3",	"ARCHIVE LOG_lag5",	"Archived_thread_error_lag2",	"Archived_thread_error_lag4",	"Archived_thread_error_lag6",	"ASMB_lag3",	"ASMB_lag5",	"Auto Tuning_lag2",	"Auto Tuning_lag4",	"Auto Tuning_lag6",	"Dbora Files_lag3",	"Dbora Files_lag5",	"Diagnostic error_lag2",	"Diagnostic error_lag4",			
           "Diagnostic error_lag5",	"Diagonal Adl error_lag2",	"Diagonal Adl error_lag4",	"Diagonal Adl error_lag6",	"Fatal Ni connections_lag3",	"Fatal Ni connections_lag5",	"GCS remote-cache_lag2",	"GCS remote-cache_lag4",	"GCS remote-cache_lag6",	"Inbound Error_lag3",	"Inbound Error_lag5",	"Killing Session note_lag2",	"Killing Session note_lag4",	"Killing Session note_lag6",	"KSFV_lag3",			
           "Diagnostic error_lag6",	"Diagonal Adl error_lag3",	"Diagonal Adl error_lag5",	"Fatal Ni connections_lag2",	"Fatal Ni connections_lag4",	"Fatal Ni connections_lag6",	"GCS remote-cache_lag3",	"GCS remote-cache_lag5",	"Inbound Error_lag2",	"Inbound Error_lag4",	"Inbound Error_lag6",	"Killing Session note_lag3",	"Killing Session note_lag5",	"KSFV_lag2",	"KSFV_lag4",			
           "KSFV_lag5",	"LGWR switch_lag2",	"LGWR switch_lag4",	"LGWR switch_lag6",	"NFS Error_lag3",	"NFS Error_lag5",	"Others_lag2",	"Others_lag4",	"Others_lag6",	"resize operation_lag3",	"resize operation_lag5",	"Roles Exceeded_lag2",	"Roles Exceeded_lag4",	"Roles Exceeded_lag6",	"Tns Error_lag3",	"Tns Error_lag5",		
           "KSFV_lag6",	"LGWR switch_lag3",	"LGWR switch_lag5",	"NFS Error_lag2",	"NFS Error_lag4",	"NFS Error_lag6",	"Others_lag3",	"Others_lag5",	"resize operation_lag2",	"resize operation_lag4",	"resize operation_lag6",	"Roles Exceeded_lag3",	"Roles Exceeded_lag5",	"Tns Error_lag2",	"Tns Error_lag4",	"Tns Error_lag6")

fault_Day2 <- lag2_data[names]


dim(fault_Day2)

str(fault_Day2)

summary(fault_Day2)

names(fault_Day2)

fault_Day2$DB_Down <- replace(fault_Day2$DB_Down,fault_Day2$DB_Down>1,1)
fault_Day2$DB_Down <- as.factor(fault_Day2$DB_Down)
fault_Day2$Date <- NULL

library(DMwR)
set.seed(122)
smoted_day2 <- SMOTE(DB_Down~., fault_Day2, perc.over=6000, perc.under=100, k=10, learner=NULL)
table(smoted_day2$DB_Down)



model2 <- glm(DB_Down ~ .,family=binomial,data=smoted_day2)
summary(model2)

#slm2 <- step(model2)
#summary(slm2)
#model13 <- glm
#summary(model2)
#anova(model2, test="Chisq")
#plot(allEffects(model2))
#plot(model2)

predicted2 <- predict(model2, fault_Day2, type="response")  # predicted scores

predicted_file2 <- data.frame(cbind(fault_Day2,predicted2))

predicted_file2 <- data.frame(cbind(lag2_data$Date,predicted_file2))

write.csv(predicted_file2,"C:\\Shiva\\Projects\\GS\\GS_fault\\EDW\\EDW_fault_prediction\\predicted_DB_Down_2.csv")

library(InformationValue)

optCutOff <- optimalCutoff(fault_Day2$DB_Down, predicted2)[1]
#optCutOff <- 0.4


misClassError(fault_Day2$DB_Down, predicted2, threshold = optCutOff)

plotROC(fault_Day2$DB_Down, predicted2)

Concordance(fault_Day2$DB_Down, predicted2)

sensitivity(fault_Day2$DB_Down, predicted2, threshold = optCutOff)

specificity(fault_Day2$DB_Down, predicted2, threshold = optCutOff)

confusionMatrix(fault_Day2$DB_Down, predicted2, threshold = optCutOff)




#######################################################################################################################

##--------------------------------------------------ORA_xxxx--------------------------------------------------------------##

##Lag1##

names <- c("Date","ORA_xxxx",	
           "ARCHIVE LOG_lag1",	"ARCHIVE LOG_lag3",	"ARCHIVE LOG_lag5",	"Archived_thread_error_lag2",	"Archived_thread_error_lag4",	"ASMB_lag1",	"ASMB_lag3",	"ASMB_lag5",	"Auto Tuning_lag2",	"Auto Tuning_lag4",	"Dbora Files_lag1",	"Dbora Files_lag3",	"Dbora Files_lag5",	"Diagnostic error_lag2",			
           "ARCHIVE LOG_lag2",	"ARCHIVE LOG_lag4",	"Archived_thread_error_lag1",	"Archived_thread_error_lag3",	"Archived_thread_error_lag5",	"ASMB_lag2",	"ASMB_lag4",	"Auto Tuning_lag1",	"Auto Tuning_lag3",	"Auto Tuning_lag5",	"Dbora Files_lag2",	"Dbora Files_lag4",	"Diagnostic error_lag1",	"Diagnostic error_lag3",			
           "Diagnostic error_lag4",	"Diagonal Adl error_lag1",	"Diagonal Adl error_lag3",	"Diagonal Adl error_lag5",	"Fatal Ni connections_lag2",	"Fatal Ni connections_lag4",	"GCS remote-cache_lag1",	"GCS remote-cache_lag3",	"GCS remote-cache_lag5",	"Inbound Error_lag2",	"Inbound Error_lag4",	"Killing Session note_lag1",	"Killing Session note_lag3",	"Killing Session note_lag5",	"KSFV_lag2",			
           "Diagnostic error_lag5",	"Diagonal Adl error_lag2",	"Diagonal Adl error_lag4",	"Fatal Ni connections_lag1",	"Fatal Ni connections_lag3",	"Fatal Ni connections_lag5",	"GCS remote-cache_lag2",	"GCS remote-cache_lag4",	"Inbound Error_lag1",	"Inbound Error_lag3",	"Inbound Error_lag5",	"Killing Session note_lag2",	"Killing Session note_lag4",	"KSFV_lag1",	"KSFV_lag3",			
           "KSFV_lag4",	"LGWR switch_lag1",	"LGWR switch_lag3",	"LGWR switch_lag5",	"NFS Error_lag2",	"NFS Error_lag4",	"Others_lag1",	"Others_lag3",	"Others_lag5",	"resize operation_lag2",	"resize operation_lag4",	"Roles Exceeded_lag1",	"Roles Exceeded_lag3",	"Roles Exceeded_lag5",	"Tns Error_lag2",	"Tns Error_lag4",		
           "KSFV_lag5",	"LGWR switch_lag2",	"LGWR switch_lag4",	"NFS Error_lag1",	"NFS Error_lag3",	"NFS Error_lag5",	"Others_lag2",	"Others_lag4",	"resize operation_lag1",	"resize operation_lag3",	"resize operation_lag5",	"Roles Exceeded_lag2",	"Roles Exceeded_lag4",	"Tns Error_lag1",	"Tns Error_lag3",	"Tns Error_lag5")

fault_Day1 <- lag1_data[names]


dim(fault_Day1)

str(fault_Day1)

summary(fault_Day1)

names(fault_Day1)

fault_Day1$ORA_xxxx <- replace(fault_Day1$ORA_xxxx,fault_Day1$ORA_xxxx>1,1)

fault_Day1$ORA_xxxx <- as.factor(fault_Day1$ORA_xxxx)
fault_Day1$Date <- NULL

library(DMwR)
set.seed(122)
smoted_day1 <- SMOTE(ORA_xxxx~., fault_Day1, perc.over=6000, perc.under=100, k=10, learner=NULL)
table(smoted_day1$ORA_xxxx)



model1 <- glm(ORA_xxxx ~ .,family=binomial,data=smoted_day1)
summary(model1)

#slm2 <- step(model13)
#summary(slm2)
#model13 <- 
#summary(model13)
#anova(model1, test="Chisq")
#plot(allEffects(model1))
#plot(model1)

predicted1 <- predict(model1, fault_Day1, type="response")  # predicted scores

predicted_file1 <- data.frame(cbind(fault_Day1,predicted1))

predicted_file1 <- data.frame(cbind(lag1_data$Date,predicted_file1))

write.csv(predicted_file1,"C:\\Shiva\\Projects\\GS\\GS_fault\\EDW\\EDW_fault_prediction\\predicted_ORA_xxxx_1.csv")

library(InformationValue)

optCutOff <- optimalCutoff(fault_Day1$ORA_xxxx, predicted1)[1]
#optCutOff <- 0.4


misClassError(fault_Day1$ORA_xxxx, predicted1, threshold = optCutOff)

plotROC(fault_Day1$ORA_xxxx, predicted1)

Concordance(fault_Day1$ORA_xxxx, predicted1)

sensitivity(fault_Day1$ORA_xxxx, predicted1, threshold = optCutOff)

specificity(fault_Day1$ORA_xxxx, predicted1, threshold = optCutOff)

confusionMatrix(fault_Day1$ORA_xxxx, predicted1, threshold = optCutOff)


##################################################################################################################################

names <- c("Date","ORA_xxxx","ARCHIVE LOG_lag2",	"ARCHIVE LOG_lag4",	"ARCHIVE LOG_lag6",	"Archived_thread_error_lag3",	"Archived_thread_error_lag5",	"ASMB_lag2",	"ASMB_lag4",	"ASMB_lag6",	"Auto Tuning_lag3",	"Auto Tuning_lag5",	"Dbora Files_lag2",	"Dbora Files_lag4",	"Dbora Files_lag6",	"Diagnostic error_lag3",			
           "ARCHIVE LOG_lag3",	"ARCHIVE LOG_lag5",	"Archived_thread_error_lag2",	"Archived_thread_error_lag4",	"Archived_thread_error_lag6",	"ASMB_lag3",	"ASMB_lag5",	"Auto Tuning_lag2",	"Auto Tuning_lag4",	"Auto Tuning_lag6",	"Dbora Files_lag3",	"Dbora Files_lag5",	"Diagnostic error_lag2",	"Diagnostic error_lag4",			
           "Diagnostic error_lag5",	"Diagonal Adl error_lag2",	"Diagonal Adl error_lag4",	"Diagonal Adl error_lag6",	"Fatal Ni connections_lag3",	"Fatal Ni connections_lag5",	"GCS remote-cache_lag2",	"GCS remote-cache_lag4",	"GCS remote-cache_lag6",	"Inbound Error_lag3",	"Inbound Error_lag5",	"Killing Session note_lag2",	"Killing Session note_lag4",	"Killing Session note_lag6",	"KSFV_lag3",			
           "Diagnostic error_lag6",	"Diagonal Adl error_lag3",	"Diagonal Adl error_lag5",	"Fatal Ni connections_lag2",	"Fatal Ni connections_lag4",	"Fatal Ni connections_lag6",	"GCS remote-cache_lag3",	"GCS remote-cache_lag5",	"Inbound Error_lag2",	"Inbound Error_lag4",	"Inbound Error_lag6",	"Killing Session note_lag3",	"Killing Session note_lag5",	"KSFV_lag2",	"KSFV_lag4",			
           "KSFV_lag5",	"LGWR switch_lag2",	"LGWR switch_lag4",	"LGWR switch_lag6",	"NFS Error_lag3",	"NFS Error_lag5",	"Others_lag2",	"Others_lag4",	"Others_lag6",	"resize operation_lag3",	"resize operation_lag5",	"Roles Exceeded_lag2",	"Roles Exceeded_lag4",	"Roles Exceeded_lag6",	"Tns Error_lag3",	"Tns Error_lag5",		
           "KSFV_lag6",	"LGWR switch_lag3",	"LGWR switch_lag5",	"NFS Error_lag2",	"NFS Error_lag4",	"NFS Error_lag6",	"Others_lag3",	"Others_lag5",	"resize operation_lag2",	"resize operation_lag4",	"resize operation_lag6",	"Roles Exceeded_lag3",	"Roles Exceeded_lag5",	"Tns Error_lag2",	"Tns Error_lag4",	"Tns Error_lag6")

fault_Day2 <- lag2_data[names]


dim(fault_Day2)

str(fault_Day2)

summary(fault_Day2)

names(fault_Day2)

fault_Day2$ORA_xxxx <- replace(fault_Day2$ORA_xxxx,fault_Day2$ORA_xxxx>1,1)

fault_Day2$ORA_xxxx <- as.factor(fault_Day2$ORA_xxxx)
fault_Day2$Date <- NULL

library(DMwR)
set.seed(123)
smoted_day2 <- SMOTE(ORA_xxxx~., fault_Day2, perc.over=6000, perc.under=100, k=10, learner=NULL)
table(smoted_day2$ORA_xxxx)



model2 <- glm(ORA_xxxx ~ .,family=binomial,data=smoted_day2)
summary(model2)

#slm2 <- step(model2)
#summary(slm2)
#model13 <- glm
#summary(model2)
#anova(model2, test="Chisq")
#plot(allEffects(model2))
#plot(model2)

predicted2 <- predict(model2, fault_Day2, type="response")  # predicted scores

predicted_file2 <- data.frame(cbind(fault_Day2,predicted2))

predicted_file2 <- data.frame(cbind(lag2_data$Date,predicted_file2))

write.csv(predicted_file2,"C:\\Shiva\\Projects\\GS\\GS_fault\\EDW\\EDW_fault_prediction\\predicted_ORA_xxxx_2.csv")

library(InformationValue)

optCutOff <- optimalCutoff(fault_Day2$ORA_xxxx, predicted2)[1]
#optCutOff <- 0.4


misClassError(fault_Day2$ORA_xxxx, predicted2, threshold = optCutOff)

plotROC(fault_Day2$ORA_xxxx, predicted2)

Concordance(fault_Day2$ORA_xxxx, predicted2)

sensitivity(fault_Day2$ORA_xxxx, predicted2, threshold = optCutOff)

specificity(fault_Day2$ORA_xxxx, predicted2, threshold = optCutOff)

confusionMatrix(fault_Day2$ORA_xxxx, predicted2, threshold = optCutOff)


#######################################################################################################################

##--------------------------------------------------GRID--------------------------------------------------------------##

##Lag1##

names <- c("Date","GRID",	
           "ARCHIVE LOG_lag1",	"ARCHIVE LOG_lag3",	"ARCHIVE LOG_lag5",	"Archived_thread_error_lag2",	"Archived_thread_error_lag4",	"ASMB_lag1",	"ASMB_lag3",	"ASMB_lag5",	"Auto Tuning_lag2",	"Auto Tuning_lag4",	"Dbora Files_lag1",	"Dbora Files_lag3",	"Dbora Files_lag5",	"Diagnostic error_lag2",			
           "ARCHIVE LOG_lag2",	"ARCHIVE LOG_lag4",	"Archived_thread_error_lag1",	"Archived_thread_error_lag3",	"Archived_thread_error_lag5",	"ASMB_lag2",	"ASMB_lag4",	"Auto Tuning_lag1",	"Auto Tuning_lag3",	"Auto Tuning_lag5",	"Dbora Files_lag2",	"Dbora Files_lag4",	"Diagnostic error_lag1",	"Diagnostic error_lag3",			
           "Diagnostic error_lag4",	"Diagonal Adl error_lag1",	"Diagonal Adl error_lag3",	"Diagonal Adl error_lag5",	"Fatal Ni connections_lag2",	"Fatal Ni connections_lag4",	"GCS remote-cache_lag1",	"GCS remote-cache_lag3",	"GCS remote-cache_lag5",	"Inbound Error_lag2",	"Inbound Error_lag4",	"Killing Session note_lag1",	"Killing Session note_lag3",	"Killing Session note_lag5",	"KSFV_lag2",			
           "Diagnostic error_lag5",	"Diagonal Adl error_lag2",	"Diagonal Adl error_lag4",	"Fatal Ni connections_lag1",	"Fatal Ni connections_lag3",	"Fatal Ni connections_lag5",	"GCS remote-cache_lag2",	"GCS remote-cache_lag4",	"Inbound Error_lag1",	"Inbound Error_lag3",	"Inbound Error_lag5",	"Killing Session note_lag2",	"Killing Session note_lag4",	"KSFV_lag1",	"KSFV_lag3",			
           "KSFV_lag4",	"LGWR switch_lag1",	"LGWR switch_lag3",	"LGWR switch_lag5",	"NFS Error_lag2",	"NFS Error_lag4",	"Others_lag1",	"Others_lag3",	"Others_lag5",	"resize operation_lag2",	"resize operation_lag4",	"Roles Exceeded_lag1",	"Roles Exceeded_lag3",	"Roles Exceeded_lag5",	"Tns Error_lag2",	"Tns Error_lag4",		
           "KSFV_lag5",	"LGWR switch_lag2",	"LGWR switch_lag4",	"NFS Error_lag1",	"NFS Error_lag3",	"NFS Error_lag5",	"Others_lag2",	"Others_lag4",	"resize operation_lag1",	"resize operation_lag3",	"resize operation_lag5",	"Roles Exceeded_lag2",	"Roles Exceeded_lag4",	"Tns Error_lag1",	"Tns Error_lag3",	"Tns Error_lag5")

fault_Day1 <- lag1_data[names]


dim(fault_Day1)

str(fault_Day1)

summary(fault_Day1)

names(fault_Day1)

fault_Day1$GRID <- replace(fault_Day1$GRID,fault_Day1$GRID>1,1)

fault_Day1$GRID <- as.factor(fault_Day1$GRID)
fault_Day1$Date <- NULL

library(DMwR)
set.seed(122)
smoted_day1 <- SMOTE(GRID~., fault_Day1, perc.over=6000, perc.under=100, k=10, learner=NULL)
table(smoted_day1$GRID)



model1 <- glm(GRID ~ .,family=binomial,data=smoted_day1)
summary(model1)

#slm2 <- step(model13)
#summary(slm2)
#model13 <- 
#summary(model13)
#anova(model1, test="Chisq")
#plot(allEffects(model1))
#plot(model1)

predicted1 <- predict(model1, fault_Day1, type="response")  # predicted scores

predicted_file1 <- data.frame(cbind(fault_Day1,predicted1))

predicted_file1 <- data.frame(cbind(lag1_data$Date,predicted_file1))

write.csv(predicted_file1,"C:\\Shiva\\Projects\\GS\\GS_fault\\EDW\\EDW_fault_prediction\\predicted_GRID_1.csv")

library(InformationValue)

optCutOff <- optimalCutoff(fault_Day1$GRID, predicted1)[1]
#optCutOff <- 0.4


misClassError(fault_Day1$GRID, predicted1, threshold = optCutOff)

plotROC(fault_Day1$GRID, predicted1)

Concordance(fault_Day1$GRID, predicted1)

sensitivity(fault_Day1$GRID, predicted1, threshold = optCutOff)

specificity(fault_Day1$GRID, predicted1, threshold = optCutOff)

confusionMatrix(fault_Day1$GRID, predicted1, threshold = optCutOff)



##################################################################################################################################

names <- c("Date","GRID","ARCHIVE LOG_lag2",	"ARCHIVE LOG_lag4",	"ARCHIVE LOG_lag6",	"Archived_thread_error_lag3",	"Archived_thread_error_lag5",	"ASMB_lag2",	"ASMB_lag4",	"ASMB_lag6",	"Auto Tuning_lag3",	"Auto Tuning_lag5",	"Dbora Files_lag2",	"Dbora Files_lag4",	"Dbora Files_lag6",	"Diagnostic error_lag3",			
           "ARCHIVE LOG_lag3",	"ARCHIVE LOG_lag5",	"Archived_thread_error_lag2",	"Archived_thread_error_lag4",	"Archived_thread_error_lag6",	"ASMB_lag3",	"ASMB_lag5",	"Auto Tuning_lag2",	"Auto Tuning_lag4",	"Auto Tuning_lag6",	"Dbora Files_lag3",	"Dbora Files_lag5",	"Diagnostic error_lag2",	"Diagnostic error_lag4",			
           "Diagnostic error_lag5",	"Diagonal Adl error_lag2",	"Diagonal Adl error_lag4",	"Diagonal Adl error_lag6",	"Fatal Ni connections_lag3",	"Fatal Ni connections_lag5",	"GCS remote-cache_lag2",	"GCS remote-cache_lag4",	"GCS remote-cache_lag6",	"Inbound Error_lag3",	"Inbound Error_lag5",	"Killing Session note_lag2",	"Killing Session note_lag4",	"Killing Session note_lag6",	"KSFV_lag3",			
           "Diagnostic error_lag6",	"Diagonal Adl error_lag3",	"Diagonal Adl error_lag5",	"Fatal Ni connections_lag2",	"Fatal Ni connections_lag4",	"Fatal Ni connections_lag6",	"GCS remote-cache_lag3",	"GCS remote-cache_lag5",	"Inbound Error_lag2",	"Inbound Error_lag4",	"Inbound Error_lag6",	"Killing Session note_lag3",	"Killing Session note_lag5",	"KSFV_lag2",	"KSFV_lag4",			
           "KSFV_lag5",	"LGWR switch_lag2",	"LGWR switch_lag4",	"LGWR switch_lag6",	"NFS Error_lag3",	"NFS Error_lag5",	"Others_lag2",	"Others_lag4",	"Others_lag6",	"resize operation_lag3",	"resize operation_lag5",	"Roles Exceeded_lag2",	"Roles Exceeded_lag4",	"Roles Exceeded_lag6",	"Tns Error_lag3",	"Tns Error_lag5",		
           "KSFV_lag6",	"LGWR switch_lag3",	"LGWR switch_lag5",	"NFS Error_lag2",	"NFS Error_lag4",	"NFS Error_lag6",	"Others_lag3",	"Others_lag5",	"resize operation_lag2",	"resize operation_lag4",	"resize operation_lag6",	"Roles Exceeded_lag3",	"Roles Exceeded_lag5",	"Tns Error_lag2",	"Tns Error_lag4",	"Tns Error_lag6")

fault_Day2 <- lag2_data[names]


dim(fault_Day2)

str(fault_Day2)

summary(fault_Day2)

names(fault_Day2)

fault_Day2$GRID <- replace(fault_Day2$GRID,fault_Day2$GRID>1,1)

fault_Day2$GRID <- as.factor(fault_Day2$GRID)
fault_Day2$Date <- NULL

library(DMwR)
set.seed(123)
smoted_day2 <- SMOTE(GRID~., fault_Day2, perc.over=6000, perc.under=100, k=10, learner=NULL)
table(smoted_day2$GRID)



model2 <- glm(GRID ~ .,family=binomial,data=smoted_day2)
summary(model2)

#slm2 <- step(model2)
#summary(slm2)
#model13 <- glm
#summary(model2)
#anova(model2, test="Chisq")
#plot(allEffects(model2))
#plot(model2)

predicted2 <- predict(model2, fault_Day2, type="response")  # predicted scores

predicted_file2 <- data.frame(cbind(fault_Day2,predicted2))

predicted_file2 <- data.frame(cbind(lag2_data$Date,predicted_file2))

write.csv(predicted_file2,"C:\\Shiva\\Projects\\GS\\GS_fault\\EDW\\EDW_fault_prediction\\predicted_GRID_2.csv")

library(InformationValue)

optCutOff <- optimalCutoff(fault_Day2$GRID, predicted2)[1]
#optCutOff <- 0.4


GRIDlassError(fault_Day2$GRID, predicted2, threshold = optCutOff)

plotROC(fault_Day2$GRID, predicted2)

Concordance(fault_Day2$GRID, predicted2)

sensitivity(fault_Day2$GRID, predicted2, threshold = optCutOff)

specificity(fault_Day2$GRID, predicted2, threshold = optCutOff)

confusionMatrix(fault_Day2$GRID, predicted2, threshold = optCutOff)



#######################################################################################################################

##--------------------------------------------------Block--------------------------------------------------------------##

##Lag1##

names <- c("Date","Block",	
           "ARCHIVE LOG_lag1",	"ARCHIVE LOG_lag3",	"ARCHIVE LOG_lag5",	"Archived_thread_error_lag2",	"Archived_thread_error_lag4",	"ASMB_lag1",	"ASMB_lag3",	"ASMB_lag5",	"Auto Tuning_lag2",	"Auto Tuning_lag4",	"Dbora Files_lag1",	"Dbora Files_lag3",	"Dbora Files_lag5",	"Diagnostic error_lag2",			
           "ARCHIVE LOG_lag2",	"ARCHIVE LOG_lag4",	"Archived_thread_error_lag1",	"Archived_thread_error_lag3",	"Archived_thread_error_lag5",	"ASMB_lag2",	"ASMB_lag4",	"Auto Tuning_lag1",	"Auto Tuning_lag3",	"Auto Tuning_lag5",	"Dbora Files_lag2",	"Dbora Files_lag4",	"Diagnostic error_lag1",	"Diagnostic error_lag3",			
           "Diagnostic error_lag4",	"Diagonal Adl error_lag1",	"Diagonal Adl error_lag3",	"Diagonal Adl error_lag5",	"Fatal Ni connections_lag2",	"Fatal Ni connections_lag4",	"GCS remote-cache_lag1",	"GCS remote-cache_lag3",	"GCS remote-cache_lag5",	"Inbound Error_lag2",	"Inbound Error_lag4",	"Killing Session note_lag1",	"Killing Session note_lag3",	"Killing Session note_lag5",	"KSFV_lag2",			
           "Diagnostic error_lag5",	"Diagonal Adl error_lag2",	"Diagonal Adl error_lag4",	"Fatal Ni connections_lag1",	"Fatal Ni connections_lag3",	"Fatal Ni connections_lag5",	"GCS remote-cache_lag2",	"GCS remote-cache_lag4",	"Inbound Error_lag1",	"Inbound Error_lag3",	"Inbound Error_lag5",	"Killing Session note_lag2",	"Killing Session note_lag4",	"KSFV_lag1",	"KSFV_lag3",			
           "KSFV_lag4",	"LGWR switch_lag1",	"LGWR switch_lag3",	"LGWR switch_lag5",	"NFS Error_lag2",	"NFS Error_lag4",	"Others_lag1",	"Others_lag3",	"Others_lag5",	"resize operation_lag2",	"resize operation_lag4",	"Roles Exceeded_lag1",	"Roles Exceeded_lag3",	"Roles Exceeded_lag5",	"Tns Error_lag2",	"Tns Error_lag4",		
           "KSFV_lag5",	"LGWR switch_lag2",	"LGWR switch_lag4",	"NFS Error_lag1",	"NFS Error_lag3",	"NFS Error_lag5",	"Others_lag2",	"Others_lag4",	"resize operation_lag1",	"resize operation_lag3",	"resize operation_lag5",	"Roles Exceeded_lag2",	"Roles Exceeded_lag4",	"Tns Error_lag1",	"Tns Error_lag3",	"Tns Error_lag5")

fault_Day1 <- lag1_data[names]


dim(fault_Day1)

str(fault_Day1)

summary(fault_Day1)

names(fault_Day1)

fault_Day1$Block <- replace(fault_Day1$Block,fault_Day1$Block>1,1)
fault_Day1$Block <- as.factor(fault_Day1$Block)
fault_Day1$Date <- NULL

library(DMwR)
set.seed(122)
smoted_day1 <- SMOTE(Block~., fault_Day1, perc.over=6000, perc.under=100, k=10, learner=NULL)
table(smoted_day1$Block)



model1 <- glm(Block ~ .,family=binomial,data=smoted_day1)
summary(model1)

#slm2 <- step(model13)
#summary(slm2)
#model13 <- 
#summary(model13)
#anova(model1, test="Chisq")
#plot(allEffects(model1))
#plot(model1)

predicted1 <- predict(model1, fault_Day1, type="response")  # predicted scores

predicted_file1 <- data.frame(cbind(fault_Day1,predicted1))

predicted_file1 <- data.frame(cbind(lag1_data$Date,predicted_file1))

write.csv(predicted_file1,"C:\\Shiva\\Projects\\GS\\GS_fault\\EDW\\EDW_fault_prediction\\predicted_Block_1.csv")

library(InformationValue)

optCutOff <- optimalCutoff(fault_Day1$Block, predicted1)[1]
#optCutOff <- 0.4


BlocklassError(fault_Day1$Block, predicted1, threshold = optCutOff)

plotROC(fault_Day1$Block, predicted1)

Concordance(fault_Day1$Block, predicted1)

sensitivity(fault_Day1$Block, predicted1, threshold = optCutOff)

specificity(fault_Day1$Block, predicted1, threshold = optCutOff)

confusionMatrix(fault_Day1$Block, predicted1, threshold = optCutOff)



##################################################################################################################################

names <- c("Date","Block","ARCHIVE LOG_lag2",	"ARCHIVE LOG_lag4",	"ARCHIVE LOG_lag6",	"Archived_thread_error_lag3",	"Archived_thread_error_lag5",	"ASMB_lag2",	"ASMB_lag4",	"ASMB_lag6",	"Auto Tuning_lag3",	"Auto Tuning_lag5",	"Dbora Files_lag2",	"Dbora Files_lag4",	"Dbora Files_lag6",	"Diagnostic error_lag3",			
           "ARCHIVE LOG_lag3",	"ARCHIVE LOG_lag5",	"Archived_thread_error_lag2",	"Archived_thread_error_lag4",	"Archived_thread_error_lag6",	"ASMB_lag3",	"ASMB_lag5",	"Auto Tuning_lag2",	"Auto Tuning_lag4",	"Auto Tuning_lag6",	"Dbora Files_lag3",	"Dbora Files_lag5",	"Diagnostic error_lag2",	"Diagnostic error_lag4",			
           "Diagnostic error_lag5",	"Diagonal Adl error_lag2",	"Diagonal Adl error_lag4",	"Diagonal Adl error_lag6",	"Fatal Ni connections_lag3",	"Fatal Ni connections_lag5",	"GCS remote-cache_lag2",	"GCS remote-cache_lag4",	"GCS remote-cache_lag6",	"Inbound Error_lag3",	"Inbound Error_lag5",	"Killing Session note_lag2",	"Killing Session note_lag4",	"Killing Session note_lag6",	"KSFV_lag3",			
           "Diagnostic error_lag6",	"Diagonal Adl error_lag3",	"Diagonal Adl error_lag5",	"Fatal Ni connections_lag2",	"Fatal Ni connections_lag4",	"Fatal Ni connections_lag6",	"GCS remote-cache_lag3",	"GCS remote-cache_lag5",	"Inbound Error_lag2",	"Inbound Error_lag4",	"Inbound Error_lag6",	"Killing Session note_lag3",	"Killing Session note_lag5",	"KSFV_lag2",	"KSFV_lag4",			
           "KSFV_lag5",	"LGWR switch_lag2",	"LGWR switch_lag4",	"LGWR switch_lag6",	"NFS Error_lag3",	"NFS Error_lag5",	"Others_lag2",	"Others_lag4",	"Others_lag6",	"resize operation_lag3",	"resize operation_lag5",	"Roles Exceeded_lag2",	"Roles Exceeded_lag4",	"Roles Exceeded_lag6",	"Tns Error_lag3",	"Tns Error_lag5",		
           "KSFV_lag6",	"LGWR switch_lag3",	"LGWR switch_lag5",	"NFS Error_lag2",	"NFS Error_lag4",	"NFS Error_lag6",	"Others_lag3",	"Others_lag5",	"resize operation_lag2",	"resize operation_lag4",	"resize operation_lag6",	"Roles Exceeded_lag3",	"Roles Exceeded_lag5",	"Tns Error_lag2",	"Tns Error_lag4",	"Tns Error_lag6")

fault_Day2 <- lag2_data[names]


dim(fault_Day2)

str(fault_Day2)

summary(fault_Day2)

names(fault_Day2)

fault_Day2$Block <- replace(fault_Day2$Block,fault_Day2$Block>1,1)

fault_Day2$Block <- as.factor(fault_Day2$Block)
fault_Day2$Date <- NULL

library(DMwR)
set.seed(123)
smoted_day2 <- SMOTE(Block~., fault_Day2, perc.over=6000, perc.under=100, k=10, learner=NULL)
table(smoted_day2$Block)



model2 <- glm(Block ~ .,family=binomial,data=smoted_day2)
summary(model2)

#slm2 <- step(model2)
#summary(slm2)
#model13 <- glm
#summary(model2)
#anova(model2, test="Chisq")
#plot(allEffects(model2))
#plot(model2)

predicted2 <- predict(model2, fault_Day2, type="response")  # predicted scores

predicted_file2 <- data.frame(cbind(fault_Day2,predicted2))

predicted_file2 <- data.frame(cbind(lag2_data$Date,predicted_file2))

write.csv(predicted_file2,"C:\\Shiva\\Projects\\GS\\GS_fault\\EDW\\EDW_fault_prediction\\predicted_Block_2.csv")

library(InformationValue)

optCutOff <- optimalCutoff(fault_Day2$Block, predicted2)[1]
#optCutOff <- 0.4


BlocklassError(fault_Day2$Block, predicted2, threshold = optCutOff)

plotROC(fault_Day2$Block, predicted2)

Concordance(fault_Day2$Block, predicted2)

sensitivity(fault_Day2$Block, predicted2, threshold = optCutOff)

specificity(fault_Day2$Block, predicted2, threshold = optCutOff)

confusionMatrix(fault_Day2$Block, predicted2, threshold = optCutOff)



#######################################################################################################################

##--------------------------------------------------Space_Database--------------------------------------------------------------##

##Lag1##

names <- c("Date","Space_Database",	
           "ARCHIVE LOG_lag1",	"ARCHIVE LOG_lag3",	"ARCHIVE LOG_lag5",	"Archived_thread_error_lag2",	"Archived_thread_error_lag4",	"ASMB_lag1",	"ASMB_lag3",	"ASMB_lag5",	"Auto Tuning_lag2",	"Auto Tuning_lag4",	"Dbora Files_lag1",	"Dbora Files_lag3",	"Dbora Files_lag5",	"Diagnostic error_lag2",			
           "ARCHIVE LOG_lag2",	"ARCHIVE LOG_lag4",	"Archived_thread_error_lag1",	"Archived_thread_error_lag3",	"Archived_thread_error_lag5",	"ASMB_lag2",	"ASMB_lag4",	"Auto Tuning_lag1",	"Auto Tuning_lag3",	"Auto Tuning_lag5",	"Dbora Files_lag2",	"Dbora Files_lag4",	"Diagnostic error_lag1",	"Diagnostic error_lag3",			
           "Diagnostic error_lag4",	"Diagonal Adl error_lag1",	"Diagonal Adl error_lag3",	"Diagonal Adl error_lag5",	"Fatal Ni connections_lag2",	"Fatal Ni connections_lag4",	"GCS remote-cache_lag1",	"GCS remote-cache_lag3",	"GCS remote-cache_lag5",	"Inbound Error_lag2",	"Inbound Error_lag4",	"Killing Session note_lag1",	"Killing Session note_lag3",	"Killing Session note_lag5",	"KSFV_lag2",			
           "Diagnostic error_lag5",	"Diagonal Adl error_lag2",	"Diagonal Adl error_lag4",	"Fatal Ni connections_lag1",	"Fatal Ni connections_lag3",	"Fatal Ni connections_lag5",	"GCS remote-cache_lag2",	"GCS remote-cache_lag4",	"Inbound Error_lag1",	"Inbound Error_lag3",	"Inbound Error_lag5",	"Killing Session note_lag2",	"Killing Session note_lag4",	"KSFV_lag1",	"KSFV_lag3",			
           "KSFV_lag4",	"LGWR switch_lag1",	"LGWR switch_lag3",	"LGWR switch_lag5",	"NFS Error_lag2",	"NFS Error_lag4",	"Others_lag1",	"Others_lag3",	"Others_lag5",	"resize operation_lag2",	"resize operation_lag4",	"Roles Exceeded_lag1",	"Roles Exceeded_lag3",	"Roles Exceeded_lag5",	"Tns Error_lag2",	"Tns Error_lag4",		
           "KSFV_lag5",	"LGWR switch_lag2",	"LGWR switch_lag4",	"NFS Error_lag1",	"NFS Error_lag3",	"NFS Error_lag5",	"Others_lag2",	"Others_lag4",	"resize operation_lag1",	"resize operation_lag3",	"resize operation_lag5",	"Roles Exceeded_lag2",	"Roles Exceeded_lag4",	"Tns Error_lag1",	"Tns Error_lag3",	"Tns Error_lag5")

fault_Day1 <- lag1_data[names]


dim(fault_Day1)

str(fault_Day1)

summary(fault_Day1)

names(fault_Day1)

fault_Day1$Space_Database <- replace(fault_Day1$Space_Database,fault_Day1$Space_Database>1,1)

fault_Day1$Space_Database <- as.factor(fault_Day1$Space_Database)
fault_Day1$Date <- NULL

library(DMwR)
set.seed(122)
smoted_day1 <- SMOTE(Space_Database~., fault_Day1, perc.over=6000, perc.under=100, k=10, learner=NULL)
table(smoted_day1$Space_Database)



model1 <- glm(Space_Database ~ .,family=binomial,data=smoted_day1)
summary(model1)

#slm2 <- step(model13)
#summary(slm2)
#model13 <- 
#summary(model13)
#anova(model1, test="Chisq")
#plot(allEffects(model1))
#plot(model1)

predicted1 <- predict(model1, fault_Day1, type="response")  # predicted scores

predicted_file1 <- data.frame(cbind(fault_Day1,predicted1))

predicted_file1 <- data.frame(cbind(lag1_data$Date,predicted_file1))

write.csv(predicted_file1,"C:\\Shiva\\Projects\\GS\\GS_fault\\EDW\\EDW_fault_prediction\\predicted_Space_Database_1.csv")

library(InformationValue)

optCutOff <- optimalCutoff(fault_Day1$Space_Database, predicted1)[1]
#optCutOff <- 0.4


Space_DatabaselassError(fault_Day1$Space_Database, predicted1, threshold = optCutOff)

plotROC(fault_Day1$Space_Database, predicted1)

Concordance(fault_Day1$Space_Database, predicted1)

sensitivity(fault_Day1$Space_Database, predicted1, threshold = optCutOff)

specificity(fault_Day1$Space_Database, predicted1, threshold = optCutOff)

confusionMatrix(fault_Day1$Space_Database, predicted1, threshold = optCutOff)


##################################################################################################################################

names <- c("Date","Space_Database","ARCHIVE LOG_lag2",	"ARCHIVE LOG_lag4",	"ARCHIVE LOG_lag6",	"Archived_thread_error_lag3",	"Archived_thread_error_lag5",	"ASMB_lag2",	"ASMB_lag4",	"ASMB_lag6",	"Auto Tuning_lag3",	"Auto Tuning_lag5",	"Dbora Files_lag2",	"Dbora Files_lag4",	"Dbora Files_lag6",	"Diagnostic error_lag3",			
           "ARCHIVE LOG_lag3",	"ARCHIVE LOG_lag5",	"Archived_thread_error_lag2",	"Archived_thread_error_lag4",	"Archived_thread_error_lag6",	"ASMB_lag3",	"ASMB_lag5",	"Auto Tuning_lag2",	"Auto Tuning_lag4",	"Auto Tuning_lag6",	"Dbora Files_lag3",	"Dbora Files_lag5",	"Diagnostic error_lag2",	"Diagnostic error_lag4",			
           "Diagnostic error_lag5",	"Diagonal Adl error_lag2",	"Diagonal Adl error_lag4",	"Diagonal Adl error_lag6",	"Fatal Ni connections_lag3",	"Fatal Ni connections_lag5",	"GCS remote-cache_lag2",	"GCS remote-cache_lag4",	"GCS remote-cache_lag6",	"Inbound Error_lag3",	"Inbound Error_lag5",	"Killing Session note_lag2",	"Killing Session note_lag4",	"Killing Session note_lag6",	"KSFV_lag3",			
           "Diagnostic error_lag6",	"Diagonal Adl error_lag3",	"Diagonal Adl error_lag5",	"Fatal Ni connections_lag2",	"Fatal Ni connections_lag4",	"Fatal Ni connections_lag6",	"GCS remote-cache_lag3",	"GCS remote-cache_lag5",	"Inbound Error_lag2",	"Inbound Error_lag4",	"Inbound Error_lag6",	"Killing Session note_lag3",	"Killing Session note_lag5",	"KSFV_lag2",	"KSFV_lag4",			
           "KSFV_lag5",	"LGWR switch_lag2",	"LGWR switch_lag4",	"LGWR switch_lag6",	"NFS Error_lag3",	"NFS Error_lag5",	"Others_lag2",	"Others_lag4",	"Others_lag6",	"resize operation_lag3",	"resize operation_lag5",	"Roles Exceeded_lag2",	"Roles Exceeded_lag4",	"Roles Exceeded_lag6",	"Tns Error_lag3",	"Tns Error_lag5",		
           "KSFV_lag6",	"LGWR switch_lag3",	"LGWR switch_lag5",	"NFS Error_lag2",	"NFS Error_lag4",	"NFS Error_lag6",	"Others_lag3",	"Others_lag5",	"resize operation_lag2",	"resize operation_lag4",	"resize operation_lag6",	"Roles Exceeded_lag3",	"Roles Exceeded_lag5",	"Tns Error_lag2",	"Tns Error_lag4",	"Tns Error_lag6")

fault_Day2 <- lag2_data[names]


dim(fault_Day2)

str(fault_Day2)

summary(fault_Day2)

names(fault_Day2)

fault_Day2$Space_Database <- replace(fault_Day2$Space_Database,fault_Day2$Space_Database>1,1)
fault_Day2$Space_Database <- as.factor(fault_Day2$Space_Database)
fault_Day2$Date <- NULL

library(DMwR)
set.seed(123)
smoted_day2 <- SMOTE(Space_Database~., fault_Day2, perc.over=6000, perc.under=100, k=10, learner=NULL)
table(smoted_day2$Space_Database)



model2 <- glm(Space_Database ~ .,family=binomial,data=smoted_day2)
summary(model2)

#slm2 <- step(model2)
#summary(slm2)
#model13 <- glm
#summary(model2)
#anova(model2, test="Chisq")
#plot(allEffects(model2))
#plot(model2)

predicted2 <- predict(model2, fault_Day2, type="response")  # predicted scores

predicted_file2 <- data.frame(cbind(fault_Day2,predicted2))

predicted_file2 <- data.frame(cbind(lag2_data$Date,predicted_file2))

write.csv(predicted_file2,"C:\\Shiva\\Projects\\GS\\GS_fault\\EDW\\EDW_fault_prediction\\predicted_Space_Database_2.csv")

library(InformationValue)

optCutOff <- optimalCutoff(fault_Day2$Space_Database, predicted2)[1]
#optCutOff <- 0.4


Space_DatabaselassError(fault_Day2$Space_Database, predicted2, threshold = optCutOff)

plotROC(fault_Day2$Space_Database, predicted2)

Concordance(fault_Day2$Space_Database, predicted2)

sensitivity(fault_Day2$Space_Database, predicted2, threshold = optCutOff)

specificity(fault_Day2$Space_Database, predicted2, threshold = optCutOff)

confusionMatrix(fault_Day2$Space_Database, predicted2, threshold = optCutOff)



#######################################################################################################################

##--------------------------------------------------Backup--------------------------------------------------------------##

##Lag1##

names <- c("Date","Backup",	
           "ARCHIVE LOG_lag1",	"ARCHIVE LOG_lag3",	"ARCHIVE LOG_lag5",	"Archived_thread_error_lag2",	"Archived_thread_error_lag4",	"ASMB_lag1",	"ASMB_lag3",	"ASMB_lag5",	"Auto Tuning_lag2",	"Auto Tuning_lag4",	"Dbora Files_lag1",	"Dbora Files_lag3",	"Dbora Files_lag5",	"Diagnostic error_lag2",			
           "ARCHIVE LOG_lag2",	"ARCHIVE LOG_lag4",	"Archived_thread_error_lag1",	"Archived_thread_error_lag3",	"Archived_thread_error_lag5",	"ASMB_lag2",	"ASMB_lag4",	"Auto Tuning_lag1",	"Auto Tuning_lag3",	"Auto Tuning_lag5",	"Dbora Files_lag2",	"Dbora Files_lag4",	"Diagnostic error_lag1",	"Diagnostic error_lag3",			
           "Diagnostic error_lag4",	"Diagonal Adl error_lag1",	"Diagonal Adl error_lag3",	"Diagonal Adl error_lag5",	"Fatal Ni connections_lag2",	"Fatal Ni connections_lag4",	"GCS remote-cache_lag1",	"GCS remote-cache_lag3",	"GCS remote-cache_lag5",	"Inbound Error_lag2",	"Inbound Error_lag4",	"Killing Session note_lag1",	"Killing Session note_lag3",	"Killing Session note_lag5",	"KSFV_lag2",			
           "Diagnostic error_lag5",	"Diagonal Adl error_lag2",	"Diagonal Adl error_lag4",	"Fatal Ni connections_lag1",	"Fatal Ni connections_lag3",	"Fatal Ni connections_lag5",	"GCS remote-cache_lag2",	"GCS remote-cache_lag4",	"Inbound Error_lag1",	"Inbound Error_lag3",	"Inbound Error_lag5",	"Killing Session note_lag2",	"Killing Session note_lag4",	"KSFV_lag1",	"KSFV_lag3",			
           "KSFV_lag4",	"LGWR switch_lag1",	"LGWR switch_lag3",	"LGWR switch_lag5",	"NFS Error_lag2",	"NFS Error_lag4",	"Others_lag1",	"Others_lag3",	"Others_lag5",	"resize operation_lag2",	"resize operation_lag4",	"Roles Exceeded_lag1",	"Roles Exceeded_lag3",	"Roles Exceeded_lag5",	"Tns Error_lag2",	"Tns Error_lag4",		
           "KSFV_lag5",	"LGWR switch_lag2",	"LGWR switch_lag4",	"NFS Error_lag1",	"NFS Error_lag3",	"NFS Error_lag5",	"Others_lag2",	"Others_lag4",	"resize operation_lag1",	"resize operation_lag3",	"resize operation_lag5",	"Roles Exceeded_lag2",	"Roles Exceeded_lag4",	"Tns Error_lag1",	"Tns Error_lag3",	"Tns Error_lag5")

fault_Day1 <- lag1_data[names]


dim(fault_Day1)

str(fault_Day1)

summary(fault_Day1)

names(fault_Day1)

fault_Day1$Backup <- replace(fault_Day1$Backup,fault_Day1$Backup>1,1)

fault_Day1$Backup <- as.factor(fault_Day1$Backup)
fault_Day1$Date <- NULL

library(DMwR)
set.seed(122)
smoted_day1 <- SMOTE(Backup~., fault_Day1, perc.over=6000, perc.under=100, k=10, learner=NULL)
table(smoted_day1$Backup)



model1 <- glm(Backup ~ .,family=binomial,data=smoted_day1)
summary(model1)

#slm2 <- step(model13)
#summary(slm2)
#model13 <- 
#summary(model13)
#anova(model1, test="Chisq")
#plot(allEffects(model1))
#plot(model1)

predicted1 <- predict(model1, fault_Day1, type="response")  # predicted scores

predicted_file1 <- data.frame(cbind(fault_Day1,predicted1))

predicted_file1 <- data.frame(cbind(lag1_data$Date,predicted_file1))

write.csv(predicted_file1,"C:\\Shiva\\Projects\\GS\\GS_fault\\EDW\\EDW_fault_prediction\\predicted_Backup_1.csv")

library(InformationValue)

optCutOff <- optimalCutoff(fault_Day1$Backup, predicted1)[1]
#optCutOff <- 0.4


BackuplassError(fault_Day1$Backup, predicted1, threshold = optCutOff)

plotROC(fault_Day1$Backup, predicted1)

Concordance(fault_Day1$Backup, predicted1)

sensitivity(fault_Day1$Backup, predicted1, threshold = optCutOff)

specificity(fault_Day1$Backup, predicted1, threshold = optCutOff)

confusionMatrix(fault_Day1$Backup, predicted1, threshold = optCutOff)


##################################################################################################################################

names <- c("Date","Backup","ARCHIVE LOG_lag2",	"ARCHIVE LOG_lag4",	"ARCHIVE LOG_lag6",	"Archived_thread_error_lag3",	"Archived_thread_error_lag5",	"ASMB_lag2",	"ASMB_lag4",	"ASMB_lag6",	"Auto Tuning_lag3",	"Auto Tuning_lag5",	"Dbora Files_lag2",	"Dbora Files_lag4",	"Dbora Files_lag6",	"Diagnostic error_lag3",			
           "ARCHIVE LOG_lag3",	"ARCHIVE LOG_lag5",	"Archived_thread_error_lag2",	"Archived_thread_error_lag4",	"Archived_thread_error_lag6",	"ASMB_lag3",	"ASMB_lag5",	"Auto Tuning_lag2",	"Auto Tuning_lag4",	"Auto Tuning_lag6",	"Dbora Files_lag3",	"Dbora Files_lag5",	"Diagnostic error_lag2",	"Diagnostic error_lag4",			
           "Diagnostic error_lag5",	"Diagonal Adl error_lag2",	"Diagonal Adl error_lag4",	"Diagonal Adl error_lag6",	"Fatal Ni connections_lag3",	"Fatal Ni connections_lag5",	"GCS remote-cache_lag2",	"GCS remote-cache_lag4",	"GCS remote-cache_lag6",	"Inbound Error_lag3",	"Inbound Error_lag5",	"Killing Session note_lag2",	"Killing Session note_lag4",	"Killing Session note_lag6",	"KSFV_lag3",			
           "Diagnostic error_lag6",	"Diagonal Adl error_lag3",	"Diagonal Adl error_lag5",	"Fatal Ni connections_lag2",	"Fatal Ni connections_lag4",	"Fatal Ni connections_lag6",	"GCS remote-cache_lag3",	"GCS remote-cache_lag5",	"Inbound Error_lag2",	"Inbound Error_lag4",	"Inbound Error_lag6",	"Killing Session note_lag3",	"Killing Session note_lag5",	"KSFV_lag2",	"KSFV_lag4",			
           "KSFV_lag5",	"LGWR switch_lag2",	"LGWR switch_lag4",	"LGWR switch_lag6",	"NFS Error_lag3",	"NFS Error_lag5",	"Others_lag2",	"Others_lag4",	"Others_lag6",	"resize operation_lag3",	"resize operation_lag5",	"Roles Exceeded_lag2",	"Roles Exceeded_lag4",	"Roles Exceeded_lag6",	"Tns Error_lag3",	"Tns Error_lag5",		
           "KSFV_lag6",	"LGWR switch_lag3",	"LGWR switch_lag5",	"NFS Error_lag2",	"NFS Error_lag4",	"NFS Error_lag6",	"Others_lag3",	"Others_lag5",	"resize operation_lag2",	"resize operation_lag4",	"resize operation_lag6",	"Roles Exceeded_lag3",	"Roles Exceeded_lag5",	"Tns Error_lag2",	"Tns Error_lag4",	"Tns Error_lag6")

fault_Day2 <- lag2_data[names]


dim(fault_Day2)

str(fault_Day2)

summary(fault_Day2)

names(fault_Day2)

fault_Day2$Backup <- replace(fault_Day2$Backup,fault_Day2$Backup>1,1)

fault_Day2$Backup <- as.factor(fault_Day2$Backup)
fault_Day2$Date <- NULL

library(DMwR)
set.seed(123)
smoted_day2 <- SMOTE(Backup~., fault_Day2, perc.over=6000, perc.under=100, k=10, learner=NULL)
table(smoted_day2$Backup)



model2 <- glm(Backup ~ .,family=binomial,data=smoted_day2)
summary(model2)

#slm2 <- step(model2)
#summary(slm2)
#model13 <- glm
#summary(model2)
#anova(model2, test="Chisq")
#plot(allEffects(model2))
#plot(model2)

predicted2 <- predict(model2, fault_Day2, type="response")  # predicted scores

predicted_file2 <- data.frame(cbind(fault_Day2,predicted2))

predicted_file2 <- data.frame(cbind(lag2_data$Date,predicted_file2))

write.csv(predicted_file2,"C:\\Shiva\\Projects\\GS\\GS_fault\\EDW\\EDW_fault_prediction\\predicted_Backup_2.csv")

library(InformationValue)

optCutOff <- optimalCutoff(fault_Day2$Backup, predicted2)[1]
#optCutOff <- 0.4


BackuplassError(fault_Day2$Backup, predicted2, threshold = optCutOff)

plotROC(fault_Day2$Backup, predicted2)

Concordance(fault_Day2$Backup, predicted2)

sensitivity(fault_Day2$Backup, predicted2, threshold = optCutOff)

specificity(fault_Day2$Backup, predicted2, threshold = optCutOff)

confusionMatrix(fault_Day2$Backup, predicted2, threshold = optCutOff)




#######################################################################################################################

##--------------------------------------------------Redo--------------------------------------------------------------##

##Lag1##

names <- c("Date","Redo",	
           "ARCHIVE LOG_lag1",	"ARCHIVE LOG_lag3",	"ARCHIVE LOG_lag5",	"Archived_thread_error_lag2",	"Archived_thread_error_lag4",	"ASMB_lag1",	"ASMB_lag3",	"ASMB_lag5",	"Auto Tuning_lag2",	"Auto Tuning_lag4",	"Dbora Files_lag1",	"Dbora Files_lag3",	"Dbora Files_lag5",	"Diagnostic error_lag2",			
           "ARCHIVE LOG_lag2",	"ARCHIVE LOG_lag4",	"Archived_thread_error_lag1",	"Archived_thread_error_lag3",	"Archived_thread_error_lag5",	"ASMB_lag2",	"ASMB_lag4",	"Auto Tuning_lag1",	"Auto Tuning_lag3",	"Auto Tuning_lag5",	"Dbora Files_lag2",	"Dbora Files_lag4",	"Diagnostic error_lag1",	"Diagnostic error_lag3",			
           "Diagnostic error_lag4",	"Diagonal Adl error_lag1",	"Diagonal Adl error_lag3",	"Diagonal Adl error_lag5",	"Fatal Ni connections_lag2",	"Fatal Ni connections_lag4",	"GCS remote-cache_lag1",	"GCS remote-cache_lag3",	"GCS remote-cache_lag5",	"Inbound Error_lag2",	"Inbound Error_lag4",	"Killing Session note_lag1",	"Killing Session note_lag3",	"Killing Session note_lag5",	"KSFV_lag2",			
           "Diagnostic error_lag5",	"Diagonal Adl error_lag2",	"Diagonal Adl error_lag4",	"Fatal Ni connections_lag1",	"Fatal Ni connections_lag3",	"Fatal Ni connections_lag5",	"GCS remote-cache_lag2",	"GCS remote-cache_lag4",	"Inbound Error_lag1",	"Inbound Error_lag3",	"Inbound Error_lag5",	"Killing Session note_lag2",	"Killing Session note_lag4",	"KSFV_lag1",	"KSFV_lag3",			
           "KSFV_lag4",	"LGWR switch_lag1",	"LGWR switch_lag3",	"LGWR switch_lag5",	"NFS Error_lag2",	"NFS Error_lag4",	"Others_lag1",	"Others_lag3",	"Others_lag5",	"resize operation_lag2",	"resize operation_lag4",	"Roles Exceeded_lag1",	"Roles Exceeded_lag3",	"Roles Exceeded_lag5",	"Tns Error_lag2",	"Tns Error_lag4",		
           "KSFV_lag5",	"LGWR switch_lag2",	"LGWR switch_lag4",	"NFS Error_lag1",	"NFS Error_lag3",	"NFS Error_lag5",	"Others_lag2",	"Others_lag4",	"resize operation_lag1",	"resize operation_lag3",	"resize operation_lag5",	"Roles Exceeded_lag2",	"Roles Exceeded_lag4",	"Tns Error_lag1",	"Tns Error_lag3",	"Tns Error_lag5")

fault_Day1 <- lag1_data[names]


dim(fault_Day1)

str(fault_Day1)

summary(fault_Day1)

names(fault_Day1)

fault_Day1$Redo <- replace(fault_Day1$Redo,fault_Day1$Redo>1,1)
fault_Day1$Redo <- as.factor(fault_Day1$Redo)
fault_Day1$Date <- NULL

library(DMwR)
set.seed(122)
smoted_day1 <- SMOTE(Redo~., fault_Day1, perc.over=6000, perc.under=100, k=10, learner=NULL)
table(smoted_day1$Redo)



model1 <- glm(Redo ~ .,family=binomial,data=smoted_day1)
summary(model1)

#slm2 <- step(model13)
#summary(slm2)
#model13 <- 
#summary(model13)
#anova(model1, test="Chisq")
#plot(allEffects(model1))
#plot(model1)

predicted1 <- predict(model1, fault_Day1, type="response")  # predicted scores

predicted_file1 <- data.frame(cbind(fault_Day1,predicted1))

predicted_file1 <- data.frame(cbind(lag1_data$Date,predicted_file1))

write.csv(predicted_file1,"C:\\Shiva\\Projects\\GS\\GS_fault\\EDW\\EDW_fault_prediction\\predicted_Redo_1.csv")

library(InformationValue)

optCutOff <- optimalCutoff(fault_Day1$Redo, predicted1)[1]
#optCutOff <- 0.4


RedolassError(fault_Day1$Redo, predicted1, threshold = optCutOff)

plotROC(fault_Day1$Redo, predicted1)

Concordance(fault_Day1$Redo, predicted1)

sensitivity(fault_Day1$Redo, predicted1, threshold = optCutOff)

specificity(fault_Day1$Redo, predicted1, threshold = optCutOff)

confusionMatrix(fault_Day1$Redo, predicted1, threshold = optCutOff)



##################################################################################################################################

names <- c("Date","Redo","ARCHIVE LOG_lag2",	"ARCHIVE LOG_lag4",	"ARCHIVE LOG_lag6",	"Archived_thread_error_lag3",	"Archived_thread_error_lag5",	"ASMB_lag2",	"ASMB_lag4",	"ASMB_lag6",	"Auto Tuning_lag3",	"Auto Tuning_lag5",	"Dbora Files_lag2",	"Dbora Files_lag4",	"Dbora Files_lag6",	"Diagnostic error_lag3",			
           "ARCHIVE LOG_lag3",	"ARCHIVE LOG_lag5",	"Archived_thread_error_lag2",	"Archived_thread_error_lag4",	"Archived_thread_error_lag6",	"ASMB_lag3",	"ASMB_lag5",	"Auto Tuning_lag2",	"Auto Tuning_lag4",	"Auto Tuning_lag6",	"Dbora Files_lag3",	"Dbora Files_lag5",	"Diagnostic error_lag2",	"Diagnostic error_lag4",			
           "Diagnostic error_lag5",	"Diagonal Adl error_lag2",	"Diagonal Adl error_lag4",	"Diagonal Adl error_lag6",	"Fatal Ni connections_lag3",	"Fatal Ni connections_lag5",	"GCS remote-cache_lag2",	"GCS remote-cache_lag4",	"GCS remote-cache_lag6",	"Inbound Error_lag3",	"Inbound Error_lag5",	"Killing Session note_lag2",	"Killing Session note_lag4",	"Killing Session note_lag6",	"KSFV_lag3",			
           "Diagnostic error_lag6",	"Diagonal Adl error_lag3",	"Diagonal Adl error_lag5",	"Fatal Ni connections_lag2",	"Fatal Ni connections_lag4",	"Fatal Ni connections_lag6",	"GCS remote-cache_lag3",	"GCS remote-cache_lag5",	"Inbound Error_lag2",	"Inbound Error_lag4",	"Inbound Error_lag6",	"Killing Session note_lag3",	"Killing Session note_lag5",	"KSFV_lag2",	"KSFV_lag4",			
           "KSFV_lag5",	"LGWR switch_lag2",	"LGWR switch_lag4",	"LGWR switch_lag6",	"NFS Error_lag3",	"NFS Error_lag5",	"Others_lag2",	"Others_lag4",	"Others_lag6",	"resize operation_lag3",	"resize operation_lag5",	"Roles Exceeded_lag2",	"Roles Exceeded_lag4",	"Roles Exceeded_lag6",	"Tns Error_lag3",	"Tns Error_lag5",		
           "KSFV_lag6",	"LGWR switch_lag3",	"LGWR switch_lag5",	"NFS Error_lag2",	"NFS Error_lag4",	"NFS Error_lag6",	"Others_lag3",	"Others_lag5",	"resize operation_lag2",	"resize operation_lag4",	"resize operation_lag6",	"Roles Exceeded_lag3",	"Roles Exceeded_lag5",	"Tns Error_lag2",	"Tns Error_lag4",	"Tns Error_lag6")

fault_Day2 <- lag2_data[names]


dim(fault_Day2)

str(fault_Day2)

summary(fault_Day2)

names(fault_Day2)

fault_Day2$Redo <- replace(fault_Day2$Redo,fault_Day2$Redo>1,1)
fault_Day2$Redo <- as.factor(fault_Day2$Redo)
fault_Day2$Date <- NULL

library(DMwR)
set.seed(123)
smoted_day2 <- SMOTE(Redo~., fault_Day2, perc.over=6000, perc.under=100, k=10, learner=NULL)
table(smoted_day2$Redo)



model2 <- glm(Redo ~ .,family=binomial,data=smoted_day2)
summary(model2)

#slm2 <- step(model2)
#summary(slm2)
#model13 <- glm
#summary(model2)
#anova(model2, test="Chisq")
#plot(allEffects(model2))
#plot(model2)

predicted2 <- predict(model2, fault_Day2, type="response")  # predicted scores

predicted_file2 <- data.frame(cbind(fault_Day2,predicted2))

predicted_file2 <- data.frame(cbind(lag2_data$Date,predicted_file2))

write.csv(predicted_file2,"C:\\Shiva\\Projects\\GS\\GS_fault\\EDW\\EDW_fault_prediction\\predicted_Redo_2.csv")

library(InformationValue)

optCutOff <- optimalCutoff(fault_Day2$Redo, predicted2)[1]
#optCutOff <- 0.4


RedolassError(fault_Day2$Redo, predicted2, threshold = optCutOff)

plotROC(fault_Day2$Redo, predicted2)

Concordance(fault_Day2$Redo, predicted2)

sensitivity(fault_Day2$Redo, predicted2, threshold = optCutOff)

specificity(fault_Day2$Redo, predicted2, threshold = optCutOff)

confusionMatrix(fault_Day2$Redo, predicted2, threshold = optCutOff)



#######################################################################################################################

##--------------------------------------------------Space_ORA_16nn--------------------------------------------------------------##

##Lag1##

names <- c("Date","Space_ORA_16nn",	
           "ARCHIVE LOG_lag1",	"ARCHIVE LOG_lag3",	"ARCHIVE LOG_lag5",	"Archived_thread_error_lag2",	"Archived_thread_error_lag4",	"ASMB_lag1",	"ASMB_lag3",	"ASMB_lag5",	"Auto Tuning_lag2",	"Auto Tuning_lag4",	"Dbora Files_lag1",	"Dbora Files_lag3",	"Dbora Files_lag5",	"Diagnostic error_lag2",			
           "ARCHIVE LOG_lag2",	"ARCHIVE LOG_lag4",	"Archived_thread_error_lag1",	"Archived_thread_error_lag3",	"Archived_thread_error_lag5",	"ASMB_lag2",	"ASMB_lag4",	"Auto Tuning_lag1",	"Auto Tuning_lag3",	"Auto Tuning_lag5",	"Dbora Files_lag2",	"Dbora Files_lag4",	"Diagnostic error_lag1",	"Diagnostic error_lag3",			
           "Diagnostic error_lag4",	"Diagonal Adl error_lag1",	"Diagonal Adl error_lag3",	"Diagonal Adl error_lag5",	"Fatal Ni connections_lag2",	"Fatal Ni connections_lag4",	"GCS remote-cache_lag1",	"GCS remote-cache_lag3",	"GCS remote-cache_lag5",	"Inbound Error_lag2",	"Inbound Error_lag4",	"Killing Session note_lag1",	"Killing Session note_lag3",	"Killing Session note_lag5",	"KSFV_lag2",			
           "Diagnostic error_lag5",	"Diagonal Adl error_lag2",	"Diagonal Adl error_lag4",	"Fatal Ni connections_lag1",	"Fatal Ni connections_lag3",	"Fatal Ni connections_lag5",	"GCS remote-cache_lag2",	"GCS remote-cache_lag4",	"Inbound Error_lag1",	"Inbound Error_lag3",	"Inbound Error_lag5",	"Killing Session note_lag2",	"Killing Session note_lag4",	"KSFV_lag1",	"KSFV_lag3",			
           "KSFV_lag4",	"LGWR switch_lag1",	"LGWR switch_lag3",	"LGWR switch_lag5",	"NFS Error_lag2",	"NFS Error_lag4",	"Others_lag1",	"Others_lag3",	"Others_lag5",	"resize operation_lag2",	"resize operation_lag4",	"Roles Exceeded_lag1",	"Roles Exceeded_lag3",	"Roles Exceeded_lag5",	"Tns Error_lag2",	"Tns Error_lag4",		
           "KSFV_lag5",	"LGWR switch_lag2",	"LGWR switch_lag4",	"NFS Error_lag1",	"NFS Error_lag3",	"NFS Error_lag5",	"Others_lag2",	"Others_lag4",	"resize operation_lag1",	"resize operation_lag3",	"resize operation_lag5",	"Roles Exceeded_lag2",	"Roles Exceeded_lag4",	"Tns Error_lag1",	"Tns Error_lag3",	"Tns Error_lag5")

fault_Day1 <- lag1_data[names]


dim(fault_Day1)

str(fault_Day1)

summary(fault_Day1)

names(fault_Day1)

fault_Day1$Space_ORA_16nn <- replace(fault_Day1$Space_ORA_16nn,fault_Day1$Space_ORA_16nn>1,1)
fault_Day1$Space_ORA_16nn <- as.factor(fault_Day1$Space_ORA_16nn)
fault_Day1$Date <- NULL

library(DMwR)
set.seed(122)
smoted_day1 <- SMOTE(Space_ORA_16nn~., fault_Day1, perc.over=6000, perc.under=100, k=10, learner=NULL)
table(smoted_day1$Space_ORA_16nn)



model1 <- glm(Space_ORA_16nn ~ .,family=binomial,data=smoted_day1)
summary(model1)

#slm2 <- step(model13)
#summary(slm2)
#model13 <- 
#summary(model13)
#anova(model1, test="Chisq")
#plot(allEffects(model1))
#plot(model1)

predicted1 <- predict(model1, fault_Day1, type="response")  # predicted scores

predicted_file1 <- data.frame(cbind(fault_Day1,predicted1))

predicted_file1 <- data.frame(cbind(lag1_data$Date,predicted_file1))

write.csv(predicted_file1,"C:\\Shiva\\Projects\\GS\\GS_fault\\EDW\\EDW_fault_prediction\\predicted_Space_ORA_16nn_1.csv")

library(InformationValue)

optCutOff <- optimalCutoff(fault_Day1$Space_ORA_16nn, predicted1)[1]
#optCutOff <- 0.4


Space_ORA_16nnlassError(fault_Day1$Space_ORA_16nn, predicted1, threshold = optCutOff)

plotROC(fault_Day1$Space_ORA_16nn, predicted1)

Concordance(fault_Day1$Space_ORA_16nn, predicted1)

sensitivity(fault_Day1$Space_ORA_16nn, predicted1, threshold = optCutOff)

specificity(fault_Day1$Space_ORA_16nn, predicted1, threshold = optCutOff)

confusionMatrix(fault_Day1$Space_ORA_16nn, predicted1, threshold = optCutOff)





##################################################################################################################################

names <- c("Date","Space_ORA_16nn","ARCHIVE LOG_lag2",	"ARCHIVE LOG_lag4",	"ARCHIVE LOG_lag6",	"Archived_thread_error_lag3",	"Archived_thread_error_lag5",	"ASMB_lag2",	"ASMB_lag4",	"ASMB_lag6",	"Auto Tuning_lag3",	"Auto Tuning_lag5",	"Dbora Files_lag2",	"Dbora Files_lag4",	"Dbora Files_lag6",	"Diagnostic error_lag3",			
           "ARCHIVE LOG_lag3",	"ARCHIVE LOG_lag5",	"Archived_thread_error_lag2",	"Archived_thread_error_lag4",	"Archived_thread_error_lag6",	"ASMB_lag3",	"ASMB_lag5",	"Auto Tuning_lag2",	"Auto Tuning_lag4",	"Auto Tuning_lag6",	"Dbora Files_lag3",	"Dbora Files_lag5",	"Diagnostic error_lag2",	"Diagnostic error_lag4",			
           "Diagnostic error_lag5",	"Diagonal Adl error_lag2",	"Diagonal Adl error_lag4",	"Diagonal Adl error_lag6",	"Fatal Ni connections_lag3",	"Fatal Ni connections_lag5",	"GCS remote-cache_lag2",	"GCS remote-cache_lag4",	"GCS remote-cache_lag6",	"Inbound Error_lag3",	"Inbound Error_lag5",	"Killing Session note_lag2",	"Killing Session note_lag4",	"Killing Session note_lag6",	"KSFV_lag3",			
           "Diagnostic error_lag6",	"Diagonal Adl error_lag3",	"Diagonal Adl error_lag5",	"Fatal Ni connections_lag2",	"Fatal Ni connections_lag4",	"Fatal Ni connections_lag6",	"GCS remote-cache_lag3",	"GCS remote-cache_lag5",	"Inbound Error_lag2",	"Inbound Error_lag4",	"Inbound Error_lag6",	"Killing Session note_lag3",	"Killing Session note_lag5",	"KSFV_lag2",	"KSFV_lag4",			
           "KSFV_lag5",	"LGWR switch_lag2",	"LGWR switch_lag4",	"LGWR switch_lag6",	"NFS Error_lag3",	"NFS Error_lag5",	"Others_lag2",	"Others_lag4",	"Others_lag6",	"resize operation_lag3",	"resize operation_lag5",	"Roles Exceeded_lag2",	"Roles Exceeded_lag4",	"Roles Exceeded_lag6",	"Tns Error_lag3",	"Tns Error_lag5",		
           "KSFV_lag6",	"LGWR switch_lag3",	"LGWR switch_lag5",	"NFS Error_lag2",	"NFS Error_lag4",	"NFS Error_lag6",	"Others_lag3",	"Others_lag5",	"resize operation_lag2",	"resize operation_lag4",	"resize operation_lag6",	"Roles Exceeded_lag3",	"Roles Exceeded_lag5",	"Tns Error_lag2",	"Tns Error_lag4",	"Tns Error_lag6")

fault_Day2 <- lag2_data[names]


dim(fault_Day2)

str(fault_Day2)

summary(fault_Day2)

names(fault_Day2)

fault_Day2$Space_ORA_16nn <- replace(fault_Day2$Space_ORA_16nn,fault_Day2$Space_ORA_16nn>1,1)
fault_Day2$Space_ORA_16nn <- as.factor(fault_Day2$Space_ORA_16nn)
fault_Day2$Date <- NULL

library(DMwR)
set.seed(123)
smoted_day2 <- SMOTE(Space_ORA_16nn~., fault_Day2, perc.over=6000, perc.under=100, k=10, learner=NULL)
table(smoted_day2$Space_ORA_16nn)



model2 <- glm(Space_ORA_16nn ~ .,family=binomial,data=smoted_day2)
summary(model2)

#slm2 <- step(model2)
#summary(slm2)
#model13 <- glm
#summary(model2)
#anova(model2, test="Chisq")
#plot(allEffects(model2))
#plot(model2)

predicted2 <- predict(model2, fault_Day2, type="response")  # predicted scores

predicted_file2 <- data.frame(cbind(fault_Day2,predicted2))

predicted_file2 <- data.frame(cbind(lag2_data$Date,predicted_file2))

write.csv(predicted_file2,"C:\\Shiva\\Projects\\GS\\GS_fault\\EDW\\EDW_fault_prediction\\predicted_Space_ORA_16nn_2.csv")

library(InformationValue)

optCutOff <- optimalCutoff(fault_Day2$Space_ORA_16nn, predicted2)[1]
#optCutOff <- 0.4


Space_ORA_16nnlassError(fault_Day2$Space_ORA_16nn, predicted2, threshold = optCutOff)

plotROC(fault_Day2$Space_ORA_16nn, predicted2)

Concordance(fault_Day2$Space_ORA_16nn, predicted2)

sensitivity(fault_Day2$Space_ORA_16nn, predicted2, threshold = optCutOff)

specificity(fault_Day2$Space_ORA_16nn, predicted2, threshold = optCutOff)

confusionMatrix(fault_Day2$Space_ORA_16nn, predicted2, threshold = optCutOff)



#######################################################################################################################

##--------------------------------------------------Space--------------------------------------------------------------##

##Lag1##

names <- c("Date","Space",	
           "ARCHIVE LOG_lag1",	"ARCHIVE LOG_lag3",	"ARCHIVE LOG_lag5",	"Archived_thread_error_lag2",	"Archived_thread_error_lag4",	"ASMB_lag1",	"ASMB_lag3",	"ASMB_lag5",	"Auto Tuning_lag2",	"Auto Tuning_lag4",	"Dbora Files_lag1",	"Dbora Files_lag3",	"Dbora Files_lag5",	"Diagnostic error_lag2",			
           "ARCHIVE LOG_lag2",	"ARCHIVE LOG_lag4",	"Archived_thread_error_lag1",	"Archived_thread_error_lag3",	"Archived_thread_error_lag5",	"ASMB_lag2",	"ASMB_lag4",	"Auto Tuning_lag1",	"Auto Tuning_lag3",	"Auto Tuning_lag5",	"Dbora Files_lag2",	"Dbora Files_lag4",	"Diagnostic error_lag1",	"Diagnostic error_lag3",			
           "Diagnostic error_lag4",	"Diagonal Adl error_lag1",	"Diagonal Adl error_lag3",	"Diagonal Adl error_lag5",	"Fatal Ni connections_lag2",	"Fatal Ni connections_lag4",	"GCS remote-cache_lag1",	"GCS remote-cache_lag3",	"GCS remote-cache_lag5",	"Inbound Error_lag2",	"Inbound Error_lag4",	"Killing Session note_lag1",	"Killing Session note_lag3",	"Killing Session note_lag5",	"KSFV_lag2",			
           "Diagnostic error_lag5",	"Diagonal Adl error_lag2",	"Diagonal Adl error_lag4",	"Fatal Ni connections_lag1",	"Fatal Ni connections_lag3",	"Fatal Ni connections_lag5",	"GCS remote-cache_lag2",	"GCS remote-cache_lag4",	"Inbound Error_lag1",	"Inbound Error_lag3",	"Inbound Error_lag5",	"Killing Session note_lag2",	"Killing Session note_lag4",	"KSFV_lag1",	"KSFV_lag3",			
           "KSFV_lag4",	"LGWR switch_lag1",	"LGWR switch_lag3",	"LGWR switch_lag5",	"NFS Error_lag2",	"NFS Error_lag4",	"Others_lag1",	"Others_lag3",	"Others_lag5",	"resize operation_lag2",	"resize operation_lag4",	"Roles Exceeded_lag1",	"Roles Exceeded_lag3",	"Roles Exceeded_lag5",	"Tns Error_lag2",	"Tns Error_lag4",		
           "KSFV_lag5",	"LGWR switch_lag2",	"LGWR switch_lag4",	"NFS Error_lag1",	"NFS Error_lag3",	"NFS Error_lag5",	"Others_lag2",	"Others_lag4",	"resize operation_lag1",	"resize operation_lag3",	"resize operation_lag5",	"Roles Exceeded_lag2",	"Roles Exceeded_lag4",	"Tns Error_lag1",	"Tns Error_lag3",	"Tns Error_lag5")

fault_Day1 <- lag1_data[names]


dim(fault_Day1)

str(fault_Day1)

summary(fault_Day1)

names(fault_Day1)

fault_Day1$Space <- replace(fault_Day1$Space,fault_Day1$Space>1,1)

fault_Day1$Space <- as.factor(fault_Day1$Space)
fault_Day1$Date <- NULL

library(DMwR)
set.seed(122)
smoted_day1 <- SMOTE(Space~., fault_Day1, perc.over=6000, perc.under=100, k=10, learner=NULL)
table(smoted_day1$Space)



model1 <- glm(Space ~ .,family=binomial,data=smoted_day1)
summary(model1)

#slm2 <- step(model13)
#summary(slm2)
#model13 <- 
#summary(model13)
#anova(model1, test="Chisq")
#plot(allEffects(model1))
#plot(model1)

predicted1 <- predict(model1, fault_Day1, type="response")  # predicted scores

predicted_file1 <- data.frame(cbind(fault_Day1,predicted1))

predicted_file1 <- data.frame(cbind(lag1_data$Date,predicted_file1))

write.csv(predicted_file1,"C:\\Shiva\\Projects\\GS\\GS_fault\\EDW\\EDW_fault_prediction\\predicted_Space_1.csv")

library(InformationValue)

optCutOff <- optimalCutoff(fault_Day1$Space, predicted1)[1]
#optCutOff <- 0.4


SpacelassError(fault_Day1$Space, predicted1, threshold = optCutOff)

plotROC(fault_Day1$Space, predicted1)

Concordance(fault_Day1$Space, predicted1)

sensitivity(fault_Day1$Space, predicted1, threshold = optCutOff)

specificity(fault_Day1$Space, predicted1, threshold = optCutOff)

confusionMatrix(fault_Day1$Space, predicted1, threshold = optCutOff)


##################################################################################################################################

names <- c("Date","Space","ARCHIVE LOG_lag2",	"ARCHIVE LOG_lag4",	"ARCHIVE LOG_lag6",	"Archived_thread_error_lag3",	"Archived_thread_error_lag5",	"ASMB_lag2",	"ASMB_lag4",	"ASMB_lag6",	"Auto Tuning_lag3",	"Auto Tuning_lag5",	"Dbora Files_lag2",	"Dbora Files_lag4",	"Dbora Files_lag6",	"Diagnostic error_lag3",			
           "ARCHIVE LOG_lag3",	"ARCHIVE LOG_lag5",	"Archived_thread_error_lag2",	"Archived_thread_error_lag4",	"Archived_thread_error_lag6",	"ASMB_lag3",	"ASMB_lag5",	"Auto Tuning_lag2",	"Auto Tuning_lag4",	"Auto Tuning_lag6",	"Dbora Files_lag3",	"Dbora Files_lag5",	"Diagnostic error_lag2",	"Diagnostic error_lag4",			
           "Diagnostic error_lag5",	"Diagonal Adl error_lag2",	"Diagonal Adl error_lag4",	"Diagonal Adl error_lag6",	"Fatal Ni connections_lag3",	"Fatal Ni connections_lag5",	"GCS remote-cache_lag2",	"GCS remote-cache_lag4",	"GCS remote-cache_lag6",	"Inbound Error_lag3",	"Inbound Error_lag5",	"Killing Session note_lag2",	"Killing Session note_lag4",	"Killing Session note_lag6",	"KSFV_lag3",			
           "Diagnostic error_lag6",	"Diagonal Adl error_lag3",	"Diagonal Adl error_lag5",	"Fatal Ni connections_lag2",	"Fatal Ni connections_lag4",	"Fatal Ni connections_lag6",	"GCS remote-cache_lag3",	"GCS remote-cache_lag5",	"Inbound Error_lag2",	"Inbound Error_lag4",	"Inbound Error_lag6",	"Killing Session note_lag3",	"Killing Session note_lag5",	"KSFV_lag2",	"KSFV_lag4",			
           "KSFV_lag5",	"LGWR switch_lag2",	"LGWR switch_lag4",	"LGWR switch_lag6",	"NFS Error_lag3",	"NFS Error_lag5",	"Others_lag2",	"Others_lag4",	"Others_lag6",	"resize operation_lag3",	"resize operation_lag5",	"Roles Exceeded_lag2",	"Roles Exceeded_lag4",	"Roles Exceeded_lag6",	"Tns Error_lag3",	"Tns Error_lag5",		
           "KSFV_lag6",	"LGWR switch_lag3",	"LGWR switch_lag5",	"NFS Error_lag2",	"NFS Error_lag4",	"NFS Error_lag6",	"Others_lag3",	"Others_lag5",	"resize operation_lag2",	"resize operation_lag4",	"resize operation_lag6",	"Roles Exceeded_lag3",	"Roles Exceeded_lag5",	"Tns Error_lag2",	"Tns Error_lag4",	"Tns Error_lag6")

fault_Day2 <- lag2_data[names]


dim(fault_Day2)

str(fault_Day2)

summary(fault_Day2)

names(fault_Day2)

fault_Day2$Space <- replace(fault_Day2$Space,fault_Day2$Space>1,1)
fault_Day2$Space <- as.factor(fault_Day2$Space)
fault_Day2$Date <- NULL

library(DMwR)
set.seed(123)
smoted_day2 <- SMOTE(Space~., fault_Day2, perc.over=6000, perc.under=100, k=10, learner=NULL)
table(smoted_day2$Space)



model2 <- glm(Space ~ .,family=binomial,data=smoted_day2)
summary(model2)

#slm2 <- step(model2)
#summary(slm2)
#model13 <- glm
#summary(model2)
#anova(model2, test="Chisq")
#plot(allEffects(model2))
#plot(model2)

predicted2 <- predict(model2, fault_Day2, type="response")  # predicted scores

predicted_file2 <- data.frame(cbind(fault_Day2,predicted2))

predicted_file2 <- data.frame(cbind(lag2_data$Date,predicted_file2))

write.csv(predicted_file2,"C:\\Shiva\\Projects\\GS\\GS_fault\\EDW\\EDW_fault_prediction\\predicted_Space_2.csv")

library(InformationValue)

optCutOff <- optimalCutoff(fault_Day2$Space, predicted2)[1]
#optCutOff <- 0.4


SpacelassError(fault_Day2$Space, predicted2, threshold = optCutOff)

plotROC(fault_Day2$Space, predicted2)

Concordance(fault_Day2$Space, predicted2)

sensitivity(fault_Day2$Space, predicted2, threshold = optCutOff)

specificity(fault_Day2$Space, predicted2, threshold = optCutOff)

confusionMatrix(fault_Day2$Space, predicted2, threshold = optCutOff)



#######################################################################################################################

##--------------------------------------------------User_Access--------------------------------------------------------------##

##Lag1##

names <- c("Date","User_Access",	
           "ARCHIVE LOG_lag1",	"ARCHIVE LOG_lag3",	"ARCHIVE LOG_lag5",	"Archived_thread_error_lag2",	"Archived_thread_error_lag4",	"ASMB_lag1",	"ASMB_lag3",	"ASMB_lag5",	"Auto Tuning_lag2",	"Auto Tuning_lag4",	"Dbora Files_lag1",	"Dbora Files_lag3",	"Dbora Files_lag5",	"Diagnostic error_lag2",			
           "ARCHIVE LOG_lag2",	"ARCHIVE LOG_lag4",	"Archived_thread_error_lag1",	"Archived_thread_error_lag3",	"Archived_thread_error_lag5",	"ASMB_lag2",	"ASMB_lag4",	"Auto Tuning_lag1",	"Auto Tuning_lag3",	"Auto Tuning_lag5",	"Dbora Files_lag2",	"Dbora Files_lag4",	"Diagnostic error_lag1",	"Diagnostic error_lag3",			
           "Diagnostic error_lag4",	"Diagonal Adl error_lag1",	"Diagonal Adl error_lag3",	"Diagonal Adl error_lag5",	"Fatal Ni connections_lag2",	"Fatal Ni connections_lag4",	"GCS remote-cache_lag1",	"GCS remote-cache_lag3",	"GCS remote-cache_lag5",	"Inbound Error_lag2",	"Inbound Error_lag4",	"Killing Session note_lag1",	"Killing Session note_lag3",	"Killing Session note_lag5",	"KSFV_lag2",			
           "Diagnostic error_lag5",	"Diagonal Adl error_lag2",	"Diagonal Adl error_lag4",	"Fatal Ni connections_lag1",	"Fatal Ni connections_lag3",	"Fatal Ni connections_lag5",	"GCS remote-cache_lag2",	"GCS remote-cache_lag4",	"Inbound Error_lag1",	"Inbound Error_lag3",	"Inbound Error_lag5",	"Killing Session note_lag2",	"Killing Session note_lag4",	"KSFV_lag1",	"KSFV_lag3",			
           "KSFV_lag4",	"LGWR switch_lag1",	"LGWR switch_lag3",	"LGWR switch_lag5",	"NFS Error_lag2",	"NFS Error_lag4",	"Others_lag1",	"Others_lag3",	"Others_lag5",	"resize operation_lag2",	"resize operation_lag4",	"Roles Exceeded_lag1",	"Roles Exceeded_lag3",	"Roles Exceeded_lag5",	"Tns Error_lag2",	"Tns Error_lag4",		
           "KSFV_lag5",	"LGWR switch_lag2",	"LGWR switch_lag4",	"NFS Error_lag1",	"NFS Error_lag3",	"NFS Error_lag5",	"Others_lag2",	"Others_lag4",	"resize operation_lag1",	"resize operation_lag3",	"resize operation_lag5",	"Roles Exceeded_lag2",	"Roles Exceeded_lag4",	"Tns Error_lag1",	"Tns Error_lag3",	"Tns Error_lag5")

fault_Day1 <- lag1_data[names]


dim(fault_Day1)

str(fault_Day1)

summary(fault_Day1)

names(fault_Day1)

fault_Day1$User_Access <- replace(fault_Day1$User_Access,fault_Day1$User_Access>1,1)

fault_Day1$User_Access <- as.factor(fault_Day1$User_Access)
fault_Day1$Date <- NULL

library(DMwR)
set.seed(122)
smoted_day1 <- SMOTE(User_Access~., fault_Day1, perc.over=6000, perc.under=100, k=10, learner=NULL)
table(smoted_day1$User_Access)



model1 <- glm(User_Access ~ .,family=binomial,data=smoted_day1)
summary(model1)

#slm2 <- step(model13)
#summary(slm2)
#model13 <- 
#summary(model13)
#anova(model1, test="Chisq")
#plot(allEffects(model1))
#plot(model1)

predicted1 <- predict(model1, fault_Day1, type="response")  # predicted scores

predicted_file1 <- data.frame(cbind(fault_Day1,predicted1))

predicted_file1 <- data.frame(cbind(lag1_data$Date,predicted_file1))

write.csv(predicted_file1,"C:\\Shiva\\Projects\\GS\\GS_fault\\EDW\\EDW_fault_prediction\\predicted_User_Access_1.csv")

library(InformationValue)

optCutOff <- optimalCutoff(fault_Day1$User_Access, predicted1)[1]
#optCutOff <- 0.4


User_AccesslassError(fault_Day1$User_Access, predicted1, threshold = optCutOff)

plotROC(fault_Day1$User_Access, predicted1)

Concordance(fault_Day1$User_Access, predicted1)

sensitivity(fault_Day1$User_Access, predicted1, threshold = optCutOff)

specificity(fault_Day1$User_Access, predicted1, threshold = optCutOff)

confusionMatrix(fault_Day1$User_Access, predicted1, threshold = optCutOff)


##################################################################################################################################

names <- c("Date","User_Access","ARCHIVE LOG_lag2",	"ARCHIVE LOG_lag4",	"ARCHIVE LOG_lag6",	"Archived_thread_error_lag3",	"Archived_thread_error_lag5",	"ASMB_lag2",	"ASMB_lag4",	"ASMB_lag6",	"Auto Tuning_lag3",	"Auto Tuning_lag5",	"Dbora Files_lag2",	"Dbora Files_lag4",	"Dbora Files_lag6",	"Diagnostic error_lag3",			
           "ARCHIVE LOG_lag3",	"ARCHIVE LOG_lag5",	"Archived_thread_error_lag2",	"Archived_thread_error_lag4",	"Archived_thread_error_lag6",	"ASMB_lag3",	"ASMB_lag5",	"Auto Tuning_lag2",	"Auto Tuning_lag4",	"Auto Tuning_lag6",	"Dbora Files_lag3",	"Dbora Files_lag5",	"Diagnostic error_lag2",	"Diagnostic error_lag4",			
           "Diagnostic error_lag5",	"Diagonal Adl error_lag2",	"Diagonal Adl error_lag4",	"Diagonal Adl error_lag6",	"Fatal Ni connections_lag3",	"Fatal Ni connections_lag5",	"GCS remote-cache_lag2",	"GCS remote-cache_lag4",	"GCS remote-cache_lag6",	"Inbound Error_lag3",	"Inbound Error_lag5",	"Killing Session note_lag2",	"Killing Session note_lag4",	"Killing Session note_lag6",	"KSFV_lag3",			
           "Diagnostic error_lag6",	"Diagonal Adl error_lag3",	"Diagonal Adl error_lag5",	"Fatal Ni connections_lag2",	"Fatal Ni connections_lag4",	"Fatal Ni connections_lag6",	"GCS remote-cache_lag3",	"GCS remote-cache_lag5",	"Inbound Error_lag2",	"Inbound Error_lag4",	"Inbound Error_lag6",	"Killing Session note_lag3",	"Killing Session note_lag5",	"KSFV_lag2",	"KSFV_lag4",			
           "KSFV_lag5",	"LGWR switch_lag2",	"LGWR switch_lag4",	"LGWR switch_lag6",	"NFS Error_lag3",	"NFS Error_lag5",	"Others_lag2",	"Others_lag4",	"Others_lag6",	"resize operation_lag3",	"resize operation_lag5",	"Roles Exceeded_lag2",	"Roles Exceeded_lag4",	"Roles Exceeded_lag6",	"Tns Error_lag3",	"Tns Error_lag5",		
           "KSFV_lag6",	"LGWR switch_lag3",	"LGWR switch_lag5",	"NFS Error_lag2",	"NFS Error_lag4",	"NFS Error_lag6",	"Others_lag3",	"Others_lag5",	"resize operation_lag2",	"resize operation_lag4",	"resize operation_lag6",	"Roles Exceeded_lag3",	"Roles Exceeded_lag5",	"Tns Error_lag2",	"Tns Error_lag4",	"Tns Error_lag6")

fault_Day2 <- lag2_data[names]


dim(fault_Day2)

str(fault_Day2)

summary(fault_Day2)

names(fault_Day2)

fault_Day2$User_Access <- replace(fault_Day2$User_Access,fault_Day2$User_Access>1,1)
fault_Day2$User_Access <- as.factor(fault_Day2$User_Access)
fault_Day2$Date <- NULL

library(DMwR)
set.seed(123)
smoted_day2 <- SMOTE(User_Access~., fault_Day2, perc.over=6000, perc.under=100, k=10, learner=NULL)
table(smoted_day2$User_Access)



model2 <- glm(User_Access ~ .,family=binomial,data=smoted_day2)
summary(model2)

#slm2 <- step(model2)
#summary(slm2)
#model13 <- glm
#summary(model2)
#anova(model2, test="Chisq")
#plot(allEffects(model2))
#plot(model2)

predicted2 <- predict(model2, fault_Day2, type="response")  # predicted scores

predicted_file2 <- data.frame(cbind(fault_Day2,predicted2))

predicted_file2 <- data.frame(cbind(lag2_data$Date,predicted_file2))

write.csv(predicted_file2,"C:\\Shiva\\Projects\\GS\\GS_fault\\EDW\\EDW_fault_prediction\\predicted_User_Access_2.csv")

library(InformationValue)

optCutOff <- optimalCutoff(fault_Day2$User_Access, predicted2)[1]
#optCutOff <- 0.4


User_AccesslassError(fault_Day2$User_Access, predicted2, threshold = optCutOff)

plotROC(fault_Day2$User_Access, predicted2)

Concordance(fault_Day2$User_Access, predicted2)

sensitivity(fault_Day2$User_Access, predicted2, threshold = optCutOff)

specificity(fault_Day2$User_Access, predicted2, threshold = optCutOff)

confusionMatrix(fault_Day2$User_Access, predicted2, threshold = optCutOff)


#######################################################################################################################

##--------------------------------------------------Blank--------------------------------------------------------------##

##Lag1##

names <- c("Date","Blank",	
           "ARCHIVE LOG_lag1",	"ARCHIVE LOG_lag3",	"ARCHIVE LOG_lag5",	"Archived_thread_error_lag2",	"Archived_thread_error_lag4",	"ASMB_lag1",	"ASMB_lag3",	"ASMB_lag5",	"Auto Tuning_lag2",	"Auto Tuning_lag4",	"Dbora Files_lag1",	"Dbora Files_lag3",	"Dbora Files_lag5",	"Diagnostic error_lag2",			
           "ARCHIVE LOG_lag2",	"ARCHIVE LOG_lag4",	"Archived_thread_error_lag1",	"Archived_thread_error_lag3",	"Archived_thread_error_lag5",	"ASMB_lag2",	"ASMB_lag4",	"Auto Tuning_lag1",	"Auto Tuning_lag3",	"Auto Tuning_lag5",	"Dbora Files_lag2",	"Dbora Files_lag4",	"Diagnostic error_lag1",	"Diagnostic error_lag3",			
           "Diagnostic error_lag4",	"Diagonal Adl error_lag1",	"Diagonal Adl error_lag3",	"Diagonal Adl error_lag5",	"Fatal Ni connections_lag2",	"Fatal Ni connections_lag4",	"GCS remote-cache_lag1",	"GCS remote-cache_lag3",	"GCS remote-cache_lag5",	"Inbound Error_lag2",	"Inbound Error_lag4",	"Killing Session note_lag1",	"Killing Session note_lag3",	"Killing Session note_lag5",	"KSFV_lag2",			
           "Diagnostic error_lag5",	"Diagonal Adl error_lag2",	"Diagonal Adl error_lag4",	"Fatal Ni connections_lag1",	"Fatal Ni connections_lag3",	"Fatal Ni connections_lag5",	"GCS remote-cache_lag2",	"GCS remote-cache_lag4",	"Inbound Error_lag1",	"Inbound Error_lag3",	"Inbound Error_lag5",	"Killing Session note_lag2",	"Killing Session note_lag4",	"KSFV_lag1",	"KSFV_lag3",			
           "KSFV_lag4",	"LGWR switch_lag1",	"LGWR switch_lag3",	"LGWR switch_lag5",	"NFS Error_lag2",	"NFS Error_lag4",	"Others_lag1",	"Others_lag3",	"Others_lag5",	"resize operation_lag2",	"resize operation_lag4",	"Roles Exceeded_lag1",	"Roles Exceeded_lag3",	"Roles Exceeded_lag5",	"Tns Error_lag2",	"Tns Error_lag4",		
           "KSFV_lag5",	"LGWR switch_lag2",	"LGWR switch_lag4",	"NFS Error_lag1",	"NFS Error_lag3",	"NFS Error_lag5",	"Others_lag2",	"Others_lag4",	"resize operation_lag1",	"resize operation_lag3",	"resize operation_lag5",	"Roles Exceeded_lag2",	"Roles Exceeded_lag4",	"Tns Error_lag1",	"Tns Error_lag3",	"Tns Error_lag5")

fault_Day1 <- lag1_data[names]


dim(fault_Day1)

str(fault_Day1)

summary(fault_Day1)

names(fault_Day1)
fault_Day1$Blank <- replace(fault_Day1$Blank,fault_Day1$Blank>1,1)
fault_Day1$Blank <- as.factor(fault_Day1$Blank)
fault_Day1$Date <- NULL

library(DMwR)
set.seed(122)
smoted_day1 <- SMOTE(Blank~., fault_Day1, perc.over=6000, perc.under=100, k=10, learner=NULL)
table(smoted_day1$Blank)



model1 <- glm(Blank ~ .,family=binomial,data=smoted_day1)
summary(model1)

#slm2 <- step(model13)
#summary(slm2)
#model13 <- 
#summary(model13)
#anova(model1, test="Chisq")
#plot(allEffects(model1))
#plot(model1)

predicted1 <- predict(model1, fault_Day1, type="response")  # predicted scores

predicted_file1 <- data.frame(cbind(fault_Day1,predicted1))

predicted_file1 <- data.frame(cbind(lag1_data$Date,predicted_file1))

write.csv(predicted_file1,"C:\\Shiva\\Projects\\GS\\GS_fault\\EDW\\EDW_fault_prediction\\predicted_Blank_1.csv")

library(InformationValue)

optCutOff <- optimalCutoff(fault_Day1$Blank, predicted1)[1]
#optCutOff <- 0.4


(fault_Day1$Blank, predicted1, threshold = optCutOff)

plotROC(fault_Day1$Blank, predicted1)

Concordance(fault_Day1$Blank, predicted1)

sensitivity(fault_Day1$Blank, predicted1, threshold = optCutOff)

specificity(fault_Day1$Blank, predicted1, threshold = optCutOff)

confusionMatrix(fault_Day1$Blank, predicted1, threshold = optCutOff)


##################################################################################################################################

names <- c("Date","Blank","ARCHIVE LOG_lag2",	"ARCHIVE LOG_lag4",	"ARCHIVE LOG_lag6",	"Archived_thread_error_lag3",	"Archived_thread_error_lag5",	"ASMB_lag2",	"ASMB_lag4",	"ASMB_lag6",	"Auto Tuning_lag3",	"Auto Tuning_lag5",	"Dbora Files_lag2",	"Dbora Files_lag4",	"Dbora Files_lag6",	"Diagnostic error_lag3",			
           "ARCHIVE LOG_lag3",	"ARCHIVE LOG_lag5",	"Archived_thread_error_lag2",	"Archived_thread_error_lag4",	"Archived_thread_error_lag6",	"ASMB_lag3",	"ASMB_lag5",	"Auto Tuning_lag2",	"Auto Tuning_lag4",	"Auto Tuning_lag6",	"Dbora Files_lag3",	"Dbora Files_lag5",	"Diagnostic error_lag2",	"Diagnostic error_lag4",			
           "Diagnostic error_lag5",	"Diagonal Adl error_lag2",	"Diagonal Adl error_lag4",	"Diagonal Adl error_lag6",	"Fatal Ni connections_lag3",	"Fatal Ni connections_lag5",	"GCS remote-cache_lag2",	"GCS remote-cache_lag4",	"GCS remote-cache_lag6",	"Inbound Error_lag3",	"Inbound Error_lag5",	"Killing Session note_lag2",	"Killing Session note_lag4",	"Killing Session note_lag6",	"KSFV_lag3",			
           "Diagnostic error_lag6",	"Diagonal Adl error_lag3",	"Diagonal Adl error_lag5",	"Fatal Ni connections_lag2",	"Fatal Ni connections_lag4",	"Fatal Ni connections_lag6",	"GCS remote-cache_lag3",	"GCS remote-cache_lag5",	"Inbound Error_lag2",	"Inbound Error_lag4",	"Inbound Error_lag6",	"Killing Session note_lag3",	"Killing Session note_lag5",	"KSFV_lag2",	"KSFV_lag4",			
           "KSFV_lag5",	"LGWR switch_lag2",	"LGWR switch_lag4",	"LGWR switch_lag6",	"NFS Error_lag3",	"NFS Error_lag5",	"Others_lag2",	"Others_lag4",	"Others_lag6",	"resize operation_lag3",	"resize operation_lag5",	"Roles Exceeded_lag2",	"Roles Exceeded_lag4",	"Roles Exceeded_lag6",	"Tns Error_lag3",	"Tns Error_lag5",		
           "KSFV_lag6",	"LGWR switch_lag3",	"LGWR switch_lag5",	"NFS Error_lag2",	"NFS Error_lag4",	"NFS Error_lag6",	"Others_lag3",	"Others_lag5",	"resize operation_lag2",	"resize operation_lag4",	"resize operation_lag6",	"Roles Exceeded_lag3",	"Roles Exceeded_lag5",	"Tns Error_lag2",	"Tns Error_lag4",	"Tns Error_lag6")

fault_Day2 <- lag2_data[names]


dim(fault_Day2)

str(fault_Day2)

summary(fault_Day2)

names(fault_Day2)

fault_Day2$Blank <- replace(fault_Day2$Blank,fault_Day2$Blank>1,1)
fault_Day2$Blank <- as.factor(fault_Day2$Blank)
fault_Day2$Date <- NULL

library(DMwR)
set.seed(123)
smoted_day2 <- SMOTE(Blank~., fault_Day2, perc.over=6000, perc.under=100, k=10, learner=NULL)
table(smoted_day2$Blank)



model2 <- glm(Blank ~ .,family=binomial,data=smoted_day2)
summary(model2)

#slm2 <- step(model2)
#summary(slm2)
#model13 <- glm
#summary(model2)
#anova(model2, test="Chisq")
#plot(allEffects(model2))
#plot(model2)

predicted2 <- predict(model2, fault_Day2, type="response")  # predicted scores

predicted_file2 <- data.frame(cbind(fault_Day2,predicted2))

predicted_file2 <- data.frame(cbind(lag2_data$Date,predicted_file2))

write.csv(predicted_file2,"C:\\Shiva\\Projects\\GS\\GS_fault\\EDW\\EDW_fault_prediction\\predicted_Blank_2.csv")

library(InformationValue)

optCutOff <- optimalCutoff(fault_Day2$Blank, predicted2)[1]
#optCutOff <- 0.4


BlanklassError(fault_Day2$Blank, predicted2, threshold = optCutOff)

plotROC(fault_Day2$Blank, predicted2)

Concordance(fault_Day2$Blank, predicted2)

sensitivity(fault_Day2$Blank, predicted2, threshold = optCutOff)

specificity(fault_Day2$Blank, predicted2, threshold = optCutOff)

confusionMatrix(fault_Day2$Blank, predicted2, threshold = optCutOff)


#######################################################################################################################

##--------------------------------------------------DB_Access--------------------------------------------------------------##

##Lag1##

names <- c("Date","DB_Access",	
           "ARCHIVE LOG_lag1",	"ARCHIVE LOG_lag3",	"ARCHIVE LOG_lag5",	"Archived_thread_error_lag2",	"Archived_thread_error_lag4",	"ASMB_lag1",	"ASMB_lag3",	"ASMB_lag5",	"Auto Tuning_lag2",	"Auto Tuning_lag4",	"Dbora Files_lag1",	"Dbora Files_lag3",	"Dbora Files_lag5",	"Diagnostic error_lag2",			
           "ARCHIVE LOG_lag2",	"ARCHIVE LOG_lag4",	"Archived_thread_error_lag1",	"Archived_thread_error_lag3",	"Archived_thread_error_lag5",	"ASMB_lag2",	"ASMB_lag4",	"Auto Tuning_lag1",	"Auto Tuning_lag3",	"Auto Tuning_lag5",	"Dbora Files_lag2",	"Dbora Files_lag4",	"Diagnostic error_lag1",	"Diagnostic error_lag3",			
           "Diagnostic error_lag4",	"Diagonal Adl error_lag1",	"Diagonal Adl error_lag3",	"Diagonal Adl error_lag5",	"Fatal Ni connections_lag2",	"Fatal Ni connections_lag4",	"GCS remote-cache_lag1",	"GCS remote-cache_lag3",	"GCS remote-cache_lag5",	"Inbound Error_lag2",	"Inbound Error_lag4",	"Killing Session note_lag1",	"Killing Session note_lag3",	"Killing Session note_lag5",	"KSFV_lag2",			
           "Diagnostic error_lag5",	"Diagonal Adl error_lag2",	"Diagonal Adl error_lag4",	"Fatal Ni connections_lag1",	"Fatal Ni connections_lag3",	"Fatal Ni connections_lag5",	"GCS remote-cache_lag2",	"GCS remote-cache_lag4",	"Inbound Error_lag1",	"Inbound Error_lag3",	"Inbound Error_lag5",	"Killing Session note_lag2",	"Killing Session note_lag4",	"KSFV_lag1",	"KSFV_lag3",			
           "KSFV_lag4",	"LGWR switch_lag1",	"LGWR switch_lag3",	"LGWR switch_lag5",	"NFS Error_lag2",	"NFS Error_lag4",	"Others_lag1",	"Others_lag3",	"Others_lag5",	"resize operation_lag2",	"resize operation_lag4",	"Roles Exceeded_lag1",	"Roles Exceeded_lag3",	"Roles Exceeded_lag5",	"Tns Error_lag2",	"Tns Error_lag4",		
           "KSFV_lag5",	"LGWR switch_lag2",	"LGWR switch_lag4",	"NFS Error_lag1",	"NFS Error_lag3",	"NFS Error_lag5",	"Others_lag2",	"Others_lag4",	"resize operation_lag1",	"resize operation_lag3",	"resize operation_lag5",	"Roles Exceeded_lag2",	"Roles Exceeded_lag4",	"Tns Error_lag1",	"Tns Error_lag3",	"Tns Error_lag5")

fault_Day1 <- lag1_data[names]


dim(fault_Day1)

str(fault_Day1)

summary(fault_Day1)

names(fault_Day1)

fault_Day1$DB_Access <- replace(fault_Day1$DB_Access,fault_Day1$DB_Access>1,1)
fault_Day1$DB_Access <- as.factor(fault_Day1$DB_Access)
fault_Day1$Date <- NULL

library(DMwR)
set.seed(122)
smoted_day1 <- SMOTE(DB_Access~., fault_Day1, perc.over=6000, perc.under=100, k=10, learner=NULL)
table(smoted_day1$DB_Access)



model1 <- glm(DB_Access ~ .,family=binomial,data=smoted_day1)
summary(model1)

#slm2 <- step(model13)
#summary(slm2)
#model13 <- 
#summary(model13)
#anova(model1, test="Chisq")
#plot(allEffects(model1))
#plot(model1)

predicted1 <- predict(model1, fault_Day1, type="response")  # predicted scores

predicted_file1 <- data.frame(cbind(fault_Day1,predicted1))

predicted_file1 <- data.frame(cbind(lag1_data$Date,predicted_file1))

write.csv(predicted_file1,"C:\\Shiva\\Projects\\GS\\GS_fault\\EDW\\EDW_fault_prediction\\predicted_DB_Access_1.csv")

library(InformationValue)

optCutOff <- optimalCutoff(fault_Day1$DB_Access, predicted1)[1]
#optCutOff <- 0.4


DB_AccesslassError(fault_Day1$DB_Access, predicted1, threshold = optCutOff)

plotROC(fault_Day1$DB_Access, predicted1)

Concordance(fault_Day1$DB_Access, predicted1)

sensitivity(fault_Day1$DB_Access, predicted1, threshold = optCutOff)

specificity(fault_Day1$DB_Access, predicted1, threshold = optCutOff)

confusionMatrix(fault_Day1$DB_Access, predicted1, threshold = optCutOff)


##################################################################################################################################
names <- c("Date","DB_Access","ARCHIVE LOG_lag2",	"ARCHIVE LOG_lag4",	"ARCHIVE LOG_lag6",	"Archived_thread_error_lag3",	"Archived_thread_error_lag5",	"ASMB_lag2",	"ASMB_lag4",	"ASMB_lag6",	"Auto Tuning_lag3",	"Auto Tuning_lag5",	"Dbora Files_lag2",	"Dbora Files_lag4",	"Dbora Files_lag6",	"Diagnostic error_lag3",			
           "ARCHIVE LOG_lag3",	"ARCHIVE LOG_lag5",	"Archived_thread_error_lag2",	"Archived_thread_error_lag4",	"Archived_thread_error_lag6",	"ASMB_lag3",	"ASMB_lag5",	"Auto Tuning_lag2",	"Auto Tuning_lag4",	"Auto Tuning_lag6",	"Dbora Files_lag3",	"Dbora Files_lag5",	"Diagnostic error_lag2",	"Diagnostic error_lag4",			
           "Diagnostic error_lag5",	"Diagonal Adl error_lag2",	"Diagonal Adl error_lag4",	"Diagonal Adl error_lag6",	"Fatal Ni connections_lag3",	"Fatal Ni connections_lag5",	"GCS remote-cache_lag2",	"GCS remote-cache_lag4",	"GCS remote-cache_lag6",	"Inbound Error_lag3",	"Inbound Error_lag5",	"Killing Session note_lag2",	"Killing Session note_lag4",	"Killing Session note_lag6",	"KSFV_lag3",			
           "Diagnostic error_lag6",	"Diagonal Adl error_lag3",	"Diagonal Adl error_lag5",	"Fatal Ni connections_lag2",	"Fatal Ni connections_lag4",	"Fatal Ni connections_lag6",	"GCS remote-cache_lag3",	"GCS remote-cache_lag5",	"Inbound Error_lag2",	"Inbound Error_lag4",	"Inbound Error_lag6",	"Killing Session note_lag3",	"Killing Session note_lag5",	"KSFV_lag2",	"KSFV_lag4",			
           "KSFV_lag5",	"LGWR switch_lag2",	"LGWR switch_lag4",	"LGWR switch_lag6",	"NFS Error_lag3",	"NFS Error_lag5",	"Others_lag2",	"Others_lag4",	"Others_lag6",	"resize operation_lag3",	"resize operation_lag5",	"Roles Exceeded_lag2",	"Roles Exceeded_lag4",	"Roles Exceeded_lag6",	"Tns Error_lag3",	"Tns Error_lag5",		
           "KSFV_lag6",	"LGWR switch_lag3",	"LGWR switch_lag5",	"NFS Error_lag2",	"NFS Error_lag4",	"NFS Error_lag6",	"Others_lag3",	"Others_lag5",	"resize operation_lag2",	"resize operation_lag4",	"resize operation_lag6",	"Roles Exceeded_lag3",	"Roles Exceeded_lag5",	"Tns Error_lag2",	"Tns Error_lag4",	"Tns Error_lag6")

fault_Day2 <- lag2_data[names]


dim(fault_Day2)

str(fault_Day2)

summary(fault_Day2)

names(fault_Day2)

fault_Day2$DB_Access <- replace(fault_Day2$DB_Access,fault_Day2$DB_Access>1,1)
fault_Day2$DB_Access <- as.factor(fault_Day2$DB_Access)
fault_Day2$Date <- NULL

library(DMwR)
set.seed(123)
smoted_day2 <- SMOTE(DB_Access~., fault_Day2, perc.over=6000, perc.under=100, k=10, learner=NULL)
table(smoted_day2$DB_Access)



model2 <- glm(DB_Access ~ .,family=binomial,data=smoted_day2)
summary(model2)

#slm2 <- step(model2)
#summary(slm2)
#model13 <- glm
#summary(model2)
#anova(model2, test="Chisq")
#plot(allEffects(model2))
#plot(model2)

predicted2 <- predict(model2, fault_Day2, type="response")  # predicted scores

predicted_file2 <- data.frame(cbind(fault_Day2,predicted2))

predicted_file2 <- data.frame(cbind(lag2_data$Date,predicted_file2))

write.csv(predicted_file2,"C:\\Shiva\\Projects\\GS\\GS_fault\\EDW\\EDW_fault_prediction\\predicted_DB_Access_2.csv")

library(InformationValue)

optCutOff <- optimalCutoff(fault_Day2$DB_Access, predicted2)[1]
#optCutOff <- 0.4


DB_AccesslassError(fault_Day2$DB_Access, predicted2, threshold = optCutOff)

plotROC(fault_Day2$DB_Access, predicted2)

Concordance(fault_Day2$DB_Access, predicted2)

sensitivity(fault_Day2$DB_Access, predicted2, threshold = optCutOff)

specificity(fault_Day2$DB_Access, predicted2, threshold = optCutOff)

confusionMatrix(fault_Day2$DB_Access, predicted2, threshold = optCutOff)



#######################################################################################################################

####################--------------------------predicting 12th May 2017----------------------------###############################################
##-----------------------Lag1------------------##

EDW_LDA_Class_test <- read.csv("C:\\Shiva\\Projects\\GS\\GS_fault\\EDW\\EDW_fault_prediction\\Faults_out\\LDA_class_Test_lag2.csv",stringsAsFactors = F,header = TRUE,check.names = FALSE)
dim(EDW_LDA_Class_test)
str(EDW_LDA_Class_test)


#converting date format##
EDW_LDA_Class_test$Date <- as.Date(EDW_LDA_Class_test$Date, "%d/%m/%Y")
#EDW_LDA_Class_test1 <- EDW_LDA_Class_test

#EDW_LDA_Class_test$Date <- NULL

col=2:dim(EDW_LDA_Class_test)[2]
j=20
for(i in col)
{
  print(i)
  table3 <- as.data.frame(EDW_LDA_Class_test %>% 
                            mutate(lag_1=as.numeric(Lag(EDW_LDA_Class_test[,i],1)),
                                   lag_2=as.numeric(Lag(EDW_LDA_Class_test[,i],2)),
                                   lag_3=as.numeric(Lag(EDW_LDA_Class_test[,i],3)),
                                   lag_4=as.numeric(Lag(EDW_LDA_Class_test[,i],4)),
                                   lag_5=as.numeric(Lag(EDW_LDA_Class_test[,i],5))))
  
  colnames(table3)=c(colnames(EDW_LDA_Class_test),paste(colnames(EDW_LDA_Class_test)[i],"_lag",1:5,sep=""))
  vec=j+4
  EDW_LDA_Class_test=cbind(EDW_LDA_Class_test,table3[,j:vec])
  rm(table3)
  j=j+5
}

names1 <- c("Date","ARCHIVE LOG_lag1",	"ARCHIVE LOG_lag3",	"ARCHIVE LOG_lag5",	"Archived_thread_error_lag2",	"Archived_thread_error_lag4",	"ASMB_lag1",	"ASMB_lag3",	"ASMB_lag5",	"Auto Tuning_lag2",	"Auto Tuning_lag4",	"Dbora Files_lag1",	"Dbora Files_lag3",	"Dbora Files_lag5",	"Diagnostic error_lag2",			
            "ARCHIVE LOG_lag2",	"ARCHIVE LOG_lag4",	"Archived_thread_error_lag1",	"Archived_thread_error_lag3",	"Archived_thread_error_lag5",	"ASMB_lag2",	"ASMB_lag4",	"Auto Tuning_lag1",	"Auto Tuning_lag3",	"Auto Tuning_lag5",	"Dbora Files_lag2",	"Dbora Files_lag4",	"Diagnostic error_lag1",	"Diagnostic error_lag3",			
            "Diagnostic error_lag4",	"Diagonal Adl error_lag1",	"Diagonal Adl error_lag3",	"Diagonal Adl error_lag5",	"Fatal Ni connections_lag2",	"Fatal Ni connections_lag4",	"GCS remote-cache_lag1",	"GCS remote-cache_lag3",	"GCS remote-cache_lag5",	"Inbound Error_lag2",	"Inbound Error_lag4",	"Killing Session note_lag1",	"Killing Session note_lag3",	"Killing Session note_lag5",	"KSFV_lag2",			
            "Diagnostic error_lag5",	"Diagonal Adl error_lag2",	"Diagonal Adl error_lag4",	"Fatal Ni connections_lag1",	"Fatal Ni connections_lag3",	"Fatal Ni connections_lag5",	"GCS remote-cache_lag2",	"GCS remote-cache_lag4",	"Inbound Error_lag1",	"Inbound Error_lag3",	"Inbound Error_lag5",	"Killing Session note_lag2",	"Killing Session note_lag4",	"KSFV_lag1",	"KSFV_lag3",			
            "KSFV_lag4",	"LGWR switch_lag1",	"LGWR switch_lag3",	"LGWR switch_lag5",	"NFS Error_lag2",	"NFS Error_lag4",	"Others_lag1",	"Others_lag3",	"Others_lag5",	"resize operation_lag2",	"resize operation_lag4",	"Roles Exceeded_lag1",	"Roles Exceeded_lag3",	"Roles Exceeded_lag5",	"Tns Error_lag2",	"Tns Error_lag4",		
            "KSFV_lag5",	"LGWR switch_lag2",	"LGWR switch_lag4",	"NFS Error_lag1",	"NFS Error_lag3",	"NFS Error_lag5",	"Others_lag2",	"Others_lag4",	"resize operation_lag1",	"resize operation_lag3",	"resize operation_lag5",	"Roles Exceeded_lag2",	"Roles Exceeded_lag4",	"Tns Error_lag1",	"Tns Error_lag3",	"Tns Error_lag5")

EDW_LDA_Class_test <- EDW_LDA_Class_test[names1]

EDW_LDA_Class_test<- tail(EDW_LDA_Class_test,-5)

EDW_LDA_Class_test1 <- EDW_LDA_Class_test

EDW_LDA_Class_test$Date <- NULL
EDW_LDA_Class_test[is.na(EDW_LDA_Class_test)] <- 0



##############################################################################################################################################################

##########----------------------------------------ORA-600------------------------------------------#############



predicted1 <- predict(model1, EDW_LDA_Class_test, type="response")  # predicted scores

predicted_file1 <- data.frame(cbind(EDW_LDA_Class_test,predicted1))

predicted_file1 <- data.frame(cbind(EDW_LDA_Class_test1$Date,predicted_file1))

write.csv(predicted_file1,"C:\\Shiva\\Projects\\GS\\GS_fault\\EDW\\EDW_fault_prediction\\predicted_ORA-600_1.csv")

#####################-------------------------Process--------------------------------################


predicted1 <- predict(model1, EDW_LDA_Class_test, type="response")  # predicted scores

predicted_file1 <- data.frame(cbind(EDW_LDA_Class_test,predicted1))

predicted_file1 <- data.frame(cbind(EDW_LDA_Class_test1$Date,predicted_file1))

write.csv(predicted_file1,"C:\\Shiva\\Projects\\GS\\GS_fault\\EDW\\EDW_fault_prediction\\predicted_process_1.csv")

#####################--------------------------ASM----------------------------------------################


predicted1 <- predict(model1, EDW_LDA_Class_test, type="response")  # predicted scores

predicted_file1 <- data.frame(cbind(EDW_LDA_Class_test,predicted1))

predicted_file1 <- data.frame(cbind(EDW_LDA_Class_test1$Date,predicted_file1))

write.csv(predicted_file1,"C:\\Shiva\\Projects\\GS\\GS_fault\\EDW\\EDW_fault_prediction\\predicted_ASM_1.csv")

#####################--------------------------Dest_Breach----------------------------------------################


predicted1 <- predict(model1, EDW_LDA_Class_test, type="response")  # predicted scores

predicted_file1 <- data.frame(cbind(EDW_LDA_Class_test,predicted1))

predicted_file1 <- data.frame(cbind(EDW_LDA_Class_test1$Date,predicted_file1))

write.csv(predicted_file1,"C:\\Shiva\\Projects\\GS\\GS_fault\\EDW\\EDW_fault_prediction\\predicted_Dest_Breach_1.csv")


#####################--------------------------Misc----------------------------------------################


predicted1 <- predict(model1, EDW_LDA_Class_test, type="response")  # predicted scores

predicted_file1 <- data.frame(cbind(EDW_LDA_Class_test,predicted1))

predicted_file1 <- data.frame(cbind(EDW_LDA_Class_test1$Date,predicted_file1))

write.csv(predicted_file1,"C:\\Shiva\\Projects\\GS\\GS_fault\\EDW\\EDW_fault_prediction\\predicted_Dest_Misc_1.csv")

#####################--------------------------DB_Down----------------------------------------################

predicted1 <- predict(model1, EDW_LDA_Class_test, type="response")  # predicted scores

predicted_file1 <- data.frame(cbind(EDW_LDA_Class_test,predicted1))

predicted_file1 <- data.frame(cbind(EDW_LDA_Class_test1$Date,predicted_file1))

write.csv(predicted_file1,"C:\\Shiva\\Projects\\GS\\GS_fault\\EDW\\EDW_fault_prediction\\predicted_Dest_DB_Down_1.csv")

#####################--------------------------ORA_xxxx---------------------------------------################


predicted1 <- predict(model1, EDW_LDA_Class_test, type="response")  # predicted scores

predicted_file1 <- data.frame(cbind(EDW_LDA_Class_test,predicted1))

predicted_file1 <- data.frame(cbind(EDW_LDA_Class_test1$Date,predicted_file1))

write.csv(predicted_file1,"C:\\Shiva\\Projects\\GS\\GS_fault\\EDW\\EDW_fault_prediction\\predicted_Dest_ORA_xxxx_1.csv")

#####################--------------------------GRID---------------------------------------################

predicted1 <- predict(model1, EDW_LDA_Class_test, type="response")  # predicted scores

predicted_file1 <- data.frame(cbind(EDW_LDA_Class_test,predicted1))

predicted_file1 <- data.frame(cbind(EDW_LDA_Class_test1$Date,predicted_file1))

write.csv(predicted_file1,"C:\\Shiva\\Projects\\GS\\GS_fault\\EDW\\EDW_fault_prediction\\predicted_Dest_GRID_1.csv")

#####################--------------------------Block---------------------------------------################


predicted1 <- predict(model1, EDW_LDA_Class_test, type="response")  # predicted scores

predicted_file1 <- data.frame(cbind(EDW_LDA_Class_test,predicted1))

predicted_file1 <- data.frame(cbind(EDW_LDA_Class_test1$Date,predicted_file1))

write.csv(predicted_file1,"C:\\Shiva\\Projects\\GS\\GS_fault\\EDW\\EDW_fault_prediction\\predicted_Dest_Block_1.csv")

#####################--------------------------Space_Database---------------------------------------################


predicted1 <- predict(model1, EDW_LDA_Class_test, type="response")  # predicted scores

predicted_file1 <- data.frame(cbind(EDW_LDA_Class_test,predicted1))

predicted_file1 <- data.frame(cbind(EDW_LDA_Class_test1$Date,predicted_file1))

write.csv(predicted_file1,"C:\\Shiva\\Projects\\GS\\GS_fault\\EDW\\EDW_fault_prediction\\predicted_Dest_Space_Database_1.csv")

#####################--------------------------Backup---------------------------------------################

predicted1 <- predict(model1, EDW_LDA_Class_test, type="response")  # predicted scores

predicted_file1 <- data.frame(cbind(EDW_LDA_Class_test,predicted1))

predicted_file1 <- data.frame(cbind(EDW_LDA_Class_test1$Date,predicted_file1))

write.csv(predicted_file1,"C:\\Shiva\\Projects\\GS\\GS_fault\\EDW\\EDW_fault_prediction\\predicted_Backup_1.csv")

#####################--------------------------Redo---------------------------------------################


predicted1 <- predict(model1, EDW_LDA_Class_test, type="response")  # predicted scores

predicted_file1 <- data.frame(cbind(EDW_LDA_Class_test,predicted1))

predicted_file1 <- data.frame(cbind(EDW_LDA_Class_test1$Date,predicted_file1))

write.csv(predicted_file1,"C:\\Shiva\\Projects\\GS\\GS_fault\\EDW\\EDW_fault_prediction\\predicted_Redo_1.csv")

#####################--------------------------Space_ORA_16nn---------------------------------------################

predicted1 <- predict(model1, EDW_LDA_Class_test, type="response")  # predicted scores

predicted_file1 <- data.frame(cbind(EDW_LDA_Class_test,predicted1))

predicted_file1 <- data.frame(cbind(EDW_LDA_Class_test1$Date,predicted_file1))

write.csv(predicted_file1,"C:\\Shiva\\Projects\\GS\\GS_fault\\EDW\\EDW_fault_prediction\\predicted_Space_ORA_16nn_1.csv")

#####################--------------------------Space---------------------------------------################

predicted1 <- predict(model1, EDW_LDA_Class_test, type="response")  # predicted scores

predicted_file1 <- data.frame(cbind(EDW_LDA_Class_test,predicted1))

predicted_file1 <- data.frame(cbind(EDW_LDA_Class_test1$Date,predicted_file1))

write.csv(predicted_file1,"C:\\Shiva\\Projects\\GS\\GS_fault\\EDW\\EDW_fault_prediction\\predicted_Space_1.csv")

#####################--------------------------User_Access---------------------------------------################

predicted1 <- predict(model1, EDW_LDA_Class_test, type="response")  # predicted scores

predicted_file1 <- data.frame(cbind(EDW_LDA_Class_test,predicted1))

predicted_file1 <- data.frame(cbind(EDW_LDA_Class_test1$Date,predicted_file1))

write.csv(predicted_file1,"C:\\Shiva\\Projects\\GS\\GS_fault\\EDW\\EDW_fault_prediction\\predicted_User_Access_1.csv")

#####################--------------------------Blank---------------------------------------################


predicted1 <- predict(model1, EDW_LDA_Class_test, type="response")  # predicted scores

predicted_file1 <- data.frame(cbind(EDW_LDA_Class_test,predicted1))

predicted_file1 <- data.frame(cbind(EDW_LDA_Class_test1$Date,predicted_file1))

write.csv(predicted_file1,"C:\\Shiva\\Projects\\GS\\GS_fault\\EDW\\EDW_fault_prediction\\predicted_Blank_1.csv")

#####################--------------------------DB_Access---------------------------------------################

predicted1 <- predict(model1, EDW_LDA_Class_test, type="response")  # predicted scores

predicted_file1 <- data.frame(cbind(EDW_LDA_Class_test,predicted1))

predicted_file1 <- data.frame(cbind(EDW_LDA_Class_test1$Date,predicted_file1))

write.csv(predicted_file1,"C:\\Shiva\\Projects\\GS\\GS_fault\\EDW\\EDW_fault_prediction\\predicted_DB_Access_1.csv")

######################################################################################################################################

##########--------------------------Predicting 12th may 2017---------------------------------------------####################
col=2:dim(EDW_LDA_Class_test)[2]
j=20
for(i in col)
{
  print(i)
  table3 <- as.data.frame(EDW_LDA_Class_test %>% 
                            mutate(lag_2=as.numeric(Lag(EDW_LDA_Class_test[,i],2)),
                                   lag_3=as.numeric(Lag(EDW_LDA_Class_test[,i],3)),
                                   lag_4=as.numeric(Lag(EDW_LDA_Class_test[,i],4)),
                                   lag_5=as.numeric(Lag(EDW_LDA_Class_test[,i],5)),
                                   lag_6=as.numeric(Lag(EDW_LDA_Class_test[,i],6))))
  
  colnames(table3)=c(colnames(EDW_LDA_Class_test),paste(colnames(EDW_LDA_Class_test)[i],"_lag",2:6,sep=""))
  vec=j+4
  EDW_LDA_Class_test=cbind(EDW_LDA_Class_test,table3[,j:vec])
  rm(table3)
  j=j+5
}

names1 <- c("Date","ARCHIVE LOG_lag2",	"ARCHIVE LOG_lag4",	"ARCHIVE LOG_lag6",	"Archived_thread_error_lag3",	"Archived_thread_error_lag5",	"ASMB_lag2",	"ASMB_lag4",	"ASMB_lag6",	"Auto Tuning_lag3",	"Auto Tuning_lag5",	"Dbora Files_lag2",	"Dbora Files_lag4",	"Dbora Files_lag6",	"Diagnostic error_lag3",			
            "ARCHIVE LOG_lag3",	"ARCHIVE LOG_lag5",	"Archived_thread_error_lag2",	"Archived_thread_error_lag4",	"Archived_thread_error_lag6",	"ASMB_lag3",	"ASMB_lag5",	"Auto Tuning_lag2",	"Auto Tuning_lag4",	"Auto Tuning_lag6",	"Dbora Files_lag3",	"Dbora Files_lag5",	"Diagnostic error_lag2",	"Diagnostic error_lag4",			
            "Diagnostic error_lag5",	"Diagonal Adl error_lag2",	"Diagonal Adl error_lag4",	"Diagonal Adl error_lag6",	"Fatal Ni connections_lag3",	"Fatal Ni connections_lag5",	"GCS remote-cache_lag2",	"GCS remote-cache_lag4",	"GCS remote-cache_lag6",	"Inbound Error_lag3",	"Inbound Error_lag5",	"Killing Session note_lag2",	"Killing Session note_lag4",	"Killing Session note_lag6",	"KSFV_lag3",			
            "Diagnostic error_lag6",	"Diagonal Adl error_lag3",	"Diagonal Adl error_lag5",	"Fatal Ni connections_lag2",	"Fatal Ni connections_lag4",	"Fatal Ni connections_lag6",	"GCS remote-cache_lag3",	"GCS remote-cache_lag5",	"Inbound Error_lag2",	"Inbound Error_lag4",	"Inbound Error_lag6",	"Killing Session note_lag3",	"Killing Session note_lag5",	"KSFV_lag2",	"KSFV_lag4",			
            "KSFV_lag5",	"LGWR switch_lag2",	"LGWR switch_lag4",	"LGWR switch_lag6",	"NFS Error_lag3",	"NFS Error_lag5",	"Others_lag2",	"Others_lag4",	"Others_lag6",	"resize operation_lag3",	"resize operation_lag5",	"Roles Exceeded_lag2",	"Roles Exceeded_lag4",	"Roles Exceeded_lag6",	"Tns Error_lag3",	"Tns Error_lag5",		
            "KSFV_lag6",	"LGWR switch_lag3",	"LGWR switch_lag5",	"NFS Error_lag2",	"NFS Error_lag4",	"NFS Error_lag6",	"Others_lag3",	"Others_lag5",	"resize operation_lag2",	"resize operation_lag4",	"resize operation_lag6",	"Roles Exceeded_lag3",	"Roles Exceeded_lag5",	"Tns Error_lag2",	"Tns Error_lag4",	"Tns Error_lag6")


EDW_LDA_Class_test <- EDW_LDA_Class_test[names1]

EDW_LDA_Class_test<- tail(EDW_LDA_Class_test,-6)

EDW_LDA_Class_test1 <- EDW_LDA_Class_test

EDW_LDA_Class_test$Date <- NULL

EDW_LDA_Class_test[is.na(EDW_LDA_Class_test)] <- 0

###########################################################################################################################################################33

##########---------------------------------ORA-600--------------------------------------------------------#############


predicted1 <- predict(model2, EDW_LDA_Class_test, type="response")  # predicted scores

predicted_file1 <- data.frame(cbind(EDW_LDA_Class_test,predicted1))

predicted_file1 <- data.frame(cbind(EDW_LDA_Class_test1$Date,predicted_file1))

write.csv(predicted_file1,"C:\\Shiva\\Projects\\GS\\GS_fault\\EDW\\EDW_fault_prediction\\predicted_ORA-600_2.csv")

##########-----------------------------Process-------------------------------------------------------------#############
predicted1 <- predict(model2, EDW_LDA_Class_test, type="response")  # predicted scores

predicted_file1 <- data.frame(cbind(EDW_LDA_Class_test,predicted1))

predicted_file1 <- data.frame(cbind(EDW_LDA_Class_test1$Date,predicted_file1))

write.csv(predicted_file1,"C:\\Shiva\\Projects\\GS\\GS_fault\\EDW\\EDW_fault_prediction\\predicted_Process_2.csv")

##########-----------------------------ASM-------------------------------------------------------------#############
predicted1 <- predict(model2, EDW_LDA_Class_test, type="response")  # predicted scores

predicted_file1 <- data.frame(cbind(EDW_LDA_Class_test,predicted1))

predicted_file1 <- data.frame(cbind(EDW_LDA_Class_test1$Date,predicted_file1))

write.csv(predicted_file1,"C:\\Shiva\\Projects\\GS\\GS_fault\\EDW\\EDW_fault_prediction\\predicted_ASM_2.csv")

##########-----------------------------Dest_Breach-------------------------------------------------------------#############
predicted1 <- predict(model2, EDW_LDA_Class_test, type="response")  # predicted scores

predicted_file1 <- data.frame(cbind(EDW_LDA_Class_test,predicted1))

predicted_file1 <- data.frame(cbind(EDW_LDA_Class_test1$Date,predicted_file1))

write.csv(predicted_file1,"C:\\Shiva\\Projects\\GS\\GS_fault\\EDW\\EDW_fault_prediction\\predicted_Dest_Breach_2.csv")

##########-----------------------------Misc-------------------------------------------------------------#############
predicted1 <- predict(model2, EDW_LDA_Class_test, type="response")  # predicted scores

predicted_file1 <- data.frame(cbind(EDW_LDA_Class_test,predicted1))

predicted_file1 <- data.frame(cbind(EDW_LDA_Class_test1$Date,predicted_file1))

write.csv(predicted_file1,"C:\\Shiva\\Projects\\GS\\GS_fault\\EDW\\EDW_fault_prediction\\predicted_Misc_2.csv")

##########-----------------------------DB_Down-------------------------------------------------------------#############
predicted1 <- predict(model2, EDW_LDA_Class_test, type="response")  # predicted scores

predicted_file1 <- data.frame(cbind(EDW_LDA_Class_test,predicted1))

predicted_file1 <- data.frame(cbind(EDW_LDA_Class_test1$Date,predicted_file1))

write.csv(predicted_file1,"C:\\Shiva\\Projects\\GS\\GS_fault\\EDW\\EDW_fault_prediction\\predicted_DB_Down_2.csv")

##########-----------------------------ORA_xxxx-------------------------------------------------------------#############
predicted1 <- predict(model2, EDW_LDA_Class_test, type="response")  # predicted scores

predicted_file1 <- data.frame(cbind(EDW_LDA_Class_test,predicted1))

predicted_file1 <- data.frame(cbind(EDW_LDA_Class_test1$Date,predicted_file1))

write.csv(predicted_file1,"C:\\Shiva\\Projects\\GS\\GS_fault\\EDW\\EDW_fault_prediction\\predicted_ORA_xxxx_2.csv")

##########-----------------------------GRID-------------------------------------------------------------#############
predicted1 <- predict(model2, EDW_LDA_Class_test, type="response")  # predicted scores

predicted_file1 <- data.frame(cbind(EDW_LDA_Class_test,predicted1))

predicted_file1 <- data.frame(cbind(EDW_LDA_Class_test1$Date,predicted_file1))

write.csv(predicted_file1,"C:\\Shiva\\Projects\\GS\\GS_fault\\EDW\\EDW_fault_prediction\\predicted_GRID_2.csv")

##########-----------------------------Block-------------------------------------------------------------#############
predicted1 <- predict(model2, EDW_LDA_Class_test, type="response")  # predicted scores

predicted_file1 <- data.frame(cbind(EDW_LDA_Class_test,predicted1))

predicted_file1 <- data.frame(cbind(EDW_LDA_Class_test1$Date,predicted_file1))

write.csv(predicted_file1,"C:\\Shiva\\Projects\\GS\\GS_fault\\EDW\\EDW_fault_prediction\\predicted_Block_2.csv")

##########-----------------------------Space_Database-------------------------------------------------------------#############
predicted1 <- predict(model2, EDW_LDA_Class_test, type="response")  # predicted scores

predicted_file1 <- data.frame(cbind(EDW_LDA_Class_test,predicted1))

predicted_file1 <- data.frame(cbind(EDW_LDA_Class_test1$Date,predicted_file1))

write.csv(predicted_file1,"C:\\Shiva\\Projects\\GS\\GS_fault\\EDW\\EDW_fault_prediction\\predicted_Space_Database_2.csv")

##########-----------------------------Backup-------------------------------------------------------------#############
predicted1 <- predict(model2, EDW_LDA_Class_test, type="response")  # predicted scores

predicted_file1 <- data.frame(cbind(EDW_LDA_Class_test,predicted1))

predicted_file1 <- data.frame(cbind(EDW_LDA_Class_test1$Date,predicted_file1))

write.csv(predicted_file1,"C:\\Shiva\\Projects\\GS\\GS_fault\\EDW\\EDW_fault_prediction\\predicted_Backup_2.csv")

##########-----------------------------Redo-------------------------------------------------------------#############
predicted1 <- predict(model2, EDW_LDA_Class_test, type="response")  # predicted scores

predicted_file1 <- data.frame(cbind(EDW_LDA_Class_test,predicted1))

predicted_file1 <- data.frame(cbind(EDW_LDA_Class_test1$Date,predicted_file1))

write.csv(predicted_file1,"C:\\Shiva\\Projects\\GS\\GS_fault\\EDW\\EDW_fault_prediction\\predicted_Redo_2.csv")

##########-----------------------------Space_ORA_16nn-------------------------------------------------------------#############
predicted1 <- predict(model2, EDW_LDA_Class_test, type="response")  # predicted scores

predicted_file1 <- data.frame(cbind(EDW_LDA_Class_test,predicted1))

predicted_file1 <- data.frame(cbind(EDW_LDA_Class_test1$Date,predicted_file1))

write.csv(predicted_file1,"C:\\Shiva\\Projects\\GS\\GS_fault\\EDW\\EDW_fault_prediction\\predicted_Space_ORA_16nn_2.csv")

##########-----------------------------Space-------------------------------------------------------------#############
predicted1 <- predict(model2, EDW_LDA_Class_test, type="response")  # predicted scores

predicted_file1 <- data.frame(cbind(EDW_LDA_Class_test,predicted1))

predicted_file1 <- data.frame(cbind(EDW_LDA_Class_test1$Date,predicted_file1))

write.csv(predicted_file1,"C:\\Shiva\\Projects\\GS\\GS_fault\\EDW\\EDW_fault_prediction\\predicted_Space_2.csv")

##########-----------------------------User_Access-------------------------------------------------------------#############
predicted1 <- predict(model2, EDW_LDA_Class_test, type="response")  # predicted scores

predicted_file1 <- data.frame(cbind(EDW_LDA_Class_test,predicted1))

predicted_file1 <- data.frame(cbind(EDW_LDA_Class_test1$Date,predicted_file1))

write.csv(predicted_file1,"C:\\Shiva\\Projects\\GS\\GS_fault\\EDW\\EDW_fault_prediction\\predicted_User_Access_2.csv")

##########-----------------------------Blank-------------------------------------------------------------#############
predicted1 <- predict(model2, EDW_LDA_Class_test, type="response")  # predicted scores

predicted_file1 <- data.frame(cbind(EDW_LDA_Class_test,predicted1))

predicted_file1 <- data.frame(cbind(EDW_LDA_Class_test1$Date,predicted_file1))

write.csv(predicted_file1,"C:\\Shiva\\Projects\\GS\\GS_fault\\EDW\\EDW_fault_prediction\\predicted_Blank_2.csv")

##########-----------------------------DB_Access-------------------------------------------------------------#############
predicted1 <- predict(model2, EDW_LDA_Class_test, type="response")  # predicted scores

predicted_file1 <- data.frame(cbind(EDW_LDA_Class_test,predicted1))

predicted_file1 <- data.frame(cbind(EDW_LDA_Class_test1$Date,predicted_file1))

write.csv(predicted_file1,"C:\\Shiva\\Projects\\GS\\GS_fault\\EDW\\EDW_fault_prediction\\predicted_DB_Access_2.csv")
