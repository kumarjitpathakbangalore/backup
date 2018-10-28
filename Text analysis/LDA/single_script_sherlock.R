library(dplyr)
#library(effects)
library(InformationValue)
library(DMwR)
library(reshape2)
library(quantmod)
library(glmnet)
library(qpcR)
library(plyr)


#calls the python script to run in shell
system("python --version")
shell('python E:/dat01/Preprocessed_files/pre_processing_script_16_Nov_2017.py',intern=TRUE)

fault_prediction <- function(T_EDW_LDA_Class) {
  
  
  #needed variables 
  var_ora<-"Incident.typeORA.600"
  var_asm<-"Incident.typeASM.Dest.Breach"
  var_racle<-"Incident.typeOracle.Process.Stopped"
  var_oh<-"Incident.typeOH.Dest.Breach"
  xx<-c("Fatal_NI_lag1",	"Fatal_NI_lag2",	"Fatal_NI_lag3",	"Fatal_NI_lag4",	"Fatal_NI_lag5",	"LGWR_switch_lag1",	"LGWR_switch_lag2",	"LGWR_switch_lag3",	"LGWR_switch_lag4",	"LGWR_switch_lag5",	"Archived_Log_entry_lag1",	"Archived_Log_entry_lag2",	"Archived_Log_entry_lag3",	"Archived_Log_entry_lag4",	"Archived_Log_entry_lag5",	"Index_Unusable_lag1",	"Index_Unusable_lag2",	"Index_Unusable_lag3",	"Index_Unusable_lag4",	"Index_Unusable_lag5",	"KSFV_lag1",	"KSFV_lag2",	"KSFV_lag3",	"KSFV_lag4",	"KSFV_lag5",	"Errors_in_file_lag1",	"Errors_in_file_lag2",	"Errors_in_file_lag3",	"Errors_in_file_lag4",	"Errors_in_file_lag5",	"ORA_lag1",	"ORA_lag2",	"ORA_lag3",	"ORA_lag4",	"ORA_lag5",	"Resize_Operation_lag1",	"Resize_Operation_lag2",	"Resize_Operation_lag3",	"Resize_Operation_lag4",	"Resize_Operation_lag5",	"Others_lag1",	"Others_lag2",	"Others_lag3",	"Others_lag4",	"Others_lag5",	"Background_Process_lag1",	"Background_Process_lag2",	"Background_Process_lag3",
        "Background_Process_lag4",	"Background_Process_lag5",	"Partition_lag1",	"Partition_lag2",	"Partition_lag3",	"Partition_lag4",	"Partition_lag5",	"NFS_Permission_lag1",	"NFS_Permission_lag2",	"NFS_Permission_lag3",	"NFS_Permission_lag4",	"NFS_Permission_lag5",	"Roles_exceeded_lag1",	"Roles_exceeded_lag2",	"Roles_exceeded_lag3",	"Roles_exceeded_lag4",	"Roles_exceeded_lag5",	"Aborting_Process_lag1",	"Aborting_Process_lag2",	"Aborting_Process_lag3",	"Aborting_Process_lag4",	"Aborting_Process_lag5",	"Dumping_diagnostic_lag1",	"Dumping_diagnostic_lag2",	"Dumping_diagnostic_lag3",	"Dumping_diagnostic_lag4",	"Dumping_diagnostic_lag5",	"Kill_Session_lag1",	"Kill_Session_lag2",	"Kill_Session_lag3",	"Kill_Session_lag4",	"Kill_Session_lag5",	"SQL_Statement_lag1",	"SQL_Statement_lag2",	"SQL_Statement_lag3",	"SQL_Statement_lag4",	"SQL_Statement_lag5",	"Memory_Notification_lag1",	"Memory_Notification_lag2",	"Memory_Notification_lag3",	"Memory_Notification_lag4",	"Memory_Notification_lag5",	"Deadlock_detected_lag1",	"Deadlock_detected_lag2",	"Deadlock_detected_lag3",	"Deadlock_detected_lag4",	"Deadlock_detected_lag5",	"GTX_Started_lag1",	"GTX_Started_lag2",
        "GTX_Started_lag4",	"GTX_Started_lag5",	"Shadows_Cancelled_lag1",	"Shadows_Cancelled_lag2",	"Shadows_Cancelled_lag3",	"Shadows_Cancelled_lag4",	"Shadows_Cancelled_lag5",	"Instance_others_lag1",	"Instance_others_lag2",	"Instance_others_lag3",	"Instance_others_lag4",	"Instance_others_lag5",	"Tablespace_Encription_lag1",	"Tablespace_Encription_lag2",	"Tablespace_Encription_lag3",	"Tablespace_Encription_lag4",	"Tablespace_Encription_lag5",	"Redo_Complete_lag1",	"Redo_Complete_lag2",	"Redo_Complete_lag3",	"Redo_Complete_lag4",	"Redo_Complete_lag5",	"KGL_Object_lag1",	"KGL_Object_lag2",	"KGL_Object_lag3",	"KGL_Object_lag4",	"KGL_Object_lag5")																						
  xx_2<-c(	"Fatal_NI_lag2",	"Fatal_NI_lag3",	"Fatal_NI_lag4",	"Fatal_NI_lag5",	"Fatal_NI_lag6",	"LGWR_switch_lag2",	"LGWR_switch_lag3",	"LGWR_switch_lag4",	"LGWR_switch_lag5",	"LGWR_switch_lag6",	"Archived_Log_entry_lag2",	"Archived_Log_entry_lag3",	"Archived_Log_entry_lag4",	"Archived_Log_entry_lag5",	"Archived_Log_entry_lag6",	"Index_Unusable_lag2",	"Index_Unusable_lag3",	"Index_Unusable_lag4",	"Index_Unusable_lag5",	"Index_Unusable_lag6",	"KSFV_lag2",	"KSFV_lag3",	"KSFV_lag4",	"KSFV_lag5",	"KSFV_lag6",	"Errors_in_file_lag2",	"Errors_in_file_lag3",	"Errors_in_file_lag4",	"Errors_in_file_lag5",	"Errors_in_file_lag6",	"ORA_lag2",	"ORA_lag3",	"ORA_lag4",	"ORA_lag5",	"ORA_lag6",	"Resize_Operation_lag2",	"Resize_Operation_lag3",	"Resize_Operation_lag4",	"Resize_Operation_lag5",	"Resize_Operation_lag6",	"Others_lag2",	"Others_lag3",	"Others_lag4",	"Others_lag5",	"Others_lag6",	"Background_Process_lag2",	"Background_Process_lag3",	"Background_Process_lag4",	"Background_Process_lag5",
           "Background_Process_lag6",	"Partition_lag2",	"Partition_lag3",	"Partition_lag4",	"Partition_lag5",	"Partition_lag6",	"NFS_Permission_lag2",	"NFS_Permission_lag3",	"NFS_Permission_lag4",	"NFS_Permission_lag5",	"NFS_Permission_lag6",	"Roles_exceeded_lag2",	"Roles_exceeded_lag3",	"Roles_exceeded_lag4",	"Roles_exceeded_lag5",	"Roles_exceeded_lag6",	"Aborting_Process_lag2",	"Aborting_Process_lag3",	"Aborting_Process_lag4",	"Aborting_Process_lag5",	"Aborting_Process_lag6",	"Dumping_diagnostic_lag2",	"Dumping_diagnostic_lag3",	"Dumping_diagnostic_lag4",	"Dumping_diagnostic_lag5",	"Dumping_diagnostic_lag6",	"Kill_Session_lag2",	"Kill_Session_lag3",	"Kill_Session_lag4",	"Kill_Session_lag5",	"Kill_Session_lag6",	"SQL_Statement_lag2",	"SQL_Statement_lag3",	"SQL_Statement_lag4",	"SQL_Statement_lag5",	"SQL_Statement_lag6",	"Memory_Notification_lag2",	"Memory_Notification_lag3",	"Memory_Notification_lag4",	"Memory_Notification_lag5",	"Memory_Notification_lag6",	"Deadlock_detected_lag2",	"Deadlock_detected_lag3",	"Deadlock_detected_lag4",	"Deadlock_detected_lag5",	"Deadlock_detected_lag6",	"GTX_Started_lag2",	"GTX_Started_lag3",	"GTX_Started_lag4",	"GTX_Started_lag5",
           "GTX_Started_lag6",	"Shadows_Cancelled_lag2",	"Shadows_Cancelled_lag3",	"Shadows_Cancelled_lag4",	"Shadows_Cancelled_lag5",	"Shadows_Cancelled_lag6",	"Instance_others_lag2",	"Instance_others_lag3",	"Instance_others_lag4",	"Instance_others_lag5",	"Instance_others_lag6",	"Tablespace_Encription_lag2",	"Tablespace_Encription_lag3",	"Tablespace_Encription_lag4",	"Tablespace_Encription_lag5",	"Tablespace_Encription_lag6",	"Redo_Complete_lag2",	"Redo_Complete_lag3",	"Redo_Complete_lag4",	"Redo_Complete_lag5",	"Redo_Complete_lag6",	"KGL_Object_lag2",	"KGL_Object_lag3",	"KGL_Object_lag4",	"KGL_Object_lag5",	"KGL_Object_lag6")																								
  
  required_log_start_date <- "2017-11-07"
  required_log_end_date <- "2017-11-13"
  current_day_prediction <- "2017-11-13"
  tommorow_prediction <- "2017-11-14"
  #old<-read.csv("C:\\Users\\609768820\\Documents\\SHIVE\\LDA\\GILBAN\\GILBAN\\all node\\all_node_25_10\\predicted_1026_node5.csv",stringsAsFactors = F)
  
  #len<-dim(old)[1]         
  #old<-old[1:(len-2),] 
  
  #T_EDW_LDA_Class<-read.csv("edw01t5_alert8_nov_prediction.csv",stringsAsFactors = F)
  #T_EDW_LDA_Class<-rbind(T_EDW_LDA_Class,old)
  dim(T_EDW_LDA_Class)
  str(T_EDW_LDA_Class)
  #converting date format##
  #T_EDW_LDA_Class$Date  = as.character(T_EDW_LDA_Class$Date )
  #formated_Date <- sapply(T_EDW_LDA_Class$Date ,format_date_function)
  #T_EDW_LDA_Class$Date <- formated_Date
  T_EDW_LDA_Class$Date <- 
    as.Date(T_EDW_LDA_Class$Date, "%d/%m/%Y")
  
  ##Filling missing dates##
  g_t_1 <- data.frame(Date=seq(min(T_EDW_LDA_Class$Date),max(T_EDW_LDA_Class$Date),1))
  T_EDW_LDA_Class1 <- merge(g_t_1,T_EDW_LDA_Class,by="Date",all.x=TRUE)
  
  
  
  ## Creating LDA class table##
  T_EDW_LDA_Class1<-T_EDW_LDA_Class1[T_EDW_LDA_Class1$Date> required_log_start_date & 
                                       T_EDW_LDA_Class1$Date< required_log_end_date,]
  T_table <- data.frame(table(T_EDW_LDA_Class1$Date,T_EDW_LDA_Class1$Prediction))
  T_table1 <- reshape(T_table,idvar = "Var1",timevar = "Var2",direction = "wide")
  colnames(T_table1)=c("Date",as.character(unique(T_table$Var2)))
  T_table1[is.na(T_table1)] <- 0
  T_table1$na<-NULL
  T_table1$Date<-as.character(T_table1$Date)
  T_table1[6,1]<- current_day_prediction
  
  ############table2 for lag2
  g_t_2<- data.frame(Date=seq(min(T_EDW_LDA_Class$Date),max(T_EDW_LDA_Class$Date),1))
  T_EDW_LDA_Class2 <- merge(g_t_2,T_EDW_LDA_Class,by="Date",all.x=TRUE)
  T_EDW_LDA_Class2<-T_EDW_LDA_Class2[T_EDW_LDA_Class2$Date> required_log_start_date & 
                                       T_EDW_LDA_Class2$Date<required_log_end_date,]
  
  T_table <- data.frame(table(T_EDW_LDA_Class2$Date,T_EDW_LDA_Class2$Prediction))
  T_table2 <- reshape(T_table,idvar = "Var1",timevar = "Var2",direction = "wide")
  colnames(T_table2)=c("Date",as.character(unique(T_table$Var2)))
  T_table2[is.na(T_table2)] <- 0
  T_table2$na<-NULL
  T_table2$Date<-as.character(T_table2$Date)
  T_table2[6,1]<- current_day_prediction
  T_table2[7,1]<- tommorow_prediction
  
  
  #33################geting date
  #T_table <- data.frame(table(T_EDW_LDA_Class1$Date,T_EDW_LDA_Class1$Topic))
  #T_table1_k <- reshape(T_table,idvar = "Var1",timevar = "Var2",direction = "wide")
  #colnames(T_table1_k)=c("Date",as.character(unique(T_table$Var2)))
  #T_table1_k[is.na(T_table1_k)] <- 0
  #T_table2 <- T_table1_k
  
  #T_table1[,2]<-NULL
  #T_table2[,2]<-NULL
  no_col<-ncol(T_table1)
  col=2:dim(T_table1)[2]
  j=ncol(T_table1)
  for(i in col)
  {
    print(i)
    T_table3 <- as.data.frame(T_table1 %>% 
                                mutate(lag_1=as.numeric(Lag(T_table1[,i],1)),
                                       lag_2=as.numeric(Lag(T_table1[,i],2)),
                                       lag_3=as.numeric(Lag(T_table1[,i],3)),
                                       lag_4=as.numeric(Lag(T_table1[,i],4)),
                                       lag_5=as.numeric(Lag(T_table1[,i],5))))
    
    colnames(T_table3)=c(colnames(T_table1),paste(colnames(T_table1)[i],"_lag",1:5,sep=""))
    vec=j+5
    s<-j+1
    T_table1=cbind(T_table1,T_table3[,s:vec])
    rm(T_table3)
    j=j+5
  }
  #T_table1$Date <- NULL
  
  ################################################Lag2 LDA class data#################################################
  
  col=2:dim(T_table2)[2]
  j=ncol(T_table2)
  for(i in col)
  {
    print(i)
    T_table3 <- as.data.frame(T_table2 %>% 
                                mutate(lag_2=as.numeric(Lag(T_table2[,i],2)),
                                       lag_3=as.numeric(Lag(T_table2[,i],3)),
                                       lag_4=as.numeric(Lag(T_table2[,i],4)),
                                       lag_5=as.numeric(Lag(T_table2[,i],5)),
                                       lag_6=as.numeric(Lag(T_table2[,i],6))))
    
    colnames(T_table3)=c(colnames(T_table2),paste(colnames(T_table2)[i],"_lag",2:6,sep=""))
    vec=j+5
    s<-j+1
    T_table2=cbind(T_table2,T_table3[,s:vec])
    rm(T_table3)
    j=j+5
  }
  
  
  
  #####################################test_fault#############################################################################################################
  
  #T_table1<-tail(T_table1,-5)
  
  T_lag1_data <- T_table1
  T_lag1_data<- tail(T_lag1_data,-5)
  #write.csv(T_lag1_data,"T_lag1_data.csv")
  #T_lag1_data<-read.csv("T_lag1_data.csv")
  
  T_lag1_data<-T_lag1_data[1,-c(2:no_col)]
  T_table1 <- T_lag1_data
  T_lag1_data$Date <- NULL
  ## Lag2 data set ##
  #T_table2<- tail(T_table2,-6)
  T_lag2_data <- cbind(T_table2)
  T_lag2_data<- tail(T_lag2_data,-6)
  T_lag2_data<-T_lag2_data[,-c(2:no_col)]
  T_table2 <-T_lag2_data 
  T_lag2_data$Date <- NULL
  
  
  
  #################attaching missing variable
  mis_1<-setdiff(xx,names(T_lag1_data))
  mis_2<-setdiff(xx_2,names(T_lag2_data))
  length(mis_1)
  T_lag1_data[c(mis_1)]<-0 
  T_lag2_data[c(mis_2)]<-0 
  
  #write.csv(T_lag2_data,"T_lag2_data.csv")
  #T_lag2_data<-read.csv("T_lag2_data.csv")
  #############tesing data
  
  #setwd("C:\\Users\\609768820\\Documents\\SHIVE\\LDA\\GILBAN\\GILBAN\\MONDAY")
  
  
  ora_600<-readRDS("E:/Company_work/model_objects/model_ORA-600.RDS")
  ora_2lag<-readRDS("E:/Company_work/model_objects/model2_ORA-600.RDS")
  
  
  
  predict_ora600_T<-predict(ora_600, newx = as.matrix(T_lag1_data[xx]), s = "lambda.min", type = "response")
  predict_ora600_T_2<-predict(ora_2lag, newx = as.matrix(T_lag2_data[xx_2]), s = "lambda.min", type = "response")
  #predict_ora600_T <- predict(ora_600, T_lag1_data, type="response")
  #predict_ora600_T_lag2<-predict(ora_2lag,T_lag2_data, type="response")
  Pre_T_ora600<-cbind.data.frame(Date=T_table1$Date,fault=var_ora,
                                 Predicted=predict_ora600_T )
  
  Pre_T_ora600_lag2<-cbind.data.frame(Date=T_table2$Date,fault=var_ora,
                                      Predicted=predict_ora600_T_2)
  
  #####################auto2
  
  oracle_Pro<-readRDS("E:/Company_work/model_objects/model_Oracle Process Stopped.RDS")
  oracle_Pro_2lag<-readRDS("E:/Company_work/model_objects/model2_Oracle Process Stopped.RDS")
  
  predict_oracle_Pro_T<-predict(oracle_Pro, newx = as.matrix(T_lag1_data[xx]), s = "lambda.min", type = "response")
  predict_oracle_Pro_T_2<-predict(oracle_Pro_2lag, newx = as.matrix(T_lag2_data[xx_2]), s = "lambda.min", type = "response")
  
  
  #predict_oracle_2_T <- predict(oracle_2, T_lag1_data, type="response")
  #predict_oracle_2_T_lag2<-predict(oracle_2_2lag,T_lag2_data, type="response")
  Pre_T_oracle_2<-cbind.data.frame(Date=T_table1$Date,fault=var_racle,
                                   Predicted =predict_oracle_Pro_T )
  
  Pre_T_oracle_2_lag2<-cbind.data.frame(Date=T_table2$Date,fault=var_racle,
                                        Predicted=predict_oracle_Pro_T_2)
  
  ##############asm_db
  
  asm_db<-readRDS("E:/Company_work/model_objects/model_ASM Dest Breach.RDS")
  asm_db_2lag<-readRDS("E:/Company_work/model_objects/model2_ASM Dest Breach.RDS")
  
  
  predict_asm_db_T<-predict(asm_db, newx = as.matrix(T_lag1_data[xx]), s = "lambda.min", type = "response")
  predict_asm_db_T_2<-predict(asm_db_2lag, newx = as.matrix(T_lag2_data[xx_2]), s = "lambda.min", type = "response")
  
  #predict_oracle_3_T <- predict(oracle_3, T_lag1_data, type="response")
  #predict_oracle_3_T_lag2<-predict(oracle_3_2lag,T_lag2_data, type="response")
  
  Pre_T_asm_db<-cbind.data.frame(Date=T_table1$Date,fault=var_asm,
                                 Predicted=predict_asm_db_T)
  
  Pre_T_asm_db_lag2<-cbind.data.frame(Date=T_table2$Date,fault=var_asm,
                                      Predicted=predict_asm_db_T_2)
  
  
  
  ##############eoh_db
  
  oh_db<-readRDS("E:/Company_work/model_objects/model_OH Dest Breach.RDS")
  oh_db_2lag<-readRDS("E:/Company_work/model_objects/model2_OH Dest Breach.RDS")
  
  
  predict_oh_db_T<-predict(oh_db, newx = as.matrix(T_lag1_data[xx]), s = "lambda.min", type = "response")
  predict_oh_db_T_2<-predict(oh_db_2lag, newx = as.matrix(T_lag2_data[xx_2]), s = "lambda.min", type = "response")
  
  #predict_oracle_3_T <- predict(oracle_3, T_lag1_data, type="response")
  #predict_oracle_3_T_lag2<-predict(oracle_3_2lag,T_lag2_data, type="response")
  
  Pre_T_oh_db<-cbind.data.frame(Date=T_table2$Date,fault=var_oh,
                                Predicted=predict_oh_db_T)
  
  Pre_T_oh_db_lag2<-cbind.data.frame(Date=T_table2$Date,fault=var_oh,
                                     Predicted=predict_oh_db_T_2)
  
  
  lag1_fault_predicted_t<-rbind(Pre_T_asm_db,Pre_T_oracle_2,Pre_T_ora600)
  colnames(lag1_fault_predicted_t)<-c("Date","Fault","predicted")
  
  lag2_fault_predicted_t<-rbind(Pre_T_asm_db_lag2,Pre_T_oracle_2_lag2,Pre_T_ora600_lag2)
  colnames(lag2_fault_predicted_t)<-c("Date","Fault","predicted")
  #setwd("C:\\Users\\609768820\\Documents\\SHIVE\\LDA\\GILBAN\\GILBAN\\all node")
  #write.csv(lag1_fault_predicted_t,"lag1_fault_predicted_t_18_node_4.csv")
  #write.csv(lag2_fault_predicted_t,"lag2_fault_predicted_t_19_node_4.csv")
  lag1_fault_predicted_t
  lag2_fault_predicted_t
  lag1_fault_predicted_t$Node <- paste0("TPL06429DAT0",index)
  lag1_fault_predicted_t$lag <- 1
  lag2_fault_predicted_t$Node <- paste0("TPL06429DAT0",index)
  lag2_fault_predicted_t$lag <- 2
  
  
  ################################Predction###############################
  
  #####Loading models##########
  mod1<-readRDS("E:/Company_work/model_objects/Lag1_pri.rds")
  mod2<-readRDS("E:/Company_work/model_objects/Lag2_pri.rds")
  
  ##################################Lag1 priority#####################################
  #predict_lag1 <- predict(mod,EDW_fault_1,"probs")
  
  EDW_fault_1<-T_lag1_data
  EDW_fault_2<-T_lag2_data
  
  not_inlag1<-setdiff(mod1$coefnames,names(EDW_fault_1))
  not_inlag1<-gsub("`","",not_inlag1)
  not_inlag2<-setdiff(mod2$coefnames,names(EDW_fault_2))
  not_inlag2<-gsub("`","",not_inlag2)
  
  EDW_fault_1[c(not_inlag1)]<-0 
  EDW_fault_2[c(not_inlag2)]<-0 
  
  #write.csv(EDW_fault_1,"EDW_fault_1.csv")
  #write.csv(EDW_fault_2,"EDW_fault_2.csv")
  #EDW_fault_1<-read.csv("edw_fault_1.csv")
  #EDW_fault_2<-read.csv("EDW_fault_2.csv")
  #gsub("."," ",EDW_fault_1)
  EDW_fault_1$`ASM Dest Breach`[1]<-1
  
  preds1_asm <- predict(mod1, type="class", newdata=EDW_fault_1)
  #predict_lag1_asm <- data.frame(cbind(lag1_fault_predicted_t,preds1_asm ))
  
  EDW_fault_1$`ASM Dest Breach`[1]<-0
  EDW_fault_1$`Oracle Process Stopped`[1]<-1
  preds1_oracle_stop <- predict(mod1, type="class", newdata=EDW_fault_1)
  #predict_lag1_asm <- data.frame(cbind(EDW_fault_1,preds1_oracle_stop))
  preds1_ORA_600<-NA
  preds1_asm <- NA
  lag1_node1_pred_06_07<-cbind(lag1_fault_predicted_t,prioriy=rbind(preds1_asm,preds1_oracle_stop,preds1_ORA_600))
  colnames(lag1_node1_pred_06_07)<-c("Date","Fault","predicted","Node","lag","Priority")
  lag1_node1_pred_06_07 <- lag1_node1_pred_06_07[,c(1,2,3,6,4,5)]
  colnames(lag1_node1_pred_06_07)
  #lag1_node1_pred_06_07 <- lag1_node1_pred_06_07[lag1_node1_pred_06_07$Priority != 'NA',]
  
  
  #setwd("C:\\Users\\609768820\\Documents\\SHIVE\\LDA\\GILBAN\\GILBAN\\all node\\allnode_01_11\\1")
  #write.csv(lag1_node1_pred_06_07,"lag1_node1_pred_01_02.csv")
  
  ###############################Lag2 priority######################################
  
  #predict_lag2 <- predict(mod1,EDW_fault_2,"probs")
  EDW_fault_2$`Oracle Process Stopped`[1]<-1
  preds2_asm<-NA
  preds2_oracle_stop<- predict(mod2, type="class", newdata=EDW_fault_2)
  
  
  EDW_fault_2$`Oracle Process Stopped`[1]<-0
  EDW_fault_2$`ORA-600`[1]<-1
  preds2_ORA_600<-predict(mod2, type="class", newdata=EDW_fault_2)
  lag2_node1_pred_06_07<-cbind(lag2_fault_predicted_t,priority=rbind(preds2_asm,preds2_oracle_stop,preds2_ORA_600))
  #predict_lag2 <- data.frame(cbind(EDW_fault_lag2,preds2))
  #head(preds2)
  
  #table(EDW_fault_2$Pri,preds2)
  colnames(lag2_node1_pred_06_07)<-c("Date","Fault","predicted","Node","lag","Priority")
  lag2_node1_pred_06_07 <- lag2_node1_pred_06_07[,c(1,2,3,6,4,5)]
  colnames(lag2_node1_pred_06_07)
  
  final_prediction<-rbind(lag1_node1_pred_06_07,lag2_node1_pred_06_07)
  final_prediction <- final_prediction[final_prediction$predicted > 0.3,]
  
  #write.table(final_prediction, file = "final_prediction_on_all_nodes.csv", sep = ",", append = TRUE, quote = FALSE,col.names = TRUE, row.names = FALSE)
  output_file <- paste0("final_output_node",index,".csv")
  index <<- index + 1
  write.csv(final_prediction,output_file,row.names = FALSE)
  
  # statements
  #return(something)
}


rm(list = ls())
setwd("E:/dat01/Preprocessed_files/preprocessed_files_13_nov_2017/preprocessed_files_13_nov_2017")

filenames <- list.files(getwd(), pattern="*.csv", full.names=FALSE)
ldf <- lapply(filenames, read.csv)
names(ldf) <- filenames
index <- 1
l <- lapply(ldf,function(x){ 
  fault_prediction(x) })

prediction_dates <- "_15_16"
filenames <- list.files(getwd(), pattern="final_output.*.csv", full.names=FALSE)
output_df <- lapply(filenames, read.csv)

final_df <- ldply(output_df, data.frame)
final_output_file <- paste0("final_output_all_nodes",prediction_dates ,".csv")
write.csv(final_df,final_output_file,row.names = FALSE)



