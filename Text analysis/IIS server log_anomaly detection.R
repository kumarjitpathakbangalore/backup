#---------------------input parameters-------------#
setwd("C:\\Shiva\\Projects\\IIS\\IIS logs")
rm(list = ls())
filename = "C:\\Shiva\\Projects\\IIS\\Data\\BWP129090001_train.csv"
dependent = "sc.status"

## Anomaly detection##
Train <- function(filename,idcolumn,dependent)
  
{
  
  library(dplyr)
  library(ggplot2)
  library(cluster)
  library(rpart)
  library(fpc)
  library(factoextra)
  library(clues)
  library(rpart)
  library(grid)
  library(partykit)
  library(class)
  library(clv)
  library(lattice)
  library(modeltools)
  library(stats4)
  library(flexclust)
  
  
  ##Importing data set##
  
  data <- read.csv(filename, stringsAsFactors = T, na.strings = c(""," ","NULL","?","NA"))
  
  dim(data)      
  
  
  
  head(data)
  
  
  
  data <- na.omit(data)
  
  data1 <- data
  
  
  char_cols <- which(sapply(data1, is.factor))  #categorical columns
  char_data <- data1[,char_cols]
  
  
  
  dv <- data1[,dependent]
  dv <- as.factor(dv)
  
  
  c1 <- which(names(data1)==dependent)
  #c2 <- which(names(data1)==idcolumn)
  iv <- data1[,-c(c1)]    #Independent variable list
  
  #####################################################################################################################################
  #CLUSTERING-STAGE1#
  #####################################################################################################################################
  
  
  num_cols <- which(sapply(iv,is.numeric))  
  clust <- iv[,num_cols]    
  
  clust=clust[complete.cases(clust),-c(2,5,6)]
  
  
  library(permute)
  library(lattice)
  library(vegan)
  set.seed(123)
  fit <- cascadeKM(scale(as.matrix(clust), center = TRUE,  scale = TRUE), 1, 10, iter = 1000)
  plot(fit, sortg = TRUE, grpmts.plot = TRUE)
  k1 <- as.numeric(which.max(fit$results[2,]))
  
  
  names_clust=colnames(clust)
  clust <- scale(as.matrix(clust)) # Standardising data set
  
  set.seed(1)
  fit.km = flexclust::kcca(clust, k1, kccaFamily("kmeans"))
  pred_train <- predict(fit.km)
  
  
  image(fit.km)
  points(clust, col=pred_train, pch=19, cex=0.3)
  
  
  clust_mem <- data.frame(iv, pred_train)
  
  
  clust_mem <- cbind(clust_mem,dv)
  
  
  ds <- split.data.frame(clust_mem,clust_mem$pred_train)
  
  
  interclust <- cls.scatt.data(clust, pred_train, dist="euclidean")
  average_interclass <- interclust$intracls.average
  average_interclass <- as.data.frame(average_interclass)
  
  
  center <- as.data.frame(interclust$cluster.center)
  names(center)=names_clust
}


list_out=Train(filename = filename,dependent = "sc.status",idcolumn = "")


##############################################################################################################################################
##New transaction##
##############################################################################################################################################													  
filename1 = "C:\\Shiva\\Projects\\IIS\\IIS logs\\Anomaly_test_.csv"
idcolumn = "FAULT_ID"

Test <- function(filename1,idcolumn,dependent)
  
{
  
  newtrans <-read.csv(filename1, stringsAsFactors = T, na.strings = c(""," ","NULL","?","NA"))
  
  data <- na.omit(newtrans)
  
  
  char_cols1 <- which(sapply(newtrans, is.factor))  #categorical columns
  char_data1 <- newtrans[,char_cols1]
  
  
  iv1 <- newtrans    #Independent variable list
  
  
  
  num_cols1 <- which(sapply(iv1,is.numeric))   
  clust1 <- iv1[,num_cols1]   
  clust1 = clust1[,-c(1,3,6,7)]
  
  
  clust1 <- scale(as.matrix(clust1))
  pred_test <- predict(fit.km, newdata=clust1) 
  
  
  clust1 <- data.frame(clust1,pred_test)
  clust1 <- split.data.frame(clust1,clust1$pred_test)
  for(i in 1:length(clust1))
  {
    clust1[[i]]= clust1[[i]][1:3]
  }
  
  clustdata_test <- data.frame(iv1,pred_test) #clusters
  
  ds_new <- split.data.frame(clustdata_test,clustdata_test$pred_test) #splitting data according to clusters
  clustdata_test$pred_test <- NULL
  
  # distance between new transaction to centroid
  newtrans_dist=as.data.frame(matrix(NA,1,3))
  colnames(newtrans_dist)=c("cluster","cluster_dist","row_name")
  for(i in 1:length(clust1))
  {
    newt=flexclust:::dist2(clust1[[i]],center[i,],method = "euclidean")
    t=cbind(rep(rownames(center)[i],length(newt)),newt,rownames(clust1[[i]]))
    colnames(t)=colnames(newtrans_dist)
    newtrans_dist=rbind(newtrans_dist,t)
  }
  newtrans_dist=newtrans_dist[-1,]
  newtrans_dist$row_name=as.integer(newtrans_dist$row_name)
  newtrans_dist=newtrans_dist %>% arrange(row_name)
  
  clustdata_test=cbind(clustdata_test,newtrans_dist[,-3])
  
  cluster_dist <- split.data.frame(clustdata_test,clustdata_test$cluster)
  
  for(i in c(clusters)){
    cluster_dist$c1$anomaly <- ifelse(cluster_dist$c1$cluster_dist>average_interclass$c1, "Anomaly", "Not Anomaly")
  }
  
}

list_out1 <- Test(filename,dependent="sc.status",idcolumn = "")


