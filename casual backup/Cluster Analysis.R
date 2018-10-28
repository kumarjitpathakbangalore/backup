setwd("E:/Hayatt/Vivek")
data<-read.csv("Cluster.csv",header=T)
mydata<-data



# Prepare Data
mydata <- na.omit(mydata) # listwise deletion of missing
mydata <- scale(mydata) # standardize variables



# Determine number of clusters
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:6) wss[i] <- sum(kmeans(mydata, 
                                     centers=i)$withinss)
plot(1:6, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")



# K-Means Cluster Analysis
fit <- kmeans(mydata, 4) # 5 cluster solution
# get cluster means 
aggregate(mydata,by=list(fit$cluster),FUN=mean)
# append cluster assignment
mydata <- data.frame(mydata, fit$cluster)




# Ward Hierarchical Clustering
d <- dist(mydata, method = "euclidean") # distance matrix
fit <- hclust(d, method="ward") 
plot(fit) # display dendogram
groups <- cutree(fit, k=5) # cut tree into 5 clusters
# draw dendogram with red borders around the 5 clusters 
rect.hclust(fit, k=5, border="red")




# Ward Hierarchical Clustering with Bootstrapped p values
library(pvclust)
fit <- pvclust(mydata, method.hclust="ward",
               method.dist="euclidean")
plot(fit) # dendogram with p values
# add rectangles around groups highly supported by the data
pvrect(fit, alpha=.95)




# Model Based Clustering
library(mclust)
fit <- Mclust(mydata)
plot(fit) # plot results 
summary(fit) # display the best model



# K-Means Clustering with 5 clusters
fit <- kmeans(mydata, 5)

# Cluster Plot against 1st 2 principal components

# vary parameters for most readable graph
library(cluster) 
clusplot(mydata, fit$cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=0)

# Centroid Plot against 1st 2 discriminant functions
library(fpc)
plotcluster(mydata, fit$cluster)


# comparing 2 cluster solutions
library(fpc)
cluster.stats(d, fit1$cluster, fit2$cluster)


#############################################CLUSTER ANALYSIS#################################################

iris2 <- iris
iris2$Species <- NULL
(kmeans.result <- kmeans(iris2, 3)) # kmeans(dataset name, no of cluster) 

table(iris$Species, kmeans.result$cluster) # table( target variable , calculated model variable)

# how to plot the cluster in different colur in graph
plot(iris2[c("Sepal.Length", "Sepal.Width")], col = kmeans.result$cluster)

# plot cluster centers
points(kmeans.result$centers[,c("Sepal.Length", "Sepal.Width")], col = 1:3, pch = 8, cex=2)


#--------------------------------------The k-Medoids Clustering

#major difference between them is that: while a cluster
is represented with its center in the k-means algorithm, it is represented with the object closest to
the center of the cluster in the k-medoids clustering. The k-medoids clustering is more robust than
k-means in presence of outliers. PAM (Partitioning Around Medoids) is a classic algorithm for
k-medoids clustering. While the PAM algorithm is inefficient for clustering large data, the CLARA
algorithm is an enhanced technique of PAM by drawing multiple samples of data, applying PAM
on each sample and then returning the best clustering. It performs better than PAM on larger
data. Functions pam() and clara() in package cluster [Maechler et al., 2012] are respectively implementations
of PAM and CLARA in R. For both algorithms, a user has to specify k, the number
of clusters to find. As an enhanced version of pam(), function pamk() in package fpc [Hennig, 2010]
does not require a user to choose k. Instead, it calls the function pam() or clara() to perform a
partitioning around medoids clustering with the number of clusters estimated by optimum average
silhouette width.



library(fpc)
pamk.result <- pamk(iris2,3) # here 3 is no of cluster
# number of clusters
pamk.result$nc

# check clustering against actual species
table(pamk.result$pamobject$clustering, iris$Species)


layout(matrix(c(1,2),1,2)) # 2 graphs per page
plot(pamk.result$pamobject)
layout(matrix(1)) # change back to one graph per page


#--------------------------------Hierarchical Clustering
idx <- sample(1:dim(iris)[1], 40)
irisSample <- iris[idx,]
irisSample$Species <- NULL
hc <- hclust(dist(irisSample), method="ave")
#-----------------54 CHAPTER 6. CLUSTERING
plot(hc, hang = -1, labels=iris$Species[idx])
# cut tree into 3 clusters
rect.hclust(hc, k=3)
groups <- cutree(hc, k=3)









