#####################################     SVM


library(e1071)

#The Gaussian and Laplace RBF and Bessel kernels are general-purpose kernels used when there is no prior knowledge about the data. 
#The linear kernel is useful when dealing with large sparse data vectors as is usually the case in text categorization. 
#The polynomial kernel is popular in image processing and the sigmoid kernel is mainly used as a proxy for neural networks. 
#The splines and ANOVA RBF kernels typically perform well in regression problems.
.  the linear kernel implementing the simplest of all kernel functions
k(x,x0) = hx,x0i	(15)
.	the Gaussian Radial Basis Function (RBF) kernel
k(x,x0) = exp(?????kx ??? x0k2)	(16)
.	the polynomial kernel
k(x,x0) = scale . hx,x0i + offsetdegree	(17)
.	the hyperbolic tangent kernel
k(x,x0) = tanh scale . hx,x0i + offset	(18)
.	the Bessel function of the first kind kernel
,	 	(19)
(kx ??? x0k)???n(??+1)
.	the Laplace Radial Basis Function (RBF) kenrel
k(x,x0) = exp(?????kx ??? x0k)	(20)
.	the ANOVA radial basis kernel



setwd("C:/Users/inkpathak/Desktop/Test data")
svmd<-read.csv("corona_phase_SVM.csv", header=TRUE)

str(svmd)


svmd$Driverid=as.factor(svmd$Driverid)

smmd=svmd[,-c(18:20)]
svmd=smmd[,-c(1)]


# dividing the training and testing set
set.seed(10)
sub <- sample(nrow(svmd), floor(nrow(svmd) * 0.8))
train <- svmd[sub, ]
test <- svmd[-sub, ]

install.packages("mlearning")

library("e1071")
library(kernlab)
library(ROCR)
library(kknn)
library(class)
library(rpart)
library(rpart.plot)
library(randomForest)
library(caret)
library(sqldf)
library(SDMTools)
library(mlearning)


tobj <- tune.svm(Driverid ~ ., data = train, gamma = 10^(-6:-3), cost = 10^(1:2)) 
summary(tobj)


#model on training data

#{  model <- svm(Driverid  ~ ., data = train,
               method = "C-classification", kernel = "radial",
               cost = 10, gamma = 0.1)
#summary(model)

#We can visualize a 2-dimensional projection of the data with highlighting classes and support vectors (see Figure 2):

#plot(model, train, Petal.Width ~
           +  Petal.Length, slice = list(Sepal.Width = 3, +	Sepal.Length = 4))

#pred <- predict(model, train, decision.values = TRUE)

}


estimate<-tune.svm(Driverid~.,data=train,kernel="radial",gamma=10^(-5:-1),cost=10^(1:4),cross=10)

#plot(estimate)
estimate$best.parameters

train.svm<-svm(Driverid~.,data=train,scale=FALSE,type="C-classification",
               kernel="radial",
               gamma=0.01,cost=10,nu=0.5,class.weight=NULL,cachesize=40,
               tolerance=0.001,epsilon=0.1,probability=TRUE,fitted=TRUE,
               na.action=na.omit)

summary(train.svm)

### checking the model accuracy in Testing data ###

mc1<-table(predict(train.svm,test),test$Driverid)
accu<-(mc1[1,1]+mc1[2,2]+mc1[3,3])/sum(mc1)
accu
mc1

conf.mat<- confusion(predict(train.svm,test), test$Driverid)
conf.mat
plot(conf.mat)




