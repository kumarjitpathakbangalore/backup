setwd("C:\\Users\\inkpathak\\Desktop\\GS Sentiment")
senti<-read.csv("Data Key Driver.csv", header=T)
kp<-senti
summary(senti)
str(senti)
senti1=senti[complete.cases(senti),]
senti1=senti1[,-1]
str(senti1)

table(senti1$Likelihood.to.Recommend)
table(senti$Likelihood.to.Recommend)

# function to remove NA with Median
f=function(x){
  x<-as.numeric(as.character(x)) #first convert each column into numeric if it is from factor
  x[is.na(x)] =median(x, na.rm=TRUE) #convert the item with NA to median value from the column
  x #display the column
}
ss=data.frame(apply(kp,2,f))
str(ss)

summary(ss)

ss=ss[,-1]
ss=ss[,-1]

mo<-lm(Likelihood.to.Recommend~.,data=senti1)
summary(mo)
plot(mo)
table(senti1$recomend_1)


senti1$xx<-senti1$Likelihood.to.Recommend
if (senti1$Likelihood.to.Recommend>6) {
  senti1$recomend_1 =1
}else {
  senti1$recomend_1 =0
}

senti1$recomend_1= ifelse(senti1$Likelihood.to.Recommend>6,1,0)
senti2<-senti1[,-1]

library(MASS)

ridgeregres<-lm.ridge(recomend_1~.,senti2,lambda = seq(0,0.1,0.05))

ridgenew<-lm.ridge(recomend_1~.,senti2,lambda = 0.1)
summary(ridgeregres)



x<- senti2[,1:24]
y<-as.data.frame(senti2[,25])

install.packages("glmnet")
library(glmnet)

senti2$recomend_1
x <- model.matrix(as.factor(recomend_1)~.,data = senti2)
y <- as.factor(senti2$recomend_1)

# Ridge Regression
glmmod <- glmnet(x, y, alpha=0, family="binomial")
plot(glmmod,xvar="lambda",label=TRUE)

cv.ridge=cv.glmnet(x,y,alpha=0, family="binomial")
plot(cv.ridge)
coef(cv.ridge)

# lasso regression
fit.lasso=glmnet(x,y,alpha=1)
plot(fit.lasso,xvar="lambda",label=TRUE)
plot(fit.lasso,xvar="dev",label=TRUE)

cv.lasso=cv.glmnet(x,y, family="binomial")
plot(cv.lasso)

coef(cv.lasso)

plot(fit.lasso,xvar="dev",label=TRUE)


####### Ridge with MASS

library(MASS)

lm_ridge <- lm.ridge( recomend_1~.,data = senti2,lambda = 0, family  = binomial)
summary(lm_ridge)
coef(lm_ridge)
lm_ridge$

#
fit.lasso <- glmnet(x, y, family="binomial", alpha=1)
fit.ridge <- glmnet(senti2[,1:24], senti2[,25], family="binomial", alpha=0)
fit.elnet <- glmnet(senti2[,1:24], senti2[,25], family="binomial", alpha=.5)

x=as.data.frame(matrix(rnorm(100*20),100,20)
g2=sample(1:2,100,replace=TRUE)
fit2=glmnet(x,g2,family="binomial")

