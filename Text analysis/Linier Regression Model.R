#***********************************************************************
#                        Author- Kumarjit Pathak                       #
#         Content- Kumarjit's way of doing linear regression model     #
#                           Date- 10-Sep-2017                          #
#***********************************************************************





"""In linear regression the biggest thing we need to keep in mind that this model 
assumes linearity of behaviour between dependent and independent variable"""


setwd("C:\\Users\\inkpathak\\Desktop\\GS Sentiment")
survaydata<-read.csv("Data Key Driver.csv", header=T)

# before direcetly jumping to fit a linear model need to do fe basic sanity check
# 
# is the data quality is good , I mean is there any missing value? If yes how do I impute them?
# Is there outlier in the data ? should I remove them before proceeding?
# whether variables are linear ? 
# is there any categorical variable on the training set?
# whether there is any correlation between predictor and dependent variable
# how is the density plot so that we can see at what interval of the data range it is influencing dependent variable
# whether Y (dependent) is following a Gaussian distribution or not
# is there any multicoliniarity exist?
# Is there any Autocorrelation exist?
# Is there any Hetaroscadasticity exist?


#************  Let's do one by one**************#


#Basic Exploration of the data first to have a feel about the data.

# list objects in the working environment
ls()   #[1] "survaydata"

# list the variables in mydata
names(survaydata)

# list the structure of mydata
str(survaydata)

# list levels of factor v1 in mydata
levels(survaydata$Account.Code)


# dimensions of an object
dim(survaydata) # output is row x column

# class of an object (numeric, matrix, data frame, etc)
class(survaydata)

# print first 10 rows of mydata
head(survaydata, n=10)

# print last 5 rows of mydata
tail(survaydata, n=5)



#______________In case you wanto to change any categorical or Ordinal variable into levels for better understanding__________

# variable v1 is coded 1, 2 or 3
# we want to attach value labels 1=red, 2=blue, 3=green

mydata$v1 <- factor(mydata$v1,
                    levels = c(1,2,3),
                    labels = c("red", "blue", "green"))


# variable y is coded 1, 3 or 5 
# we want to attach value labels 1=Low, 3=Medium, 5=High

mydata$v1 <- ordered(mydata$y,
                     levels = c(1,3, 5),
                     labels = c("Low", "Medium", "High"))

#________________________________________________________________________________




# Let's check do we have missing data?

summary(survaydata) # We have loads of missing data

# there are different ways of treating the missing values 
# Mean Imputation 
# Extreem Value Imputation
# Elimination of NULL
# Regression Imputation
# KNN imputation/ CART imputation--- My fav two
#  Based on the subject knowledge you may want to check the same, currently with our data we have different countries feedback on service 
#  hence just mean imputation will not make sence rather it will create issue on the analysis


#**************Missinbg value Treatment*************

# recode 99 to missing for variable v1
# select rows where v1 is 99 and recode column v1 
mydata<-survaydata # Just creating a copy so that original file is intact
mydata$v1[mydata$v1==99] <- NA # Where ever there is 99 value we are recoding it to NA


#Excluding Missing Values from Analyses

mean(survaydata$Reliability, na.rm=TRUE) 

# Mean Imputation
mydata$Reliability[is.na(mydata$Reliability)] <-mean(mydata$Reliability,na.rm=T) # Replacing the NA by respective mean

summary(mydata$Reliability)




# Hard- coded imputation

mydata$Reliability[is.na(mydata$Reliability)] <- 99

# Incase of the NA's existance are just a small proportion then mightr as well exclude them all then create a new data set

# Exaample of creating new data wiout any missing value 

# list rows of data that have missing values 
survay1<- survaydata[complete.cases(survaydata),]

# create new dataset without missing data 
survay2 <- na.omit(survaydata)


# for all other methods you can use Multivariate Imputation By Chained Equations (MICE) package

# https://www.rdocumentation.org/packages/mice/versions/2.25/topics/mice
# https://cran.r-project.org/web/packages/mice/mice.pdf

install.packages("mice")

library(mice)
library("lattice")
# Syntax
# mice(data, m = 5, method = vector("character", length = ncol(data)), predictorMatrix = (1 - diag(1, ncol(data))), visitSequence = (1:ncol(data))[apply(is.na(data), 2, any)], form = vector("character", length = ncol(data)), post = vector("character", length = ncol(data)), defaultMethod = c("pmm", "logreg", "polyreg", "polr"), maxit = 5, diagnostics = TRUE, printFlag = TRUE, seed = NA, imputationMethod = NULL, defaultImputationMethod = NULL, data.init = NULL, ...)

#Before going for any imputation create subset of the data for which you want to do thesse operations

#_____________________Example of subsetting data____________________________
# select variables v1, v2, v3
myvars <- c("v1", "v2", "v3")
newdata <- mydata[myvars]

# another method
myvars <- paste("v", 1:3, sep="")
newdata <- mydata[myvars]

# select 1st and 5th thru 10th variables
newdata <- mydata[c(1,5:10)]

# exclude variables v1, v2, v3
myvars <- names(mydata) %in% c("v1", "v2", "v3") 
newdata <- mydata[!myvars]

# exclude 3rd and 5th variable 
newdata <- mydata[c(-3,-5)]

# delete variables v3 and v5
mydata$v3 <- mydata$v5 <- NULL

# using subset function 
newdata <- subset(mydata, age >= 20 | age < 10, 
                  select=c(ID, Weight))

# take a random sample of size 50 from a dataset mydata 
# sample without replacement
mysample <- mydata[sample(1:nrow(mydata), 50,
                          replace=FALSE),]


#____________________________End of subsetting data__________________________________

# KNN imputation in R for missing value

# we can do the same using DMwR package which provides wide varity of techniques
install.packages("DMwR")
library(DMwR)
library(mice)

md.pattern(survaydata) # Missing data pattern for all the variables

knnOutput <- knnImputation(survaydata[, !names(survaydata) %in% "Likelihood.to.Recommend"])  # perform knn imputation apart from "likelihood to recomend which is a dependent variable.
anyNA(knnOutput)

#The limitation with DMwR::knnImputation is that it sometimes may not be appropriate to use when the missing value comes from a factor
# variable. Both rpart and mice has flexibility to handle that scenario. The advantage with rpart is that you just need only one of the variables to be non NA in the predictor fields

# Randome forest Imputation

library(mice)
library(randomForest)
# before executing the below please ensure that you don't have any index variable or high no of levels variable
# always try to keep it to the data set where all other variable can be considered independent variable for imputation

miceMod <- mice(survaydata[, !names(survaydata) %in% "Likelihood.to.Recommend"], method="rf")  # perform mice imputation, based on random forests.
miceOutput <- complete(miceMod)  # generate the completed data.
anyNA(miceOutput)

# mice has lot of other methods for imputation please check below and use as appropriate
"""
Built-in elementary imputation methods are:

pmm
Predictive mean matching (any)
norm
Bayesian linear regression (numeric)
norm.nob
Linear regression ignoring model error (numeric)
norm.boot
Linear regression using bootstrap (numeric)
norm.predict
Linear regression, predicted values (numeric)
mean
Unconditional mean imputation (numeric)
2l.norm
Two-level normal imputation (numeric)
2l.pan
Two-level normal imputation using pan (numeric)
2lonly.mean
Imputation at level-2 of the class mean (numeric)
2lonly.norm
Imputation at level-2 by Bayesian linear regression (numeric)
2lonly.pmm
Imputation at level-2 by Predictive mean matching (any)
quadratic
Imputation of quadratic terms (numeric)
logreg
Logistic regression (factor, 2 levels)
logreg.boot
Logistic regression with bootstrap
polyreg
Polytomous logistic regression (factor, >= 2 levels)
polr
Proportional odds model (ordered, >=2 levels)
lda
Linear discriminant analysis (factor, >= 2 categories)
cart
Classification and regression trees (any)
rf
Random forest imputations (any)
ri
Random indicator method for nonignorable data (numeric)
sample
Random sample from the observed values (any)
"""




# CART Imputation
# here you would see creating a model and then using it for imputation

library(rpart)
#In case of any discrete variable imputation---- independentVariableWithMissing and DependentVariable needs to be replaced as fit
class_mod <- rpart(independentVariableWithMissing ~ . - DependentVariable, data=survaydata[!is.na(survaydata$independentVariableWithMissing), ], method="class", na.action=na.omit)  # since rad is a factor
predicted_value <- predict(class_mod, survaydata[is.na(survaydata$independentVariableWithMissing), ])

# above two are not applicable here and hence going for continious variable imputation

# below needs to be done each variable at a time with misssing value beter to take a subset with complete case and then train the model
anova_mod <- rpart(Reliability ~ . - Likelihood.to.Recommend, data=survaydata[!is.na(survaydata$Reliability), ], method="anova", na.action=na.omit)  # since ptratio is numeric.
ptratio_pred <- predict(anova_mod, survaydata[is.na(survaydata$Reliability), ])

#http://r-statistics.co/Missing-Value-Treatment-With-R.html



# now we need to check how the imputation has worked -- good or bad do the below by each variable
library(VIM)
library(VIMGUI)
marginplot(mydata[c(1,2)]) # check for any two comumns

xyplot(tempData,Ozone ~ Wind+Temp+Solar.R,pch=18,cex=1)


densityplot(miceMod)

stripplot(miceMod$, pch = 20, cex = 1.2)

# Please note imputation is subject to your imagination and not limited by the examples I have just given



#********************* END OF IMPUTATION*********************

#***************************FEATURE SELECTION AND TRANSFORMATION*****************************


# Check minimum variance of the attributes which does not have enough disportions -- decide to remove them 
var(survay1$Likelihood.to.Recommend) # these step shouldd be done before imputation

# In case of high missing value more than 30% -- decide to remove them --- decide before imputation




# Now we have complete data with no missing value, as a next step we need to check in case we have outliers in the data

# Stage1 check correlation

# check for normality
# check bivariate plot
# check for transformation required or not
survay1<-survay1[,-(1:2)]

M<-as.data.frame(cor(survay1[,(1:5)]))

M<-cor(survay1[,(1:5)])
write.csv("correlation.csv",M,header=T)
plot(M)


write.csv(M,"correlation.csv")
# Visulaization of correlation
cor.prob(mydata)# correlation matrix with p-value

# "flatten" that table
flattenSquareMatrix(cor.prob(mydata))

library(PerformanceAnalytics)
chart.Correlation(survay1[,(1:5)])

install.packages("corrplot")
library(corrplot)
M<-cor(survay1[,(1:15)])
corrplot(M, method="circle")
corrplot(M, method="pie")
corrplot(M, method="color")
corrplot(M, method="number")
corrplot(M, type="upper")
corrplot(M, type="upper", order="hclust")

# Using different color spectrum
col<- colorRampPalette(c("red", "white", "blue"))(20)
corrplot(M, type="upper", order="hclust", col=col)

#Changing the color of the correlogram
library(RColorBrewer)
corrplot(M, type="upper", order="hclust", 
         col=brewer.pal(n=8, name="RdBu"))

# Combining correlogram with the significance test some times thouh the corr value might be high but non significant

# To compute the matrix of p-value, a custom R function is used 

# mat : is a matrix of data
# ... : further arguments to pass to the native R cor.test function
cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}



# matrix of the p-value of the correlation
p.mat <- cor.mtest(survay1[,(1:15)])
head(p.mat[, 1:5])


# Specialized the insignificant value according to the significant level
corrplot(M, type="upper", order="hclust", 
         p.mat = p.mat, sig.level = 0.01)

corrplot(M, type="upper", order="hclust", p.mat = p.mat, sig.level = 0.01, insig = "blank")

# color-wise and value wise visualization of correlation

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(M, method="color", col=col(200),  
         type="upper", order="hclust", 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, #Text label color and rotation
         # Combine with significance
         p.mat = p.mat, sig.level = 0.01, insig = "blank", 
         # hide correlation coefficient on the principal diagonal
         diag=FALSE 
)


# Now let's check wheather there are any outliers in our data or not

#Detecting Outliers, step 1:  Graphs
#create a histogram
hist(survay1$Professionalism, breaks = 10, freq = F, xlab = 'Lickerd Scale', ylab = 'Relative Frequency', main = 'Histogram of Proffessionalism')

#create a box and whisker plot
boxplot(survay1$Professionalism, ylab = 'Lickerd Scale', main = 'Proffessionalism')

# Plot of data with outliers.
par(mfrow=c(1, 2))
plot(survay1$Professionalism, survay1$Likelihood.to.Recommend, xlim=c(0, 28), ylim=c(0, 230), main="With Outliers", xlab="speed", ylab="dist", pch="*", col="red", cex=2)
abline(lm(Likelihood.to.Recommend ~ Professionalism, data=survay1), col="blue", lwd=3, lty=2)

# Plot of original data without outliers. Note the change in slope (angle) of best fit line.
plot(survay1$Professionalism, survay1$Likelihood.to.Recommend, xlim=c(0, 28), ylim=c(0, 230), main="Outliers removed \n A much better fit!", xlab="speed", ylab="dist", pch="*", col="red", cex=2)
abline(lm(Likelihood.to.Recommend ~ Professionalism, data=survay1), col="blue", lwd=3, lty=2)

#________________For Discrete variable_____________________

# For categorical variable
boxplot(Likelihood.to.Recommend ~ Country, data=survaydata, main="Countrywise distribution of (ikelihood to recomend)")  # clear pattern is noticeable.
boxplot(Likelihood.to.Recommend  ~ Month, data=survaydata, main="monthly distribution of (ikelihood to recomend)")  # this may not be significant, as day of week variable is a subset of the month var.




# For continuous variable (convert to categorical if needed.)
#boxplot(ozone_reading ~ pressure_height, data=ozone, main="Boxplot for Pressure height (continuos var) vs Ozone")
#boxplot(ozone_reading ~ cut(pressure_height, pretty(inputData$pressure_height)), data=ozone, main="Boxplot for Pressure height (categorial) vs Ozone", cex.axis=0.5)


#______________________________________________________________

"""
Cooks Distance

Cook's distance is a measure computed with respect to a given regression model and 
therefore is impacted only by the X variables included in the model. But, 
what does cook's distance mean? It computes the influence exerted by 
each data point (row) on the predicted outcome.


"""

survay1<-survay1[,-(1:2)]

#Bivariate analysis for outlier

mod <- lm(Likelihood.to.Recommend  ~ Professionalism, data=survay1)
cooksd <- cooks.distance(mod)

plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4*mean(cooksd, na.rm=T), col="red")  # add cutoff line
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), col="red")  # add labels

# Statisticaltext on outlier Bon-ferronni test

library(car)
outlierTest(mod)

# Finding out Influential observation
influential <- as.numeric(names(cooksd)[(cooksd > 4*mean(cooksd, na.rm=T))])  # influential row numbers
head(survay1[influential, c("Professionalism") ])  # influential observations.


#By Now we have understood that there is outlier in the data or not

#*************Treating outlier**************

# Removal of the rows with outlier with simple subsetting of the data

#Capping ----For missing values that lie outside the 1.5*IQR limits, we could cap it 
#           by replacing those observations outside the lower limit with the value of
#           5th %ile and those that lie above the upper limit, with the value of 
#           95th %ile

x <- survay1$Professionalism
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
x[x < (qnt[1] - H)] <- caps[1]
x[x > (qnt[2] + H)] <- caps[2]



#---------------Another Script for outlier check and removal-------------
# another way of autometically removing outlier: Below is the function for the same

outlierKD <- function(dt, var) {
  var_name <- eval(substitute(var),eval(dt))
  na1 <- sum(is.na(var_name))
  m1 <- mean(var_name, na.rm = T)
  par(mfrow=c(2, 2), oma=c(0,0,3,0))
  boxplot(var_name, main="With outliers")
  hist(var_name, main="With outliers", xlab=NA, ylab=NA)
  outlier <- boxplot.stats(var_name)$out
  mo <- mean(outlier)
  var_name <- ifelse(var_name %in% outlier, NA, var_name)
  boxplot(var_name, main="Without outliers")
  hist(var_name, main="Without outliers", xlab=NA, ylab=NA)
  title("Outlier Check", outer=TRUE)
  na2 <- sum(is.na(var_name))
  cat("Outliers identified:", na2 - na1, "n")
  cat("Propotion (%) of outliers:", round((na2 - na1) / sum(!is.na(var_name))*100, 1), "n")
  cat("Mean of the outliers:", round(mo, 2), "n")
  m2 <- mean(var_name, na.rm = T)
  cat("Mean without removing outliers:", round(m1, 2), "n")
  cat("Mean if we remove outliers:", round(m2, 2), "n")
  response <- readline(prompt="Do you want to remove outliers and to replace with NA? [yes/no]: ")
  if(response == "y" | response == "yes"){
    dt[as.character(substitute(var))] <- invisible(var_name)
    assign(as.character(as.list(match.call())$dt), dt, envir = .GlobalEnv)
    cat("Outliers successfully removed", "n")
    return(invisible(dt))
  } else{
    cat("Nothing changed", "n")
    return(invisible(var_name))
  }
}


#_____________Yet another simple script__________

remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}

survay1$Professionalism <- remove_outliers(survay1$Professionalism)


# Prediction
# yet another approach, the outliers can be replaced with missing values (NA) and then can be predicted by considering them as a response variable





# Now let's check for normality

#create a normal probability plot
qqout = qqnorm(survay1$Likelihood.to.Recommend, ylab = 'Lilihood to recomend', main = 'Lickert Scale')
qqline(survay1$Likelihood.to.Recommend)      #add a straight line to the normal probability plot

## Have a look at the densities
plot(density(survay1$Likelihood.to.Recommend));plot(density(survay1$Speed.of.response))


## Perform the test
shapiro.test(survay1$Likelihood.to.Recommend); shapiro.test(survay1$Speed.of.response)

# Skewness and Kurtosis checking 

kurtosis.test <- function (x) {
  m4 <- sum((x-mean(x))^4)/length(x)
  s4 <- var(x)^2
  kurt <- (m4/s4) - 3
  sek <- sqrt(24/length(x))
  totest <- kurt/sek
  pvalue <- pt(totest,(length(x)-1))
  pvalue 
}

skew.test <- function (x) {
  m3 <- sum((x-mean(x))^3)/length(x)
  s3 <- sqrt(var(x))^3
  skew <- m3/s3
  ses <- sqrt(6/length(x))
  totest <- skew/ses
  pt(totest,(length(x)-1))
  pval <- pt(totest,(length(x)-1))
  pval
}

skew.test(survay1$Likelihood.to.Recommend) # to check skewness
kurtosis.test(survay1$Likelihood.to.Recommend)

# Apart from this using simple package nortest we can achieve the same

library(nortest)

#Perform Anderson-Darling normality test
ad.test(survay1$Likelihood.to.Recommend)
#Perform Cramér-von Mises test for normality
cvm.test(survay1$Likelihood.to.Recommend)
#Perform Pearson chi-square test for normality
pearson.test(survay1$Likelihood.to.Recommend)
#Perform Shapiro-Francia test for normality
sf.test(survay1$Likelihood.to.Recommend)


# Let's look at bi-variate study between selected independent variables and dependent variables
# this is generally not done and hence the model many a times becomes prone to mistake

plot(survay1$Likelihood.to.Recommend ~ survay1$Professionalism) # Plot will help to identify the relaationship degree/ linear non linear

model = lm(survay1$Likelihood.to.Recommend ~ survay1$Professionalism) # Direct effect modelling - Individual Effect
model1 = lm(survay1$Likelihood.to.Recommend ~ survay1$Professionalism + survay1$Lead.times + survay1$Professionalism*survay1$Lead.times) # Joint Effect modelling - Coveriance effect
model2 = lm(survay1$Likelihood.to.Recommend ~ survay1$Professionalism^2) # Quadric Effect Modelling with degree 2
summary(model)
summary(model1)
summary(model2)

plot(model2) # to see different diagonistic of the bivariate analysis



coefficients(model) # model coefficients
confint(model, level=0.95) # CIs for model parameters 
fitted(model) # predicted values
residuals(model) # raw residuals
anova(model) # anova table 
vcov(model) # covariance matrix for model parameters 
influence(model) # regression diagnostics, including hat matrix

# Continue this process till you get the right combinations of variable which explains the dependent



# I think now after priliminary diagonistics you are good to go with first cut models

#Try with OLS (Ordinary Least Square)
survay1<-survay1[,-(1:2)]
model_ols<- lm(Likelihood.to.Recommend~.,data=survay1) # first column contains dependent variable


summary(model_ols)

plot(model_ols)
#Multiple R-squared:      1,	Adjusted R-squared:      1 # Error was That dependent variable was part of the predictor variable# remodevd the same

# it was observed to have a negative intercept along with r-square =100% clearly estimates are un reliable

library(car)
library(MASS)

# Regression Diagonistic plots
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page 
plot(model_ols)

#Checking for multicoliniarity using variation infletion factor
vif(model_ols)

# Checking for Auto-correlation , in case exist chek for differentition factor
library(lmtest)
dwtest(model_ols)

#There are probably many ways to do this but the first one that comes to mind is
#based on linear regression. You can regress the consecutive residuals against 
#each other and test for a significant slope. If there is auto-correlation, 
#then there should be a linear relationship between consecutive residuals
mod = lm(Likelihood.to.Recommend~.,data=survay1)
res = mod$res 
n = length(res) 
mod2 = lm(res[-n] ~ res[-1]) 
summary(mod2)

#mod2 is a linear regression of the time tt error, ??t??t, against the time t???1t???1 error, ??t???1??t???1. if the coefficient for res[-1] is significant, you have evidence of autocorrelation in the residuals

# Testing for Heteroscadasticity

# check from resedual vs fitted graph on the diagonistic plot of regression if 
#  The plots we are interested in are at the top-left and bottom-left. 
#The top-left is the chart of residuals vs fitted values, while in the bottom-left one, 
#it is standardised residuals on Y axis. If there is absolutely no heteroscedastity, 
#you should see a completely random, equal distribution of points throughout the 
#range of X axis and a flat red line.

#you can notice from the top-left plot, the red line is slightly curved and the residuals seem to increase as the fitted Y values increase. So, the inference here is, heteroscedasticity exists.


bptest(model_ols) # Breusch-Pagan test studentized Breusch-Pagan tes
library(car)
ncvTest(model_ols) # Breusch-Pagan test Non-constant Variance Score Test


# Both these test have a p-value less that a significance level of 0.05, therefore we can reject the null hypothesis that the variance of the residuals is constant and infer that heteroscedasticity is indeed present, thereby confirming our graphical inference.


# How do I rectify this?

# well start again with neew predictors or check the design of research
# Boxcox- Transformation
# Take error variance as a model parameter
# Divide by the variation--- Need to check this
library(caret)
survay1$Professionalism_tran <-BoxCoxTrans(survay1$Professionalism)

# Small heterso scadasticity does not matter much and hence need to decide wisely whether to treat Heteroscadasticity or not



# for Multicoliniarity you might want to delete the variable from equation or do PCA to derive new orthogonal feature which can be used in regression
# post every step iterate the model and compare the new model vs old one

# compare models

anova(model2, model_ols) # check for alll the combinations of models














# compare models
fit1 <- lm(y ~ x1 + x2 + x3 + x4, data=mydata)
fit2 <- lm(y ~ x1 + x2)
anova(fit1, fit2)


# K-fold cross-validation
library(DAAG)
cv.lm(df=mydata, fit, m=3) # 3 fold cross-validation
      
      



#Tocover-- Regularization
#    adaptive regression
# Gradient descent implementation
# Variable selection
# PCA modelling
# Geographical Bias elimination
# Robust Regression
# Cross validation







