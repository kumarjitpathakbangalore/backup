setwd("F:/Practice R")
AR<-read.csv("TIMESERIES.csv",header=T)
AR

# Once you have read the time series data into R, the next step is to store the data in a time series object in R, so that
#you can use R's many functions for analysing time series data. To store the data in a time series object, we use the
#ts() function in R.

timeseries <- ts(AR) # this just added up 37 unique no randomly as there are 37 data points. This is not desired
timeseries


# Sometimes the time series data set that you have may have been collected at regular intervals that were less than
#one year, for example, monthly or quarterly. In this case, you can specify the number of times that data was
#collected per year by using the 'frequency' parameter in the ts() function. For monthly time series data, you set
#frequency=12, while for quarterly time series data, you set frequency=4.


timeseries <- ts(AR$actual,frequency=12,start=c(2012,4));
timeseries


#OUT PUT

#Jan   Feb   Mar   Apr   May   Jun   Jul   Aug   Sep   Oct   Nov   Dec
#2010                    7328 28162 23829 24817 19275 37751 36065 24898 18607
#2011 70938 56057 58215 38399 30973 27159 19808 37411 15934 32360 34358 14478
#2012 39647 31623 57908 22964 27153 20596 28895 22459 23960 17102 12877  9209
#2013 41388 35211 23049  7944 



plot.ts(timeseries)


# Some times, it may appears that an additive model is not appropriate for describing this time series, since the size of
#the seasonal fluctuations and random fluctuations seem to increase with the level of the time series. Thus, we may
#need to transform the time series in order to get a transformed time series that can be described using an additive
#model. For example, we can transform the time series by calculating the natural log of the original data:


logtimeseries <- log(timeseries)
plot.ts(logtimeseries)


## Library

library(stats);
library(tseries);
library(forecast);
library(data.table);
library(lawstat);
install.packages("lawstat")

# Test for Stationarity

ts_stationary_kpss <- kpss.test(timeseries); ### small p-values suggests that the series is not stationary


#____________________Decomposing Time Series

#Decomposing a time series means separating it into its constituent components, which are usually a trend component
#and an irregular component, and if it is a seasonal time series, a seasonal component.


# A non-seasonal time series consists of a trend component and an irregular component. Decomposing the time
#series involves trying to separate the time series into these components, that is, estimating the the trend component
#and the irregular component.
#To estimate the trend component of a non-seasonal time series that can be described using an additive model, it is
#common to use a smoothing method, such as calculating the simple moving average of the time series.
#The SMA() function in the "TTR" R package can be used to smooth time series data using a simple moving
#average. To use this function, we first need to install the "TTR" R package (for instructions on how to install an
#                                                                             R package, see How to install an R package). Once you have installed the "TTR" R package, you can load the
#"TTR" R package by typing:
#  > library("TTR")
#You can then use the "SMA()" function to smooth time series data. To use the SMA() function, you need to specify
#the order (span) of the simple moving average, using the parameter "n". For example, to calculate a simple moving
#average of order 5, we set n=5 in the SMA() function.

TRY with different order to have a smooth curve

library("TTR")

timeseriesSMA8 <- SMA(timeseries,n=6)
plot.ts(timeseriesSMA8)


#___________________________Decomposing Seasonal Data


#A seasonal time series consists of a trend component, a seasonal component and an irregular component. Decomposing
#the time series means separating the time series into these three components: that is, estimating these three
#components.#####

#To estimate the trend component and seasonal component of a seasonal time series that can be described using
#an additive model, we can use the "decompose()" function in R. This function estimates the trend, seasonal, and
#irregular components of a time series that can be described using an additive model.
#The function "decompose()" returns a list object as its result, where the estimates of the seasonal component, trend
#component and irregular component are stored in named elements of that list objects, called "seasonal", "trend",
#and "random" respectively.


timeseriescomponents <- decompose(timeseries)

plot(timeseriescomponents)  # this will show the Observed, random, Seasonal, Trend seperately


#______________________________Seasonally Adjusting


#If you have a seasonal time series that can be described using an additive model, you can seasonally adjust the
#time series by estimating the seasonal component, and subtracting the estimated seasonal component from the
#original time series. We can do this using the estimate of the seasonal component calculated by the "decompose()"
#function.

#For example, to seasonally adjust the time series of the number of births per month in New York city, we can
#estimate the seasonal component using "decompose()", and then subtract the seasonal component from the original
#time series:

timeseriescomponents <- decompose(timeseries)
timeseriesseasonallyadjusted <- timeseries - timeseriescomponents$seasonal  # This is how we might want to treat each of this component seperately



plot(timeseriesseasonallyadjusted)


#_____________________Forecasts using Exponential Smoothing

#Exponential smoothing can be used to make short-term forecasts for time series data

#______________________Simple Exponential Smoothing

#If you have a time series that can be described using an additive model with constant level and no seasonality, you
#can use simple exponential smoothing to make short-term forecasts
#The simple exponential smoothing method provides a way of estimating the level at the current time point.
#Smoothing is controlled by the parameter alpha; for the estimate of the level at the current time point. The
#value of alpha; lies between 0 and 1. Values of alpha that are close to 0 mean that little weight is placed on the
#most recent observations when making forecasts of future values.
____________________________________________Example code from other__________________________________________________
no_of_weeks_to_predict <- 8;

train_series <- window(timeseries,end=c(2014,12)); ## Selecting the train series
test_series <- window(timeseries,start=c(2015,1)); ## Selecting the test series

### ARIMA

ts_data_arima   <- auto.arima(train_series,stepwise=TRUE,trace=TRUE); ### Applying ARIMA model -- (AR(p),I(d),MA(q))
forecast_arima <- forecast(ts_data_arima,h=no_of_weeks_to_predict,level=c(95,80)); ### prediction ARIMA

png_file_name_arima <- "Arima.png";
png(png_file_name_arima,width=1000);
plot.forecast(forecast_arima,col="red");
lines(fitted(forecast_arima),col="blue");
lines(test_series,col="darkorange",lwd=2);
dev.off();

accuracy_arima <- data.frame(accuracy(forecast_arima));

### HW Additive

ts_data_hw_add <-  HoltWinters(train_series,alpha=NULL, beta=NULL, gamma=NULL,seasonal="additive"); ### Applying Holt winters Additive
forecast_hw_add <- forecast(ts_data_hw_add,h=4,level=c(95,80)); ### prediction Holt winters Additive

png_file_name_hw_add <- "hw_additive.png";
png(png_file_name_hw_add,width=1000);

plot.forecast(forecast_hw_add,col="red"); # these 3 lins are important to plot predicted vs actual VVI
lines(fitted(forecast_hw_add),col="blue");
lines(test_series,col="darkorange",lwd=2);

dev.off();

accuracy_hw_add <- data.frame(accuracy(forecast_hw_add));

### HW Multiplicative

ts_data_hw_mult <-  HoltWinters(train_series,alpha=NULL, beta=NULL, gamma=NULL,seasonal="multiplicative"); ### Applying Holt winters Multiplicative
forecast_hw_mult <- forecast(ts_data_hw_mult,h=no_of_weeks_to_predict,level=c(95,80)); ### prediction Holt winters Multiplicative

png_file_name_hw_mult <- "hw_multiplicative.png";
png(png_file_name_hw_mult,width=1000);
plot.forecast(forecast_hw_mult,col="red");
lines(fitted(forecast_hw_mult),col="blue");
lines(test_series,col="darkorange",lwd=2);
dev.off();

accuracy_hw_mult <- data.frame(accuracy(forecast_hw_mult));

## T bats

ts_data_tbats 	<- tbats(train_series, use.box.cox=NULL, use.trend=NULL, use.damped.trend=NULL, seasonal.periods=NULL); ### Applying TBATS
forecast_tbats <- forecast(ts_data_tbats,h=no_of_weeks_to_predict,level=c(95,80)); ### prediction TBATS

png_file_name_tbats <- "hw_tabats.png";
png(png_file_name_tbats,width=1000);
plot.forecast(forecast_tbats,col="red");
lines(fitted(forecast_tbats),col="blue");
lines(test_series,col="darkorange",lwd=2);
dev.off();

accuracy_tbats <- data.frame(accuracy(forecast_tbats)); 
accuracy_metrix <- rbind(accuracy_arima,accuracy_hw_add,accuracy_hw_mult,accuracy_tbats);

__________________________________________________________________________________________________

HW <- HoltWinters(train_series, beta=FALSE, gamma=FALSE)
HW

#OUTPUT
Smoothing parameters:
  alpha: 0.451403
beta : FALSE
gamma: FALSE

Coefficients:
  [,1]
a 13737.67

#The output of HoltWinters() tells us that the estimated value of the alpha parameter is about 0.4514. This is not
#close to zero, telling us that the forecasts are not based on both recent and less recent observations but with a lag (although somewhat
#more weight is placed on recent observations

HW$fitted #  it gives the value of fitted line for fitted line
plot(HW)

HW$SSE   # It gives value of the SSE on Holt Winter 

# It is common in simple exponential smoothing to use the first value in the time series as the initial value for the level.
HoltWinters(train_series, beta=FALSE, gamma=FALSE, l.start=7328)

#As explained above, by default HoltWinters() just makes forecasts for the time period covered by the original
#data, which is 1813-1912 for the rainfall time series. We can make forecasts for further time points by using the
#"forecast.HoltWinters()" function in the R "forecast" package

HWforecast <- forecast.HoltWinters(HW, h=8)


plot.forecast(HWforecast)

#The forecast.HoltWinters() function gives you the forecast for a year, a 80% prediction interval for the forecast,
and a 95% prediction interval for the forecast. For example, the forecasted rainfall for 1920 is about 24.68 inches,
with a 95% prediction interval of (16.24, 33.11).



#The in-sample forecast errors are stored in the named element "residuals" of the list variable returned by forecast.
#HoltWinters(). If the predictive model cannot be improved upon, there should be no correlations between
#forecast errors for successive predictions. In other words, if there are correlations between forecast errors for
#successive predictions, it is likely that the simple exponential smoothing forecasts could be improved upon by
#another forecasting technique.

#To figure out whether this is the case, we can obtain a correlogram of the in-sample forecast errors for lags 1-20.
#We can calculate a correlogram of the forecast errors using the "acf()" function in R. To specify the maximum lag
#that we want to look at, we use the "lag.max" parameter in acf().

acf(HWforecast$residuals, lag.max=12) # to get the ACF curve and check for the order of "P"

pacf(HWforecast$residuals, lag.max=12) # to get the ACF curve and check for the order of "Q"

#You can see from the sample correlogram that the autocorrelation at lag 3 is just touching the significance bounds.
#To test whether there is significant evidence for non-zero correlations at lags 1-20, we can carry out a Ljung-
#  Box Jenkin test .                    This can be done in R using the "Box.test()", function. The maximum lag that we want to look at is
# specified using the "lag" parameter in the Box.test() function


Box.test(HWforecast$residuals, lag=12, type="Ljung-Box")


#   _________OUTPUT 
Box-Ljung test
data:  HWforecast$residuals
X-squared = 5.1878, df = 12, p-value = 0.9514

#Here the Ljung-Box test statistic is 5.18, and the p-value is 0.9, so there is little evidence of non-zero autocorrelations
#in the in-sample forecast errors at lags 1-20.


#To be sure that the predictive model cannot be improved upon, it is also a good idea to check whether the forecast
#errors are normally distributed with mean zero and constant variance. To check whether the forecast errors have
#constant variance, we can make a time plot of the in-sample forecast errors:


plot.ts(HWforecast$residuals) # this plot shows there are high flactuation in the error





#To check whether the forecast errors are normally distributed with mean zero, we can plot a histogram of the forecast
#errors, with an overlaid normal curve that has mean zero and the same standard deviation as the distribution
#of forecast errors. To do this, we can define an R function "plotForecastErrors()", below:

plotForecastErrors <- function(forecasterrors)
  {
    # make a histogram of the forecast errors:
    mybinsize <- IQR(forecasterrors)/4
    mysd <- sd(forecasterrors)
    mymin <- min(forecasterrors) - mysd*5
    mymax <- max(forecasterrors) + mysd*3
    # generate normally distributed data with mean 0 and standard deviation mysd
    mynorm <- rnorm(10000, mean=0, sd=mysd)
    mymin2 <- min(mynorm)
    mymax2 <- max(mynorm)
    if (mymin2 < mymin) { mymin <- mymin2 }
    if (mymax2 > mymax) { mymax <- mymax2 }
    # make a red histogram of the forecast errors, with the normally distributed data overlaid:
    mybins <- seq(mymin, mymax, mybinsize)
    hist(forecasterrors, col="red", freq=FALSE, breaks=mybins)
    # freq=FALSE ensures the area under the histogram = 1
    # generate normally distributed data with mean 0 and standard deviation mysd
    myhist <- hist(mynorm, plot=FALSE, breaks=mybins)
    # plot the normal curve as a blue line on top of the histogram of forecast errors:
    points(myhist$mids, myhist$density, type="l", col="blue", lwd=2)
}


#Now to apply the user defined function

plotForecastErrors(HWforecast$residuals)


#The plot shows that the distribution of forecast errors is roughly centred on zero, and is more or less normally
#distributed, although it seems to be slightly skewed to the right compared to a normal curve. However, the right
#skew is relatively small, and so it is plausible that the forecast errors are normally distributed with mean zero.
#The Ljung-Box test showed that there is little evidence of non-zero autocorrelations in the in-sample forecast
#errors, and the distribution of forecast errors seems to be normally distributed with mean zero. This suggests
#that the simple exponential smoothing method provides an adequate predictive model


#Furthermore, the assumptions that the 80% and 95% predictions intervals
#were based upon (that there are no autocorrelations in the forecast errors, and the forecast errors are normally
#  distributed with mean zero and constant variance) are probably valid.







#__________________________Holt's Exponential Smoothing______________________


#If you have a time series that can be described using an additive model with increasing or decreasing trend and no
#seasonality, you can use Holt's exponential smoothing to make short-term forecasts.
#Holt's exponential smoothing estimates the level and slope at the current time point. Smoothing is controlled by
#two parameters, alpha, for the estimate of the level at the current time point, and beta for the estimate of the slope
#b of the trend component at the current time point. As with simple exponential smoothing, the paramters alpha
#and beta have values between 0 and 1, and values that are close to 0 mean that little weight is placed on the most
#recent observations when making forecasts of future values

#To make forecasts, we can fit a predictive model using the HoltWinters() function in R. To use HoltWinters() for
#Holt's exponential smoothing, we need to set the parameter gamma=FALSE (the gamma parameter is used for  Holt-Winters exponential smoothing, as described below).


HWforecast_exp_smooth <- HoltWinters(train_series, gamma=FALSE)
HWforecast_exp_smooth
HWforecast_exp_smooth$SSE
# Output below

Smoothing parameters:
  alpha: 0.6438639
beta : 0.2893684
gamma: FALSE

Coefficients:
  [,1]
a  9983.008
b -3241.634

# Explanasion

#The estimated value of alpha is 0.64, and of beta is 1.00. These are both high, telling us that both the estimate
#of the current value of the level, and of the slope b of the trend component, are based mostly upon very recent
#observations in the time series. This makes good intuitive sense, since the level and the slope of the time series
#both change quite a lot over time. The value of the sum-of-squared-errors for the in-sample forecast errors is
#9654416479.


plot(HWforecast_exp_smooth) #We can plot the original time series as a black line, with the forecasted values as a red line on top of that


#If you wish, you can specify the initial values of the level and the slope b of the trend component by using the
#"l.start" and "b.start" arguments for the HoltWinters() function. It is common to set the initial value of the level
#to the first value in the time series (608 for the skirts data), and the initial value of the slope to the second value
#minus the first value (9 for the skirts data).


HoltWinters(train_series, gamma=FALSE, l.start=608, b.start=9) #This is not tested here


# rest other procedures are same

-------------------------------------------------------------------------------
#------------------------Holt-Winters Exponential Smoothing for seasonal variation
--------------------------------------------------------------------------------

  
 # If you have a time series that can be described using an additive model with increasing or decreasing trend and
#seasonality, you can use Holt-Winters exponential smoothing to make short-term forecasts.
  
#Holt-Winters exponential smoothing estimates the level, slope and seasonal component at the current time point.
#Smoothing is controlled by three parameters: alpha, beta, and gamma, for the estimates of the level, slope b of
#the trend component, and the seasonal component, respectively, at the current time point. The parameters alpha,
#beta and gamma all have values between 0 and 1, and values that are close to 0 mean that relatively little weight is
#placed on the most recent observations when making forecasts of future values


log_train_series <- log(train_series)

HWforecast_exp_smooth_with_seasonal <- HoltWinters(train_series)
HWforecast_exp_smooth_with_seasonal

HWforecast_exp_smooth_with_seasonal$SSE


#The estimated values of alpha, beta and gamma are 0.41, 0.00, and 0.96, respectively. The value of alpha (0.41)
#is relatively low, indicating that the estimate of the level at the current time point is based upon both recent
#observations and some observations in the more distant past. The value of beta is 0.00, indicating that the estimate
#of the slope b of the trend component is not updated over the time series, and instead is set equal to its initial value.
#This makes good intuitive sense, as the level changes quite a bit over the time series, but the slope b of the trend
#component remains roughly the same. In contrast, the value of gamma (0.96) is high, indicating that the estimate
#of the seasonal component at the current time point is just based upon very recent observations.

plot(HWforecast_exp_smooth_with_seasonal)

HWforecast_exp_smooth_with_seasonal_forecast <- forecast.HoltWinters(HWforecast_exp_smooth_with_seasonal, h=10)
plot.forecast(HWforecast_exp_smooth_with_seasonal_forecast)



#  Now to test the predicted vs actual with additive decomposition with a graphical plot

forecast_hw_add_decompoition <- forecast(HWforecast_exp_smooth_with_seasonal_forecast ,h=12,level=c(95,80)); ### prediction Holt winters Additive

plot.forecast(forecast_hw_add_decompoition,col="orange", main="Additive Decomposition and exponential Smoothing"); # these 3 lins are important to plot predicted vs actual VVI
lines(fitted(forecast_hw_add_decompoition),col="blue");
lines(test_series,col="red",lwd=2);
lines(train_series,col="green",lwd=2);


accuracy_hw_add_decompoition <- data.frame(accuracy(forecast_hw_add_decompoition));
accuracy_hw_add_decompoition


acf(forecast_hw_add_decompoition$residuals, lag.max=20)
Box.test(forecast_hw_add_decompoition$residuals, lag=20, type="Ljung-Box")

#OUT PUT
Box-Ljung test
data:  forecast_hw_add_decompoition$residuals
X-squared = 4.5215, df = 20, p-value = 0.9999

#The correlogram shows that the autocorrelations for the in-sample forecast errors do not exceed the significance
#bounds for lags 1-20. Furthermore, the p-value for Ljung-Box test is 0.6, indicating that there is little evidence of
#non-zero autocorrelations at lags 1-20.

plot.ts(forecast_hw_add_decompoition$residuals) # make a time plot
plotForecastErrors(forecast_hw_add_decompoition$residuals) #make a histogram




#____--------------------ARIMA Models


#Exponential smoothing methods are useful for making forecasts, and make no assumptions about the correlations
#between successive values of the time series. However, if you want to make prediction intervals for forecasts made
#using exponential smoothing methods, the prediction intervals require that the forecast errors are uncorrelated and
#are normally distributed with mean zero and constant variance.

#While exponential smoothing methods do not make any assumptions about correlations between successive values
#of the time series, in some cases you can make a better predictive model by taking correlations in the data into
#account. Autoregressive Integrated Moving Average (ARIMA) models include an explicit statistical model for the
#irregular component of a time series, that allows for non-zero autocorrelations in the irregular component

#ARIMA models are defined for stationary time series. Therefore, if you start off with a non-stationary time series,
#you will first need to 'difference' the time series until you obtain a stationary time series. If you have to difference
#the time series d times to obtain a stationary series, then you have an ARIMA(p,d,q) model, where d is the order
#of differencing used.

You can difference a time series using the "diff()" function in R.

fit <- Arima(train_series,order=c(0,0,0))
plot(forecast(fit,h=20))
summary(fit)

arima212=arima(log.appl,order=c(2,1,2))
summary(arima212)




fitets <- ets(train_series)
plot(forecast(fitets))

ets(y, model="ZZZ", damped=NULL, alpha=NULL, beta=NULL, gamma=NULL,
    phi=NULL, additive.only=FALSE, lambda=NULL,
    lower=c(rep(0.0001,3), 0.8), upper=c(rep(0.9999,3),0.98),
    opt.crit=c("lik","amse","mse","sigma","mae"), nmse=3,
    bounds=c("both","usual","admissible"), ic=c("aicc","aic","bic"),
    restrict=TRUE, allow.multiplicative.trend=FALSE, use.initial.values=FALSE, ...)


#y a numeric vector or time series
#model Usually a three-character string identifying method using the framework terminologyof Hyndman et al. (2002) and Hyndman et al. (2008). The first letter denotes the error type ("A", "M" or "Z"); the second letter denotes the trend type
#("N","A","M" or "Z"); and the third letter denotes the season type ("N","A","M"
                                                                    or "Z"). In all cases, "N"=none, "A"=additive, "M"=multiplicative and "Z"=automatically
#selected. So, for example, "ANN" is simple exponential smoothing with additive
#errors, "MAM" is multiplicative Holt-Winters' method with multiplicative
#errors, and so on.
#It is also possible for the model to be of class "ets", and equal to the output
#from a previous call to ets. In this case, the same model is fitted to y without
#re-estimating any smoothing parameters. See also the use.initial.values
#argument.
#damped If TRUE, use a damped trend (either additive or multiplicative). If NULL, both
#damped and non-damped trends will be tried and the best model (according to
#                                                               the information criterion ic) returned.
#alpha Value of alpha. If NULL, it is estimated.
#beta Value of beta. If NULL, it is estimated.
#gamma Value of gamma. If NULL, it is estimated.
#phi Value of phi. If NULL, it is estimated.
#additive.only If TRUE, will only consider additive models. Default is FALSE.
#lambda Box-Cox transformation parameter. Ignored if NULL. Otherwise, data transformed
#before model is estimated. When lambda=TRUE, additive.only is set
#to FALSE.
#lower Lower bounds for the parameters (alpha, beta, gamma, phi)
#upper Upper bounds for the parameters (alpha, beta, gamma, phi)
#opt.crit Optimization criterion. One of "mse" (Mean Square Error), "amse" (Average
#                                                                           MSE over first nmse forecast horizons), "sigma" (Standard deviation of residuals),
#"mae" (Mean of absolute residuals), or "lik" (Log-likelihood, the default).
#nmse Number of steps for average multistep MSE (1<=nmse<=10).
#bounds Type of parameter space to impose: "usual" indicates all parameters must lie
#between specified lower and upper bounds; "admissible" indicates parameters
#must lie in the admissible space; "both" (default) takes the intersection of these
#regions.
#Information criterion to be used in model selection.
#restrict If TRUE (default), the models with infinite variance will not be allowed.
#allow.multiplicative.trend
#If TRUE (default), models with multiplicative trend are allowed when searching
#for a model. Otherwise, the model space excludes them. This argument
#is ignored if a multiplicative trend model is explicitly requested (e.g., using
#                                                                    model="MMN").
#use.initial.values
#If TRUE and model is of class "ets", then the initial values in the model are also
#not re-estimated.
#... Other undocumented arguments.


findfrequency(train_series) #findfrequency returns the period of the dominant frequency of a time series. For seasonal data, it
#will return the seasonal period. For cyclic data, it will return the average cycle length.




