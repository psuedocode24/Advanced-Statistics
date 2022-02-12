setwd("/Users/prasad/Desktop/Teaching/BAX493A/2022/Lectures/Class2")
rm(list=ls(all=TRUE)) 	# clear data

# update.packages(ask=FALSE,checkBuilt=TRUE,repos="https://cloud.r-project.org")

# install.packages(c("Rcpp", "caret", "forecast", "ggplot2", "quadprog"), dependencies=TRUE,repos="https://cloud.r-project.org")


library("forecast")
library("tseries") 		# reqired for adf.test of stationarity

data <- read.csv("data_week.csv", sep=",",dec=".",header=T) 	# weekly data
names(data)
head(data,5)

sales <- data[,3]/1000		# millions in xyz currency




##### Represeniting Data as Time Series Object #####

yy <- ts(sales, frequency = 52, start = c(2015,1))		# coverts sales data as time series object with start date and frequency (weekly here)
plot.ts(yy)												# ALWAYS plot time series to see patterns: trend, cycle, variance over time




##### General Process for Fitting ARIMA(p,d,q) x (P, D, Q) Models #####

# Typical values of (p,q) are 0, 1, 2. So you will have 3 ps x 3 qs = 9 models

# ARMA models require STATIONARY time series as inputs. Stationarity implies mean and variance are approx constant over time

# To assess stationarity, use adf.test(). If series is non-stationary, then take first difference. If first-differenced series still not stationary, take higher order difference. Usually d = 2 suffices for most time series. 

# Given p, d, q taking 3 values (0,1,2), you will have a set of 27 models. Apply AICC to select the best one or a set of few good ones. The "good ones" are within +/- 1 point from the Minimum AICC value

# If seasonal arima with (P, D, Q) were also included, then you will have additonal 27 models. So a total of 54 models.  Apply AICC to select the best one or a set of few good ones. 
 


## Let's learn the process using sales series

## Step 1. Is the time series stationary? 

# Use Augmented Dickey-Fuller Test to test stationarity == > large p-value means nonstationary

					
# install and load "tseries" package 
adf.test(yy)							# if p-value is large (> 0.10), then nonstationary

yd <- diff(yy,differences = 1)			
plot.ts(yd)								# looks stationary visually
adf.test(yd)							# estimated p = 0.01 => small p-value (< 0.10) => so yd is stationary ==> fix d = 1 in ARIMA models to be fitted




## Step 2. Decide AR(p) or MA(q) or both ARMA(p,q). Use the stationary series from Step 1. 

# To decide AR(p), plot Pacf. For AR(p) => Pacf becomes zero at some lag p

Pacf(yd, lag.max = 10)					# Pacf suggests p = 1 


# To decide MA, plot Acf. For MA(q) => Acf becomes zero at some lag q

Acf(yd, lag.max = 10)				# Acf suggests q = 1 




## Step 3. Fit several ARIMA models. 	

m1 <- Arima(yy, order = c(2,1,1))			# note: differencing (d = 1) is specified in the "order"; so fit the original yy series (yy, not yd)

m1				# see the output of m1. The estimated phi value and its std err to assess significnace

summary(m1)		# see Accuracy using MAPE = Mean Absolute Percentage Error 

# do this for other p,d,q values


m2 <- Arima(yy, order = c(2,1,0))			
m3 <- Arima(yy, order = c(1,1,1))	
# ... and so on

# Consider Seasonal ARIMA(p,d,q) x (P, D, Q) components when seasonality is expected/suspected

m4 <- Arima(yy, order=c(1,1,0), seasonal = list(order = c(0,0,1), period = 52))
m5 <- Arima(yy, order=c(2,1,0), seasonal = list(order = c(1,0,0), period = 52))

# ... and so on



## Step 4. Use Information Criteria to retain the "best model" 
## Use AIC_c if sample size is small (i.e., p/T ratio is large = 10% or more)
## Use BIC if sample size is large (i.e., p/T ratio is small = 5% of less)


## Step 5. Identify a **set of few good models**. Good models are within plus/minus 1 point difference from the smallest value of AIC_c or BIC values.  


## Step 6. Make Out-of-Sample Forecasts with Prediction Interval based on your retained model

m1.predict <- forecast:::forecast.Arima(m1, h = 52, level = c(68, 90))
plot(m1.predict)

summary(m1.predict)		# prints numerical values of forecasts and CIs.


# To see the numerical values of forecast and CIs, 

m2.predict <- forecast:::forecast.Arima(m2, h = 52, level = c(68, 90))
plot(m2.predict)

m3.predict <- forecast:::forecast.Arima(m3, h = 52, level = c(68, 90))
plot(m3.predict)

m4.predict <- forecast:::forecast.Arima(m4, h = 52, level = c(68, 90))
plot(m4.predict)

m5.predict <- forecast:::forecast.Arima(m5, h = 52, level = c(68, 90))
plot(m5.predict)





################ Automated Version #############
## The above steps (a bit more sophisticated) is written in a function called "auto.arima". 

m6 <- auto.arima(yy)		# fits ARIMA(p,d,q) x (P, D, Q) automatically

m6.predict <- forecast:::forecast.Arima(m6, h = 52, level = c(68, 90))
plot(m6.predict)

## So why use the above step-by-step procedure? 
## First, modeling is a process of discovery. Auto.arima can/does miss the best model. 
## Second, even when auto.arima finds the best model, it does not inform you about the **set of few good models** in the vicinity of the best model. Forecasts combination of a few good models incorporates this model uncertainty and provides *Consensus Forecast*.

## So do use auto.arima and *then* do plus/minus 1 on its reported (p,d,q) and (P,D,Q). Then base your forecasts/conclusions/recommendations from this set of good models to meet your business needs



##  Deliverables for HW 2

# 1. Step-by-Step fit different ARIMA (p,d,q) x (P, D, Q) for the caces and deaths. Can you discover a better model than auto.arima?

# 2. Selet the best model using information criteria (AIC_c, BIC), and present out-of-sample forecasts with prediction intervals

# 3. Discover a set of good models in the neighborhood of the best model. Then present the "Consensus Forecast" for cases and deaths over the next week (from Sunday to Saturday after the submission date). 








  