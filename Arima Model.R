rm(list=ls(all=TRUE)) 	# clear data

# update.packages(ask=FALSE,checkBuilt=TRUE,repos="https://cloud.r-project.org")

# install.packages(c("Rcpp", "caret", "forecast", "ggplot2", "quadprog"), dependencies=TRUE,repos="https://cloud.r-project.org")

install.packages("forecast")
install.packages("quadprog")
install.packages("tseries")
install.packages("xlsx")


library("forecast")
library("tseries") 		# required for adf.test of stationarity
library("olsrr")
library("xlsx")

data <- read.xlsx2("United_States_COVID-19_Confirmed_Cases.xlsx", sheetIndex = 1,
                   startRow=4) 	#Covid Cases Data
names(data)
head(data,5)

data <- data[,8]
data
head(data,5)

cases <- data.frame(data)
cases
cases <- as.numeric(cases[41:723,])
cases


##### Representing Data as Time Series Object #####

yy <- ts(cases, frequency = 52, start = c(1,1))
plot.ts(yy)												# ALWAYS plot time series to see patterns: trend, cycle, variance over time

adf.test(yy)							# if p-value is large (> 0.10), then nonstationary

yd <- diff(yy,differences = 1)			
plot.ts(yd)								# looks stationary visually
adf.test(yd)							# estimated p = 0.01 => small p-value (< 0.10) => so yd is stationary ==> fix d = 1 in ARIMA models to be fitted






################ Automated Version #############
## The above steps (a bit more sophisticated) is written in a function called "auto.arima". 

auto_arima <- auto.arima(yy)		# fits ARIMA(p,d,q) x (P, D, Q) automatically

auto_arima <- forecast:::forecast.Arima(auto_arima, h = 7, level = c(68, 90))
plot(auto_arima)
summary(auto_arima)

## Auto ARIMA has PDQ as ARIMA(2,2,3), Performing plus/minus 1 on these

# Consider Seasonal ARIMA(p,d,q) x (P, D, Q) components when seasonality is expected/suspected

arima1 <- Arima(yy, order=c(1,1,0), seasonal = list(order = c(0,0,1), period = 52))
arima1_forecast <- forecast:::forecast.Arima(arima1, h = 7, level = c(68, 90))
plot(arima1)
summary(arima1)
AIC(arima1)
#or
arima1$aic

BIC = AIC(arima1, k = log(length(sunspots)))
BIC
#or
arima1$bic

arima1$aicc
#or
n = nrow(cases)
p = ncol(cases) + 1 

AIC(arima(x=sunspots)) + 2 * + 2*p*(p+1)/(n - p - 1)



arima2 <- Arima(yy, order=c(1,1,2), seasonal = list(order = c(1,0,0), period = 52))
arima2_forecast <- forecast:::forecast.Arima(arima2, h = 7, level = c(68, 90))
plot(arima2)
summary(arima2)
arima2$aic
arima2$bic
arima2$aicc


arima3 <- Arima(yy, order=c(2,2,0), seasonal = list(order = c(1,0,0), period = 52))
arima3_forecast <- forecast:::forecast.Arima(arima3, h = 7, level = c(68, 90))
plot(arima3)
summary(arima3)
arima3$aic
arima3$bic
arima3$aicc


arima4 <- Arima(yy, order=c(2,2,0), seasonal = list(order = c(1,0,0), period = 52))
arima4_forecast <- forecast:::forecast.Arima(arima4, h = 7, level = c(68, 90))
plot(arima4)
summary(arima4)
arima4$aic
arima4$bic
arima4$aicc
arima4_forecast$fitted

#First Run auto ARIMA, that will tell best model
#Then +-1 for each of those pdq
#Then AIC, AICc, BIC for each of those and within that you rank order and see if AIC is +-1 point.
#Do summary of each model and extract the point forecast and then average it for all the good models, say for 2 good models it is ((model 1+model2) /2). This will be the average model


