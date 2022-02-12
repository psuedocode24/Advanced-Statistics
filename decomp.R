setwd("/Users/prasad/Desktop/Teaching/BAX493A/2022/Lectures/Class1")
rm(list=ls(all=TRUE)) #clear data

## install.packages(c("forecast", "TTR"), dependencies=TRUE,repos="https://cloud.r-project.org")

data<-read.csv("data_week.csv", sep=",",dec=".",header=T) 	# weekly data
names(data)
head(data,5)

sales = data[,3]/1000000		# millions in xyz currency
temp = data[,11]				# centigrade



##### Representing Data as Time Series Objects #####

yy = ts(sales, frequency = 52, start = c(2015,1)) # coverts sales data as time series object with start date and weekly frequency
plot.ts(yy)									# ALWAYS plot time series to see patterns: trend, cycle, variance over time

xx = ts(temp, frequency = 52, start = c(2015,1))
plot.ts(xx)


##### Time Series Decomposition  #####

## Sales: What's the growth rate? How do we obtain seasonally adjusted sales?
sales = decompose(yy) 
sales.trend = sales$trend
sales.seasonal = sales$seasonal
sales.resid = sales$random
sales.season.adj = yy - sales.seasonal									# seasonally adjusted sales
plot.ts(cbind(yy,sales.trend, sales.seasonal, sales.resid, sales.season.adj))

## Temperature: Are temperatures increasing?
temp = decompose(xx) 
trend = temp$trend
seasonal = temp$seasonal
random = temp$random
plot.ts(cbind(xx,trend, seasonal, random))



##### Holt-Winters Filter  #####

out1 = HoltWinters(xx, beta=TRUE, gamma=TRUE) 				# Holt-Winters Filtering - only level updating b/c beta and gamma are zeros
out1														# output -- see alpha estimate
out1$fitted 												# fitted values in training data
plot(out1)													# graph of actual (black) vs fitted (red)

##### Out of Sample Forecasts

library("forecast")								# install "forecast" package and load the library		
out2 = forecast:::forecast.HoltWinters(out1, h = 26, level = c(68, 95))	 

# R should directly call forecast.HoltWinters() function, but does not. Hence, to force it, I used the prefix forecast::: 
# forecast horizon 26 weeks. 
# CI with 1 SD and 2SD

plot(out2)


##### Autocorrelation Function (ACF)

## Can forecasts be improved upon using information in the residuals?
## To answer this Q, we need to assess whether the residuals contain information. That is, are residuals correlated at various time lags?

resid = out2$residuals				# output the residuals
corr.lag = Acf(resid[53:130])		# correlations at 4 and 13 seem significant ==> information may be present in residuals ==> so model can be improved further

# Remarks
# Check resid vector to see NAs, and then use Acf and Pacf. 
# Acf() gives autocorrelation function w/o the correlation = 1 at lag = 0. 
# acf() with the lower case "a" gives correlations at all lags, including lag 0, which by definition is 1. 
# acf()tends to mask small correlations in the plot due to the wider y-scale to include 1.



##### Partial Autocorrelation Function (PACF)

corr.lag = Pacf(resid[53:130]) 		# "Partial" refers to correlations obtained *after removing the effects of previous correlations*


# Remarks
# Plotting and then assessing whether correlations spike above confidence bands requires manual process. 
# To automate this assessment, we use Ljung-Box test. If p-value < 0.05, then significant correlations exist; else, not



#### Ljung-Box test 
Box.test(resid, type = "Ljung-Box")		

# The above test yeilds a large p-value ==> so OVERALL no significant correlations 
# If p-value < 0.05, the some correlations are significant. Which ones? See Acf, Pacf for guidance

checkresiduals(out1)


##  Deliverables for HW 1

# 1. Select one time series from data_week.csv

# 2. Try the four combinations of beta and gamma as on/off 

# 3. Present the plots of data series, trend, and seasonal components, and checkresiduals for the four combinations

# 4. Make out-of-sample forecast for 26 weeks with confidence bands. 

# 5. Retain only one out of four models -- give your justifcation






  