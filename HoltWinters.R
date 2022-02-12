setwd("/Users/prasad/Desktop/Teaching/BAX493A/2022/Lectures/Class1")
rm(list=ls(all=TRUE)) #clear data

## install.packages(c("forecast", "TTR"), dependencies=TRUE,repos="https://cloud.r-project.org")

data<-read.csv("data_week.csv", sep=",",dec=".",header=T) 	# weekly data
names(data)
head(data,5)

temp = data[,11]				# centigrade

##### Representing Data as Time Series Objects #####

xx = ts(temp, frequency = 52, start = c(2015,1))
plot.ts(xx)


##### Time Series Decomposition  #####

## Temperature: Are temperatures increasing?
temp = decompose(xx) 
trend = temp$trend
seasonal = temp$seasonal
random = temp$random
plot.ts(cbind(xx,trend, seasonal, random))


##### Holt-Winters Filter  #####

out1 = HoltWinters(xx, beta = TRUE, gamma = TRUE) 				# Holt-Winters Filtering - only level updating b/c beta and gamma are zeros
out1														# output -- see alpha estimate
out1$fitted 												# fitted values in training data
plot(out1)													# graph of actual (black) vs fitted (red)

out2 = HoltWinters(xx, beta = TRUE, gamma = FALSE) 				# Holt-Winters Filtering - only level updating b/c beta and gamma are zeros
out2														# output -- see alpha estimate
out2$fitted 												# fitted values in training data
plot(out2)

out3 = HoltWinters(xx, beta = FALSE, gamma = TRUE) 				# Holt-Winters Filtering - only level updating b/c beta and gamma are zeros
out3														# output -- see alpha estimate
out3$fitted 												# fitted values in training data
plot(out3)

out4 = HoltWinters(xx, beta = FALSE, gamma = FALSE) 				# Holt-Winters Filtering - only level updating b/c beta and gamma are zeros
out4														# output -- see alpha estimate
out4$fitted 												# fitted values in training data
plot(out4)

##### Check Residuals
library("forecast")	
checkresiduals(out1)
checkresiduals(out2)
checkresiduals(out3)
checkresiduals(out4)


##### Out of Sample Forecasts

# install "forecast" package and load the library		
out1 = forecast:::forecast.HoltWinters(out1, h = 26, level = c(68, 95))	 
out2 = forecast:::forecast.HoltWinters(out2, h = 26, level = c(68, 95))	
out3 = forecast:::forecast.HoltWinters(out3, h = 26, level = c(68, 95))	
out4 = forecast:::forecast.HoltWinters(out4, h = 26, level = c(68, 95))	
# R should directly call forecast.HoltWinters() function, but does not. Hence, to force it, I used the prefix forecast::: 
# forecast horizon 26 weeks. 
# CI with 1 SD and 2SD

plot(out1)
plot(out2)
plot(out3)
plot(out4)



##  Deliverables for HW 1

# 1. Select one time series from data_week.csv

# 2. Try the four combinations of beta and gamma as on/off 

# 3. Present the plots of data series, trend, and seasonal components, and checkresiduals for each of the four combinations

# 4. Make out-of-sample forecast for 26 weeks with confidence bands. 

# 5. Reccomend one forecasting model -- give your justifcation






  