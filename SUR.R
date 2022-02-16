setwd("/Users/prasad/Desktop/Jan_2022/Teaching/BAX442/2022/Lectures/Class7/")
rm(list=ls(all=TRUE)) #clear data

# install.packages(c("systemfit", "Hmisc", "psych"), dependencies = TRUE, repos = "https://cloud.r-project.org")


library(systemfit)		# Package for Seemingly Unrelated Regression 




##### Read Data #####

data <- read.csv("sur_data.csv", sep = ",", dec = ".", header = T)

# variable names and 10 rows of data
names(data)
head(data, 5)

# get descriptive stats for variables 
# summary(data)

week = data[, 1]


### DVs
online.sales = log(data[, 2])			# DV1 in dollars
offline.sales = log(data[, 3])			# DV2 in dollars


### IVs
# Dummy variables
founder.day = data[, 4]
holdiay = data[, 5]
promo = data[, 6]

# Online Marketing
online.budget = log(data[, 10])			# dollars
online.image = log(1 + data[, 11])		# dollars
online.affiliate = log(data[, 12])   	# affiliates clicks dollars
online.display = log(data[,13])	    	# display clicks dollars
online.paid.search = log(data[, 14])	# paid search dollars
online.retargeting = log(data[, 15])    # retargeting dollars
online.sem = log(data[, 16])	        # search engine adv
emails = log(data[, 8])					# number 

# Offline Marketing
direct = log(data[, 7])      				 # number 
calls = log(1 + data[, 9])					 # number 
radio = log(1 + data[, 17])				     # radio (media spend)
print = log(1 + data[, 18])				     # print (media spend)
tv = log(1 + data[, 19])				     # TV (media spend)
catalog = log(1 + data[, 20])				 # dollars


cloud = log(data[, 21])							# index
temp = log(1 - min(data[,22]) + data[, 22])		# celsius
wind = log(data[, 23])							# speed
rain = log(data[, 24])							# level
sunny = log(data[, 25])							# hours per day
snow = log(1 + data[, 26])						# level


#### Extract principal component

library(psych)									# Package for PCA and Factor Analysis 
weather = cbind(cloud, temp, wind, rain, sunny, snow)
PC.fit <- principal(weather, nfactors = 1, rotate = "varimax")
z.bad.weather = PC.fit$scores		# check signs in PC.fit and verfiy that it's bad weather

# online and offline activities are highly correlated, so combine them in one PC

marketing.xs = cbind(online.image, online.affiliate, online.display, online.paid.search, online.retargeting, online.sem, emails, direct, calls, radio, print, tv, catalog)
PC.marketing <- principal(marketing.xs, nfactors = 1, rotate = "varimax")
z.marketing = PC.marketing$scores	# check signs in PC.marketing and verfiy that it's mostly positive coeffs




library(Hmisc)									# package to create lagged variables
online.lag.sales = as.matrix(Lag(online.sales))
offline.lag.sales = as.matrix(Lag(offline.sales))

# check
cbind(online.sales, online.lag.sales)
cbind(offline.sales, offline.lag.sales)




# OLS on DV1
online.out <- lm(online.sales ~ z.marketing + z.bad.weather + founder.day + holdiay)
summary(online.out)

# OLS on DV2
offline.out <- lm(offline.sales ~ online.lag.sales + offline.lag.sales + z.marketing + z.bad.weather + founder.day + holdiay)
summary(offline.out)


# SUR on DV1 and DV2 simultaneously
 

EQ1 <- online.sales ~  z.marketing + z.bad.weather + founder.day + holdiay 
EQ2 <- offline.sales ~ online.lag.sales + offline.lag.sales + z.marketing + z.bad.weather + founder.day + holdiay 


sur.out <- systemfit(list(s1 = EQ1, s2 = EQ2), method = "SUR", data = data)
summary(sur.out)


#### Compare Individual OLS with Joint SUR

# Percentage Bias => (OLS_coeff - SUR_coeff)/SUR_coeff

ols.beta1 = coef(summary(online.out))[, 1]
ols.beta2 = coef(summary(offline.out))[, 1]

sur.beta1 = coef(summary(sur.out))[, 1][1:5]
sur.beta2 = coef(summary(sur.out))[, 1][6:12]

# Percentage Bias
pct.bias.eq1 = 100*(ols.beta1 - sur.beta1) / sur.beta1
pct.bias.eq2 = 100*(ols.beta2 - sur.beta2) / sur.beta2


# Efficiency => var(OLS)/var(SUR)

ols.se1 = coef(summary(online.out))[, 2]
ols.se2 = coef(summary(offline.out))[, 2]

sur.se1 = coef(summary(sur.out))[, 2][1:5]
sur.se2 = coef(summary(sur.out))[, 2][6:12]

efficiency.eq1 = (ols.se1/sur.se1)^2
efficiency.eq2 = (ols.se2/sur.se2)^2


## Practice Qs: 
# Q1 What is the elasticity of various marketing efforts on online sales?

# Q2 What is the elasticity of various marketing efforts on offline sales?

# Q3 What is the elasticity of various weather components on online sales?

# Q4 What is the elasticity of various weather components on offline sales?




