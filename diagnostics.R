setwd("/Users/prasad/Desktop/Teaching/BAX442/2022/Lectures/Class2/Diagnostics")
rm(list=ls(all=TRUE)) #clear data

# install.packages(c("olsrr"), dependencies=TRUE,repos="https://cloud.r-project.org")

library(olsrr)


data <- read.csv("data_week.csv", sep=",",dec=".",header=T) 	# weekly data
names(data)
head(data,5)

sales = data[,3]		      		# in xyz currency
xx = as.data.frame(data[,4:15])

model <- lm(sales~., data = xx)       	# fit lm with 12 variables
summary(model)

######################################################################################################

# Step 1. Check Multicollinearity
# If vif near unity, no prpoblem
# If vif near 4, tolerable
# If vif greater than 4, standard errors inflated by a factor of 2
# If vif greater than 10, standard errors inflated by a factor of 3 
# 

inflate = ols_vif_tol(model)

# Fix Multicolinearity: do Principal Component Regression (Topic of Class 4) on conceptually related variables

xcov = cov(xx[,7:12])       
out = eigen(xcov)
weights = as.matrix(out$vectors[,1])
weather = as.matrix(xx[,7:12]) %*% weights     # extract the principal component

new_xx = cbind(xx[,1:6],weather) # create new x matrix by replacing correlated variables with the principal component

colnames(new_xx) <-c("Direct Mail", "Emails", "Phone Calls", "Online Budget", "Promotion", "Holidays", "Weather")

new_xx = as.matrix(new_xx)

model2 <- lm(sales~new_xx)
summary(model2)

# check VIF again

inflate2 = ols_vif_tol(model2)      # no more collinearity b/c vif near 1

######################################################################################################



# Step 2. Do Variable Selection: Use Stepwise, but know about Forward and Backward selection method

df_xx = as.data.frame(new_xx)			# need to use data frame, not data matrix
model3 <- lm(sales~., data=df_xx)
summary(model3)

# Forward Selection
ols_step_forward_p(model3)

# Backward Elimination
ols_step_backward_p(model3)

# Stepwise Selection
ols_step_both_p(model3)

######################################################################################################




# Step 3. Are residuals Normal?

# Fit the model with only the retained variables
		
model4 <- lm(sales~., data=df_xx[,4:7])
summary(model4)


# Visual Check for Normality
# If strong departure from the 45 degree line, then residuals are non-normal
# which = 2 gives Q-Q plot
plot(model4, which = 2)


# Numerical Test for Normality
# p-value > 0.05 means "Residuals are Normal". 
ols_test_normality(model4)

######################################################################################################




# Step 4. Are residuals Homoscadestic (i.e., variance is the same across observations)? 

# Heteroscadestic means variance varies across cases; that is, variance is a function x-variables.

# Visual Check: Residuals Vs Fit Fitted plot
# If we observe patern, then either nonlinearity or nonconstant variance or both 
plot(model4, which = 1)

# Numerical Test for Heteroscadesticty 
# p-value > 0.05  means "Residuals have constant variance"
ols_test_breusch_pagan(model4)

# So here variance is borderline constant. 
# There is pattern in residuals ==> so nonlinearity? May be heteroscadesticty
######################################################################################################



# Step 5. Influential Observations
# Does removal of an observation change the fit and/or betas? 
# In others words, we want to detect not just observations with large X values, but speific observations that affect our regression results. These latter observations are called "influential observations"

# We use Cook's distance to identify influential points 
ols_plot_cooksd_bar(model4)

# How does the fit change when we drop an i-th observation?
ols_plot_dffits(model4)

# How does the estimated beta change when we drop an i-th observation?
ols_plot_dfbetas(model4)
######################################################################################################




# Step 6. Model Selection (used when we have to select the "best" model from a set of many models, including *Non-Nested* ones)

# AIC (Akaike Information Criterion), BIC (Bayesian Information Criterion), AICC (Akaike Information Criterion - Corrected) 

# Compute AIC for each model in the set of various models. 
# Then select the model associtaed with the *smallest* value on a given metric. 
# Do the above for BIC and for AICC


# When the three criteria select the same model as the best one, we attain "convergent validity" and enhances our confidence
# If the three criteria select different models as the best ones, then rely on BIC if sample size is large (p/n < 0.05 to 0.1), else use AICC (p/n > 0.1)


# AIC = -2*LL + 2*p
# leave-one-out cross-validation and AIC yields the same results asymptotically (very large n)
AIC = ols_aic(model4, method = 'R')  

# BIC = -2*LL + Ln(n)*p
# works well when n is large (i.e. p/n less than 10%)
BIC = ols_sbc(model4, method = 'R')

# AICC was developed by UCD GSM faculty, Prof. Chih-Ling Tsai (along with Prof. Clifford Hurvich of NYU)
# works well when n is small (i.e. p/n greater than 10%)
n = nrow(df_xx)
p = ncol(df_xx[,4:7]) + 1 # (for intercept)

AICC = ols_aic(model4, method = 'R')  + 2*p*(p+1)/(n - p - 1)

######################################################################################################





