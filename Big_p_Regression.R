setwd("/Users/prasad/Desktop/Jan_2022/Teaching/BAX442/2022/Lectures/Class5/")
rm(list=ls(all=TRUE)) #clear data

# install.packages(c("glmnet"), dependencies=TRUE,repos="https://cloud.r-project.org")


library(glmnet)



# Cars Data

data <- read.csv("Cars_Data.csv", header=T)    # read csv file and label the data as "data"

y <- data[,17]
x <- as.matrix(data[,2:16])
n <- nrow(x)
p <- ncol(x)


############ Regularization Models (Ridge, Lasso, Elastic Net) #########


# LASSO when alpha = 1. 
# Ridge Regression when alpha = 0. 
# Elastic Net when alpha = strictly positive fraction. Why? Exam Q


######### Lasso Regression

out.lasso <- glmnet(x, y, alpha = 1)     # fits lasso because alpha = 1 vanishes the quadratic penalty

# Coeffcients plots

plot(out.lasso, xvar= "lambda", label = TRUE)	      # plots estimates vs log(lambda) values. See how *all* estimates tend to zero as lambda increases. Why? Exam Q	
title("Lasso Coefficients Plot",line=2.5)

# Extract LASSO estimates for specific lambda values

est_1 <- coef(out.lasso, s = 0.01)			  	  # estimates at lambda = 0.01 => many vars deemed significant, yet their magnitudes differ b/w significant and nonsignificant vars
est_2 <- coef(out.lasso, s = 0.5)					  # estimates when lambda = 0.5 ==> separation b/w sig an insignifcant vars improves substantially

      
# Optimal Lambda value?  Select by using n-fold cross-validation 

cvlasso <- cv.glmnet(x, y, alpha = 1, type.measure = "mse", nfolds = 3)    				# n-fold cross-validation
plot(cvlasso, main = "Select Best Lambda")								# plot of MSE vs Lambda
lam_est <- cvlasso$lambda.min											# best lambda --> one that minimizes mse
lasso_est <- coef(out.lasso, s = lam_est)								# best parameter estimates via Lasso




######### Ridge Regression

out.ridge <- glmnet(x, y, alpha = 0)     # fits lasso because alpha = 1 vanishes the quadratic penalty

# Coeffcients plots

plot(out.ridge, xvar = "lambda", label = TRUE)	      # plots estimates vs log(lambda) values. See how *all* estimates tend to zero as lambda increases. Why? Exam Q	
title("Ridge Coefficients Plot",line = 2.5)

# Extract RIDGE estimates for specific lambda values

ridge_est_1 <- coef(out.ridge, s = 0.01)			  	  # estimates at lambda = 0.01 => many vars deemed significant, yet their magnitudes differ b/w significant and nonsignificant vars
ridge_est_2 <- coef(out.ridge, s = 0.5)					  # estimates when lambda = 0.5 ==> separation b/w sig an insignifcant vars improves substantially

      
# Optimal Lambda value?  Select by using n-fold cross-validation 

cvridge <- cv.glmnet(x, y, alpha = 0, type.measure = "mse", nfolds = 3)    			# 10-fold cross-validation
plot(cvridge, main = "Select Best Lambda")								# plot of MSE vs Lambda
ridge_lam_est <- cvridge$lambda.min											# best lambda --> one that minimizes mse
ridge_est <- coef(out.ridge, s = ridge_lam_est)								# best parameter estimates via Lasso


# *Approximate* Inference in Ridge Regression with p > n

rr.est <- as.data.frame(summary(ridge_est))[,3]				# Extract estimates from sparse matrix
rr.est <- as.matrix(rr.est, (p+1), 1)						# as vector

ones <- as.matrix(rep(1, n), n, 1)							# vector of 1s for intercept
xx <- cbind(ones, x)											# xx matrix with intercept
k <- ridge_lam_est											# k is best lambda from ridge regression
xpx <- t(xx) %*% xx + k*diag(p+1)							# X'X matrix with Ridge Penalty: X'X + kI. Also p is (p + 1) b/c of intercept
xpxi <- solve(xpx)											# Inv(X'X + k I)

yhat <- xx %*% rr.est										# yhat
sig.sq <- (sd(y - yhat))^2									# variance of err = y - yhat
var.cov <- sig.sq * xpxi %*% (t(xx) %*% xx) %*% xpxi			# Variance- Covaraince Matrix 
se <- sqrt(diag(var.cov))									# std err of estimates
tval <- rr.est / se											# tvals

out <- cbind(rr.est, se, tval)								# Table of Est, SEs, Tvals

colnames(out) <- c("Ridge Est", "Std Err", "t-values")
rownames(out) <- c("Intercept", "Attractive",  "Quiet", "Unreliable", "Poorly.Built", 
"Interesting", "Sporty", "Uncomfortable", "Roomy", "Easy.Service", "Prestige", "Common", "Economical", "Successful", "AvantGarde", "Poor.Value")

out <- round(out, digits = 4)								# Use abs(t) > 1.65 as threshold ==> 90% CIs. 
ridge_est_90pct <- ridge_est * (abs(tval) > 1.65)


######### Elastic Net Regression

out.en <- glmnet(x, y, alpha = 0.5)     # fits lasso because alpha = 1 vanishes the quadratic penalty

# Coeffcients plots

plot(out.en, xvar = "lambda", label=TRUE)	      # plots estimates vs log(lambda) values. See how *all* estimates tend to zero as lambda increases. Why?
title("Elastic Net Coefficients Plot",line=2.5)

# Extract RIDGE estimates for specific lambda values

en_est_1 <- coef(out.en, s = 0.01)			  	  # estimates at lambda = 0.01 => many vars deemed significant, yet their magnitudes differ b/w sig and nonsig vars
en_est_2 <- coef(out.en, s = 0.5)					  # estimates when lambda = 0.5 ==> separation b/w significant an insignifcant vars improves substantially

      
# Optimal Lambda value?  Select by using n-fold cross-validation 

cv.en <- cv.glmnet(x, y, alpha = 0.5, type.measure="mse", nfolds = 3)    			# 10-fold cross-validation
plot(cv.en, main = "Select Best Lambda")								# plot of MSE vs Lambda
en_lam_est <- cv.en$lambda.min											# best lambda --> one that minimizes mse
en_est <- coef(out.en, s = en_lam_est)	


## Compare Estimates from All Three Approaches

all.out.est <- cbind(lasso_est, en_est, ridge_est_90pct)
colnames(all.out.est) <- c("Lasso Est", "EN Est", "Ridge Est 90%")



##### HW4 #####


# The goal of HW4 is to learn Shrinkage Regressions (Ridge, Lasso, and Elastic Net). They provide useful information even when the number of variables exceed the sample size. Specifically, we aim to discover Top 3 attributes influencing car preferences. 


# Use Cars_Data.csv. Run shrinkage regressions. Set alpha = 0 to 1 in steps of 0.1. That is, run Shrinkage Regression 11 times. 

# For Elastic Net Regression, find the best alpha value between (0.1, 0.9), which is obtained by  tabulating mse at lambda.min and selecting the alpha value that corresponds to minimum mse across 9 alpha values. 

# Then, do Ridge (alpha = 0) and Lasso (alpha = 1) regressions with the best lambda.min. 

# Next, check which variables are significant across the three shrinkage regressions. Run lm() with the significant variables identified by the three shrinkage regressions. 


# Finally, you are ready to answer the following Qs:

# 1. Provide and Explain the Coefficients Plots for the best Ridge, EN, Lasso regressions. 

# 2. Provide and Explain MSE vs Lambda Plots for Ridge, Best-alpha EN, Lasso

# 3. In one table with proper row/col names, present the parameter estimates from Ridge, Best-alpha EN, Lasso -- all at their respective best lambdas.

# 4. Which are the TOP 3 attributes according to the best Lasso, EN, Ridge?

# 5. What is the extent of bias in the estimated parameters relative to the corresponding OLS estimates via lm (using only three variables from point 4)?

# 6. Assess your understanding:
	# Why does OLS fail when p > n? 
	# How do Ridge, Lasso, and EN circumvent the p > n problem? 
	# Explain the "nested strcuture" of Lasso, EN, Ridge








  