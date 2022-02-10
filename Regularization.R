setwd("/Users/prasad/Desktop/Jan_2022/Teaching/BAX442/2022/Lectures/Class5/")
rm(list=ls(all=TRUE)) #clear data

# install.packages(c("glmnet"), dependencies=TRUE,repos="https://cloud.r-project.org")

library(glmnet)



# Create simulated data 

set.seed(1234) 								# set seed to generate same data every time you run the code 
n <- 100 									# sample size
p <- 120									# p > n => Big-p Data Problem
pp <- 10 									# true variables (always less than n)
beta1 <- as.matrix(rep(1,10)) 				# 10 coefficients for first five vars as a 5 x 1 vector
beta0 <- as.matrix(rep(0,110))				# 110 insignificant betas as 110 x 1 vector

x <- matrix(runif(n*p, min = 0, max = 10), n, p)	# randon uniform variates from U(0, 10) arranged in matrix n x p

signal <- x[,1:10] %*% beta1				# signal based on first 10 independent variable

non.sig.signal <- x[,11:120] %*% beta0		# insignificant contributions from 110 vars

noise <- rnorm(n)							# random noise (Normal, zero mean, unit variance)

y <- signal + non.sig.signal + noise		# DV based on 100 insignificant vars but we don't know that ex ante


## Can we identify significant variables contributing to the signal 
## even though p = 120 > n = 100. OLS cannot when p > n. 


## Partition training and test data sets

m <- 0.8*n									# 80% training size ==> 20% holdout sample
ts <- sample(1:n,m)							# random draw of 80 out of 100 rows
x.train <-  x[ts,]							
y.train <-  y[ts]

x.test <- x[-ts,]
y.test <- y[-ts]



############ Regularization Models (Ridge, Lasso, Elastic Net) #########


# LASSO when alpha = 1. 
# Ridge Regression when alpha = 0. 
# Elastic Net when alpha = strictly positive fraction Why? Exam Q


# Lasso Regression

out.lasso <- glmnet(x.train, y.train, alpha = 1)     # fits lasso because alpha = 1 vanishes the quadratic penalty

# Coefficients plots

plot(out.lasso, xvar="lambda", label=TRUE)	      # plots estimates vs log(lambda) values. See how *all* estimates tend to zero as lambda increases. Why? Exam Q	
title("Lasso Coefficients Plot",line = 2.5)

# Extract LASSO estimates for specific lambda values

est_1 <-  coef(out.lasso, s = 0.01)			  	  # estimates at lambda = 0.01 => many vars deemed significant, yet their magnitudes differ b/w significant and nonsignificant vars
est_2 <-  coef(out.lasso, s = 0.5)					  # estimates when lambda = 0.5 ==> separation b/w sig an insignifcant vars improves substantially

      
# Optimal Lambda value?  Select by using n-fold cross-validation 

cv.lasso <-  cv.glmnet(x.train, y.train, type.measure = "mse", nfolds = 10)    # 10-fold cross-validation
plot(cv.lasso, main = "Select Best Lambda")								# plot of MSE vs Lambda
lam_est <- cv.lasso$lambda.min											# best lambda --> one that minimizes mse
lasso_est <- coef(out.lasso, s = lam_est)								# best parameter estimates via Lasso


# Prediction Using Test Sample Data

yhat <-  predict(cv.lasso, s = lam_est, newx = x.test)					# x.test provides data from holdout sample
sse.test <-  sum((y.test - yhat)^2)										# sum of square errors in holdout sample
sst.test <-  sum((y.test-mean(y.test))^2)								# total sum of squares around ybar in holdout sample
rsq.test <- 1 - sse.test/sst.test										# R square = 1 - (SSE divided by Total SSE) 

# Practice Qs (Not HW4): 
# Change alpha values from 0 to 1 in steps of 0.1
# Do ridge regression (alpha = 0) 
# Do elastic net (alpha = fraction)
# Do lasso regression (alpha = 1)












  