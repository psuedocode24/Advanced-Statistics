setwd("/Users/prasad/Desktop/Teaching2021/BAX442/2021/Lectures/Class1")

exam = read.csv("exam.csv", header=T)    # read csv file and label the data as "exam"


#####################################
# PART ONE: OLS from first principles
#####################################

x = as.matrix(exam[,1])        # midterm as x variable
y = as.matrix(exam[,2])        # finals as y variable
n = nrow(x)                    # number of observations

one = matrix(1,n,1)           # creates a column of ones for the intercept
xx = as.matrix(cbind(one,x))  # attaches ones to the x data
p = ncol(xx)                  # number of variables in xx data

### Estimation
xpx = t(xx) %*% xx             # X'X matrix, t() denotes transpose
xpxi = solve(xpx)              # solve() inverts the matrix X'X
xpy = t(xx) %*% y              # X'Y matrix
bhat = xpxi %*% xpy            # estimate bhat = Inverse(X'X)*X'Y as per the derived OLS estimator in class1 PDF and pptx

# OLS Estimation Done!



### Sum of Squared Errors
yhat = xx %*% bhat             # forecast y values = X*bhat
err = y-yhat                   # compute error = y - yhat
sse = t(err) %*% err           # compute sum of squared errors to compute error variance s2
s2 = sse/(n-p)                 # error variance, sigma^2 in class notes

### T-values
se = sqrt(c(s2)*diag(xpxi))       # extrat standard errors of bhat via the diagonal of the square-root of sigma^2 * Inverse(X'X) -- see the formula in Class 1 Notes and pptx
tval = bhat/se                 # obtain t-values = estimates divided by their std errors

my_out = cbind(bhat, se, tval)    # output from my OLS built from first principles
colnames(my_out) = c("Estimates", "Std Errors", "t-values")
rownames(my_out) = c("Intercept","Midterm Scores")

# OLS Inference Done!



### 95% Confidence Intervals based on t-istribution as in R
t_critical = qt(0.975, df=n-p)		# critical value = quantile at 97.5% from t-distribution with degrees of freedom = n-p
CI_lb = bhat - t_critical*se		# lower bound of confidence interval
CI_ub = bhat + t_critical*se 		# upper bound of confidence interval

my_CI = cbind(CI_lb, CI_ub)
colnames(my_CI) = c("2.5%", "97.5%")
rownames(my_CI) = c("Intercept","Midterm Scores")

### My Output
print("My Estimates, Std Errors, t-values"); my_out
print("My 95% CI from t-distribution"); my_CI

### Compare with the built-in lm() function in R      

lm_out = lm(y~x)         # R built-in OLS
print("R built-in Output"); summary(lm_out)
print("R built-in Confidence Intervals");  confint(lm_out, level=0.95)






######################################
# PART TWO: What is a "function" in R? 
######################################



## It's a step-by-step procedure that transforms the given inputs (x1, x2, ...) into desired outputs (y1, y2, ...). A function has three parts: (1) a string name, (2) a set of input arguments, and (3) a set of output objects. 

## For us, inputs are x=as.matrix(exam[,1]) and y=as.matrix(exam[,2])
## For us, outputs are my_out and my_CI


## It's structure looks like: name = function(x1, x2,..) { 
# 
#   your code that specifies how to transform x1, x2, ... to y1, y2,..
#   
#   then specfy outputs y1, y2, ...
#   
#   end the function with a curly bracket to match with the starting curly bracket
#   
#   return (y1,y2,...) }. 

## The curly brackets indicate the start and the end of your function






#############################################
# PART THREE: Let us build a my_lm *function*
#############################################

my_lm = function(y,x) {
  
  n = nrow(x)                    # number of observations
  
  one = matrix(1,n,1)            # creates column of ones for the itercept
  xx = as.matrix(cbind(one,x))   # attaches ones to the x data
  p = ncol(xx)                   # number of variables in xx data
  
  xpx = t(xx) %*% xx             # X'X matrix, t() denotes transpose
  xpxi = solve(xpx)              # solve() inverts the matrix X'X
  xpy = t(xx) %*% y              # X'Y matrix
  bhat = xpxi %*% xpy            # estimate bhat = Inverse(X'X)*X'Y as per OLS estimator in class notes
  
  yhat = xx %*% bhat             # forecasted y values = X*bhat
  err = y-yhat                   # residuals = y - yhat
  sse = t(err) %*% err           # sum of square residuals to compute error variance s2
  
  s2 = sse/(n-p)                 # error variance, sigma^2 in class notes
  se = sqrt(c(s2)*diag(xpxi))       # standard errors of bhat given by diagonal of sigma^2 * Inverse(X'X)
  tval = bhat/se                 # t-values = estimates divided by their std errors
  
  my_out = cbind(bhat, se, tval)    # output from my OLS built from first principles
  colnames(my_out) = c("Estimates", "Std Errors", "t-values")
  rownames(my_out) = c("Intercept","Midterm Scores")

  CI_lb = bhat - qt(0.975, df=n-p)*se		# lower bound based on t-dist
  CI_ub = bhat + qt(0.975,df=n-p)*se 		# upper bound based on t-dist

  my_CI = cbind(CI_lb, CI_ub)
  colnames(my_CI)= c("2.5%", "97.5%")
  rownames(my_CI)=c("Intercept","Midterm Scores")
  
  
  return(list(my_out, my_CI)) 	
  
  # R returns only ONE output. If you have more than one output, create a list(out1, out2, out3, etc) to return  multiple outputs. 
  
  }





########################################
# PART FOUR: Calling the function output 
########################################

y.in = as.matrix(exam[,2])
x.in = as.matrix(exam[,1])
mylm_out = my_lm(y.in,x.in)

print("Results from *my_lm()* function!"); mylm_out

# Extract a specific ouput, say, confidence interval

CI_out = mylm_out[[2]]

# print command via the parenthesis ()

(round(CI_out, digits = 4)) 	# upto 4 numbers after the decimal



############################################
# PART FIVE: Saving the outputs to text file
############################################

sink("compare R Built-in lm with My LM.txt", append = F, split=T)

print("My LM Estimates"); round(mylm_out[[1]], digits = 4) 

print("My LM Confidence Intervals"); round(mylm_out[[2]], digits = 4) 

print("R built-in Output"); summary(lm_out)

print("R built-in Confidence Intervals");  confint(lm_out, level = 0.95)


sink()




  