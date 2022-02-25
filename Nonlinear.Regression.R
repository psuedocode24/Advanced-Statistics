setwd("/Users/prasad/Desktop/Jan_2022/Teaching/BAX442/2022/Lectures/Class8/")
rm(list=ls(all=TRUE)) #clear data


#### Exponential Growth 

### Real GDP in Million Pounds (2013 = 1)
### Population in England in Thousands


data_gdp <- read.csv("real_gdp.csv", header = T)    		# read csv file and label the data as "data"
# data_pop <- read.csv("population.csv", header = T)    	# read csv file and label the data as "data"

names(data_gdp)
xx <- data_gdp[,1]
yy <- data_gdp[,2]
plot(xx,yy)

df0 <- as.data.frame(cbind(xx, yy))


# fit nonlinear regression

# get good starting values
ln_yy <- log(yy)
xvar <- xx - 1700
fit <- lm(ln_yy ~ xvar)
summary(fit)

# call for nls

m0 <- nls( yy ~ b1*exp(b2*(xx-1700)), data = df0, start = list(b1 = 5000, b2 = 0.02), control = nls.control(maxiter = 1000) )

summary(m0)

CI.m0 <- confint(m0, level = 0.95) 	# 95% CI
print(CI.m0)

yhat0 <- predict(m0)					# predicted sales
rho0 <- cor(yy, yhat0) 				# correlation b/w actual and predicted sales
print(rho0)

## Plot Actual and Predcited Model
plot(xx, yy)
lines(xx, yhat0, lty = 2, col = "red", lwd = 3)




# Logistic Growth 

data1 <- read.csv("data_logistic.csv", header = T)    # read csv file and label the data as "data"

names(data1)
week1 <- data1[,1]
sales1 <- data1[,2]
plot(week1, sales1)

df1 <- as.data.frame(cbind(week1, sales1))

n1 <- nrow(df1)
y1 <- sales1
x1 <- week1


# fit nonlinear regression
# see slides for the nonlinear function of sales and week
# also see Equation (5) on page 91 in Meyer (1994)
# In Equation (5), kappa = b1 is the market size; Delta_t = b2; and t_m = b3


m1 <- nls( y1 ~ b1/(1 + exp(-log(81)*(x1 - b3)/b2)), data = df1, start = list(b1 = 100, b2 = 10, b3 = 10), control = nls.control(maxiter = 1000) )

summary(m1)

CI.m1 <- confint(m1, level = 0.95) 	# 95% CI
print(CI.m1)

yhat1 <- predict(m1)					# predicted sales
rho1 <- cor(sales1, yhat1) 			# correlation b/w actual and predicted sales
print(rho1)

## Plot Actual and Predcited Model
plot(week1, sales1)
lines(week1, yhat1, lty = 2, col = "red", lwd = 3)





# Bi-Logistic Growth  -- Two Peaks

data2 <- read.csv("data_bi_logistic.csv", header = T)    # read csv file and label the data as "data"

names(data2)
week2 <- data2[,1]
sales2 <- data2[,2]
plot(week2, sales2)

df2 <- as.data.frame(cbind(week2, sales2))

n2 <- nrow(df2)
y2 <- sales2
x2 <- week2


# fit nonlinear regression
# See Equation (11) on page 92 in Meyer (1994)
# In Equation (11), kappa1 = b1 is the market size; Delta_t1 = b2; and t_m1 = b3; 
# For the second growth spurt, kappa2 = b4 is the market size; Delta_t2 = b5; and t_m2 = b6


m2 <- nls( y2 ~ b1/(1 + exp(-log(81)*(x2 - b3)/b2)) + b4/(1 + exp(-log(81)*(x2 - b6)/b5)), 
start = list(b1 = 100, b2 = 10, b3 = 25, 
			 b4 = 200, b5 = 20, b6 = 75), data = df2, control = nls.control(maxiter = 1000)
)
summary(m2)

CI.m2 <- confint(m2, level = 0.95) 	# 95% CI
print(CI.m2)

yhat2 <- predict(m2)					# predicted sales
rho2 <- cor(sales2, yhat2) 			# correlation b/w actual and predicted sales
print(rho2)

## Plot Actual and Predcited Model
plot(week2, sales2)
lines(week2, yhat2, lty = 2, col = "red", lwd = 3)





# HW7: Tr-Logistic Growth Regression 
# Q1. Formulate the tri-logistic model analogous to bi-logistic model
# Q2. Estimate the model using data up to 140 weeks. Report param estimates, their SEs and 95% CIs. 
# Q3. Predict sales from weeks 141 through 198
# Q4. Provide 95% CIs for predicted sales using *Monte Carlo Simulation* (for details, see class 3 and Bootstrap.R)
# Q5. What percentage of the actual observations are within your prediction intervals?



## Refer to it if you get stuck. Try HW7 without the following code. But feel free to study

data3 <- read.csv("data_tri_logistic.csv", header = T)    # read csv file and label the data as "data"

names(data3)
week3 <- data3[,1]
sales3 <- data3[,2]
plot(week3, sales3)

df3 <- as.data.frame(cbind(week3, sales3))

n3 <- nrow(df3)
y3 <- sales3
x3 <- week3


m3 <- nls( y3 ~ b1/(1 + exp(-log(81)*(x3 - b3)/b2)) + b4/(1 + exp(-log(81)*(x3 - b6)/b5)) + b7/(1 + exp(-log(81)*(x3 - b9)/b8)), 
start = list(b1 = 100, b2 = 23, b3 = 26, 
			 b4 = 217, b5 = 17, b6 = 79,  
			 b7 = 600, b8 = 20, b9 = 130), data = df3,  control = nls.control(maxiter = 1000)
)
summary(m3)

CI.m3 <- confint(m3, level = 0.95) 	# 95% CI
print(CI.m3)

yhat3 <- predict(m3)					# predicted sales
rho3 <- cor(sales3, yhat3) 			# correlation b/w actual and predicted sales
print(rho3)

## Plot Actual and Predicted Model
plot(week3, sales3)
lines(week3, yhat3, lty = 2, col = "red", lwd = 3)



## Create 1000 Monte Carlo (i.e., simulated) sales trajectories over future 59 weeks

library("MASS")

ra <- 1000
out <- matrix(0, ra, 59)

mu <- t(t(coef(m3)))
Sigma <- vcov(m3)

for (ii in 1:ra) {

# Monte Carlo Simulation of beta vector with specified N(mu, Sigma)

b.out <- mvrnorm(n = 1, mu, Sigma, tol = 1e-4, empirical = FALSE, EISPACK = FALSE)

yhat <- b.out[1]/(1 + exp(-log(81)*(week3 - b.out[3])/b.out[2])) + b.out[4]/(1 + exp(-log(81)*(week3 - b.out[6])/b.out[5])) + b.out[7]/(1 + exp(-log(81)*(week3 - b.out[9])/b.out[8]))

y.out <- yhat[141:199]
out[ii,] <- y.out				# out[] contains 1000 trajectories of sales over future 59 weeks 


}


## Generate Prediction Interval
library(matrixStats)

yhat.ci <- colQuantiles(out, prob = c(0.025, 0.5, 0.975))


## Plot sales trajectories for the sample paths at percentiles 2.5%, 50% and 97.5% 

pdf('Prediction Interval.pdf')

df <- data.frame(x = 1:59, ff = yhat.ci[,2], L = yhat.ci[,1], U = yhat.ci[,3])
plot(df$x, df$ff, 
ylim = c(min(yhat.ci[,1]), max(yhat.ci[,3])), 
type = "l", 
xlab = "Weeks", ylab = "Predicted Sales",
main = "Third Peak Forecast Interval" )

# draws the grey confidence region
polygon(c(df$x, rev(df$x)), c(df$L, rev(df$U)), col = "grey90", border = FALSE) 
lines(df$x, df$ff, col = "blue", lwd = 1.5)

par(new = TRUE)		# suppreses overwriting of the previous plot							
points(sales3[140:199], col = "red", pch = "o", cex = 0.75)	# overlays points on the previous plot
par(new = FALSE)

legend("bottomright", legend = c("Forecast", "Actual"), 
col = c("blue", "red"), lty = c(1, NA), pch = c(NA, "o"), cex = 0.75, bty = "n")


dev.off()






  