setwd("/Users/prasad/Desktop/Jan_2022/Teaching/BAX493A/2022/Lectures/Class4")
rm(list=ls(all=TRUE)) 	# clear data

# update.packages(ask = FALSE,checkBuilt = TRUE,repos="https://cloud.r-project.org")

# install.packages(c("tseries", "forecast"), dependencies = TRUE, repos = "https://cloud.r-project.org")


library(ggplot2)
library(gridExtra)




##### Read Data #####

my.data <- read.csv("Class4b.csv", sep = ",", dec = ".", header = T) 	# daily data
names(my.data)
head(my.data,10)

days <- my.data[,1]		# time
y1 <- my.data[,2]	 	# Noise Free Signal
y2 <- my.data[,3]	 	# Noisy Signal
nn <- nrow(my.data)		# number of observations


####### Model: y = 10*Sin(t/5) + t + error ######

##### Plot Data #####


# Plot Noise Free Signal
p1 <- ggplot(data = my.data, aes(x = days, y = y1 )) + geom_line() + geom_point(colour = "blue", size = 2, shape = 21, fill = "white") # overlay line

p1 + labs(x = "Time", y = "Noise Free Signal")		# see output plot with labels


# Plot Noisy Signal
p2 <- ggplot(data = my.data, aes(x = days, y = y2)) +  geom_point(colour = "blue", size = 2, shape = 21, fill = "white")

p2 + labs(x = "Time", y = "Noisy Signal")	




##### KALMAN FILTER  #####

# Setup system matrices {Z, T, c, d, H, Q} 

zz <- matrix(0, 1, 2)		# link matrix
tt <- matrix(0, 2, 2)		# transition matrix
c <- 0						# obs drift
d <- matrix(0, 2, 1)			# transition drift
hh <- 1						# obs noise variance
qq <- matrix(0, 2, 2)		# transition noise covariance


# link matrix
zz[1] <- 1; zz[2] <- 0; 
zz  # check zz

# transition matrix
tt[1, 1] <- 98/50; tt[1, 2] <- -1
tt[2, 1] <- 1; tt[2, 2] <- 0;  

tt  # check tt


# transition noise matrix
qq[1, 1] <- 0.01; qq[1, 2] <- 0
qq[2, 1] <- 0; qq[2, 2] <- 0.01

qq  # check qq


# Setup output matrices for prediction and lower/upper CIs

at.out <- matrix(0, nn, 2)		# for the mean of state vector
lo.CI <- matrix(0,nn,1)			# save lower CI
hi.CI <- matrix(0,nn,1)			# save upper CI



# Initialize state means and covar at t = 0

at <- matrix(0,2,1)		# initial state vector
pt <- diag(2)			# initial state covaraince matrix ==> diffused prior ==> large variances compared to the mean state vector

## Kalman Filter Recursions


for (ii in 1:nn) { 
       	
     	# Kalman Filter Recursions
       	# Notation ==> Read "m"  as "minus". tm1 means t-1. 
       	# So xt.tm1 denotes the value of x at t|t-1 ==> x value at t based on info up to (t-1). That is, "Prior" in the Bayesian language
       	# And xtm1 is the value of x at t-1 based on info up to (t-1). That is, "Posterior" in the Bayesian language.
       	
       
       	
  		atm1 <- at									# reset posterior mean		
       	ptm1 <- pt 									# reset posterior covar			
  
       	d <- rbind((2/50)*days[ii],0)				# add trend effect. Check your understanding on WHY 2/50? WHY rbind 0?
       	
       	# TIME UPDATE (based on model dynamics)
       	at.tm1 <- tt %*% atm1 + d					# KF recursion for prior mean
       	pt.tm1 <- tt %*% ptm1 %*% t(tt) + qq  		# KF recursion for prior covar
       		
       		
       	yhat <- zz %*% at.tm1 + c					# forecast 
       	err <- y2[ii] - yhat							# error 
       	f <- zz %*% pt.tm1 %*% t(zz) + hh			# forecast variance
        kgain <- pt.tm1 %*% t(zz) %*% solve(f) 		# Kalman Gain Factor
        
        
        # INFORMATION UPDATE (based on observed data)
      	at <- at.tm1 + kgain %*% err;				# KF recursion for posterior mean
       	pt <- pt.tm1 - kgain %*% zz %*% pt.tm1		# KF recursion for posterior covar
       	
       	at.out[ii,] <- t(at)							# save output for mean state vector 	
       	lo.CI[ii] <- at[1] - 1.96*sqrt(pt[1,1])		# save output for lo.CI state 1 
		hi.CI[ii] <- at[1] + 1.96*sqrt(pt[1,1])		# save output for hi.CI state 1
		
      }



#### Plot Actual Observations and Estimated Signal Overlaid by 95% CI #####

M <- at.out[, 1]
L <- lo.CI
U <- hi.CI
out <- data.frame(days, M, L, U)


plot(out$days, out$M, ylim = c(min(out$L), max(out$U)), type = "l", xlab = "Days", ylab = "Observed and Estimated", main = "Kalman Filter State Estimates with 95% CI")

# draws the grey confidence region
polygon(c(out$days,rev(out$days)), c(out$L, rev(out$U)), col = "grey85", border = FALSE) 
lines(out$days, out$M, lwd = 2)

add red lines on borders of polygon
lines(out$days, out$U, col = "red", lty = 2)
lines(out$days, out$L, col = "red", lty = 2)

par(new = TRUE)										# suppreses overwriting of the previous plot
points(y2, col = "blue", pch = "o")				# overlays points on the previous plot
par(new = FALSE)


