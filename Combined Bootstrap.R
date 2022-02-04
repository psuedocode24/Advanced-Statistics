# Defining the libraries
library(ggplot2)
library(hrbrthemes)
library(dplyr)
library(ggpubr)

# Reading the input data
rank_data <- read.csv("DesignMatrix_Master.csv")
head(rank_data)

# Setting the seed to replicate the results
set.seed(420)

get_wtp_ci_residual <- function(rank_member) {
  # Storing the dummy variables into vectors
  screen_75 <- rank_data$Screen_75inch
  screen_85 <- rank_data$Screen_85inch
  res_4k <- rank_data$Resolution_4K
  sony <- rank_data$Sony
  price <- rank_data$Price_High
  
  # Running a basic model to get the residuals to bootstrap
  model <- lm(rank_member ~ screen_75 + screen_85 + res_4k + sony + price)
  rank_pred <- predict(model)
  rank_residuals <- model$residuals
  
  # Defining empty matrices to store the bootstrapped wtp for each attribute
  wtp.out <- matrix(0, 1000, 4)	
  
  # Running the bootstrap 1000 times
  for (i in 1:1000) {
    
    # Sampling the residuals with replacement
    index_with_replacement <- sample(nrow(rank_data), nrow(rank_data), replace = TRUE)
    # ystar <- yhat + residual
    rank_new <- rank_pred + rank_residuals[index_with_replacement]
    
    # Running a regression with ystar against x
    model_new <- lm(rank_new ~ screen_75 + screen_85 + res_4k + sony + price)
    
    # Getting beta_star
    coefficient <- summary(model_new)$coefficients # coefficients
    partworths <- coefficient[2:nrow(coefficient),1]
    
    # WTP_attribute4 = beta_attribute4* 500/ (beta_price_max – beta_price_min) 
    
    price_range <- max(partworths[5],0) - min(partworths[5],0)
    
    wtp1.star <- partworths[1] * 500 / price_range
    wtp2.star <- partworths[2] * 500 / price_range
    wtp3.star <- partworths[3] * 500 / price_range
    wtp4.star <- partworths[4] * 500 / price_range
    
    wtp.out[i,] <- cbind(wtp1.star, wtp2.star, wtp3.star, wtp4.star)
  }
  
  wtp.CI.lower.75 <- sort(wtp.out[,1])[25]		
  wtp.CI.median.75 <- sort(wtp.out[,1])[500]
  wtp.CI.upper.75 <- sort(wtp.out[,1])[975]		
  
  wtp.CI.lower.85 <- sort(wtp.out[,2])[25]		
  wtp.CI.median.85 <- sort(wtp.out[,2])[500]
  wtp.CI.upper.85 <- sort(wtp.out[,2])[975]
  
  wtp.CI.lower.res <- sort(wtp.out[,3])[25]		
  wtp.CI.median.res <- sort(wtp.out[,3])[500]
  wtp.CI.upper.res <- sort(wtp.out[,3])[975]
  
  wtp.CI.lower.sony <- sort(wtp.out[,4])[25]		
  wtp.CI.median.sony <- sort(wtp.out[,4])[500]
  wtp.CI.upper.sony <- sort(wtp.out[,4])[975]
  
  # Creating a data frame of CI of Willingness to pay of each attribute
  quantile_025 <- c(wtp.CI.lower.75, wtp.CI.lower.85, 
                    wtp.CI.lower.res, wtp.CI.lower.sony)
  quantile_50 <- c(wtp.CI.median.75, wtp.CI.median.85, 
                   wtp.CI.median.res, wtp.CI.median.sony)
  quantile_975 <- c(wtp.CI.upper.75, wtp.CI.upper.85, 
                    wtp.CI.upper.res, wtp.CI.upper.sony)
  
  wtp_df <- data.frame(cbind(quantile_025, quantile_50, quantile_975))
  rownames(wtp_df) <- c("Screen_75_inch", "Screen_85_inch", "Resolution_4K", "Sony")
  colnames(wtp_df) <- c("2.5Percentile","50Percentile","97.5Percentile")
  
  return(wtp_df)
}

get_wtp_ci_data <- function(rank_member) {
  # Defining empty matrices to store the bootstrapped wtp for each attribute
  wtp2.out <- matrix(0, 1000, 4)
  
  for (i in 1:1000) {
    # Sampling the index to resample the data
    index_with_replacement <- sample(nrow(rank_data), nrow(rank_data), replace = TRUE)
    
    # Storing the dummy variables into vectors
    screen_75 <- rank_data$Screen_75inch[index_with_replacement]
    screen_85 <- rank_data$Screen_85inch[index_with_replacement]
    res_4k <- rank_data$Resolution_4K[index_with_replacement]
    sony <- rank_data$Sony[index_with_replacement]
    price <- rank_data$Price_High[index_with_replacement]
    rank_new <- rank_member[index_with_replacement]
    
    # Running the regression on bootstrapped data
    model <- lm(rank_new ~ screen_75 + screen_85 + res_4k + sony + price)
    coefficient <- summary(model)$coefficients # coefficients
    partworths <- coefficient[2:nrow(coefficient),1]
    
    price_range <- max(partworths[5],0) - min(partworths[5],0)
    wtp1.star <- partworths[1] * 500 / price_range
    wtp2.star <- partworths[2] * 500 / price_range
    wtp3.star <- partworths[3] * 500 / price_range
    wtp4.star <- partworths[4] * 500 / price_range
    
    wtp2.out[i,] <- cbind(wtp1.star, wtp2.star, wtp3.star, wtp4.star)
  }
  
  wtp.CI.lower2.75 <- sort(wtp2.out[,1])[25]		
  wtp.CI.median2.75 <- sort(wtp2.out[,1])[500]
  wtp.CI.upper2.75 <- sort(wtp2.out[,1])[975]		
  
  wtp.CI.lower2.85 <- sort(wtp2.out[,2])[25]		
  wtp.CI.median2.85 <- sort(wtp2.out[,2])[500]
  wtp.CI.upper2.85 <- sort(wtp2.out[,2])[975]
  
  wtp.CI.lower2.res <- sort(wtp2.out[,3])[25]		
  wtp.CI.median2.res <- sort(wtp2.out[,3])[500]
  wtp.CI.upper2.res <- sort(wtp2.out[,3])[975]
  
  wtp.CI.lower2.sony <- sort(wtp2.out[,4])[25]		
  wtp.CI.median2.sony <- sort(wtp2.out[,4])[500]
  wtp.CI.upper2.sony <- sort(wtp2.out[,4])[975]
  
  # Creating a data frame of CI of Willingness to pay of each attribute
  quantile_025 <- c(wtp.CI.lower2.75, wtp.CI.lower2.85, 
                    wtp.CI.lower2.res, wtp.CI.lower2.sony)
  quantile_50 <- c(wtp.CI.median2.75, wtp.CI.median2.85, 
                   wtp.CI.median2.res, wtp.CI.median2.sony)
  quantile_975 <- c(wtp.CI.upper2.75, wtp.CI.upper2.85, 
                    wtp.CI.upper2.res, wtp.CI.upper2.sony)
  
  wtp_df <- data.frame(cbind(quantile_025, quantile_50, quantile_975))
  rownames(wtp_df) <- c("Screen_75_inch", "Screen_85_inch", "Resolution_4K", "Sony")
  colnames(wtp_df) <- c("2.5Percentile","50Percentile","97.5Percentile")
  
  return(wtp_df)
}

get_wtp_ci_residual(rank_data$Rank_Arvind)
get_wtp_ci_data(rank_data$Rank_Arvind)

get_wtp_ci_residual(rank_data$Rank_Ashi)
get_wtp_ci_data(rank_data$Rank_Ashi)

get_wtp_ci_residual(rank_data$Rank_Jeet)
get_wtp_ci_data(rank_data$Rank_Jeet)

get_wtp_ci_residual(rank_data$Rank_Karina)
get_wtp_ci_data(rank_data$Rank_Karina)

get_wtp_ci_residual(rank_data$Rank_Thea)
get_wtp_ci_data(rank_data$Rank_Thea)