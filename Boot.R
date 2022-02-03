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

# Defining a function to get the coefficients
get_coefficients <- function(rank_vector, model=NULL) {
  
  if (missing(model)) {
    attribute_cols <- c("Screen_75inch","Screen_85inch","Resolution_4K","Sony","Price_High") # attribute columns
    attribute_df <- as.data.frame(rank_data[attribute_cols]) # creating a subset of the attributes from the design matrix
    attribute_df$rank <- rank_vector # Adding the rank of a team member to the attribute df
  
    partworth_model <- lm(rank ~ .,data = attribute_df) # Fitting a linear model on the preference ranks
    partworth_summary <- summary(partworth_model)
  
    return(round(partworth_summary$coefficients,3))
  } else {
    partworth_summary <- summary(model)
    return(round(partworth_summary$coefficients,3))
  }
}

# Defining a function to get the attribute importance
get_attribute_importance <- function(coefficients) {
  partworths <- coefficients[2:nrow(coefficients),1]
  
  # Calculating the Range of utilities for each attribute
  screen_range <- max(partworths[1],partworths[2],0) - min(partworths[1],partworths[2],0)
  resolution_range <- max(partworths[3],0) - min(partworths[3],0)
  brand_range <- max(partworths[4],0) - min(partworths[4],0)
  price_range <- max(partworths[5],0) - min(partworths[5],0)

  attributes <- c("Screen Size", "Screen Resolution", "Brand Name", "Price")
  
  # Creating a df of attributes and utility range to calculate attribute importance
  attribute_imp_df <- as.data.frame(cbind(attributes, c(screen_range, resolution_range, brand_range, price_range)))
  colnames(attribute_imp_df) <- c("attributes","range")
  
  attribute_imp_df$range <- as.numeric(attribute_imp_df$range)
  attribute_imp_df$importance <- round(attribute_imp_df$range*100/sum(attribute_imp_df$range),2)

  return(attribute_imp_df)
}

# Defining a function to get willingness to pay
get_willingness_to_pay <- function(attribute_imp_df, coefficients) {
  partworths <- as.data.frame(coefficients[2:5,1])
  
  # Calculating the dollar value of utility
  partworths$utility_dollar_val <- 500/attribute_imp_df$range[4]
  
  colnames(partworths) <- c("Part_Worth","Utility_Dollar_Val")
  partworths$willingness_to_pay <- partworths$Part_Worth*partworths$Utility_Dollar_Val
  
  return(partworths)
}

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
  list_screen_75 <- matrix(0, 1000, 1)
  list_screen_85 <- matrix(0, 1000, 1)
  list_res_4k <- matrix(0, 1000, 1)
  list_sony <- matrix(0, 1000, 1)
  
  # Running the bootstrap 1000 times
  for (i in 1:1000) {
    
    # Sampling the residuals with replacement
    index_with_replacement <- sample(nrow(rank_data), nrow(rank_data), replace = TRUE)
    # ystar <- yhat + residual
    rank_new <- rank_pred + rank_residuals[index_with_replacement]
    
    # Running a regression with ystar against x
    model_new <- lm(rank_new ~ screen_75 + screen_85 + res_4k + sony + price)
    
    # Getting beta_star
    coef_new <- get_coefficients(model=model_new)
    attr_df <- get_attribute_importance(coef_new)
    
    # Calculating willingness to pay of each attribute using beta_star
    wtp <- get_willingness_to_pay(attr_df, coef_new)
    
    list_screen_75[i] <- wtp[1,3]
    list_screen_85[i] <- wtp[2,3]
    list_res_4k[i] <- wtp[3,3]
    list_sony[i] <- wtp[4,3]
  }
  
  # Creating a data frame of CI of Willingness to pay of each attribute
  quantile_025 <- c(quantile(list_screen_75, 0.025), quantile(list_screen_85, 0.025), 
                    quantile(list_res_4k, 0.025), quantile(list_sony, 0.025))
  quantile_50 <- c(quantile(list_screen_75, 0.5), quantile(list_screen_85, 0.5), 
                   quantile(list_res_4k, 0.5), quantile(list_sony, 0.5))
  quantile_975 <- c(quantile(list_screen_75, 0.975), quantile(list_screen_85, 0.975), 
                    quantile(list_res_4k, 0.975), quantile(list_sony, 0.975))
  
  wtp_df <- data.frame(cbind(quantile_025, quantile_50, quantile_975))
  rownames(wtp_df) <- c("Screen_75_inch", "Screen_85_inch", "Resolution_4K", "Sony")
  colnames(wtp_df) <- c("2.5Percentile","50Percentile","97.5Percentile")
  
  return(wtp_df)
}

get_wtp_ci_data <- function(rank_member) {
  # Defining empty matrices to store the bootstrapped wtp for each attribute
  list_screen_75 <- matrix(0, 1000, 1)
  list_screen_85 <- matrix(0, 1000, 1)
  list_res_4k <- matrix(0, 1000, 1)
  list_sony <- matrix(0, 1000, 1)
  
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
    coef_member <- get_coefficients(model=model)
    attr_df <- get_attribute_importance(coef_member)

    # Calculating willingness to pay of each attribute
    wtp <- get_willingness_to_pay(attr_df, coef_member)
  
    list_screen_75[i] <- wtp[1,3]
    list_screen_85[i] <- wtp[2,3]
    list_res_4k[i] <- wtp[3,3]
    list_sony[i] <- wtp[4,3]
  }
  
  # Creating a data frame of CI of Willingness to pay of each attribute
  quantile_025 <- c(quantile(list_screen_75, 0.025), quantile(list_screen_85, 0.025),
                    quantile(list_res_4k, 0.025), quantile(list_sony, 0.025))
  quantile_50 <- c(quantile(list_screen_75, 0.5), quantile(list_screen_85, 0.5),
                   quantile(list_res_4k, 0.5), quantile(list_sony, 0.5))
  quantile_975 <- c(quantile(list_screen_75, 0.975), quantile(list_screen_85, 0.975),
                    quantile(list_res_4k, 0.975), quantile(list_sony, 0.975))
  
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