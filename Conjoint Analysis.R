# Defining the libraries
library(ggplot2)
library(hrbrthemes)
library(dplyr)

# Reading the input data
rank_data <- read.csv("DesignMatrix_Master.csv")
head(rank_data)

# Defining a function to get the coefficients
get_coefficients <- function(rank_vector) {
  
  attribute_cols <- c("Screen_75inch","Screen_85inch","Resolution_4K","Sony","Price_High") # attribute columns
  attribute_df <- as.data.frame(rank_data[attribute_cols]) # creating a subset of the attributes from the design matrix
  attribute_df$rank <- rank_vector # Adding the rank of a team member to the attribute df
  
  partworth_model <- lm(rank ~ .,data = attribute_df) # Fitting a linear model on the preference ranks
  partworth_summary <- summary(partworth_model)
  
  return(round(partworth_summary$coefficients,3))
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

# Defining a function to get market share and profit
get_marketshare_profit <- function(coefficients, design) {
  
  # Defining the cost vector for profit calculations
  cost <- c(500, 1000, 250, 250)
  net_cost <- sum(cost)
  
  prices <- seq(1500,3000,by=100)
  
  # Empty vector to store the market size and profit
  market_share <- c()
  profit <- c()
  market_size <- 100
  
  for (price in prices) {
    competitor_a <- c(1,0,1,1,1,2500) # Design for competitor A
    competitor_b <- c(1,1,0,1,1,2000) # Design for competitor B
    own_design <- c(design, price) # Dynamic own design vector
    partworths <- as.data.frame(coefficients[,1])
    colnames(partworths) <- c("estimate")
    
    design_df = data.frame(own_design, competitor_a, competitor_b, partworths)
    
    utility_a <- sum(design_df$competitor_a[1:5] * design_df$partworths[1:5]) + 
      design_df$estimate[6] * ((design_df$competitor_a[6] -2000)/(2500-2000))
    utility_b <- sum(design_df$competitor_b[1:5] * design_df$partworths[1:5]) + 
      design_df$estimate[6] * ((design_df$competitor_b[6] -2000)/(2500-2000))
    utility_own <- sum(design_df$own_design[1:5] * design_df$partworths[1:5]) + 
      design_df$estimate[6] * ((design_df$own_design[6] -2000)/(2500-2000))
    
    # Calculating the market share and profit
    market_share_own <- exp(utility_own)/(exp(utility_b) + exp(utility_a) + exp(utility_own))
    sales <- market_share_own * market_size
    margin <- price - net_cost
    profit_own <- margin*sales
    
    market_share <- c(market_share, market_share_own)
    profit <- c(profit, profit_own)
  }
  
  mkt_profit_df <- data.frame(prices, market_share, profit)
  
  return(mkt_profit_df)
}

homework1 <- function(rank_vector, design) {
  # Printing the coefficients for the member
  coef_member <- get_coefficients(rank_vector)
  print("######################################################################")
  print("####### Partworths, Standard Error and T-values for the member #######")
  print("######################################################################")
  print(coef_member)
  print(" ")
  
  # Printing the attribute importance for the member
  attr_imp <- get_attribute_importance(coef_member)
  print(" ")
  print("######################################################################")
  print("#### Attribute importance for different attributes for the member ####")
  print("######################################################################")
  print(attr_imp)
  print(" ")
  
  # Printing the willingness to pay for different attributes for the member
  wtp <- get_willingness_to_pay(attr_imp, coef_member)
  print(" ")
  print("######################################################################")
  print("##### Willingness to pay for different attributes for the member #####")
  print("######################################################################")
  print(wtp)
  print(" ")
  
  # Printing the market share and profit for different designs for the member
  mktshare_profit <- get_marketshare_profit(coef_member, c(1,design))
  print(" ")
  print("######################################################################")
  print("##### Market Share and Profit for different prices for the member ####")
  print("######################################################################")
  print(mktshare_profit)
  print(" ")
  
  # Extracting the maximum profit and the optimal price corresponding to the maximum profit
  max_profit <- max(mktshare_profit$profit)
  optimal_price <- mktshare_profit %>% filter(profit == max_profit) %>% select(prices)
  print(" ")
  print("######################################################################")
  print(paste("The optimal price for the selected design is", optimal_price))
  print(paste("The corresponding profit for the optimal price is", round(max_profit,2)))
  print("######################################################################")
  print(" ")
    
  # Plotting the graphs
  plot_mktshare <- ggplot(mktshare_profit, aes(x=prices, y=market_share)) +
                      geom_line(color="#800000", size=2, alpha=0.9, linetype=2) +
                      theme_ipsum() +
                      ggtitle("Market Share vs Price")
  plot_mktshare
  
  plot_profit <- ggplot(mktshare_profit, aes(x=prices, y=profit)) +
                    geom_line(color="#013220", size=2, alpha=0.9, linetype=2) +
                    theme_ipsum() +
                    ggtitle("Profit vs Price")
  plot_profit
}

# Define the rank and design vector here
# Arvind
rank_member <- rank_data$Rank_Arvind
design <- c(0,1,1,1)

homework1(rank_member, design)

# Ashi
rank_member <- rank_data$Rank_Ashi
design <- c(0,1,0,0)

homework1(rank_member, design)

# Jeet
rank_member <- rank_data$Rank_Jeet
design <- c(1,0,1,0)

homework1(rank_member, design)

# Karina
rank_member <- rank_data$Rank_Karina
design <- c(1,0,1,1)

homework1(rank_member, design)

# Thea
rank_member <- rank_data$Rank_Thea
design <- c(1,0,0,1)

homework1(rank_member, design)