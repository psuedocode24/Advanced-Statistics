#libraries
library(readxl)
library(MASS)

#set seed
set.seed(420)

#thea design matrix
rank_data <- read.csv("DesignMatrix_Master.csv")
head(rank_data)
nn <- nrow(rank_data)

#variables
rank <- rank_data$Rank_Arvind
screen75 <- rank_data$Screen_75inch
screen85 <- rank_data$Screen_85inch
res4k <- rank_data$Resolution_4K
sony <- rank_data$Sony
pricehi <- rank_data$Price_High

#original regression
out <- lm(rank ~ screen75 + screen85 + res4k + sony + pricehi)

#hw1 computations for reference and consistency

#coefficients to partworths
coefficients <- summary(out)$coefficients
partworths <- coefficients[2:nrow(coefficients),1]

#ranges
screen_range <- max(partworths[1],partworths[2],0) - min(partworths[1],partworths[2],0)
resolution_range <- max(partworths[3],0) - min(partworths[3],0)
brand_range <- max(partworths[4],0) - min(partworths[4],0)
price_range <- max(partworths[5],0) - min(partworths[5],0)

#attribute importance
attributes <- c("Screen Size", "Screen Resolution", "Brand Name", "Price")
attribute_imp_df <- as.data.frame(cbind(attributes, c(screen_range, resolution_range, brand_range, price_range)))
colnames(attribute_imp_df) <- c("attributes","range")
attribute_imp_df$range <- as.numeric(attribute_imp_df$range)
attribute_imp_df$importance <- round(attribute_imp_df$range*100/sum(attribute_imp_df$range),2)

#utility dollar value and willingness to pay
partworths <- as.data.frame(coefficients[2:5,1])
partworths$utility_dollar_val <- 500/attribute_imp_df$range[4]
colnames(partworths) <- c("Part_Worth","Utility_Dollar_Val")
partworths$willingness_to_pay <- partworths$Part_Worth*partworths$Utility_Dollar_Val

#############
# BOOTSTRAP #
#############

#yhat and residuals
rr <- out$residuals
yhat <- predict(out)

#resampling with replacement
bb <- 1000
screen75.out <- matrix(0, bb, 1)		
screen85.out <- matrix(0, bb, 1)
res4k.out <- matrix(0, bb, 1)
sony.out <- matrix(0, bb, 1)
price.out <- matrix(0, bb, 1)

#residual boostrap
for(ii in 1:bb) {
  
  ystar <- yhat + rr[sample(nn, nn, replace = TRUE)]                        # y* with original yhat plus r*
  out.star <- lm(ystar~screen75 + screen85 + res4k + sony + pricehi)				# lm with new y* and same x to get new bhat*
  screen75.star <- summary(out.star)$coefficients[2,1]
  screen85.star <- summary(out.star)$coefficients[3,1]
  res4k.star <- summary(out.star)$coefficients[4,1]
  sony.star <- summary(out.star)$coefficients[5,1]
  price.star <- summary(out.star)$coefficients[6,1]
  screen75.out[ii] <- screen75.star		
  screen85.out[ii] <- screen85.star		
  res4k.out[ii] <- res4k.star		
  sony.out[ii] <- sony.star		
  price.out[ii] <- price.star
}

##########################
# MEDIAN WTP COMPUTATION #
##########################

#median coefficients
screen75.median <- median(sort(screen75.out))
screen85.median <- median(sort(screen85.out))
res4k.median <- median(sort(res4k.out))
sony.median <- median(sort(sony.out))
price.median <- median(sort(price.out))

#ranges
med_screen_range <- max(screen75.median,screen85.median,0) - min(screen75.median,screen85.median,0)
med_resolution_range <- max(res4k.median,0) - min(res4k.median,0)
med_brand_range <- max(sony.median,0) - min(sony.median,0)
med_price_range <- max(price.median,0) - min(price.median,0)

#attribute importance
med_attribute_imp_df <- as.data.frame(cbind(attributes, c(med_screen_range, med_resolution_range, med_brand_range, med_price_range)))
colnames(med_attribute_imp_df) <- c("attributes","range")
med_attribute_imp_df$range <- as.numeric(med_attribute_imp_df$range)
med_attribute_imp_df$importance <- round(med_attribute_imp_df$range*100/sum(med_attribute_imp_df$range),2)

#utility dollar value and willingness to pay
med_partworths <- as.data.frame(c(screen75.median, screen85.median, res4k.median, sony.median))
rownames(med_partworths) <- c("Screen Size 75", "Screen Size 85", "Resolution 4k", "Brand sony")
med_partworths$utility_dollar_val <- 500/med_attribute_imp_df$range[4]
colnames(med_partworths) <- c("Part_Worth","Utility_Dollar_Val")
med_partworths$willingness_to_pay <- med_partworths$Part_Worth*med_partworths$Utility_Dollar_Val

#final for median
med_partworths

##########################
# 2.5PCT WTP COMPUTATION #
##########################

#2.5 percentile coefficients
screen75.CI.lower <- sort(screen75.out)[25]
screen85.CI.lower <- sort(screen85.out)[25]
res4k.CI.lower <- sort(res4k.out)[25]
sony.CI.lower <- sort(sony.out)[25]
price.CI.lower <- sort(price.out)[25]

#ranges
p25_screen_range <- max(screen75.CI.lower,screen85.CI.lower,0) - min(screen75.CI.lower,screen85.CI.lower,0)
p25_resolution_range <- max(res4k.CI.lower,0) - min(res4k.CI.lower,0)
p25_brand_range <- max(sony.CI.lower,0) - min(sony.CI.lower,0)
p25_price_range <- max(price.CI.lower,0) - min(price.CI.lower,0)

#attribute importance
p25_attribute_imp_df <- as.data.frame(cbind(attributes, c(p25_screen_range, p25_resolution_range, p25_brand_range, p25_price_range)))
colnames(p25_attribute_imp_df) <- c("attributes","range")
p25_attribute_imp_df$range <- as.numeric(p25_attribute_imp_df$range)
p25_attribute_imp_df$importance <- round(p25_attribute_imp_df$range*100/sum(p25_attribute_imp_df$range),2)

#utility dollar value and willingness to pay
p25_partworths <- as.data.frame(c(screen75.CI.lower, screen85.CI.lower, res4k.CI.lower, sony.CI.lower))
rownames(p25_partworths) <- c("Screen Size 75", "Screen Size 85", "Resolution 4k", "Brand sony")
p25_partworths$utility_dollar_val <- 500/p25_attribute_imp_df$range[4]
colnames(p25_partworths) <- c("Part_Worth","Utility_Dollar_Val")
p25_partworths$willingness_to_pay <- p25_partworths$Part_Worth*p25_partworths$Utility_Dollar_Val

#final for 2.5 percentile
p25_partworths

###########################
# 97.5PCT WTP COMPUTATION #
###########################

#97.5 percentile coefficients
screen75.CI.upper <- sort(screen75.out)[975]
screen85.CI.upper <- sort(screen85.out)[975]
res4k.CI.upper <- sort(res4k.out)[975]
sony.CI.upper <- sort(sony.out)[975]
price.CI.upper <- sort(price.out)[975]

#ranges
p975_screen_range <- max(screen75.CI.upper,screen85.CI.upper,0) - min(screen75.CI.upper,screen85.CI.upper,0)
p975_resolution_range <- max(res4k.CI.upper,0) - min(res4k.CI.upper,0)
p975_brand_range <- max(sony.CI.upper,0) - min(sony.CI.upper,0)
p975_price_range <- max(price.CI.upper,0) - min(price.CI.upper,0)

#attribute importance
p975_attribute_imp_df <- as.data.frame(cbind(attributes, c(p975_screen_range, p975_resolution_range, p975_brand_range, p975_price_range)))
colnames(p975_attribute_imp_df) <- c("attributes","range")
p975_attribute_imp_df$range <- as.numeric(p975_attribute_imp_df$range)
p975_attribute_imp_df$importance <- round(p975_attribute_imp_df$range*100/sum(p975_attribute_imp_df$range),2)

#utility dollar value and willingness to pay
p975_partworths <- as.data.frame(c(screen75.CI.upper, screen85.CI.upper, res4k.CI.upper, sony.CI.upper))
rownames(p975_partworths) <- c("Screen Size 75", "Screen Size 85", "Resolution 4k", "Brand sony")
p975_partworths$utility_dollar_val <- 500/p975_attribute_imp_df$range[4]
colnames(p975_partworths) <- c("Part_Worth","Utility_Dollar_Val")
p975_partworths$willingness_to_pay <- p975_partworths$Part_Worth*p975_partworths$Utility_Dollar_Val

#final for 97.5 percentile
p975_partworths

###################
# WTP COMPILATION #
###################

WTP_compiled <- cbind(p25_partworths$willingness_to_pay, med_partworths$willingness_to_pay, p975_partworths$willingness_to_pay)
rownames(WTP_compiled) <- c("Screen Size 75", "Screen Size 85", "Resolution 4k", "Brand sony")
colnames(WTP_compiled) <- c("2.5 WTP", "Median WTP", "97.5 WTP")
round(WTP_compiled, 3)
