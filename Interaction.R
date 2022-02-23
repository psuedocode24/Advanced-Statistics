rm(list=ls(all=TRUE))

library(systemfit)
#install.packages("xlsx")
library("xlsx")


#Read the data
data <- read.xlsx("data_week.xlsx", 1)
data <- data[,3:15]
data

# Run linear regression model for all variables
linear_model <- lm(netsales_online ~ ., data = data)
summary(linear_model)

# Stepwise forward selection
library(olsrr)
forward_model <- ols_step_forward_p(linear_model)
forward_model$model
summary(forward_model)

## Stepwise backward selection
library(olsrr)
backward_model <- ols_step_backward_p(linear_model)
backward_model$model
summary(backward_model)

# The best model is : Netsales_Online = 2.507e+07 + 1.433e+01 online_budget + 
#                     5.049e+06 promotion -9.949e+05 avg_hours_sun -3.783e+06 holiday
#                     -1.355e+06 avg_cloud_index + 5.860e+05 avg_wind_speed -9.949e+05 avg_hours_sun

# IV
# The dummy variables are promotion and holidays.
holdiay = data[, 7]
promo = data[, 6]

# The significant non dummy variables are 
# online_budget, avg_hours_sun, avg_cloud_index, avg_wind_speed, phone_calls

budget <- log(data[,5])
avg_sun <- log(data[,12])
avg_cloud <- log(data[,8])
avg_wind <- log(data[,10])
calls <- log(1 + data[,4]) #Contains 0

# DV
netsales <- log(data[,1])

transformed_model <- lm(netsales ~ budget + avg_sun + avg_cloud + avg_wind + calls)
summary(transformed_model)
plot(transformed_model)


