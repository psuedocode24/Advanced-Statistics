# Clear the existing data
rm(list=ls(all=TRUE))

# Loading the necessary libraries
library(glmnet)

# Reading the data
data <- read.csv("Cars_Data.csv")
y <- data[,17]
x <- as.matrix(data[,2:16])

#################
## regressions ## 
#################

alpha <- seq(0, 1, 0.1)

# alpha = 1 corresponds to Lasso
# alpha = 0 corresponds to Ridge
# Rest of the alphas are elastic net

# Defining a coefficient matrix for all values of alpha
coef_matrix <- matrix(nrow = 16, ncol = 11)
mse_matrix <- matrix(nrow = 1, ncol = 11)

set.seed(25)

for (i in 1:length(alpha)) {
  # Running the shrinkage regression
  alpha_i <- alpha[i]
  shrinkage_model <- glmnet(x, y, alpha = alpha_i)

  # Plotting the coefficient vs lambda plot
  plot(shrinkage_model, xvar= "lambda", label = TRUE)	
  plot_title <- paste("Coefficient Plot at alpha =",alpha_i, sep=" ")
  title(plot_title,line=2.5)
  
  # Doing cross validation to find the optimal lambda
  shrinkage_crossval <- cv.glmnet(x, y, alpha = alpha_i, type.measure = "mse", nfolds = 3)
  
  # Plotting the MSE vs Lambda plot
  plot(shrinkage_crossval)
  
  # Extracting coefficient corresponding to the minimum lambda
  lambda_est <- shrinkage_crossval$lambda.min
  coef_matrix[,i] <- as.matrix(coef(shrinkage_crossval, s = lambda_est), ncol=1)
  
  # Extracting the minimum mse value for all values of alpha
  index <- which(shrinkage_crossval$lambda == shrinkage_crossval$lambda.min)
  mse_min <- shrinkage_crossval$cvm[index]
  mse_matrix[,i] <- mse_min
}

colnames(coef_matrix) <- c("Ridge Est", "EN Est 0.1", "EN Est 0.2", "EN Est 0.3", "EN Est 0.4", "EN Est 0.5", "EN Est 0.6", "EN Est 0.7", "EN Est 0.8", "EN Est 0.9", "Lasso Est")
rowname <- colnames(data[,2:16])
rownames(coef_matrix) <- c("Intercept",rowname)
colnames(mse_matrix) <- c("Ridge", "alpha 0.1", "alpha 0.2", "alpha 0.3", "alpha 0.4", "alpha 0.5", "alpha 0.6", "alpha 0.7", "alpha 0.8", "alpha 0.9", "Lasso")

mse_matrix[2:10]
# The minimum MSE corresponds to alpha = 0.1

coef_matrix

# Top 3 Coefficients as per EN, Ridge and Lasso
# Lasso: Interesting, Successful, Uncomfortable
# Ridge: Interesting, Uncomfortable, Poor.Value
# Elastic Net (alpha = 0.1): Interesting, Successful, Uncomfortable


### Final linear models ###

# For Lasso and Elastic Net
model1 <- lm(Overall.Preference ~ Interesting + Uncomfortable + Successful, data=data)
summary_mod1 <- summary(model1)

# For Ridge
model2 <- lm(Overall.Preference ~ Interesting + Uncomfortable + Poor.Value, data=data)
summary_mod2 <- summary(model2)


# Biases of the coefficients
# For Lasso and Elastic Net
Bias1 <- as.matrix(summary_mod1$coefficients[2:nrow(summary_mod1$coefficients),1])
rownames(Bias1) <- c("Interesting","Uncomfortable","Successful")
Bias1
Interesting_Lasso <- (coef_matrix[6,11] / Bias1[1])
Interesting_Lasso
Uncomfortable_Lasso <- 1 - (coef_matrix[8,11] / Bias1[2])
Uncomfortable_Lasso
Successful_Lasso <- 1 - (coef_matrix[14,11] / Bias1[3])
Successful_Lasso

Interesting_EN <- (coef_matrix[6,2] / Bias1[1])
Uncomfortable_EN <- 1 - (coef_matrix[8,2] / Bias1[2])
Successful_EN <- 1 - (coef_matrix[14,2] / Bias1[3])

cbind(Interesting_Lasso, Uncomfortable_Lasso, Successful_Lasso, Interesting_EN, Uncomfortable_EN, Successful_EN)

# For Ridge
Bias2 <- as.matrix(summary_mod2$coefficients[2:nrow(summary_mod2$coefficients),1])
rownames(Bias2) <- c("Interesting","Uncomfortable","Poor.Value")
Bias2

Interesting_Ridge <- (coef_matrix[6,1] / Bias2[1])
Uncomfortable_Ridge <- 1 - (coef_matrix[8,1] / Bias2[2])
PV_Ridge <- 1 - (coef_matrix[16,1] / Bias2[3])

cbind(Interesting_Ridge, Uncomfortable_Ridge, PV_Ridge)
