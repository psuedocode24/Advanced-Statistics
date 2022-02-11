# Clear the existing data
rm(list=ls(all=TRUE)) 

# Loading the required libraries
library(readxl)
library(dplyr)
library(ggplot2)

# Loading the data
data <- read_excel("Cars_Data.xlsx", sheet="Infinity Data")
head(data)

# Renaming the columns
cols <- colnames(data)
cols[5] <- "Poorly_Built"
cols[10] <- "Easy_Service"
cols[16] <- "Poor_Value"
cols[17] <- "Overall_Preference"
colnames(data) <- cols

# Verifying the changes
head(data)

# Declaring the x and y variables
y <- as.matrix(data[,ncol(data)])
x <- as.matrix(data[,2:16])

# Calculating the correlation matrix
corr_mat <- cor(x)

# Calculating the eigen vectors and eigen values based on the correlation matrix
eigen_out <- eigen(corr_mat)
eigen_values <- eigen_out$values
eigen_vectors <- eigen_out$vectors

# SCREE PLOT
plot(eigen_values, 
     xlab = "Num Components", 
     ylab = "Eigen Value",
     main = "Scree Plot")
num_components_scree = 5
# Based on the scree plot, we can see that the elbow occurs at 5 components

# EGO Rule
num_components_ego <- nrow(as.matrix(eigen_values[eigen_values > 1]))
# Based on the EGO Rule, we should consider 4 components. 

# We will proceed with the number of components suggested by the EGO rule.
num_components <- num_components_ego
pc <- round(eigen_vectors[,1:num_components],4)

# Retaining only the values that are more than 0.3 or less than -0.3
mod_pc <- ifelse(abs(pc) < 0.3, 0, pc)
rownames(mod_pc) <- cols[2:16]

# Forming the vector Z
z <- x %*% mod_pc

# Running initial regression Y ~ Z
initModel <- lm(y ~ z)
summary(initModel)

# Based on the results of the initial regression, we will flip the coefficients of V1, V2 and V5
# We flip the coefficients to maintain upward trajectory of improvement (Going up, right means increase in value)
mod_pc[,c(1,2)] <- mod_pc[,c(1,2)] * (-1)

# Updating the z-vector using the updated principal components
z <- as.matrix(x %*% mod_pc)
rownames(z) <- data %>% pull(Brands)
colnames(z) <- c("Z1","Z2","Z3","Z4")

# Running the regression again to extract the slopes for calculating the ideal vector and the iso-preference line
finModel <- lm(y ~ z)
summary_finModel <- summary(finModel)
summary_finModel
# Only z1 and z3 are significant at 5% significance level.

# Extracting Coefficients of the principal components
slopes <- as.matrix(summary_finModel$coefficients[2:nrow(summary_finModel$coefficients),1])
rownames(slopes) <- c("Z1","Z2","Z3","Z4")
slopes

# Slope of Iso Preference line for Z1 and Z3 benefit space
iso_pref_slope <- -1*slopes[1]/slopes[3]
iso_pref_angle <- round((180/pi) * atan(iso_pref_slope),2)
iso_pref_angle

# Slope of Ideal vector for Z1 and Z3 benefit space
ideal_vector_slope <- slopes[3]/slopes[1]
ideal_vector_angle <- round((180/pi) * atan(ideal_vector_slope),2)
ideal_vector_angle

# Bootstrapping the iso preference angle and the ideal vector angle
pca_data <- cbind(y,z)
nn <- nrow(pca_data)

bb <- 1000

ideal_vector_bootstrp <- as.matrix(0, bb, 1)
iso_preference_bootstrp <- as.matrix(0, 1000, 1)

for (i in 1:bb){
  data_hat <- data[sample(nn,nn,replace = TRUE),]
  
  bootstrapRegression <- lm(Overall_Preference ~ ., data=as.data.frame(data_hat))
  bootstrapSummary <- summary(bootstrapRegression)
  slopes <- as.matrix(bootstrapSummary$coefficients[2:nrow(bootstrapSummary$coefficients),1])
  
  iso_pref_slope <- -1 * slopes[1]/slopes[3]
  iso_pref_angle <- round((180/pi) * atan(iso_pref_slope),2)
  
  ideal_vector_slope <- slopes[3]/slopes[1]
  ideal_vector_angle <- round((180/pi) * atan(ideal_vector_slope),2)
  
  iso_preference_bootstrp[i] <- iso_pref_angle
  ideal_vector_bootstrp[i] <- ideal_vector_angle
}

quantile(ideal_vector_bootstrp, 0.025, na.rm = TRUE)
quantile(ideal_vector_bootstrp, 0.975, na.rm = TRUE)

quantile(iso_preference_bootstrp, 0.025, na.rm = TRUE)
quantile(iso_preference_bootstrp, 0.975, na.rm = TRUE)

plot(z[,1], z[,2], main = "Brands in Z1 and Z2 space", xlab = "Benefit Z1", ylab = "Benefit Z2", col = "lightblue", pch = 19, cex = 2)		# Brand Map in Z1-Z2 space
text(z, labels = row.names(z), font = 2, cex = 0.5, pos = 1)