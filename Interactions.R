rm(list=ls(all=TRUE)) #clear data

# Loading the relevant libraries
library(ggplot2)
library(gridExtra)

# Reading the data
data <- read.csv("chocolate.csv", sep = ",", dec = ".", header = T) 	# weekly data
names(data)
head(data,5)

# Plotting the data
p1 <- ggplot(data, aes(x = Week, y = GRP)) + 
  geom_bar(stat = "identity") 
p2 <- ggplot(data, aes(x = Week, y = Awareness)) + 
  geom_line() + 
  geom_point(colour="blue", size = 2, shape = 21, fill="white") 
p3 <- ggplot(data, aes(x = Week, y = Sales)) + 
  geom_line() + 
  geom_point(colour="red", size = 2, shape = 21, fill="white") 

# present GRP, Awareness and Sales plots stacked in 3 rows
grid.arrange(p1,p2,p3,  nrow = 3)  
theme_set(theme_bw())

# Defining a matrix to store the outcomes of data bootstrap
data_bootstrap = matrix(, nrow = 1000, ncol = 5)
colnames(data_bootstrap) <- c("A","B","C","Mediation_Effect","Total_Effect")

# Defining the variables
grp <- data[,2]			# Proportional to $ AdSpends per week. GRP = reach x frequency = percentage market reached x number of times per week 
aware <- data[,3]		# percentage of the market aware of ad campaign 
sales <- data[,4]		# sales

# Calculating the parameter estimates
m1 <- lm(sales ~ grp); summary(m1)
m2 <- lm(aware ~ grp); summary(m2)
m3 <- lm(sales ~ aware); summary(m3)

# Direct and Indirect Effects
a <- unname(m2$coeff[2])
b <- unname(m3$coeff[2])
c <- unname(m1$coeff[2])
mediation.effect = unname(a * b)
total.effect = unname(c + a * b)

# Running the bootstrap for 1000 iterations
set.seed(420)
for (i in 1:1000) {
  index <- sample(nrow(data), nrow(data), replace = TRUE)
  grp_bts <- grp[index]
  aware_bts <- aware[index]
  sales_bts <- sales[index]
  
  # Direct Effect: GRP -> Sales 
  # Indirect Effects: GRP -> Aware and Aware -> Sales
  m1 <- lm(sales_bts ~ grp_bts); summary(m1)
  m2 <- lm(aware_bts ~ grp_bts); summary(m2)
  m3 <- lm(sales_bts ~ aware_bts); summary(m3)
  
  # Direct and Indirect Effects
  a <- m2$coeff[2]
  b <- m3$coeff[2]
  c <- m1$coeff[2]
  mediation.effect = unname(a * b)
  total.effect = unname(c + a * b)
  
  # Logging the values of the bootstrap
  data_bootstrap[i,1] <- a
  data_bootstrap[i,2] <- b
  data_bootstrap[i,3] <- c
  data_bootstrap[i,4] <- mediation.effect
  data_bootstrap[i,5] <- total.effect
}

# Converting matrix to dataframe
bootstrap_df <- as.data.frame(data_bootstrap)

# Plotting the histogram for direct effect of GRP on Awareness
lbound_a <- round(mean(bootstrap_df$A) - 2*sd(bootstrap_df$A), 4)
ubound_a <- round(mean(bootstrap_df$A) + 2*sd(bootstrap_df$A), 4)
mean_a <- round(mean(bootstrap_df$A), 4)

plot_a <- ggplot(data = bootstrap_df, aes(x = A)) + 
  geom_histogram(color = "#1d395c", fill = "#20497a") +
  labs(title = "Effect of GRP on Awareness", x = "Effect A", y = "Frequency") +
  theme_classic() +
  geom_vline(aes(xintercept = mean(A)), color = "#000000", size = 1, linetype = "dashed") +
  geom_vline(aes(xintercept = mean(A) + 2*sd(A)), color = "#1d395c", size = 1, linetype = "dashed") +
  geom_vline(aes(xintercept = mean(A) - 2*sd(A)), color = "#1d395c", size = 1, linetype = "dashed") +
  annotate("text", x = lbound_a, y = 50, label=paste("Lower Confidence Bound (95%):", lbound_a), size= 4, angle = 90, vjust = -1) +
  annotate("text", x = ubound_a, y = 50, label=paste("Upper Confidence Bound (95%):", ubound_a), size= 4, angle = 90, vjust = 2) +
  annotate("text", x = mean_a, y = 0, label=paste("Mean Effect", mean_a), size= 4, vjust = 1.5, hjust = -0.1)
plot_a

# Plotting the histogram for direct effect of Awareness on Sales
lbound_b <- round(mean(bootstrap_df$B) - 2*sd(bootstrap_df$B), 4)
ubound_b <- round(mean(bootstrap_df$B) + 2*sd(bootstrap_df$B), 4)
mean_b <- round(mean(bootstrap_df$B), 4)

plot_b <- ggplot(data = bootstrap_df, aes(x = B)) + 
  geom_histogram(color = "#1d5c4c", fill = "#1e856b") +
  labs(title = "Effect of Awareness on Sales", x = "Effect B", y = "Frequency") +
  theme_classic() +
  geom_vline(aes(xintercept = mean(B)), color = "#000000", size = 1, linetype = "dashed") +
  geom_vline(aes(xintercept = mean(B) + 2*sd(B)), color = "#1d5c4c", size = 1, linetype = "dashed") +
  geom_vline(aes(xintercept = mean(B) - 2*sd(B)), color = "#1d5c4c", size = 1, linetype = "dashed") +
  annotate("text", x = lbound_b, y = 50, label=paste("Lower Confidence Bound (95%):", lbound_b), size= 4, angle = 90, vjust = -1) +
  annotate("text", x = ubound_b, y = 50, label=paste("Upper Confidence Bound (95%):", ubound_b), size= 4, angle = 90, vjust = 2) +
  annotate("text", x = mean_b, y = 0, label=paste("Mean Effect", mean_b), size= 4, vjust = 1.5, hjust = -0.1)
plot_b

# Plotting the histogram for direct effect of GRP on Sales
lbound_c <- round(mean(bootstrap_df$C) - 2*sd(bootstrap_df$C), 4)
ubound_c <- round(mean(bootstrap_df$C) + 2*sd(bootstrap_df$C), 4)
mean_c <- round(mean(bootstrap_df$C), 4)

plot_c <- ggplot(data = bootstrap_df, aes(x = C)) + 
  geom_histogram(color = "#615d1f", fill = "#8f871f") +
  labs(title = "Effect of GRP on Sales", x = "Effect C", y = "Frequency") +
  theme_classic() +
  geom_vline(aes(xintercept = mean(C)), color = "#000000", size = 1, linetype = "dashed") +
  geom_vline(aes(xintercept = mean(C) + 2*sd(C)), color = "#615d1f", size = 1, linetype = "dashed") +
  geom_vline(aes(xintercept = mean(C) - 2*sd(C)), color = "#615d1f", size = 1, linetype = "dashed") +
  annotate("text", x = lbound_c, y = 50, label=paste("Lower Confidence Bound (95%):", lbound_c), size= 4, angle = 90, vjust = -1) +
  annotate("text", x = ubound_c, y = 50, label=paste("Upper Confidence Bound (95%):", ubound_c), size= 4, angle = 90, vjust = 2) +
  annotate("text", x = mean_c, y = 0, label=paste("Mean Effect", mean_c), size= 4, vjust = 1.5, hjust = -0.1)
plot_c

# Plotting the histogram for mediation effect of GRP and Awareness on Sales
lbound_me <- round(mean(bootstrap_df$Mediation_Effect) - 2*sd(bootstrap_df$Mediation_Effect), 4)
ubound_me <- round(mean(bootstrap_df$Mediation_Effect) + 2*sd(bootstrap_df$Mediation_Effect), 4)
mean_me <- round(mean(bootstrap_df$Mediation_Effect), 4)

plot_me <- ggplot(data = bootstrap_df, aes(x = Mediation_Effect)) + 
  geom_histogram(color = "#69311b", fill = "#8a3b1c") +
  labs(title = "Mediation Effect of GRP & Awareness on Sales", x = "Mediation Effect (A*B)", y = "Frequency") +
  theme_classic() +
  geom_vline(aes(xintercept = mean(Mediation_Effect)), color = "#000000", size = 1, linetype = "dashed") +
  geom_vline(aes(xintercept = mean(Mediation_Effect) + 2*sd(Mediation_Effect)), color = "#69311b", size = 1, linetype = "dashed") +
  geom_vline(aes(xintercept = mean(Mediation_Effect) - 2*sd(Mediation_Effect)), color = "#69311b", size = 1, linetype = "dashed") +
  annotate("text", x = lbound_me, y = 50, label=paste("Lower Confidence Bound (95%):", lbound_me), size= 4, angle = 90, vjust = -1) +
  annotate("text", x = ubound_me, y = 50, label=paste("Upper Confidence Bound (95%):", ubound_me), size= 4, angle = 90, vjust = 2) +
  annotate("text", x = mean_me, y = 0, label=paste("Mean Effect", mean_me), size= 4, vjust = 1.5, hjust = -0.1)
plot_me

# Plotting the histogram for total effect of GRP and Awareness on Sales
lbound_te <- round(mean(bootstrap_df$Total_Effect) - 2*sd(bootstrap_df$Total_Effect), 4)
ubound_te <- round(mean(bootstrap_df$Total_Effect) + 2*sd(bootstrap_df$Total_Effect), 4)
mean_te <- round(mean(bootstrap_df$Total_Effect), 4)

plot_te <- ggplot(data = bootstrap_df, aes(x = Total_Effect)) + 
  geom_histogram(color = "#1e6b75", fill = "#1d8491") +
  labs(title = "Total Effect of GRP & Awareness on Sales", x = "Total Effect (C + A*B)", y = "Frequency") +
  theme_classic() +
  geom_vline(aes(xintercept = mean(Total_Effect)), color = "#000000", size = 1, linetype = "dashed") +
  geom_vline(aes(xintercept = mean(Total_Effect) + 2*sd(Total_Effect)), color = "#1e6b75", size = 1, linetype = "dashed") +
  geom_vline(aes(xintercept = mean(Total_Effect) - 2*sd(Total_Effect)), color = "#1e6b75", size = 1, linetype = "dashed") +
  annotate("text", x = lbound_te, y = 50, label=paste("Lower Confidence Bound (95%):", lbound_te), size= 4, angle = 90, vjust = -1) +
  annotate("text", x = ubound_te, y = 50, label=paste("Upper Confidence Bound (95%):", ubound_te), size= 4, angle = 90, vjust = 2) +
  annotate("text", x = mean_te, y = 0, label=paste("Mean Effect", mean_te), size= 4, vjust = 1.5, hjust = -0.1)
plot_te