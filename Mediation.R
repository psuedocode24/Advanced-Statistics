setwd("/Users/prasad/Desktop/Jan_2022/Teaching/BAX442/2022/Lectures/Class7/")
rm(list=ls(all=TRUE)) #clear data

# install.packages(c("ggplot2", "gridExtra"), dependencies = TRUE, repos = "https://cloud.r-project.org")

library(ggplot2)
library(gridExtra)


##### Read Data #####

data <- read.csv("chocolate.csv", sep = ",", dec = ".", header = T) 	# weekly data
names(data)
head(data,5)


##### Plot Data #####

p1 <- ggplot(data, aes(x = Week, y = GRP)) + geom_bar(stat = "identity") 
p2 <- ggplot(data, aes(x = Week, y = Awareness)) + geom_line() + geom_point(colour="blue", size = 2, shape = 21, fill="white") 
p3 <- ggplot(data, aes(x = Week, y = Sales)) + geom_line() + geom_point(colour="red", size = 2, shape = 21, fill="white") 

grid.arrange(p1,p2,p3,  nrow = 3)  # present both GRP and Awareness plots stacked in 2 rows
theme_set(theme_bw())



##### Variables #####

grp <- data[,2]			# Proportional to $ AdSpends per week. GRP = reach x frequency = percentage market reached x number of times per week 
aware <- data[,3]		# percentage of the market aware of ad campaign 
sales <- data[,4]		# sales


m0 <- lm(sales ~ grp + aware)
summary(m0)				



# Direct Effect: GRP -> Sales 
# Indirect Effects: GRP -> Aware and Aware -> Sales

m1 <- lm(sales ~ grp); summary(m1)
m2 <- lm(aware ~ grp); summary(m2)
m3 <- lm(sales ~ aware); summary(m3)


# Direct and Indirect Effects
a <- m2$coeff[2]
b <- m3$coeff[2]
c <- m1$coeff[2]
mediation.effect = unname(a * b)
total.effect = unname(c + a * b)

# Mediational Inference
# What's the 95% CI for indirect effect? Not available from regression theory. The distribution of a x b is not normal and not known yet. So apply data bootstrap.



##### HW6: Residual Bootstrap to answer Q1 and Q2 ##### 
# Q1. Plot histograms for a, b, c, mediation.effect, total.effect 
# Q2. Find 95% CI for indirect effect, direct effect, and total effect












