install.packages("tidyverse")
library(tidyverse)
library(caret)
install.packages('caTools')
library(caTools)

ball <- read.csv("Intra College Basketball.csv")
ball1 <- within(ball, rm(Player, ht, year, pid, pick))
ball2 <- na.omit(ball1)

ball3 <- sample.split(ball2$pts, SplitRatio = 0.85)
trainball <- subset(ball2, ball3 == TRUE)
testball <- subset(ball2, ball3 == FALSE)

# Define training control
set.seed(123) 
train.control <- trainControl(method = "cv", number = 5)
# Train the model
model <- train(pts ~., data = trainball, method = "lm",
               trControl = train.control)
# Summarize the results
print(model)
