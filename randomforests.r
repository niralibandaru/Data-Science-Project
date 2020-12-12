##### NIRALI BANDARU
##### APPLIED DATA SCIENCE - PROF. ALEXANDER HERZOG
##### PURPOSE: GROUP ASSIGNMENT 4
##### ALTERNATIVE MODEL: RANDOM FOREST

# Load the data from EDA

mydata <- read.csv("df.csv")
View(mydata)
attach(mydata)

# Load libraries

library(ggplot2)
library(randomForest)
library(boot)
library(caret)
set.seed(100)

# Summary of data

summary(mydata)

# Implementing random forests to data

train <- sample(nrow(mydata), 0.7*nrow(mydata), replace = FALSE)
TrainSet <- mydata[train,]
ValidSet <- mydata[-train,]
rf <- randomForest(popularity ~ ., data = TrainSet)

# Cross-validation

train.control <- trainControl(method = "cv", number = 10)
