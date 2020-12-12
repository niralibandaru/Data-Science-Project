##### NIRALI BANDARU
##### APPLIED DATA SCIENCE - PROF. ALEXANDER HERZOG
##### PURPOSE: GROUP ASSIGNMENT 4
##### ALTERNATIVE MODEL: Support Vector Classifier (Non-linear)

list(rm=ls())

# Load the data from EDA

mydata <- read.csv("df1.csv")
#View(mydata)
attach(mydata)

# remove unwanted columns

mydata$mode <- NULL
mydata$year <- NULL
mydata$valence <- NULL
mydata$energy <- NULL

#View(mydata)

na.omit(mydata)

#View(mydata)
# Load libraries

library(ggplot2)
library(boot)
library(caret)
library(e1071)
library(dplyr)
# Summary of data
set.seed(100)
mydata <- sample_n(mydata, 10000)
summary(mydata)
View(mydata)
summary(mydata)
#View(mydata)

# Implementing SVM

intrain <- createDataPartition(y = mydata$popularity, p= 0.75, list = FALSE)
training <- mydata[intrain,]
testing <- mydata[-intrain,]
dim(training)
dim(testing)

anyNA(mydata)


# training and cross-validation

svmfit <- svm(popularity~., data=training, kernel="radial", gamma=1, cost=1)

summary(svmfit)

set.seed(100)

tune.out <- tune(svm, popularity~., data = training, kernel = "radial", ranges = list(cost = c(0.1, 1, 10)), gamma = c(0.5, 1, 2))
summary(tune.out)

table(true = mydata[testing, "popularity"], pred = predict(tune.out$best.model, newdata = mydata[testing, ]))


