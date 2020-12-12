##### NIRALI BANDARU and ROHAN GANGISETTY
##### APPLIED DATA SCIENCE - PROF. ALEXANDER HERZOG
##### PURPOSE: GROUP ASSIGNMENT 4
##### ALTERNATIVE MODEL: RANDOM FOREST

# Load the data from EDA

mydata <- read.csv("df.csv")

View(mydata)
##View(df)
attach(mydata)

# remove unwanted columns

mydata$mode <- NULL
mydata$year <- NULL
mydata$valence <- NULL
mydata$energy <- NULL

View(mydata)
# Load libraries

library(ggplot2)
install.packages("randomForest")
library(randomForest)
library(boot)
library(caret)

# Summary of data
##split(mydata,cut(mydata$popularity,2))
summary(mydata)



# Implementing random forests to data

set.seed(1234)
df1<-mydata[sample(nrow(mydata), 10000), ]

training <- sample(1:10000, 7500)
testing <- seq(1:10000)[-training]

df1.testing <- df1[testing,"popularity"]


rf1 <- randomForest(popularity ~ .,data=df1 ,mtry = 3,subset = training,importance=TRUE)
yhat.rf1 <- predict(rf1,df1[testing,])
mean((yhat.rf1-df1.testing)^2)
print(rf1)

##2
set.seed(123)
df2<-mydata[sample(nrow(mydata), 10000), ]

training <- sample(1:10000, 7500)
testing <- seq(1:10000)[-training]

df2.testing <- df2[testing,"popularity"]


rf2 <- randomForest(popularity ~ .,data=df2 ,mtry = 3,subset = training,importance=TRUE)
yhat.rf2 <- predict(rf2,df2[testing,])
mean((yhat.rf2-df2.testing)^2)
print(rf2)


##3
set.seed(234)
df3<-mydata[sample(nrow(mydata), 10000), ]

training <- sample(1:10000, 2500)
testing <- seq(1:10000)[-training]

df3.testing <- df3[testing,"popularity"]


rf3 <- randomForest(popularity ~ .,data=df3 ,mtry = 3,subset = training,importance=TRUE)
yhat.rf3 <- predict(rf3,df3[testing,])
mean((yhat.rf3-df3.testing)^2)
print(rf3)


##4
set.seed(345)
df4<-mydata[sample(nrow(mydata), 10000), ]

training <- sample(1:10000, 2500)
testing <- seq(1:10000)[-training]

df4.testing <- df4[testing,"popularity"]


rf4 <- randomForest(popularity ~ .,data=df4 ,mtry = 3,subset = training,importance=TRUE)
yhat.rf4 <- predict(rf4,df4[testing,])
mean((yhat.rf4-df4.testing)^2)
print(rf4)


##5
set.seed(456)
df5<-mydata[sample(nrow(mydata), 10000), ]

training <- sample(1:10000, 2500)
testing <- seq(1:10000)[-training]

df5.testing <- df5[testing,"popularity"]


rf5 <- randomForest(popularity ~ .,data=df5 ,mtry = 3,subset = training,importance=TRUE)
yhat.rf5 <- predict(rf5,df5[testing,])
mean((yhat.rf5-df5.testing)^2)
print(rf5)


##6
set.seed(1234)
df6<-mydata[sample(nrow(mydata), 10000), ]

training <- sample(1:10000, 2500)
testing <- seq(1:10000)[-training]

df6.testing <- df6[testing,"popularity"]


rf6 <- randomForest(popularity ~ .,data=df6 ,mtry = 3,subset = training,importance=TRUE)
yhat.rf6 <- predict(rf6,df6[testing,])
mean((yhat.rf6-df6.testing)^2)
print(rf6)


##7
set.seed(678)
df7<-mydata[sample(nrow(mydata), 10000), ]

training <- sample(1:10000, 2500)
testing <- seq(1:10000)[-training]

df7.testing <- df7[testing,"popularity"]


rf7 <- randomForest(popularity ~ .,data=df7 ,mtry = 3,subset = training,importance=TRUE)
yhat.rf7 <- predict(rf7,df7[testing,])
mean((yhat.rf7-df7.testing)^2)
print(rf7)


##8
set.seed(6215)
df8<-mydata[sample(nrow(mydata), 10000), ]

training <- sample(1:10000, 2500)
testing <- seq(1:10000)[-training]

df8.testing <- df8[testing,"popularity"]


rf8 <- randomForest(popularity ~ .,data=df1 ,mtry = 3,subset = training,importance=TRUE)
yhat.rf8 <- predict(rf8,df8[testing,])
mean((yhat.rf8-df8.testing)^2)
print(rf8)


##9
set.seed(1111)
df9<-mydata[sample(nrow(mydata), 10000), ]

training <- sample(1:10000, 2500)
testing <- seq(1:10000)[-training]

df9.testing <- df9[testing,"popularity"]


rf9 <- randomForest(popularity ~ .,data=df9 ,mtry = 3,subset = training,importance=TRUE)
yhat.rf9 <- predict(rf9,df9[testing,])
mean((yhat.rf9-df9.testing)^2)
print(rf9)


##10
set.seed(1919)
df10<-mydata[sample(nrow(mydata), 10000), ]

training <- sample(1:10000, 2500)
testing <- seq(1:10000)[-training]

df10.testing <- df10[testing,"popularity"]


rf10 <- randomForest(popularity ~ .,data=df10 ,mtry = 3,subset = training,importance=TRUE)
yhat.rf10 <- predict(rf10,df10[testing,])
mean((yhat.rf10-df10.testing)^2)
print(rf10)


newdata=data.frame(acousticness=0.99,danceability=0.99,instrumentalness=0.99,key=7,speechiness=0.99,loudness=-1,liveness=0.99,duration_ms=180000,tempo=100)
newdata1=data.frame(acousticness=0.15,danceability=0.15,instrumentalness=0.15,key=7,speechiness=0.15,loudness=-55,liveness=0.15,duration_ms=180000,tempo=55)
newdata2=data.frame(acousticness=0.5,danceability=0.5,instrumentalness=0.85,key=7,speechiness=0.3,loudness=-10,liveness=0.2,duration_ms=180000,tempo=30)
predict(rf1,newdata )
predict(rf1,newdata1 )
predict(rf1,newdata2 )











