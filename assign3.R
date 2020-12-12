## Loading the data frame from EDA into this file
library(ggplot2)
mydata <- read.csv("df.csv")
mydata$mode <- NULL
mydata$valence <- NULL
mydata$energy <- NULL
mydata$year <- NULL

View(mydata)

attach(mydata)
## fitting the polynomial logistic regression model on all the 6 attributes
ggplot(mydata, aes(x=speechiness, y=popularity, color = speechiness)) + 
  geom_point()+  ggtitle("Speechiness") + theme(plot.title = element_text(hjust = 0.5))

fitspeech<- glm(I(popularity>30)~poly(speechiness,4))



ggplot(mydata, aes(as.factor(danceability), popularity, group=popularity)) +
  geom_tile(aes(fill = danceability)) +
  scale_fill_gradient(low = "white", high = "red") 

fitdance<- glm(I(popularity>30)~poly(danceability,4))

ggplot(mydata, aes(as.factor(liveness), popularity, group=popularity)) +
  geom_tile(aes(fill = liveness)) +
  scale_fill_gradient(low = "white", high = "red") 

fitlive <- glm(I(popularity>30)~poly(liveness,4))

ggplot(mydata, aes(as.factor(loudness), popularity, group=popularity)) +
  geom_tile(aes(fill = loudness)) +
  scale_fill_gradient(low = "white", high = "red")

fitloud <- glm(I(popularity)>30~poly(loudness,4))

ggplot(mydata, aes(as.factor(acousticness), popularity, group=popularity)) +
  geom_tile(aes(fill = acousticness)) +
  scale_fill_gradient(low = "white", high = "red")

fitacoustic <- glm(I(popularity>30)~poly(acousticness,4))

ggplot(mydata, aes(x=as.factor(mode), y=popularity))+ geom_boxplot(fill="slateblue", alpha=0.2) +xlab("cyl")

fitmode <- glm(I(popularity>30)~poly(mode,1))


ggplot(mydata, aes(as.factor(instrumentalness), popularity, group=popularity)) +
  geom_tile(aes(fill = instrumentalness)) +
  scale_fill_gradient(low = "white", high = "red")

fitinstrumental <- glm(I(popularity>30)~poly(instrumentalness,4))

ggplot(mydata, aes(as.factor(duration_ms), popularity, group=popularity)) +
  geom_tile(aes(fill = duration_ms)) +
  scale_fill_gradient(low = "white", high = "red")

fitduration <- glm(I(popularity>25)~poly(duration_ms,4))

ggplot(mydata, aes(as.factor(tempo), popularity, group=popularity)) +
  geom_tile(aes(fill = tempo)) +
  scale_fill_gradient(low = "white", high = "red")

fittempo <- glm(I(popularity>30)~poly(tempo,4))

ggplot(mydata, aes(as.factor(key), popularity, group=popularity)) +
  geom_tile(aes(fill = key)) +
  scale_fill_gradient(low = "white", high = "red")

fitkey <- glm (I(popularity)>25~poly(key,4))

fitmodel <- glm(I(popularity>25)~ poly(acousticness,10)+poly(speechiness,10)
                +poly(liveness,10)+poly(loudness,10)
                +poly(instrumentalness,10)+poly(duration_ms,10)+poly(tempo,10)+poly(key,10)+poly(danceability,10))



# Define training control
set.seed(123) 
##install.packages("caret")
##install.packages("e1071")



library(boot)
library(caret)

train.control <- trainControl(method = "cv", number = 10)
# Train the model
model <- train(popularity ~., data  = mydata, method = "glm",
               trControl = train.control)
# Summarize the results
print(model)

r.square[] <- summary(fitlive)$r.squared
adj.r.square[] <- summary(fitlive)$adj.r.squared

r.square = matrix(data=NA,nrow=10,ncol=1)
ggplot(mydata, aes(x=liveness)) +
  geom_line(aes(y=r.square), color="red", size=2) + 
  geom_line(aes(y=adj.r.square), color="blue", size=2) + 
  xlab('Order of the Polynomial') +
  ylab('R^2 (red) or Adjusted R^2 (blue)') + 
  xlim(1,154417) + 
  ylim(0.5,1) 



newdata=data.frame(acousticness=0.99,danceability=0.99,instrumentalness=0.99,key=7,speechiness=0.99,loudness=-1,liveness=0.99,duration_ms=180000,tempo=100)
newdata1=data.frame(acousticness=0.15,danceability=0.15,instrumentalness=0.15,key=7,speechiness=0.15,loudness=-55,liveness=0.15,duration_ms=180000,tempo=55)
newdata2=data.frame(acousticness=0.5,danceability=0.5,instrumentalness=0.85,key=7,speechiness=0.3,loudness=-10,liveness=0.2,duration_ms=180000,tempo=30)

predict(fitmodel,newdata )
predict(fitmodel,newdata1 )
predict(fitmodel,newdata2 )
