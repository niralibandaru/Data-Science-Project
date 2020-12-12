## GROUP NAME: SPOTIFY 2
## GROUP MEMBERS: NIRALI BANDARU AND ROHAN GANGISETTY
## DUE DATE: OCTOBER 11, 2020
## PURPOSE: Exploratory Data Analysis of Spotify data set-Group Assignment 2

########LOAD DATA############

library(tidyverse)
library(ggplot2)

mydata <- read.csv("data.csv")
View(mydata)
tab <- summary(mydata)
tab
summary(mydata$acousticness)


########DATA CLEANING#########
##Nirali's work##
#Column with artist names is messy due to accents.Fixed with the following line:
Encoding(mydata$artists) <- "UTF-8"

View(mydata)

#Check for missing values

variables <- c(mydata$acousticness, mydata$artists, mydata$danceability,
               mydata$duration_ms, mydata$energy,
               mydata$explicit, mydata$id, mydata$instrumentalness,
               mydata$key, mydata$liveness, mydata$loudness,
               mydata$mode, mydata$name, mydata$popularity,
               mydata$release_date, mydata$speechiness, mydata$tempo,
               mydata$valence, mydata$year)
df <- data.frame(mydata)

is.na(df)

#sum(is.na(df$col))

sum(is.na(df)) ##Returns 0 - indicating there are no missing values

#Checking column-by-column
sum(is.nan(mydata$acousticness)) ##Returns 0 - indicating there are no NaN values in data
sum(is.nan(mydata$artists))
sum(is.nan(mydata$danceability))
sum(is.nan(mydata$duration_ms))
sum(is.nan(mydata$energy))
sum(is.nan(mydata$explicit))
sum(is.nan(mydata$id))
sum(is.nan(mydata$key))
sum(is.nan(mydata$liveness))
sum(is.nan(mydata$loudness))
sum(is.nan(mydata$mode))
sum(is.nan(mydata$name))
sum(is.nan(mydata$popularity))
sum(is.nan(mydata$release_date))
sum(is.nan(mydata$speechiness))
sum(is.nan(mydata$tempo))
sum(is.nan(mydata$valence))
sum(is.nan(mydata$year))

sum(is.na(mydata$acousticness)) ##Returns 0 - indicating there are no NA values
sum(is.na(mydata$artists))
sum(is.na(mydata$danceability))
sum(is.na(mydata$duration_ms))
sum(is.na(mydata$energy))
sum(is.na(mydata$explicit))
sum(is.na(mydata$id))
sum(is.na(mydata$key))
sum(is.na(mydata$liveness))
sum(is.na(mydata$loudness))
sum(is.na(mydata$mode))
sum(is.na(mydata$name))
sum(is.na(mydata$popularity))
sum(is.na(mydata$release_date))
sum(is.na(mydata$speechiness))
sum(is.na(mydata$tempo))
sum(is.na(mydata$valence))
sum(is.na(mydata$year))

##Rohan's work##
########FINDING UNIQUE SONG NAMES############
length(unique(mydata$name)) ##returns number of unique song names

########REMOVE UNWANTED COLUMNS############
df$artists <- NULL
df$release_date <- NULL
df$explicit <- NULL
df$id <- NULL
df$name <- NULL

####CHECKING DATA TYPES AND RANGES####
summary(mydata)

####REMOVING ILLEGAL VALUES####
table(df$loudness > 0) #gives the number of false values(which do not satisfy the range)
df<-df[!(df$loudness>0),]
df<-df[!(df$duration_ms<60000),]
df<-df[!(df$duration_ms>360000),]
df <- df %>% distinct()  #checks for duplicate rows, keeps the first observation and deletes remaining obs


###NIRALI'S WORK#######

## Question 2 ##

attach(df)
#Visual representation of the response variable - Popularity

#looking at the data frame in order of popularity
View(df)
df1 <- sort(df$popularity, decreasing = TRUE)
View(df1)

#numerical summary of response variable
summary(df$popularity)
mean(df$popularity)

#make a histogram mapping the frequency of popularity index

ggplot(df, aes(x=popularity)) + geom_histogram(bins = 100, color = "blue", fill="grey") + 
  geom_vline(aes(xintercept = mean(popularity)), linetype = "dashed", size = 0.5) +
  ggtitle("Histogram Plot")+theme(plot.title = element_text(hjust = 0.5))


#make boxplot of popularity

ggplot(df, aes(x=popularity)) + geom_boxplot() + ggtitle("Boxplot") + theme(plot.title = element_text(hjust = 0.5))

#install.packages("ggpubr")

library(ggpubr)

#Plotting the density distribution of popularity index

ggdensity(df, x = "popularity",
          fill = "#0073C2FF", color = "#0073C2FF",
          add = "mean", rug = TRUE) + ggtitle("Density Plot") + theme(plot.title = element_text(hjust = 0.5))

## QUESTION 4##

##Predictors used in this question: Danceability, Speechiness and Loudness##

library(ggcorrplot)
library(dplyr)
#install.packages("ggcorrplot")

#Danceability vs. Popularity

#1 Scatterplot
ggplot(df, aes(x=danceability, y = popularity, color = danceability)) + geom_point() +
  ggtitle("Danceability") + theme(plot.title = element_text(hjust = 0.5))

#Loudness vs. Popularity

ggplot(df, aes(x=loudness, y=popularity, color = loudness)) + geom_point()+
  ggtitle("Loudness") + theme(plot.title = element_text(hjust = 0.5))

#Speechiness vs. Popularity

ggplot(df, aes(x=speechiness, y=popularity, color = speechiness)) + geom_point()+
  ggtitle("Speechiness") + theme(plot.title = element_text(hjust = 0.5))

#####Other Predictors#####

#Liveness vs. Popularity

ggplot(df, aes(x=liveness, y=popularity, color = liveness)) + geom_point()+
  ggtitle("Liveness") + theme(plot.title = element_text(hjust = 0.5))

#Acousticness vs. Popularity

ggplot(df, aes(x=acousticness, y=popularity, color = acousticness)) + geom_point()+
  ggtitle("Acousticness") + theme(plot.title = element_text(hjust = 0.5))

#Mode vs. Popularity

ggplot(df, aes(x=mode, y=popularity, color = mode)) + geom_point()+
  ggtitle("Mode") + theme(plot.title = element_text(hjust = 0.5))
  #Above plot does not provide any information about the distribution

  #Group by major scale and minor scale
group_by(df, mode) %>%
  summarise(
    count = n(),
  )
  #plotting mode by major and minor groups
ggboxplot(df, x = "mode", y = "popularity", 
          color = "mode", palette = c("#00AFBB", "#E7B800"),
          order = c("Minor", "Major"),
          ylab = "popularity", xlab = "Mode") + ggtitle("Mode") + 
          theme(plot.title = element_text(hjust = 0.5))

#Key vs. Popularity

ggplot(df, aes(x=key, y=popularity, color = key)) + geom_point()+
  ggtitle("Key") + theme(plot.title = element_text(hjust = 0.5))
#Group by major scale and minor scale
group_by(df, key) %>%
  summarise(
    count = n(),
  )
  #plot the frequency of songs in different scales
key_data_x <- c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)
key_data_y <- c(19474, 11730, 17071, 6523, 11786, 14625, 7937, 18929, 9767,
                16133, 10790, 9652)
key_df <- data.frame(key_data_x,key_data_y)

myplot <- ggplot(key_df, aes(x=key_data_x, y=key_data_y)) + 
  geom_point() + geom_line() +
  ggtitle("Key") + theme(plot.title = element_text(hjust = 0.5))
print(myplot + labs(x = "Key", y = "Frequency"))

  #plotting mode by key groups
ggboxplot(df, x = "key", y = "popularity", 
          color = "key",
          order = c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11"),
          ylab = "Popularity", xlab = "Mode") + ggtitle("Key") + 
          theme(plot.title = element_text(hjust = 0.5))
  
#Instrumentalness vs. Popularity

ggplot(df, aes(x=instrumentalness, y=popularity, color = instrumentalness)) + geom_point()+
  ggtitle("Instrumentalness") + theme(plot.title = element_text(hjust = 0.5))

#Duration vs. Popularity

ggplot(df, aes(x=duration_ms, y=popularity, color = duration_ms)) + geom_point()+
  ggtitle("Duration") + theme(plot.title = element_text(hjust = 0.5))

#Tempo vs. Popularity

ggplot(df, aes(x=tempo, y=popularity, color = tempo)) + geom_point()+
  ggtitle("Tempo") + theme(plot.title = element_text(hjust = 0.5))






