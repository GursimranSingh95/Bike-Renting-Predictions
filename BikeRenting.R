#Get Working Directory
getwd()

#set working Directory
setwd("F:/")

#Removing all the stored objects
rm(list = ls())

#Installing required packages. 
install.packages("randomForest")
install.packages('usdm')
install.packages("rpart")
install.packages("tree")
install.packages("party")
install.packages("h20")
install.packages("mlr")
install.packages("e1071")
install.packages("MASS")
install.packages("RColorBrewer")
install.packages("rpart.plot")
install.packages("ggplot2")
install.packages("corrplot")
install.packages("fBasics")
install.packages("tidyverse")
install.packages("dplyr")
install.packages("plyr")
install.packages("scales")
install.packages("magritter")
install.packages("ggpubr")
install.packages("lattice")
install.packages("devtools")
install.packages("vcd")
install.packages("corrgram")
install.packages("caret")
install.packages("randomForest")

#Loading Libraries.
library(corrgram)
library(e1071)
library(party)
library(usdm)
library(xlsx)
library(randomForest)
library(caret)
library(RColorBrewer)
library(rpart.plot)
library(randomForest)
library(rpart)
library(tree)
library(MASS)
library(corrplot)
library(magrittr)
library(randomForest)
library(devtools)
library(ggplot2)
library(scales)
library(vcd)
library(lattice)
library(fBasics)
library(data.table)
library(ggpubr)
library(tidyverse)
library(dplyr)
library(plyr)

#Reading Data from CSV file wih Header = True because first row contains column names.
BikeData <- read.csv("day.csv",header=T)

#Viewing First six rows of dataset
head(BikeData)
#All the Categorical variables are already encoded.So we doesn't need to do that operation at later stages.

#Renaming Variables to get better undersatnding.
names(BikeData)[names(BikeData) == 'instant'] <- 'Index'
names(BikeData)[names(BikeData) == 'dteday'] <- 'Date'
names(BikeData)[names(BikeData) == 'season'] <- 'Season'
names(BikeData)[names(BikeData) == 'yr'] <- 'Year'
names(BikeData)[names(BikeData) == 'mnth'] <- 'Month'
names(BikeData)[names(BikeData) == 'holiday'] <- 'Holiday'
names(BikeData)[names(BikeData) == 'weekday'] <- 'WeekDay'
names(BikeData)[names(BikeData) == 'workingday'] <- 'WorkingDay'
names(BikeData)[names(BikeData) == 'weathersit'] <- 'DayWeather'
names(BikeData)[names(BikeData) == 'temp'] <- 'Temperature'
names(BikeData)[names(BikeData) == 'atemp'] <- 'FeelingTemperature'
names(BikeData)[names(BikeData) == 'hum'] <- 'Humidity'
names(BikeData)[names(BikeData) == 'windspeed'] <- 'WindSpeed'
names(BikeData)[names(BikeData) == 'casual'] <- 'CasualUsers'
names(BikeData)[names(BikeData) == 'registered'] <- 'RegisteredUsers'
names(BikeData)[names(BikeData) == 'cnt'] <- 'RentedBikes'

#Viewing first Six rows of dataset after renaming variables.
head(BikeData)

#Viewing Summary of a dataset
summary(BikeData)

#Viewing structure of a sataset
str(BikeData)

#Categorical Variables = Month, Season, Holiday, WorkingDay, WeekDay, DayWeather
#Contineous Variables = WindSpeed, Temperature, FeelingTemperature, Humidity, CasualUsers, RegisteredUsers.
#Target Variables = RentedBikes. Its is contineous variable.

#Viewing Detailed Summary of Variables
basicStats(BikeData$Season)
basicStats(BikeData$Year)
basicStats(BikeData$Month)
basicStats(BikeData$Holiday)
basicStats(BikeData$`WeekDay`)
basicStats(BikeData$`WorkingDay`)
basicStats(BikeData$`DayWeather`)
basicStats(BikeData$`Temperature`)
basicStats(BikeData$`FeelingTemperature`)
basicStats(BikeData$`Humidity`)
basicStats(BikeData$`WindSpeed`)
basicStats(BikeData$`CasualUsers`)
basicStats(BikeData$RegisteredUsers)
basicStats(BikeData$RentedBikes)


#Checking Missing Values in the DataSet
sapply(BikeData, function(x) sum(is.na(x)))
#There is no Missing Value in the above DataSet

#Viewing Structure of the DataSet
str(BikeData)

#Analyzing Data using Visualizations

#Univariate Analysis

#Analyzing Distribution of Target Variable that is 'Rented Bikes'.
ggplot(BikeData,aes(x=BikeData$RentedBikes))+geom_histogram(aes(y=..density..),
                bandwidth=5,fill='cornsilk',colour="red")+
                geom_density()+theme_bw()+xlab("Total Count of Rented Bikes")+ylab("Frequency")+
                ggtitle("Rented Bikes")+theme(text=element_text(size=10))
#From the Distribution of Target Variable we came to knew that Data is Normally Distributed

#Analyzing Distribution of Independent Variable that is 'Temperature'.
ggplot(BikeData,aes(x=BikeData$Temperature))+geom_histogram(aes(y=..density..),
                    bandwidth=5,fill='cornsilk',colour="red")+
                    geom_density()+theme_bw()+xlab("Normalized Temperature in Celsius")+
                    ylab("Frequency")+ggtitle("Normalized Temperature")+theme(text=element_text(size=10))
#From the Distribution of 'Temperature' attribute I came to knew that Data is not Normally Distributed 
#and it is a Bimodal Distribution.

#Analyzing Distribution of Independent Variable that is 'Feeling Temperature'.
ggplot(BikeData,aes(x=BikeData$`FeelingTemperature`))+geom_histogram(aes(y=..density..),
                    bandwidth=5,fill='cornsilk',colour="red")+geom_density()+theme_bw()+
                    xlab("Normalized Feeling Temperature in Celsius")+
                    ylab("Frequency")+ggtitle("Normalized Feeling Temperature")+theme(text=element_text(size=10))
#From the Distribution of 'Feeling Temperature' Attribute I came to knew that Data is not Normally Distributed 
#and it is a Bimodal Distribution.

#Analyzing Distribution of Independent Variable that is 'Humidity'.
ggplot(BikeData,aes(x=BikeData$Humidity))+geom_histogram(aes(y=..density..),
                    bandwidth=5,fill='cornsilk',colour="red")+geom_density()+theme_bw()+
                    xlab("Normalized Humidity")+ylab("Frequency")+ggtitle("Normalized Humidity")+
                    theme(text=element_text(size=10))
#From the Distribution of 'Humidity' Attribute I came to knew that Data is not Normally Distributed 
#and it is a Unimodal Distribution in which data is Right Skewed.

#Analyzing Distribution of Independent Variable that is 'Wind Speed'.
ggplot(BikeData,aes(x=BikeData$WindSpeed))+geom_histogram(aes(y=..density..),
                    bandwidth=5,fill='cornsilk',colour="red")+geom_density()+theme_bw()+
                    xlab("Normalized Wind Speed")+ylab("Frequency")+ggtitle("Normalized Wind Speed")+
                    theme(text=element_text(size=10))
#From the Distribution of 'Wind Speed' Attribute I came to knew that Data is not Normally Distributed 
#and it is a Unimodal Distribution in which data is Left Skewed.

#Analyzing Distribution of Independent Variable that is 'Casual Users'.
ggplot(BikeData,aes(x=BikeData$CasualUsers))+geom_histogram(aes(y=..density..),
                    bandwidth=5,fill='cornsilk',colour="red")+geom_density()+theme_bw()+
                    xlab("Count of Casual Users")+ylab("Frequency")+ggtitle("Casual Users")+
                    theme(text=element_text(size=10))
#From the Distribution of 'Wind Speed' Attribute I came to knew that Data is not Normally Distributed 
#and it is a Bnimodal Distribution in which data is Left Skewed.

#Analyzing Distribution of Independent Variable that is 'Registered Users'.
ggplot(BikeData,aes(x=BikeData$RegisteredUsers))+geom_histogram(aes(y=..density..),
                   bandwidth=5,fill='cornsilk',colour="red")+geom_density()+theme_bw()+
                   xlab("Count of Registered Users")+ylab("Frequency")+ggtitle("Registered Users")+
                   theme(text=element_text(size=10))
#From the Distribution of 'Target Variables' I came to knew that Data is Normally Distributed

#Making Copy to make changes into it for batter visuaization and understanding of categorical data.
BikeData1 = BikeData

#Frequency Table for Categorical Variabe that is 'Season'
count(BikeData1,'Season')
#Converting Season to categorical/binning
BikeData1$Season[BikeData1$Season==1]="Springer"
BikeData1$Season[BikeData1$Season==2]="Summer"
BikeData1$Season[BikeData1$Season==3]="Fall"
BikeData1$Season[BikeData1$Season==4]="Winter"
#Frequency Table for Categorical Variabe that is 'Season' after binning process
count(BikeData1,'Season')
#Now we are getting clear picture of values.

#Frequency Table for Categorical Variabe that is 'Season'
count(BikeData1,'Month')
#Converting Month to categorical/binning
BikeData1$Month[BikeData1$Month==1]="January"
BikeData1$Month[BikeData1$Month==2]="February"
BikeData1$Month[BikeData1$Month==3]="March"
BikeData1$Month[BikeData1$Month==4]="April"
BikeData1$Month[BikeData1$Month==5]="May"
BikeData1$Month[BikeData1$Month==6]="June"
BikeData1$Month[BikeData1$Month==7]="July"
BikeData1$Month[BikeData1$Month==8]="August"
BikeData1$Month[BikeData1$Month==9]="September"
BikeData1$Month[BikeData1$Month==10]="October"
BikeData1$Month[BikeData1$Month==11]="November"
BikeData1$Month[BikeData1$Month==12]="December"
#Frequency Table for Categorical Variabe that is 'Month'
count(BikeData1,'Month')

#Frequency Table for Categorical Variabe that is 'Year'
count(BikeData1,'Year')
#Converting Year to categorical/binning
BikeData1$Year[BikeData1$Year==0]="2011"
BikeData1$Year[BikeData1$Year==1]="2012"
#Frequency Table for Categorical Variabe that is 'Year'
count(BikeData1,'Year')

#Frequency Table for Categorical Variabe that is 'Holiday'
count(BikeData1,'Holiday')
#Converting Year to categorical/binning
BikeData1$Holiday[BikeData1$Holiday==0]="Not Holiday"
BikeData1$Holiday[BikeData1$Holiday==1]="Holiday"
#Frequency Table for Categorical Variabe that is 'Holiday'
count(BikeData1,'Holiday')

#Frequency Table for Categorical Variabe that is 'Day Weather'
count(BikeData1$DayWeather)
#Converting Day weather to categorical/binning
BikeData1$DayWeather[BikeData1$DayWeather==1]="Clear, Few Clouds,Partly Cloudy, Partly Cloudy"
BikeData1$DayWeather[BikeData1$DayWeather==2]="Mist + Cloudy, Mist + Broken Clouds,Mist + Few clouds, Mist"
BikeData1$DayWeather[BikeData1$DayWeather==3]="Light Snow,Light Rain + Thunderstorm + Scattered clouds,Light Rain + Scattered Clouds"
#Frequency Table for Categorical Variabe that is 'Day Weather'
count(BikeData1$DayWeather)

#Frequency Table for Categorical Variabe that is 'Week Day'
count(BikeData1$WeekDay)
#Converting Week Day to categorical/binning
BikeData1$WeekDay[BikeData1$WeekDay==0]="Saturday"
BikeData1$WeekDay[BikeData1$WeekDay==1]="Sunday"
BikeData1$WeekDay[BikeData1$WeekDay==2]="Monday"
BikeData1$WeekDay[BikeData1$WeekDay==3]="Tuesday"
BikeData1$WeekDay[BikeData1$WeekDay==4]="Wednesday"
BikeData1$WeekDay[BikeData1$WeekDay==5]="Thursday"
BikeData1$WeekDay[BikeData1$WeekDay==6]="Friday"
#Frequency Table for Categorical Variabe that is 'Holiday'
count(BikeData1,'WeekDay')

#Frequency Table for Categorical Variabe that is 'Working Day'
count(BikeData1$WorkingDay)
#Converting Working Day to categorical/binning
BikeData1$WorkingDay[BikeData1$WorkingDay==1]="Non Holiday (Neither Weekend nor Holiday"
BikeData1$WorkingDay[BikeData1$WorkingDay==0]="Holiday (Either Weekend or Holiday"
#Frequency Table for Categorical Variabe that is 'Working Day'
count(BikeData1$WorkingDay)

# Code to view Colours in R
grDevices::colors() 

#Plotting Categorical Variable that is 'Season' Using Bar Graph and Pie Chart
#Bar Graph
ggplot(BikeData1,aes(x=factor(Season),fill=factor(Season)))+geom_bar()+theme_classic()+
      xlab("Season")+ylab("Count")+labs(fill="Season")+geom_text(aes(label=..count..),
      stat="count",position=position_stack(1.05))+ggtitle('Seasons')

#Pie Chart
ggplot(BikeData1,aes(x = "", fill = factor(Season)))+geom_bar(width = 1, size = 1, color = "black")+
      theme_classic()+theme(axis.line = element_blank(),axis.text = element_blank(),axis.ticks = element_blank(),
      plot.title = element_text(hjust = 0.5, color = "#666666"))+coord_polar(theta = "y")+
      scale_x_discrete("")+labs(x = NULL, y = NULL, fill = NULL,title = "Season")+
      guides(fill = guide_legend(reverse = TRUE))+geom_text(aes(label=..count..),
      stat="count",position=position_stack(0.5))
#From the Bar Graph I came to knew that thre is very less difference among frequencies of season.
#Highest Count is of Fall Season.
#Lowest Count is of Winter Season.

#Plotting Categorical Variable that is 'Holiday' Using Bar Graph and Pi Chart.
#Bar Grapgh
ggplot(BikeData1,aes(x=factor(Holiday),fill=factor(Holiday)))+geom_bar()+theme_classic()+
      xlab("Holiday Schedule")+ylab("Count")+labs(fill="Holiday Schedule")+ggtitle("Holiday Schedule")+
      geom_text(aes(label=..count..),stat="count",position=position_stack(1.05))
#Pie Chart
ggplot(BikeData1,aes(x = "", fill = factor(Holiday)))+geom_bar(width = 1, size = 1, color = "black")+
      theme_classic()+theme(axis.line = element_blank(),axis.text = element_blank(),axis.ticks = element_blank(),
      plot.title = element_text(hjust = 0.5, color = "#666666"))+coord_polar(theta = "y")+
      scale_x_discrete("")+labs(x = NULL, y = NULL, fill = NULL,title = "Holiday Schedule")+
      guides(fill = guide_legend(reverse = TRUE))+geom_text(aes(label=..count..),
      stat="count",position=position_stack(0.5))
#From the Bar Grapgh, I came to knew that Difference of Non Holidays and Holidays is Very High.

#Plotting Categorical Variable that is 'Month' Using Bar Graph and Pi Chart.
#Bar Grapgh
ggplot(BikeData1,aes(x=factor(Month),fill=factor(Month)))+geom_bar()+theme_classic()+
  xlab("Mnths")+ylab("Count")+labs(fill="Months")+ggtitle("Month")+
  geom_text(aes(label=..count..),stat="count",position=position_stack(1.05))

#Pie Chart
ggplot(BikeData1,aes(x = "", fill = factor(Month)))+geom_bar(width = 1, size = 1, color = "black")+
      theme_classic()+theme(axis.line = element_blank(),axis.text = element_blank(),axis.ticks = element_blank(),
      plot.title = element_text(hjust = 0.5, color = "#666666"))+coord_polar(theta = "y")+
      scale_x_discrete("")+labs(x = NULL, y = NULL, fill = NULL,title = "Month")+guides(fill = guide_legend(reverse = FALSE))+geom_text(aes(label=..count..),
      stat="count",position=position_stack(0.5))

#This grapgh is displaying total number of days in the data set(Month wise).

#Plotting Categorical Variable that is 'Week Day' Using Bar Graph
#Bar Grapgh
ggplot(BikeData1,aes(x=factor(WeekDay),fill=factor(WeekDay)))+geom_bar()+theme_classic()+
      xlab("Day of the Week")+ylab("Count")+labs(fill="Months")+ggtitle("Days")+
      geom_text(aes(label=..count..),stat="count",position=position_stack(1.05))

#Pie Chart
ggplot(BikeData1,aes(x = "", fill = factor(WeekDay)))+geom_bar(width = 1, size = 1, color = "black")+
       theme_classic()+theme(axis.line = element_blank(),axis.text = element_blank(),axis.ticks = element_blank(),
       plot.title = element_text(hjust = 0.5, color = "#666666"))+coord_polar(theta = "y")+
       scale_x_discrete("")+labs(x = NULL, y = NULL, fill = NULL,title = "Week Days")+
       guides(fill = guide_legend(reverse = FALSE))+geom_text(aes(label=..count..),
      stat="count",position=position_stack(0.5))

#This grapgh is displaying total number of days in the data set(Week wise).

#Plotting Categorical Variable that is 'Day Weather' Using Bar Graph

#Bar Grapgh
ggplot(BikeData1,aes(x=factor(DayWeather),fill=factor(DayWeather)))+geom_bar()+theme_classic()+
      xlab("Weather")+ylab("Count")+labs(fill="Weather")+ggtitle('Weather')
      geom_text(aes(label=..count..),stat="count",position=position_stack(1.05))+
      theme(axis.text.x=element_blank(),axis.ticks.x=element_blank())
#Pie Chart
ggplot(BikeData1,aes(x = "", fill = factor(DayWeather)))+geom_bar(width = 1, size = 1, color = "black")+
       theme_classic()+theme(axis.line = element_blank(),axis.text = element_blank(),axis.ticks = element_blank(),
       plot.title = element_text(hjust = 0.5, color = "#666666"))+coord_polar(theta = "y")+
       scale_x_discrete("")+labs(x = NULL, y = NULL, fill = NULL,title = "Weather")+
       guides(fill = guide_legend(reverse = TRUE))+geom_text(aes(label=..count..),
       stat="count",position=position_stack(0.5))

#From the above grapgh it is clear that most of the time weather is Pleasent.

#Plotting Categorical Variable that is 'Working Days' Using Bar Graph
#Bar Grapgh
ggplot(BikeData1,aes(x=factor(WorkingDay),fill=factor(WorkingDay)))+geom_bar()+theme_classic()+
  xlab("Working Schedule")+ylab("Count")+labs(fill="Working Schedule")+ggtitle("Working Schedule")+
  theme(axis.text.x=element_blank(),axis.ticks.x=element_blank())
#Pie Chart
ggplot(BikeData1,aes(x = "", fill = factor(WorkingDay)))+geom_bar(width = 1, size = 1, color = "black")+
      theme_classic()+theme(axis.line = element_blank(),axis.text = element_blank(),axis.ticks = element_blank(),
      plot.title = element_text(hjust = 0.5, color = "#666666"))+coord_polar(theta = "y")+
      scale_x_discrete("")+labs(x = NULL, y = NULL, fill = NULL,title = "Working Schedule")+
      guides(fill = guide_legend(reverse = TRUE))+geom_text(aes(label=..count..),
      stat="count",position=position_stack(0.5))
#From this graph I am came to knew that Working Days are more as compared to Non Working Days.

#Plotting Categorical Variable that is 'Year' Using Bar Graph
#Bar Grapgh
ggplot(BikeData1,aes(x=factor(Year),fill=factor(Year)))+geom_bar()+theme_classic()+
  xlab("Year")+ylab("Count")+labs(fill="Year")+ggtitle('Year')+
  geom_text(aes(label=..count..),stat="count",position=position_stack(1.05))
#Pie Chart
ggplot(BikeData1,aes(x = "", fill = factor(Year)))+geom_bar(width = 1, size = 1, color = "black")+
      theme_classic()+theme(axis.line = element_blank(),axis.text = element_blank(),axis.ticks = element_blank(),
      plot.title = element_text(hjust = 0.5, color = "#666666"))+coord_polar(theta = "y")+
      scale_x_discrete("")+labs(x = NULL, y = NULL, fill = NULL,title = "Year")+
      guides(fill = guide_legend(reverse = TRUE))+geom_text(aes(label=..count..),
      stat="count",position=position_stack(0.5))
#Above graph is showing the number of days in a year. Year 2012 was LEAP year as it has 366 days.

#Bi-variate Analysis
#Analyzing Numerical attributes

#checking the relationship between 'Temperature' and 'Feeling Temperature' variable
ggplot(BikeData,aes(x=BikeData$Temperature,y=BikeData$FeelingTemperature))+geom_point(col='red')+
  ggtitle("Normalized Temperature In Celsius V/S Normalized Feeling Temperature In Celsius")+
  geom_smooth(method=lm, se=FALSE,col='black') + xlab("Normalized Temperature In Celsius")+
  ylab("Normalized Feeling Temperature In Celsius")+stat_cor(method = "pearson", label.x = 0.5, label.y =0.9)
  
#From the Above visualization I came to knew that both variables are "HIGHLY CORRELATED".

#checking the relationship between 'Temperature' and 'Humidity' variable
ggplot(BikeData,aes(x=BikeData$Temperature,y=BikeData$Humidity))+geom_point(col='red')+
  ggtitle("Normalized Temperature In Celsius V/S Normalized Humidity")+
  geom_smooth(method=lm, se=FALSE,col='black') + xlab("Normalized Temperature In Celsius")+
  ylab("Normalized Humidity")+stat_cor(method = "pearson", label.x = 0.5, label.y =0.0)
#From the Above visualization I came to knew that both variables are "POSITIVELY CORRELATED".

#checking the relationship between 'Temperature' and 'Wind Speed' variable
ggplot(BikeData,aes(x=BikeData$Temperature,y=BikeData$WindSpeed))+geom_point(col='red')+
      ggtitle("Normalized Temperature In Celsius V/S Normalized Wind Speed")+
      geom_smooth(method=lm, se=FALSE,col='black')+
      xlab("Normalized Temperature In Celsius")+
      ylab("Normalized Wind Speed")+stat_cor(method = "pearson", label.x = 0.5, label.y =0.0)
#From the Above visualization I came to knew that both variables are "NEGATIVELY CORRELATED"

#checking the relationship between 'Temperature' and 'Casual Users' variable
ggplot(BikeData,aes(x=BikeData$Temperature,y=BikeData$CasualUsers))+geom_point(col='red')+
  ggtitle("Normalized Temperature In Celsius V/S Casual Users")+
  geom_smooth(method=lm, se=FALSE,col='black')+
  xlab("Normalized Temperature In Celsius")+
  ylab("Casual Users")+stat_cor(method = "pearson", label.x = 0.5, label.y =0.0)
#From the Above visualization I came to knew that both variables are "POSITIVELY CORRELATED"

#checking the relationship between 'Temperature' and 'Registered Users' variable
ggplot(BikeData,aes(x=BikeData$Temperature,y=BikeData$RegisteredUsers))+geom_point(col='red')+
  ggtitle("Normalized Temperature In Celsius V/S Registered Users")+
  geom_smooth(method=lm, se=FALSE,col='black')+
  xlab("Normalized Temperature In Celsius")+
  ylab("Registered Users")+stat_cor(method = "pearson", label.x = 0.5, label.y =0.0)
#From the Above visualization I came to knew that both variables are "POSITIVELY CORRELATED"

#checking the relationship between 'Temperature' and 'Rented Bikes(Target Variable)' variable
ggplot(BikeData,aes(x=BikeData$Temperature,y=BikeData$RentedBikes))+geom_point(col='red')+
  ggtitle("Normalized Temperature In Celsius V/S Rented Bikes")+
  geom_smooth(method=lm, se=FALSE,col='black')+
  xlab("Normalized Temperature In Celsius")+
  ylab("Rented Bikes")+stat_cor(method = "pearson", label.x = 0.5, label.y =0.0)
#From the Above visualization I came to knew that both variables are "POSITIVELY CORRELATED"

#checking the relationship between 'Feeling Temperature' and 'Humidity' variable
ggplot(BikeData,aes(x=BikeData$FeelingTemperature,y=BikeData$Humidity))+geom_point(col='red')+
  ggtitle("Normalized Feeling Temperature In Celsius V/S Humidity")+
  geom_smooth(method=lm, se=FALSE,col='black')+
  xlab("Normalized Feeling Temperature In Celsius")+
  ylab("Humidity")+stat_cor(method = "pearson", label.x = 0.5, label.y =0.0)
#From the Above visualization I came to knew that both variables are "POSITIVELY CORRELATED"

#checking the relationship between 'Feeling Temperature' and 'Wind Speed' variable
ggplot(BikeData,aes(x=BikeData$FeelingTemperature,y=BikeData$WindSpeed))+geom_point(col='red')+
  ggtitle("Normalized Feeling Temperature In Celsius V/S Wind Speed")+
  geom_smooth(method=lm, se=FALSE,col='black')+
  xlab("Normalized Feeling Temperature In Celsius")+
  ylab("Wind Speed")+stat_cor(method = "pearson", label.x = 0.5, label.y =0.0)
#From the Above visualization I came to knew that both variables are "NEGATIVELY CORRELATED"

#checking the relationship between 'Feeling Temperature' and 'Casual Users' variable
ggplot(BikeData,aes(x=BikeData$FeelingTemperature,y=BikeData$CasualUsers))+geom_point(col='red')+
  ggtitle("Normalized Feeling Temperature In Celsius V/S Casual Users")+
  geom_smooth(method=lm, se=FALSE,col='black')+
  xlab("Normalized Feeling Temperature In Celsius")+
  ylab("Casual Users")+stat_cor(method = "pearson", label.x = 0.5, label.y =0.0)
#From the Above visualization I came to knew that both variables are "POSITIVELY CORRELATED"

#checking the relationship between 'Feeling Temperature' and 'Registered Users' variable
ggplot(BikeData,aes(x=BikeData$FeelingTemperature,y=BikeData$RegisteredUsers))+geom_point(col='red')+
  ggtitle("Normalized Feeling Temperature In Celsius V/S Registered Users")+
  geom_smooth(method=lm, se=FALSE,col='black')+
  xlab("Normalized Feeling Temperature In Celsius")+
  ylab("Registered Users")+stat_cor(method = "pearson", label.x = 0.5, label.y =0.0)
#From the Above visualization I came to knew that both variables are "POSITIVELY CORRELATED"

#checking the relationship between 'Feeling Temperature' and 'Bike Rented(target Variable)' variable
ggplot(BikeData,aes(x=BikeData$FeelingTemperature,y=BikeData$RentedBikes))+geom_point(col='red')+
  ggtitle("Normalized Feeling Temperature In Celsius V/S Rented Bikes")+
  geom_smooth(method=lm, se=FALSE,col='black')+
  xlab("Normalized Feeling Temperature In Celsius")+
  ylab("Rented Bikes")+stat_cor(method = "pearson", label.x = 0.5, label.y =0.0)
#From the Above visualization I came to knew that both variables are "POSITIVELY CORRELATED"

#checking the relationship between 'Humidity' and 'Wind Speed'
ggplot(BikeData,aes(x=BikeData$Humidity,y=BikeData$WindSpeed))+geom_point(col='red')+
      ggtitle("Normalized Humidity V/S Normalized Wind Speed")+geom_smooth(method=lm, se=FALSE,col='black')+
      xlab("Normalized Humidity")+ylab("Normalized Wind Speed")+
      stat_cor(method = "pearson", label.x = 0.30, label.y =0.50)
#From the Above visualization I came to knew that both variables are "NEGATIVELY CORRELATED"

#checking the relationship between 'Humidity' and 'Casual Users'
ggplot(BikeData,aes(x=BikeData$Humidity,y=BikeData$CasualUsers))+geom_point(col='red')+
  ggtitle("Normalized Humidity V/S Casual Users")+geom_smooth(method=lm, se=FALSE,col='black')+
  xlab("Normalized Humidity")+ylab("Casual Users")+
  stat_cor(method = "pearson", label.x = 0.30, label.y =4000)
#From the Above visualization I came to knew that both variables are "NEGATIVELY CORRELATED"

#checking the relationship between 'Humidity' and 'Registered Users'
ggplot(BikeData,aes(x=BikeData$Humidity,y=BikeData$RegisteredUsers))+geom_point(col='red')+
  ggtitle("Normalized Humidity V/S Registered Users")+geom_smooth(method=lm, se=FALSE,col='black')+
  xlab("Normalized Humidity")+ylab("Registered Users")+
  stat_cor(method = "pearson", label.x = 0.00, label.y =0.00)
#From the Above visualization I came to knew that both variables are "NEGATIVELY CORRELATED"

#checking the relationship between 'Humidity' and 'Rented Bikes(Target Variable)'
ggplot(BikeData,aes(x=BikeData$Humidity,y=BikeData$RentedBikes))+geom_point(col='red')+
  ggtitle("Normalized Humidity V/S Rented Bikes")+geom_smooth(method=lm, se=FALSE,col='black')+
  xlab("Normalized Humidity")+ylab("Rented Bikes")+
  stat_cor(method = "pearson", label.x = 0.00, label.y =0.00)
#From the Above visualization I came to knew that both variables are "NEGATIVELY CORRELATED"

#checking the relationship between 'Wind Speed' and 'Casual Users'
ggplot(BikeData,aes(x=BikeData$WindSpeed,y=BikeData$CasualUsers))+geom_point(col='red')+
  ggtitle("Wind Speed V/S Casual Users")+geom_smooth(method=lm, se=FALSE,col='black')+
  xlab("Normalized Wind Speed")+ylab("Casual Users")+
  stat_cor(method = "pearson", label.x = 0.00, label.y =3500)
#From the Above visualization I came to knew that both variables are "NEGATIVELY CORRELATED"

#checking the relationship between 'Wind Speed' and 'Registered Users'
ggplot(BikeData,aes(x=BikeData$WindSpeed,y=BikeData$RegisteredUsers))+geom_point(col='red')+
  ggtitle("Wind Speed V/S Registered Users")+geom_smooth(method=lm, se=FALSE,col='black')+
  xlab("Normalized Wind Speed")+ylab("Registered Users")+
  stat_cor(method = "pearson", label.x = 0.00, label.y =0.00)
#From the Above visualization I came to knew that both variables are "NEGATIVELY CORRELATED"

#checking the relationship between 'Wind Speed' and 'Rented Bikes (Target Variable)'
ggplot(BikeData,aes(x=BikeData$WindSpeed,y=BikeData$RentedBikes))+geom_point(col='red')+
  ggtitle("Wind Speed V/S Rented Bikes")+geom_smooth(method=lm, se=FALSE,col='black')+
  xlab("Normalized Wind Speed")+ylab("Rented Bikes")+
  stat_cor(method = "pearson", label.x = 0.00, label.y =0.0)
#From the Above visualization I came to knew that both variables are "NEGATIVELY CORRELATED"

#checking the relationship between 'Casual Users' and 'Registerd Users'
ggplot(BikeData,aes(x=BikeData$CasualUsers,y=BikeData$RegisteredUsers))+geom_point(col='red')+
  ggtitle("Casual Users V/S Registered Users")+geom_smooth(method=lm, se=FALSE,col='black')+
  xlab("Casual Users")+ylab("Registered Users")+
  stat_cor(method = "pearson", label.x = 0.00, label.y =0.0)
#From the Above visualization I came to knew that both variables are "POSITIVELY CORRELATED"

#checking the relationship between 'Casual Users' and 'Rented Bikes(Target Variable)'
ggplot(BikeData,aes(x=BikeData$CasualUsers,y=BikeData$RentedBikes))+geom_point(col='red')+
  ggtitle("Casual Users V/S Rented Bikes")+geom_smooth(method=lm, se=FALSE,col='black')+
  xlab("Casual Users")+ylab("Rented Bikes")+
  stat_cor(method = "pearson", label.x = 0.00, label.y =0.0)
#From the Above visualization I came to knew that both variables are "POSITIVELY CORRELATED"

#checking the relationship between 'Registered Users' and 'Rented Bikes(Target Variable)'
ggplot(BikeData,aes(x=BikeData$RegisteredUsers,y=BikeData$RentedBikes))+geom_point(col=c("red"))+
  ggtitle("Registered Users V/S Rented Bikes")+geom_smooth(method=lm, se=FALSE,col='black')+
  xlab("Registered Users")+ylab("Rented Bikes")+stat_cor(method = "pearson", label.x = 0.00, label.y =0.0)
#From the Above visualization I came to knew that both variables are "POSITIVELY CORRELATED"

#Checking Relationship among all Numerical attributes using Pair Plot Matrix
panel.cor <- function(x, y, digits = 2, cex.cor, ...) # FUNCTION TO CREATE PAIR PLOT MATRIX
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  # correlation coefficient
  r <- cor(x, y)
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste("r= ", txt, sep = "")
  text(0.5, 0.6, txt)
  
  # p-value calculation
  p <- cor.test(x, y)$p.value
  txt2 <- format(c(p, 0.123456789), digits = digits)[1]
  txt2 <- paste("p= ", txt2, sep = "")
  if(p<0.01) txt2 <- paste("p= ", "<0.01", sep = "")
  text(0.5, 0.4, txt2)
}
PanelData=BikeData[c(10,11,12,13,14,15,16)]
colors <- c('blue', 'red')
pairs(PanelData, upper.panel = panel.cor,col=colors)

#Biaviate Analysis for Categorical Variables

#Checking relationship between Season and Holiday
  Table1 <- table(BikeData1$Season,BikeData1$Holiday)
  #Viewing Two Table
  Table1
  #To display the joint distribution (which gives the proportion for each cell):
  prop.table(Table1)
  #Analyzing Categorical 'Table1' using BarPlot Method
  c1<-c("blue","red","black","orange")
  barplot(as.matrix(Table1),main="Season V/S Holiday Schedule",ylab="Frequency",cex.lab = 1,
          cex.main = 1,beside=TRUE, col=c1)
  legend("topleft", legend = c("Springer", "Summer","Fall","Winter"), fill = c1,ncol=1,cex=0.75)

#Checking relationship between Season and Month
  Table2 <- table(BikeData1$Season,BikeData1$Month)
  #Viewing Two Table
  Table2
  #To display the joint distribution (which gives the proportion for each cell):
  prop.table(Table2)
  #Analyzing Categorical 'Table2 ' using BarPlot Method
  c2<-c("blue","red","black","orange")
  barplot(as.matrix(Table2),main="Season V/S Month",ylab="Frequency",cex.lab = 1,
          cex.main = 1,beside=TRUE, col=c2)
  legend("topright", legend = c("Spring", "Summer","Fall","Winter"), fill = c2,ncol=1,cex=0.50)
  
#Checking relationship between Season and Week Days
  Table3 <- table(BikeData1$Season,BikeData1$WeekDay)
  #Viewing Two Table
  Table3
  #To display the joint distribution (which gives the proportion for each cell):
  prop.table(Table3)
  #Analyzing Categorical 'Table3' using BarPlot Method
  c3<-c("blue","red","black","orange")
  barplot(as.matrix(Table3),main="Season V/S Week Days",ylab="Frequency",cex.lab = 1,
          cex.main = 1,beside=TRUE, col=c3)
  legend("bottomleft", legend = c("Spring", "Summer","Fall","Winter"), fill = c3,ncol=1,cex=0.50)  
  
#Checking relationship between Season and Day Weather
  Table4 <- table(BikeData1$Season,BikeData1$DayWeather)
  #Viewing Two Table
  Table4
  #To display the joint distribution (which gives the proportion for each cell):
  prop.table(Table4)
  #Analyzing Categorical 'Table4' using BarPlot Method
  c4<-c("blue","red","black","orange")
  barplot(as.matrix(Table4),main="Season V/S Day Weather",ylab="Frequency",cex.lab = 1,
          cex.main = 1,beside=TRUE, col=c4)
  legend("topright", legend = c("Spring", "Summer","Fall","Winter"), fill = c4,ncol=1,cex=0.50)  

#Checking relationship between Season and Working Day
  Table5 <- table(BikeData1$Season,BikeData1$WorkingDay)
  #To display the joint distribution (which gives the proportion for each cell):
  prop.table(Table5)
  #Analyzing Categorical 'Table5' using BarPlot Method
  c5<-c("blue","red","black","orange")
  barplot(as.matrix(Table5),main="Season V/S Working Day",ylab="Frequency",cex.lab = 1,
          cex.main = 1,beside=TRUE, col=c5)
  legend("topleft", legend = c("Spring", "Summer","Fall","Winter"), fill = c5,ncol=1,cex=0.50)  
  
#Checking relationship between Holiday and Month
  Table6 <- table(BikeData1$Month,BikeData1$Holiday)
  #Viewing Two Table
  Table6
  #To display the joint distribution (which gives the proportion for each cell):
  prop.table(Table6)
  #Analyzing Categorical 'Table6' using BarPlot Method
  c6<-c("blue","red","green","black","orange","darkgreen","yellow","darkblue","maroon","grey","tomato","brown")
  barplot(as.matrix(Table6),main="Holiday V/S Month",ylab="Frequency",cex.lab = 1,
  cex.main = 1,beside=TRUE, col=c6)
  legend("topleft", legend = c("January","February","March","April","May","June","July","August","September","October","November","December"), 
  fill = c6,ncol=1,cex=0.50)  
  
  #Checking relationship between Holiday and Week Days
  Table7 <- table(BikeData$WeekDay,BikeData$Holiday)
  #Renaming Column Names to get better understanding of Two way table.
  colnames(Table7) <- c("Non Holiday", "Holiday")
  #Renaming Row Names to get better understanding of Two way table.
  rownames(Table7) <- c("Sunday","Monday","Tuesday","Wednuesday","Thursday","Friday","Saturday")
  #Viewing Two Table
  Table7
  #To display the joint distribution (which gives the proportion for each cell):
  prop.table(Table7)
  #Analyzing Categorical 'Table7' using BarPlot Method
  c7<-c("blue","red","green","black","orange","darkgreen","yellow")
  barplot(as.matrix(Table7),main="Holiday V/S Week Days",ylab="Frequency",cex.lab = 1,
          cex.main = 1,beside=TRUE, col=c7)
  legend("topright", legend = c("Sunday","Monday","Tuesday","Wednuesday","Thursday","Friday","Saturday"), 
         fill = c7,ncol=1,cex=0.50)  
  
#Checking relationship between Holiday and Day Weather
  Table8 <- table(BikeData$DayWeather,BikeData$Holiday)
  #Renaming Column Names to get better understanding of Two way table.
  colnames(Table8) <- c("Non Holiday", "Holiday")
  #Renaming Row Names to get better understanding of Two way table.
  rownames(Table8) <- c("Pleasent","Partly Pleasent","Non Pleasent")
  #Viewing Two Table
  Table8
  #To display the joint distribution (which gives the proportion for each cell):
  prop.table(Table8)
  #Analyzing Categorical 'Table8' using BarPlot Method
  c8<-c("blue","red","green")
  barplot(as.matrix(Table8),main="Holiday V/S Day Weather",ylab="Frequency",cex.lab = 1,
          cex.main = 1,beside=TRUE, col=c8)
  legend("topright", legend = c("Pleasent","Partly Pleasent","Non Pleasent"), 
         fill = c8,ncol=1,cex=0.50)  
  
#Checking relationship between Holiday and Working Day
  Table9 <- table(BikeData$WorkingDay,BikeData$Holiday)
  #Renaming Column Names to get better understanding of Two way table.
  colnames(Table9) <- c("Non Holiday", "Holiday")
  #Renaming Row Names to get better understanding of Two way table.
  rownames(Table9) <- c("Non Working Day","Working Day")
  #Viewing Two Table
  Table9
  #To display the joint distribution (which gives the proportion for each cell):
  prop.table(Table9)
  #Analyzing Categorical 'Table9' using BarPlot Method
  c9<-c("blue","red")
  barplot(as.matrix(Table9),main="Holiday V/S Working Day",ylab="Frequency",cex.lab = 1,
          cex.main = 1,beside=TRUE, col=c9)
  legend("topright", legend = c("Non Working Day","Working Day"), 
         fill = c9,ncol=1,cex=0.50)  
  
#Checking relationship between Month and Working Day
  Table10 <- table(BikeData$Month,BikeData$WorkingDay)
  #Renaming Column Names to get better understanding of Two way table.
  colnames(Table10) <- c("Non Working Day","Working Day")
  #Renaming Row Names to get better understanding of Two way table.
  rownames(Table10) <- c("January","February","March","April","May","June","July","August","September","October","November","December")
  #Viewing Two Table
  Table10
  #To display the joint distribution (which gives the proportion for each cell):
  prop.table(Table10)
  #Analyzing Categorical 'Table10' using BarPlot Method
  c10<-c("blue","red","green","black","orange","darkgreen","yellow","darkblue","maroon","grey","tomato","brown")
  barplot(as.matrix(Table10),main="Month V/S Working Day",ylab="Frequency",cex.lab = 1,
          cex.main = 1,beside=TRUE, col=c10)
  legend("topleft", legend = c("January","February","March","April","May","June","July","August","September","October","November","December"), 
         fill = c10,ncol=1,cex=0.35)  
  
  #Checking relationship between Month and Day Weather
  Table11 <- table(BikeData$Month,BikeData$DayWeather)
  #Renaming Column Names to get better understanding of Two way table.
  colnames(Table11) <- c("Pleasent","Partly Pleasent","Non Pleasent")
  #Renaming Row Names to get better understanding of Two way table.
  rownames(Table11) <- c("January","February","March","April","May","June","July","August","September","October","November","December")
  #Viewing Two Table
  Table11
  #To display the joint distribution (which gives the proportion for each cell):
  prop.table(Table11)
  #Analyzing Categorical 'Table11' using BarPlot Method
  c11<-c("blue","red","green","black","orange","darkgreen","yellow","darkblue","maroon","grey","tomato","brown")
  barplot(as.matrix(Table11),main="Month V/S Day Weather",ylab="Frequency",cex.lab = 1,
          cex.main = 1,beside=TRUE, col=c11)
  legend("topright", legend = c("January","February","March","April","May","June","July","August","September","October","November","December"), 
         fill = c11,ncol=1,cex=0.35)  

  #Checking relationship between Month and Week Days
  Table12 <- table(BikeData$Month,BikeData$WeekDay)
  #Renaming Column Names to get better understanding of Two way table.
  colnames(Table12) <- c("Sunday","Monday","Tuesday","Wednuesday","Thursday","Friday","Saturday")
  #Renaming Row Names to get better understanding of Two way table.
  rownames(Table12) <- c("January","February","March","April","May","June","July","August","September","October","November","December")
  #Viewing Two Table
  Table12
  #To display the joint distribution (which gives the proportion for each cell):
  prop.table(Table12)
  #Analyzing Categorical 'Table12' using BarPlot Method
  c12<-c("blue","red","green","black","orange","darkgreen","yellow","darkblue","maroon","grey","tomato","brown")
  barplot(as.matrix(Table12),main="Month V/S Week Days",ylab="Frequency",cex.lab = 1,
          cex.main = 1,beside=TRUE, col=c12)
  legend("bottomleft", legend = c("January","February","March","April","May","June","July","August","September","October","November","December"), 
         fill = c12,ncol=1,cex=0.35)    

  #Checking relationship between Week Days and Working Days
  Table13 <- table(BikeData$WeekDay,BikeData$WorkingDay)
  #Renaming Column Names to get better understanding of Two way table.
  colnames(Table13) <- c("Non Working Day","Working Day")
  #Renaming Row Names to get better understanding of Two way table.
  rownames(Table13) <- c("Sunday","Monday","Tuesday","Wednuesday","Thursday","Friday","Saturday")
  #Viewing Two Table
  Table13
  #To display the joint distribution (which gives the proportion for each cell):
  prop.table(Table13)
  #Analyzing Categorical 'Table13' using BarPlot Method
  c13<-c("blue","red","green","black","orange","darkgreen","yellow")
  barplot(as.matrix(Table13),main="Week Days V/S Working Days",ylab="Frequency",cex.lab = 1,
  cex.main = 1,beside=TRUE, col=c13)
  legend("bottomright",legend=c("Sunday","Monday","Tuesday","Wednuesday","Thursday","Friday","Saturday"),
  fill = c13,ncol=1,cex=0.3)    
  
#Checking relationship between Week Days and Day Weather
  Table14 <- table(BikeData$WeekDay,BikeData$DayWeather)
  #Renaming Column Names to get better understanding of Two way table.
  colnames(Table14) <- c("Pleasent","Partly Pleasent","Non Pleasent")
  #Renaming Row Names to get better understanding of Two way table.
  rownames(Table14) <- c("Sunday","Monday","Tuesday","Wednuesday","Thursday","Friday","Saturday")
  #Viewing Two Table
  Table14
  #To display the joint distribution (which gives the proportion for each cell):
  prop.table(Table14)
  #Analyzing Categorical 'Table14' using BarPlot Method
  c14<-c("blue","red","green","black","orange","darkgreen","yellow")
  barplot(as.matrix(Table14),main="Week Days V/S Day Weather",ylab="Frequency",cex.lab = 1,
          cex.main = 1,beside=TRUE, col=c14)
  legend("topright", legend = c("Sunday","Monday","Tuesday","Wednuesday","Thursday","Friday","Saturday"), 
         fill = c14,ncol=1,cex=0.3)    
  
#Checking relationship between Day Weather and Working Days
  Table15 <- table(BikeData$DayWeather,BikeData$WorkingDay)
  #Renaming Column Names to get better understanding of Two way table.
  colnames(Table15) <- c("Non Working Day","Working Day")
  #Renaming Row Names to get better understanding of Two way table.
  rownames(Table15) <- c("Pleasent","Partly Pleasent","Non Pleasent")
  #Viewing Two Table
  Table15
  #To display the joint distribution (which gives the proportion for each cell):
  prop.table(Table15)
  #Analyzing Categorical 'Table15' using BarPlot Method
  c15<-c("blue","red","green")
  barplot(as.matrix(Table15),main="Day Weather V/S Working Days",ylab="Frequency",cex.lab = 1,
          cex.main = 1,beside=TRUE, col=c15)
  legend("topleft", legend = c("Pleasent","Partly Pleasent","Non Pleasent"), 
         fill = c15,ncol=1,cex=0.40)    
  
#MISSING VALUE ANALYSIS

#Creating a dataframe with Missing Values
MissingValues = data.frame(apply(BikeData,2,function(x){sum(is.na(x))}))
write.csv(MissingValues,'missing.csv')

#Converting Row Names into Column
MissingValues$columns = row.names(MissingValues)
row.names(MissingValues)=NULL
MissingValues

#Renaming the Variable Name
names(MissingValues)[1] =  "MissingPercentage"
MissingValues

#Calculating Percentage of Missing Data
Percentage = MissingValues$MissingPercentage=(MissingValues$MissingPercentage/nrow(BikeData))*100
Percentage

#There is no missing value. I have done the same process earlier with another method.


#OULIER ANALYSIS

#Univariate Outlier Analysis
#Making a Copy of Data
BikeDataCopy =BikeData
#Detecting Outliers in 'Humidity' using Box Plot Method
ggplot(BikeData, aes(x ="", y = BikeData$Humidity)) + geom_boxplot() 
#There are outliers in Humidity Attribute
#Analyzing the relationship of Humidity attribute with Target Attribute
  ggplot(BikeData, aes(x=BikeData$Humidity,y=BikeData$RentedBikes))+geom_point()+
  geom_smooth()+xlab("Humidity")+ylab("Total Count Of Rented Bikes")+
  ggtitle("Analysing relation of Humidity with Count of Rented Bikes(Targer Attribute)")
  #Removing Outliers using Box Plot Method
  XYZ = BikeDataCopy$Humidity[BikeDataCopy$Humidity %in% boxplot.stats(BikeData$Humidity)$out]
  BikeDataCopy = BikeDataCopy[which(!BikeDataCopy$Humidity %in% XYZ),]
  #Analyzing after removing outliers
  #Boxplot for Humidity user Attributes
  ggplot(BikeDataCopy, aes(x ="", y = BikeDataCopy$Humidity)) + geom_boxplot() 
  #Verifying Relationship between Humidity and Target Variable
  ggplot(BikeDataCopy, aes(x=BikeDataCopy$Humidity,y=BikeDataCopy$RentedBikes))+geom_point()+
  geom_smooth()+xlab("Humidity")+ylab("Total Count Of Rented Bikes")+
  ggtitle("Analysing relationship of Humidity with Count of Rented Bikes(Targer Attribute)")
  #Comparing Correlation of BikeData Set and BikeDataCopy Data Set
  cor(BikeData$Humidity,BikeData$RentedBikes)
  cor(BikeDataCopy$Humidity,BikeDataCopy$RentedBikes) 
  #Since Correlation is moving towards more stronger relationship we will accept it.Moving closer to -1.
  BikeData = BikeDataCopy
  #Verifying Changes
  cor(BikeData$Humidity,BikeData$RentedBikes)
  cor(BikeDataCopy$Humidity,BikeDataCopy$RentedBikes) 

#Detecting Outliers in 'Wind Speed' using Box Plot Method
ggplot(BikeData, aes(x ="", y = BikeData$WindSpeed)) + geom_boxplot() 
#There are  outlier in Count of Wind Speed Attribute
#Analyzing the relationship of Wind Speed attribute with Target Attribute
  ggplot(BikeData, aes(x=BikeData$WindSpeed,y=BikeData$RentedBikes))+geom_point()+
  geom_smooth()+xlab("Wind Speed")+ylab("Total Count Of Rented Bikes")+
  ggtitle("Analysing relationship of Wind Speed with Count of Rented Bikes(Targer Attribute)")
  #Removing Outliers using Box Plot Method
  XYZ = BikeDataCopy$WindSpeed[BikeDataCopy$WindSpeed %in% boxplot.stats(BikeData$WindSpeed)$out]
  BikeDataCopy = BikeDataCopy[which(!BikeDataCopy$WindSpeed %in% XYZ),]
  #Analyzing after removing outliers
  #Boxplot for Count of Wind Speed user Attributes
  ggplot(BikeDataCopy, aes(x ="", y = BikeDataCopy$WindSpeed)) + geom_boxplot() 
  #Verifying Relationship between Count of Casual Users and Target Variable
  ggplot(BikeDataCopy, aes(x=BikeDataCopy$WindSpeed,y=BikeDataCopy$RentedBikes))+geom_point()+
  geom_smooth()+xlab("Wind Speed")+ylab("Total Count Of Rented Bikes")+
  ggtitle("Analysing relationship of Wind Speed with Count of Rented Bikes(Targer Attribute)")
  #Comparing Correlation of BikeData Set and BikeDataCopy Data Set
  cor(BikeData$WindSpeed,BikeData$RentedBikes)
  cor(BikeDataCopy$WindSpeed,BikeDataCopy$RentedBikes) 
  #Making Changes 
  BikeData=BikeDataCopy
  #Verifying Changes
  cor(BikeData$WindSpeed,BikeData$RentedBikes)
  cor(BikeDataCopy$WindSpeed,BikeDataCopy$RentedBikes) 
  
  #Detecting Outliers in 'Registered Users' using Box Plot Method
  ggplot(BikeData, aes(x ="", y = BikeData$RegisteredUsers)) + geom_boxplot() 
  #There is no outlier in Registered Users Attribute

  #Detecting Outliers in 'Rented Bikes' using Box Plot Method
  ggplot(BikeData, aes(x ="", y = BikeData$RentedBikes)) + geom_boxplot() 
  #There is no outlier in Total count of Rented Bikes Attribute

  #Detecting Outliers in 'Temperature' using Box Plot Method
  ggplot(BikeData, aes(x ="", y = BikeData$Temperature)) + geom_boxplot() 
  #There is no outlier in Temperature Bikes Attribute
  
  #Detecting Outliers in 'Feeling Temperature' using Box Plot Method
  ggplot(BikeData, aes(x ="", y = BikeData$FeelingTemperature)) + geom_boxplot() 
  #There is no outlier in Feeling Temperature Bikes Attribute

  #Detecting Outliers in 'Casual Users' using Box Plot Method
  ggplot(BikeData, aes(x ="", y = BikeData$CasualUsers)) + geom_boxplot() 
  #There are  outlier in Count of Casual Users Attribute
  #Analyzing the relationship of Count Casual Users attribute with Target Attribute
  ggplot(BikeData, aes(x=BikeData$CasualUsers,y=BikeData$RentedBikes))+geom_point()+
      geom_smooth()+xlab("Count of Casual Users")+ylab("Total Count Of Rented Bikes")+
      ggtitle("Analysing relationship of Casual Users with Count of Rented Bikes(Targer Attribute)")
  #Removing Outliers using Box Plot Method
  XYZ = BikeDataCopy$CasualUsers[BikeDataCopy$CasualUsers %in% boxplot.stats(BikeData$CasualUsers)$out]
  BikeDataCopy = BikeDataCopy[which(!BikeDataCopy$CasualUsers %in% XYZ),]
  #Analyzing after removing outliers
  #Boxplot for Count of Count of Casual user Attributes
  ggplot(BikeDataCopy, aes(x ="", y = BikeDataCopy$CasualUsers)) + geom_boxplot() 
  #Verifying Relationship between Count of Casual Users and Target Variable
  ggplot(BikeDataCopy, aes(x=BikeDataCopy$CasualUsers,y=BikeDataCopy$RentedBikes))+geom_point()+
      geom_smooth()+xlab("Count of Casual Users")+ylab("Total Count Of Rented Bikes")+
      ggtitle("Analysing relationship of Casual Users with Count of Rented Bikes(Targer Attribute)")
  #Comparing Correlation of BikeData Set and BikeDataCopy Data Set
  cor(BikeData$CasualUsers,BikeData$RentedBikes)
  cor(BikeDataCopy$CasualUsers,BikeDataCopy$RentedBikes)
  #MaKING Changes
  BikeData=BikeDataCopy
  #Verifying Changes
  cor(BikeData$CasualUsers,BikeData$RentedBikes)
  cor(BikeDataCopy$CasualUsers,BikeDataCopy$RentedBikes)
  
#Feature Engineering or Feature Selection
#Verifying Correlation among all the Numeric Attributes in the BikeData dataset
  corrgram(BikeData[,c('Temperature','FeelingTemperature','Humidity','WindSpeed','CasualUsers','RegisteredUsers','RentedBikes')],
           order = F,upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot")
#correlation matrix  Clearly shows that "Temperature" and "Feeling Temperature has a very strong relationship.
#There is very weak relationship between Humidity abd Rented Bikes 

#Dimension Reduction Process
BikeData= subset(BikeData,select=-c(FeelingTemperature,Humidity))

#Normality Check
CNames = c("CasualUsers","RegisteredUsers")
for(i in CNames){
  print(i)
  BikeData[i] = (BikeData[i] - min(BikeData[i]))/
    (max(BikeData[i] - min(BikeData[i])))
}
 BikeData$CasualUsers
 BikeData$RegisteredUsers
 
 
 #Modal Development
 
 
 FeaturesSelected=BikeData[c("Season" ,"Year" ,"Month","Holiday","WeekDay","WorkingDay","DayWeather","Temperature",
                           "WindSpeed","CasualUsers","RegisteredUsers","RentedBikes")]
 
 #Divide data into train and test using stratified sampling method

 train.index = sample(1:nrow(FeaturesSelected),0.8*nrow(FeaturesSelected))
 Train = FeaturesSelected[ train.index,]
 Test  = FeaturesSelected[-train.index,]
 
 
 #Decision Tree Model
 #Fit a tree based on Training Data
 DecisionTree = rpart(RentedBikes~.,Train,method='anova')
 DecisionTree
 #Plotting Decision Tree
 rpart.plot(DecisionTree,type=5)
  #Check hoq the model is doing using test dataset
 Predictions = predict(DecisionTree,Test)
 Predictions
 #Root Mean Squared Error
 RMSE <- function(y_test,y_predict) {
   
   difference = y_test - y_predict
   root_mean_square = sqrt(mean(difference^2))
   return(root_mean_square)
   }
 RMSE(Test$RentedBikes,Predictions)
 #RMSE :- 522.0794
 
  #MAPE
 MAPE = function(y, yhat){
   mean(abs((y - yhat)/y)*100)
 }
 
 MAPE(Test[,12],Predictions)
 #Error Rate :- 12.22685%
 #Accuracy :- 100 - 12.22 =  87.78%

 #Training Random Forest Model
 RandomForest = randomForest(RentedBikes~.,data=Train,importance = TRUE)
 RandomForest
 #Plot Random Forest
 plot(RandomForest)
 #Predicting for Test Data
 Predictions = predict(RandomForest,Test)
 Predictions
 #MAPE
 MAPE(Test[,12],Predictions)
 #Error Rate = 9.305516%
 #Accuracy = 100 - 9.30= 90.70%
 
 #Parameter Tuning For Random Forest
 RandomForest = randomForest(RentedBikes~.,data=Train,importance = TRUE,ntree = 800, mtry = 6)
 RandomForest
 #Plot Random Forest
 plot(RandomForest)
 #Predicting for Test Data
 Predictions = predict(RandomForest,Test)
 Predictions
 #MAPE
 MAPE(Test[,12],Predictions)
 #Error Rate :- 4.472374%
 #Accuracy = 100-4.47 =95.53%
 
 #Parameter Tuning For Random Forest
 RandomForest = randomForest(RentedBikes~.,data=Train,importance = TRUE,ntree = 1200, mtry = 10)
 RandomForest
 #Plot Random Forest
 plot(RandomForest)
 #Predicting for Test Data
 Predictions = predict(RandomForest,Test)
 Predictions
 #MAPE
 MAPE(Test[,12],Predictions)
 #Error Rate :- 2.19683%
 #Accuracy = 100-2.19 =97.81%
 
 #Parameter Tuning For Random Forest
 RandomForest = randomForest(RentedBikes~.,data=Train,importance = TRUE,ntree = 1500, mtry = 10)
 RandomForest
 #Plot Random Forest
 plot(RandomForest)
 #Predicting for Test Data
 Predictions = predict(RandomForest,Test)
 Predictions
 #MAPE
 MAPE(Test[,12],Predictions)
 #Error Rate :- 2.148435
 #Accuracy = 100-2.14 =97.86%
 
 #Checking Variable Impotance
 # check Variable  Importance 
  Importance <- importance(RandomForest)
  Importance
  #Sorting Variables  
  Sort <- names(sort(Importance[,1],decreasing =T))
  Sort
  # draw varimp plot 
 varImpPlot(RandomForest,type = 2)
 
 #Tuning Random Forest Dimensional reduction
 #Remove four variables  which is  contributing  less are
 #Season, DayWeather, Holiday,WindSpeed
 
 Train1 = Train[c('Year','Month','WeekDay','WorkingDay','Temperature','CasualUsers','RegisteredUsers','RentedBikes')]
 Test1 = Test[c('Year','Month','WeekDay','WorkingDay','Temperature','CasualUsers','RegisteredUsers','RentedBikes')]
 
 #Developing Model wil Reduced Attributes
 RandomForest = randomForest(RentedBikes~.,data=Train1,importance = TRUE,ntree = 1500, mtry = 6)
 RandomForest
 #Plot Random Forest
 plot(RandomForest)
 #Predicting for Test Data
 Predictions = predict(RandomForest,Test)
 Predictions
 #MAPE
 MAPE(Test1$RentedBikes,Predictions)
 #Error Rate :- 2.118801%
 #Accuracy = 100-2.11 =97.89%
 
 #ON futher increase number of trees error rate is increasing so I am stopping my iterations here.
 #NOTE :- I am not sure here to stop or not. PLease let me know in your reviews.
 
 #Linear Regression Model
  #Checking Multicollinearity
  vif(Train[,-12])
 vifcor(Train[,-12],th=0.9)
 # Correleation between two variables is 'Season' and 'Month' is 0.82.
 #Correleation between two variables is 'Year' and 'Month' is -0.0016.
 #So I am remiving Month from all other attrributes
 
 Train2 = Train[c('Season','Year','Holiday','WeekDay','WorkingDay','DayWeather','Temperature','WindSpeed','CasualUsers',
                  'RegisteredUsers','RentedBikes')]
 Test2 = Test[c('Season','Year','Holiday','WeekDay','WorkingDay','DayWeather','Temperature','WindSpeed','CasualUsers',
                  'RegisteredUsers','RentedBikes')]
 
 #Developing Linear Regression Model
 LinearRegression = lm(RentedBikes~.,data=Train2)
 #Summary of the model
 summary(LinearRegression)
 #Prediction on test data
 Predictions = predict(LinearRegression,Test2[,-11])
 Predictions
 #Checking Accuracy of the Model
 MAPE(Test2$RentedBikes,Predictions)
 #Error RAte :- 5.703285e-14%
 #Accuracy of the the Model = Around 99.9 %
      
 #Linear Regression Model is best Model this DataSet.
 
 
 
 