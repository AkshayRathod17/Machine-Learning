 
#Data Preprocessing 

#Importing the Dataset

getwd()
setwd("C:\\Users\\admin\\Downloads\\Machine Learning")

CountryData = read.csv("Data.csv")

#Taking care of missing values

CountryData$Age = ifelse(is.na(CountryData$Age),ave(CountryData$Age, 
                                                      FUN = function(x) 
                                                      mean(x,na.rm = TRUE)),
                                                  CountryData$Age)

CountryData$Salary = ifelse(is.na(CountryData$Salary),ave(CountryData$Salary, 
                                                    FUN = function(x) 
                                                      mean(x,na.rm = TRUE)),
                                                CountryData$Salary)

#Encoding Catogorical data

CountryData$Country = factor(CountryData$Country,
                      levels = c('France','Spain','Germany'),
                      labels = c(1,2,3))


CountryData$Purchased = factor(CountryData$Purchased,
                             levels = c('No','Yes'),
                             labels = c(1,2))

#splitting the dataset into Training and test set


install.packages("caTools")
library(caTools)

split = sample.split(CountryData$Purchased,SplitRatio = 0.8)
split

training_set = subset(CountryData,split==TRUE)

test_set = subset(CountryData,split==FALSE)

#Feature scalling
training_set[2:3] = scale(training_set[2:3])

test_set[2:3] = scale(test_set[2:3])










