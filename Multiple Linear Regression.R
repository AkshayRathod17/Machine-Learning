
#Multiple linear regression

#Importing the Dataset

getwd()
setwd("C:\\Users\\admin\\Downloads\\Machine Learning")

dataset = read.csv("50_Startups.csv")

#Encoding Catogorical data

dataset$State = factor(dataset$State,
                             levels = c('New York','California','Florida'),
                             labels = c(1,2,3))


#splitting the dataset into Training and test set

#install.packages("caTools")
library(caTools)

split = sample.split(dataset$Profit,SplitRatio = 0.8)
split

training_set = subset(dataset,split==TRUE)

test_set = subset(dataset,split==FALSE)



#Fitting Multiple Linear Regression for training set
regressor = lm( formula = Profit ~ .,data = training_set)

#Predicting the Test Set
y_pred = predict(regressor,newdata = test_set)



#Building the optimal model using Backward Elimination
regressor = lm( formula = Profit ~ R.D.Spend + Administration + Marketing.Spend + State 
                  ,data = training_set)

summary(regressor)

regressor = lm( formula = Profit ~ R.D.Spend + Administration + Marketing.Spend
                ,data = training_set)

summary(regressor)

regressor = lm( formula = Profit ~ R.D.Spend + Administration
                ,data = training_set)

summary(regressor)

regressor = lm( formula = Profit ~ R.D.Spend 
                ,data = training_set)

summary(regressor)



