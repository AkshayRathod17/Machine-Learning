#Simple Linear Regression


#Importing the Dataset

getwd()
#setwd("C:\\Users\\admin\\Downloads\\Machine Learning")

dataset = read.csv("Salary_Data.csv")


#splitting the dataset into Training and test set
#install.packages("caTools")
library(caTools)
set.seed(123)
split = sample.split(dataset$Salary,SplitRatio = 2/3)
training_set = subset(dataset,split==TRUE)
test_set = subset(dataset,split==FALSE)

#Feature scalling
#training_set[2:3] = scale(training_set[2:3])
#test_set[2:3] = scale(test_set[2:3])

#Fitting the Simple Linear Regression on the Trainning set
regressor = lm(formula = Salary ~ YearsExperience,data = training_set )
summary(regressor)

#Predicting the Test_set results
y_pred = predict(regressor,newdata = test_set)
y_pred


#Visualising the training set results
library(ggplot2)

ggplot() + geom_point(aes(x = training_set$YearsExperience,
                          y = training_set$Salary),
                          colour = 'red') +

         geom_line(aes(x = training_set$YearsExperience, 
                       y = predict(regressor,newdata = training_set)),
                       colour = 'blue') +
                     
         ggtitle('Salary Vs YearsExp') +
         xlab ('YearsExperiance') +
         ylab ('Salary')
                     
#Visualising the Test_set        

ggplot() + geom_point(aes(x = test_set$YearsExperience,
                          y = test_set$Salary),
                      colour = 'blue') +
  
  geom_line(aes(x = training_set$YearsExperience, 
                y = predict(regressor,newdata = training_set))) +
  
  ggtitle('Salary Vs YearsExp') +
  xlab ('YearsExperiance') +
  ylab ('Salary')

