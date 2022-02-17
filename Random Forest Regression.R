
#Random Forest Regression

#Importing the Dataset

dataset = read.csv("Position_Salaries.csv")
dataset = dataset[2:3]

#Fitting Random Forest Regression to the dataset
install.packages("randomForest")
library(randomForest)
set.seed(1234)
regressor = randomForest(x = dataset[1],
                         y = dataset$Salary,
                         ntree = 500)

#Predicting the new result with Random Forest Regression
y_pred = predict(regressor , data.frame(Level = 6.5))


#Visualising the Random Forest Regression results(For Higher Resolution and smoother curve)
library(ggplot2)

x_grid = seq(min(dataset$Level), max(dataset$Level), 0.01) 
ggplot() +
  geom_point(aes(x=dataset$Level, y=dataset$Salary),
             colour = 'red') +
  geom_line(aes(x=x_grid, y = predict(regressor, newdata = data.frame(Level = x_grid))),
            colour = 'blue') +
  ggtitle('Truth and Bluff(Random Forest Regression)') +
  xlab('Level') +
  ylab('Salary')

