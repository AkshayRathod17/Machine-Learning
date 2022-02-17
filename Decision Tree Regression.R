
#Decision Tree Regression

#Importing the Dataset

dataset = read.csv("Position_Salaries.csv")
dataset = dataset[2:3]



#Fitting the Decision Tree Regression to the dataset
install.packages('rpart')
library(rpart)

regressor = rpart(formula = Salary ~ .,
                  data = dataset,
                  control = rpart.control(minsplit = 1))



#Predicting the new result with Regression Model
y_pred = predict(regressor , data.frame(Level = 6.5))


#Visualising the Decision Tree Regression results(For Higher Resolution and smoother curve)
#install.packages('ggplot2')
library(ggplot2)
x_grid = seq(min(dataset$Level), max(dataset$Level), 0.01) 

ggplot() +
  geom_point(aes(x=dataset$Level, y=dataset$Salary),
             colour = 'red') +
  geom_line(aes(x = x_grid, y = predict(regressor, newdata = data.frame(Level = x_grid))),
            colour = 'blue') +
  ggtitle('Truth and Bluff(Regression Model)') +
  xlab('Level') +
  ylab('Salary')
