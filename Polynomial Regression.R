
#Polynomial Regression

#Importing the Dataset

dataset = read.csv("Position_Salaries.csv")

dataset = dataset[2:3]


#Fitting Linear Regression

lin_reg = lm(formula = Salary ~ .,
             data = dataset)


#Fitting the Polynomial Regression

dataset$Level2 = dataset$Level^2
dataset$Level3 = dataset$Level^3
dataset$Level4 = dataset$Level^4

poly_reg = lm(formula = Salary ~ .,
              data = dataset)

#Visualising the Linear Regression results
#install.packages('ggplot2')
library(ggplot2)

ggplot() +
  geom_point(aes(x=dataset$Level, y=dataset$Salary),
             colour = 'red') +
  geom_line(aes(x=dataset$Level, y = predict(lin_reg, newdata = dataset)),
             colour = 'blue') +
  ggtitle('Truth and Bluff(Linear Regression)') +
  xlab('Level') +
  ylab('Salary')


#Visualising the polynomial regression results
#library(ggplot2)

ggplot() +
  geom_point(aes(x=dataset$Level, y=dataset$Salary),
             colour = 'red') +
  geom_line(aes(x=dataset$Level, y = predict(poly_reg, newdata = dataset)),
            colour = 'blue') +
  ggtitle('Truth and Bluff(Polynomial Regression)') +
  xlab('Level') +
  ylab('Salary')


#Predicting the new result with Linear Regression
y_pred = predict(lin_reg , data.frame(Level = 6.5))



#Predicting the new result with Linear Regression
y_pred = predict(poly_reg , data.frame(Level = 6.5,
                                       Level2 = 6.5^2,
                                       Level3 = 6.5^3,
                                       Level4 = 6.5^4))

