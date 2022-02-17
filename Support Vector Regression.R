
#Support Vector Regression

#Importing the Dataset

dataset = read.csv("Position_Salaries.csv")
dataset = dataset[2:3]

#Fitting the SVR to the dataset
install.packages('e1071')
library(e1071)
regressor = svm(formula = Salary ~ .,
                data = dataset,
                type = 'eps-regression')
              

#Predicting the new result with SVR
y_pred = predict(regressor , data.frame(Level = 6.5))


#Visualising the SVR results

ggplot() +
  geom_point(aes(x=dataset$Level, y=dataset$Salary),
             colour = 'red') +
  geom_line(aes(x=dataset$Level, y = predict(regressor, newdata = dataset)),
            colour = 'blue') +
  ggtitle('Truth and Bluff(SVR)') +
  xlab('Level') +
  ylab('Salary')

