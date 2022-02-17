
#Regression Template

#Importing the Dataset

dataset = read.csv("Position_Salaries.csv")
dataset = dataset[2:3]


# #splitting the dataset into Training and test set
# install.packages("caTools")
# library(caTools)
 
# split = sample.split(CountryData$Purchased,SplitRatio = 0.8)
# split

# training_set = subset(CountryData,split==TRUE)
# test_set = subset(CountryData,split==FALSE)

# #Feature scalling
# training_set[2:3] = scale(training_set[2:3])
# test_set[2:3] = scale(test_set[2:3])


#Fitting the Regression Model to the dataset
#Create your regressor here


#Predicting the new result with Regression Model
y_pred = predict(regressor , data.frame(Level = 6.5))
                                       

#Visualising the Regression Model results

ggplot() +
  geom_point(aes(x=dataset$Level, y=dataset$Salary),
             colour = 'red') +
  geom_line(aes(x=dataset$Level, y = predict(regressor, newdata = dataset)),
            colour = 'blue') +
  ggtitle('Truth and Bluff(Regression Model)') +
  xlab('Level') +
  ylab('Salary')



#Visualising the Regression Model results(For Higher Resolution and smoother curve)
library(ggplot2)

x_grid = seq(min(dataset$Level), max(dataset$Level), 0.1) 
ggplot() +
  geom_point(aes(x=dataset$Level, y=dataset$Salary),
             colour = 'red') +
  geom_line(aes(x=x_grid, y = predict(regressor, newdata = data.frame(Level = x_grid))),
            colour = 'blue') +
  ggtitle('Truth and Bluff(Regression Model)') +
  xlab('Level') +
  ylab('Salary')

