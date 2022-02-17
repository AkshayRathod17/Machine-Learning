
#Classification Template

#Importing the Dataset
dataset = read.csv("Social_Network_Ads.csv")
dataset = dataset[3:5]

#Encoding the target feature as factor
dataset$Purchased = factor(dataset$Purchased, levels = c(0,1))

#splitting the dataset into Training and test set
#install.packages("caTools")
library(caTools)
set.seed(123)
split = sample.split(dataset$Purchased,SplitRatio = 0.75)
split
training_set = subset(dataset,split==TRUE)
test_set = subset(dataset,split==FALSE)

#Feature scalling
training_set[1:2] = scale(training_set[1:2])
test_set[1:2] = scale(test_set[1:2])

#Fitting Classifier to the training set
#Create your Classifier here

#Predicting the Test set results
y_pred = predict(classifier, newdata = test_set[-3])


#Making the confusion matrix
cm = table(test_set[,3],y_pred)

#Visualising the Training set results
#install.packages(ElemStatLearn)
library(ElemStatLearn)
set = training_set
x1 = seq(min(set[,1]) - 1,max(set[,1]) + 1, by = 0.01)
x2 = seq(min(set[,2]) - 1,max(set[2]) + 1, by = 0.01)
grid_set = expand.grid(x1,x2)

colnames(grid_set) = c('Age','EstimatedSalary')

prob_pred = predict(classifier, type = 'response', newdata = grid_set)
y_grid = ifelse(prob_pred > 0.5 , 1, 0)

plot(set[, -3],
     main = 'Classifier (Training Set)',
     xlab = 'Age' ,ylab = 'Estimated Salary',
     xlim = range(x1),ylim = range(x2))

contour(x1,x2,matrix(as.numeric(y_grid),length(x1),length(x2)),add = TRUE)
points(grid_set,pch = '.', col = ifelse(y_grid == 1 , 'Springgreen','tomato'))
points(set,pch =21 , bg = ifelse(set[, 3] == 1 , 'green4','red3'))


#Visualising the Test set results
#install.packages(ElemStatLearn)
library(ElemStatLearn)
set = test_set
x1 = seq(min(set[,1]) - 1,max(set[,1]) + 1, by = 0.01)
x2 = seq(min(set[,2]) - 1,max(set[2]) + 1, by = 0.01)
grid_set = expand.grid(x1,x2)

colnames(grid_set) = c('Age','EstimatedSalary')

y_grid = predict(classifier, newdata = grid_set)

plot(set[, -3],
     main = 'Classifier (Test Set)',
     xlab = 'Age' ,ylab = 'Estimated Salary',
     xlim = range(x1),ylim = range(x2))

contour(x1,x2,matrix(as.numeric(y_grid),length(x1),length(x2)),add = TRUE)
points(grid_set,pch = '.', col = ifelse(y_grid == 1 , 'Springgreen','tomato'))
points(set,pch =21 , bg = ifelse(set[, 3] == 1 , 'green4','red3'))




