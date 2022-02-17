
#Logistic Regression

#Importing the Dataset
dataset = read.csv("Social_Network_Ads.csv")
dataset = dataset[3:5]

#splitting the dataset into Training and test set
#install.packages("caTools")
library(caTools)
split = sample.split(dataset$Purchased,SplitRatio = 0.75)
split
training_set = subset(dataset,split==TRUE)
test_set = subset(dataset,split==FALSE)

#Feature scalling
training_set[1:2] = scale(training_set[1:2])
test_set[1:2] = scale(test_set[1:2])

#Fitting the Logistic Regression to the training set
classifier = glm(formula = Purchased ~ . ,
                 family = binomial,
                 data = training_set)

#Predicting the Test set results
prob_pred = predict(classifier, type = 'response', newdata = test_set[-3])
y_pred = ifelse(prob_pred > 0.5 , 1, 0)


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
     main = 'Logistic Regression(Training Set)',
     xlab = 'Age' ,ylab = 'Estimated Salary',
     xlim = range(x1),ylim = range(x2))
?contour()
?as.numeric
?matrix()
?points
contour(x1,x2,matrix(as.numeric(y_grid),length(x1),length(x2)),add = TRUE)
points(grid_set,pch = '.', col = ifelse(y_grid == 1 , 'Springgreen','tomato'))
points(set,pch =21 , bg = ifelse(set[, 3] == 1 , 'green4','red3'))


#Visualising the Training set results
#install.packages(ElemStatLearn)
library(ElemStatLearn)
set = test_set
x1 = seq(min(set[,1]) - 1,max(set[,1]) + 1, by = 0.01)
x2 = seq(min(set[,2]) - 1,max(set[2]) + 1, by = 0.01)
grid_set = expand.grid(x1,x2)

colnames(grid_set) = c('Age','EstimatedSalary')

prob_pred = predict(classifier, type = 'response', newdata = grid_set)
y_grid = ifelse(prob_pred > 0.5 , 1, 0)

plot(set[, -3],
     main = 'Logistic Regression(Test Set)',
     xlab = 'Age' ,ylab = 'Estimated Salary',
     xlim = range(x1),ylim = range(x2))

contour(x1,x2,matrix(as.numeric(y_grid),length(x1),length(x2)),add = TRUE)
points(grid_set,pch = '.', col = ifelse(y_grid == 1 , 'Springgreen','tomato'))
points(set,pch =21 , bg = ifelse(set[, 3] == 1 , 'green4','red3'))


