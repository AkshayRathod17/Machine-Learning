
#K-means Clustering Algorithm

#Import the Dataset 
dataset = read.csv('Mall_Customers.csv')
dataset = dataset[4:5]

## Using the elbow method to find the optimal number of clusters
set.seed(6)
wcss = vector()
for (i in 1:10) wcss[i] = sum(kmeans(dataset,i)$withinss)
plot(1:10,
     wcss,
     type = 'b',
     main = 'The Elbow Method',
     xlab = 'Number of clusters',
     ylab = 'wcss')
  
#Fitting K-means to Dataset
set.seed(29)
kmeans = kmeans(dataset,centers = 5)
y_kmeans = kmeans$cluster

# Visualising the clusters
library(cluster)
clusplot(dataset,
         y_kmeans,
         lines = 0,
         shade = TRUE,
         color = TRUE,
         labels = 2,
         plotchar = FALSE,
         span = TRUE,
         main = paste('Clusters of customers'),
         xlab = 'Annual Income',
         ylab = 'Spending Score')