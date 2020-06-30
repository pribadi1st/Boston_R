Boston <- read.csv("Boston.csv")

#Scaling data for KNN
scaledData <- scale(Boston)
library(factoextra)

#check optimasisasi cluster
kmax <- 10
optimizeClusterNumber <- sapply(1:kmax, function(k) kmeans(scaledData, centers = k, nstart = 10)$tot.withinss)

plot(1:kmax, optimizeClusterNumber, type = 'b', xlab = 'k', ylab = 'Total wss')
abline(v=4, lty=2)

set.seed(50)

#Cluster Data with kmeans
clusteredData <- kmeans(scaledData, centers = 4, nstart = 50)
#Visualize cluster
plot(x=scaledData[,1], y=scaledData[,2], col=clusteredData$cluster)
points(clusteredData$centers, pch=3, cex=2)

### unreliable value ###

#Cluster with PCA
res.pca <- prcomp(Boston, scale = FALSE)
fviz_eig(res.pca)
