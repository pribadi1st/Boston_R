Boston <- read.csv("Boston.csv")

#Scaling data for KNN
#scaledData <- scale(Boston)
library( ggplot2 )
library(corrplot)
library(caTools)
library(gridExtra)
library(ROCR)

library ( rpart )
library ( rpart.plot )


Boston [ , "X" ] = list ( NULL )
str(Boston)

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

#Cluster with Binary Classification
Boston[Boston$chas == 0 ,]$chas <- "farFromRiver"
Boston[Boston$chas == 1 ,]$chas <- "closeFromRiver"
Boston$chas <- as.factor(Boston$chas) 
#To find how many 0 / 1 we have for chas
summary(Boston)

#Split data to train
set.seed(333)
splitDataSet = sample.split(Boston$chas, SplitRatio = 0.75)
trainDataSet = subset(Boston, splitDataSet == TRUE)
testDataSet = subset(Boston, splitDataSet == FALSE)

#visualize data set
#View(trainDataSet)
#-4  is to exclude fourth column
cormat <- cor(trainDataSet[,-4])
corrplot(cormat,method='number')
# Find high correlation variable
# Nox & dis | rad & tax | age & dis
p1 <- ggplot(data = trainDataSet,aes(nox,rad,col=chas))+geom_point(alpha=0.5)
p2 <- ggplot(data = trainDataSet,aes(tax,rad,col=chas))+geom_point(alpha=0.5)
p3 <- ggplot(data = trainDataSet,aes(age,rad,col=chas))+geom_point(alpha=0.5)
grid.arrange(p1,p2,p3,nrow=3)
#check against chas
#box plot
p1<-ggplot(data = trainDataSet,aes(x = chas,y = crim,fill=chas))+geom_boxplot()
p2<-ggplot(data = trainDataSet,aes(x = chas,y = zn,fill=chas))+geom_boxplot()
p3<-ggplot(data = trainDataSet,aes(x = chas,y = indus,fill=chas))+geom_boxplot()
p4<-ggplot(data = trainDataSet,aes(x = chas,y = nox,fill=chas))+geom_boxplot()
p5<-ggplot(data = trainDataSet,aes(x = chas,y = rm,fill=chas))+geom_boxplot()
p6<-ggplot(data = trainDataSet,aes(x = chas,y = age,fill=chas))+geom_boxplot()
p7<-ggplot(data = trainDataSet,aes(x = chas,y = dis,fill=chas))+geom_boxplot()
p8<-ggplot(data = trainDataSet,aes(x = chas,y = rad,fill=chas))+geom_boxplot()
p9<-ggplot(data = trainDataSet,aes(x = chas,y = tax,fill=chas))+geom_boxplot()
p10<-ggplot(data = trainDataSet,aes(x = chas,y = ptratio,fill=chas))+geom_boxplot()
p11<-ggplot(data = trainDataSet,aes(x = chas,y = black,fill=chas))+geom_boxplot()
p12<-ggplot(data = trainDataSet,aes(x = chas,y = lstat,fill=chas))+geom_boxplot()
p13<-ggplot(data = trainDataSet,aes(x = chas,y = medv,fill=chas))+geom_boxplot()
grid.arrange(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,nrow=3)

#create the glm model
# Nox & dis | rad & tax | age & dis
#model1 <- glm( chas ~ indus + tax + rad, trainDataSet, family = binomial)
#lrm <- glm ( chas ~ rm + dis + indus + lstat, data = trainDataSet, family = binomial )
#lrm <- glm ( chas ~ . - crim - zn - black, data = trainDataSet, family = binomial )
#model 1
lrm <- glm ( chas ~ indus + rad + tax + ptratio, data = trainDataSet, family = binomial )
#base model
baseModel <- glm(chas ~ ., trainDataSet, family= binomial)
summary(lrm)
predictTest <- predict ( lrm, type = "response", newdata = testDataSet )

table ( testDataSet$chas, predictTest > 0.5 )

# Baseline accuracy
1 - mean ( baseModel$chas ) 

ROCRpred <- prediction ( predictTest, testDataSet$chas )
as.numeric ( performance ( ROCRpred, "auc" ) @y.values )
ROCRperf = performance ( ROCRpred, "tpr", "fpr" )
plot ( ROCRperf, colorize = TRUE, print.cutoffs.at = seq ( 0, 1, by = 0.1 ), text.adj = c ( -0.2, 1.7 ) )

# Test for clustering #
hc.complete <- hclust ( dist ( trainDataSet ), method = "complete" )
hc.average <- hclust ( dist ( trainDataSet ), method = "average" )
hc.single <- hclust ( dist ( trainDataSet ), method = "single" )

par ( mfrow = c ( 1, 3 ) )
plot ( hc.complete , main = " Complete Linkage ", xlab= "", sub = "" )
plot ( hc.average , main = " Average Linkage ", xlab= "", sub = "", cex = .9 )
plot ( hc.single , main =" Single Linkage ", xlab= "", sub ="", cex = .9 )

##scaling
xsc = scale ( trainDataSet )
par ( mfrow <- c ( 1, 1 ) )
plot ( hclust ( dist ( xsc ), method = "single" ), main = "Hierarchical Clustering with Scaled Features")

#predict
treePart = rpart ( chas ~ . , data = trainDataSet ) # base model
treePart = rpart ( chas ~ indus + rad + tax + ptratio , data = trainDataSet ) # Model 1
rpart.plot ( treePart )
