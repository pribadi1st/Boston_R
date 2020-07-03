Boston <- read.csv("Boston.csv")

library(caTools)
library(corrplot)
library(ROCR)
library(rpart)
library(rpart.plot)
library(gridExtra)

Boston$older90 <- ifelse(Boston$age <= 90, "Younger equal than 90", "Older than 90")

Boston [ , "X" ] = list ( NULL )

set.seed(333)
splitDataSet = sample.split(Boston$older90, SplitRatio = 0.75)
trainDataSet = subset(Boston, splitDataSet == TRUE)
testDataSet = subset(Boston, splitDataSet == FALSE)

trainDataSet$older90 <- as.factor(trainDataSet$older90)
#summary(Boston)

cormat <- cor(trainDataSet[, -15])
corrplot(cormat,method='number')
#cor(trainDataSet)

#check plot
p1<-ggplot(data = trainDataSet,aes(x = older90,y = crim,fill=older90))+geom_boxplot()
p2<-ggplot(data = trainDataSet,aes(x = older90,y = zn,fill=older90))+geom_boxplot()
p3<-ggplot(data = trainDataSet,aes(x = older90,y = indus,fill=older90))+geom_boxplot()
p4<-ggplot(data = trainDataSet,aes(x = older90,y = nox,fill=older90))+geom_boxplot()
p5<-ggplot(data = trainDataSet,aes(x = older90,y = rm,fill=older90))+geom_boxplot()
p6<-ggplot(data = trainDataSet,aes(x = older90,y = chas,fill=older90))+geom_boxplot()
p7<-ggplot(data = trainDataSet,aes(x = older90,y = dis,fill=older90))+geom_boxplot()
p8<-ggplot(data = trainDataSet,aes(x = older90,y = rad,fill=older90))+geom_boxplot()
p9<-ggplot(data = trainDataSet,aes(x = older90,y = tax,fill=older90))+geom_boxplot()
p10<-ggplot(data = trainDataSet,aes(x = older90,y = ptratio,fill=older90))+geom_boxplot()
p11<-ggplot(data = trainDataSet,aes(x = older90,y = black,fill=older90))+geom_boxplot()
p12<-ggplot(data = trainDataSet,aes(x = older90,y = lstat,fill=older90))+geom_boxplot()
p13<-ggplot(data = trainDataSet,aes(x = older90,y = medv,fill=older90))+geom_boxplot()
grid.arrange(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,nrow=3)
# ----- #

#Base Model
baseModel = glm ( older90 ~ . - age , data = trainDataSet, family = binomial )
modelSet = glm ( older90 ~ dis + indus + lstat + medv , data = trainDataSet, family = binomial )
modelSet2 = glm ( older90 ~ indus + nox + tax + rad + lstat + medv , data = trainDataSet, family = binomial )
summary(modelSet2)
summary(baseModel)
summary(modelSet)
predictTest = predict ( modelSet, type = "response", newdata = testDataSet )
table ( testDataSet$older90, predictTest > 0.5 )

# Baseline accuracy
# Don't use as.factor yet
1 - mean ( Boston$age )

ROCRpred <- prediction ( predictTest, testDataSet$older90 )
as.numeric ( performance ( ROCRpred, "auc" ) @y.values )
ROCRperf = performance ( ROCRpred, "tpr", "fpr" )
plot ( ROCRperf, colorize = TRUE, print.cutoffs.at = seq ( 0, 1, by = 0.1 ), text.adj = c ( -0.2, 1.7 ) )

# Test for clustering #
hc.complete = hclust ( dist ( trainDataSet ), method = "complete" )
hc.average = hclust ( dist ( trainDataSet ), method = "average" )
hc.single = hclust ( dist ( trainDataSet ), method = "single" )

par ( mfrow = c ( 1, 3 ) )
plot ( hc.complete , main = " Complete Linkage ", xlab= "", sub = "", cex = .9 )
plot ( hc.average , main = " Average Linkage ", xlab= "", sub = "", cex = .9 )
plot ( hc.single , main =" Single Linkage ", xlab= "", sub ="", cex = .9 )

##scaling
xsc = scale ( trainDataSet[,-15] )
par ( mfrow = c ( 1, 1 ) )
plot ( hclust ( dist ( xsc ), method = "single" ), main = "Hierarchical Clustering with Scaled Features")

#predict
#Base Model
treePart = rpart ( older90 ~ . - age + indus + lstat + medv , data = trainDataSet )
#model 1
treePart = rpart ( older90 ~ dis + indus + lstat + medv , data = trainDataSet )
#model 2
treePart = rpart ( older90 ~ indus + nox + tax + rad + lstat + medv , data = trainDataSet )
rpart.plot ( treePart )

#Accuracy
tree.pred = predict (treePart, testDataSet, type = "class" )
summary(tree.pred)