#library ( tree )
#library( ggplot2 )
#import necessary libary
library (caret)
library (caTools)
library (relaimpo)

#import data set
Boston <- read.csv("Boston.csv")

#rm(Boston)
#na.omit ( Hitters )
#Remove unnecessary column
#cor(Boston)
#plot(Boston)
#Remove index column
Boston [ , "X" ] = list ( NULL )
head(Boston, n = 5) #check the first 5 row

#check correlation for each variable
cor(Boston)
plot(Boston)
pairs ( Boston [ c ( "chas", "nox", "rm", "dis", "ptratio", "black", "lstat") ] )

#Find best model
baseLinearModel <- lm(medv ~ . , data= Boston)
summary(baseLinearModel)
#better R-Square
# - is to remove the less significant independent variable
# + is to add """""""
testLinearModel <- lm(medv ~ . - indus - age - tax - zn - rad - crim, data = Boston)
#--------------#
linearModel <- lm(medv ~ lstat + chas + ptratio + dis + zn + nox + black
                  + rm  + rad, data = Boston)
summary(testLinearModel)
#interaction model
#Abhisek as to fabio
interactionModel <- lm(medv ~ I(lstat ^ 2)+ rm + tax + lstat, data= Boston)
summary(interactionModel)

# Relative importance of variables
interactionModel <- calc.relimp (interactionModel, type = "lmg" )
sum ( interactionModel$lmg )

barplot ( sort ( interactionModel$lmg, decreasing = TRUE ), 
          col = c ( 2:10 ), main = "Relative Importance of Predictors", 
          xlab = "Predictor Labels", ylab = "Shapley Value Regression", 
          font.lab = 2 )


#Create prediction test
#1. set split ratio
set.seed(100)
splitData <- sample.split(Boston$medv, SplitRatio = 0.75)
View(splitData)

#2. Set train data
dataForTrain <- subset(Boston, splitData == TRUE)
dataForTest <- subset(Boston, splitData == FALSE)

#4. Check error for test data
#Root mean square E
sqrt (mean ( ( dataForTest$medv - predict.lm ( interactionModel, dataForTest ) ) ^ 2 ))

#check using RMSE
predictionsModel <- predict ( interactionModel, dataForTest)
RMSE (predictionsModel, dataForTest$medv)
