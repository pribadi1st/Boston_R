#import all necessary libraries
library ( tree )
library( ggplot2 )
library ( caret )
library (caTools)

Boston <- read.csv("Boston.csv")

#rm(Boston)
#na.omit ( Hitters )
#Remove unnecessary column
cor(Boston)
plot(Boston)
Boston [ , "X" ] = list ( NULL )
head(Boston, n = 5)
summary(Boston)

#check correlation first
cor(Boston)
plot(Boston)

baseLinearModel <- lm(medv ~ . , data= Boston)
#better R-Square
prevLinearModel <- lm(medv ~ . - indus - age - tax - zn - rad - crim, data = Boston)
#--------------#
linearModel <- lm(medv ~ lstat + chas + ptratio + dis + zn + nox + black
                  + rm  + rad, data = Boston)
#glm Model
glmModel <- glm(medv ~ . - indus - age, data = Boston)
summary(baseLinearModel)
summary(linearModel)
summary(prevLinearModel)
summary(glmModel)

#Create prediction test

#1. set split ratio
set.seed(100)
splitData = sample.split(Boston$medv, SplitRatio = 0.75)
View(splitData)
#2. Set train data
dataForTrain = subset(Boston, splitData == TRUE)
dataForTest = subset(Boston, splitData == FALSE)

#4. Create Predict Model
predictModel <- glm(medv ~ . - indus - age, data = dataForTrain)
predictTrain <- predict(predictModel, type="response")

#5. Analyze the Model
summary(predictTrain)
