# Week3-Predicting_w_Trees.R
# 
# Predicting with Trees

setwd("~/GitHub/coursera/MachineLearning")

data(iris)
library(ggplot2)
library(caret)
names(iris)
table(iris$Species)

inTrain <- createDataPartition(y=iris$Species, p=0.7, list = FALSE)
training <- iris[inTrain, ]
testing <- iris[-inTrain,]
dim(training)
dim(testing)

qplot(Petal.Width, Sepal.Width, data = training, colour=Species)

# Training the model
modFit <- train(Species ~ . , method='rpart', data = training)
print(modFit$finalModel)
plot(modFit$finalModel, uniform = TRUE, main='Classification Tree')
text(modFit$finalModel, use.n = TRUE, all=TRUE, cex=0.8)

# A prettier plot
library(rattle)
fancyRpartPlot(modFit$finalModel)

# Predicting
pred <- predict(modFit, newdata = testing)
pred
