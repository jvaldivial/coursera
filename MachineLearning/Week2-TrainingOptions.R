# Week2-ex1.R

setwd("~/GitHub/coursera/MachineLearning")

library(caret)
library(e1071)
library(kernlab)
data(spam)

# Creating the data partitions for training & testing
inTrain <- createDataPartition(y=spam$type, p=0.75, list = FALSE)
training <- spam[inTrain, ]
testing <- spam[-inTrain, ]
dim(training)

# Fitting the model
set.seed(32343)
modelFit <- train(type ~ . , data = training, method="glm")

modelFit
modelFit$finalModel

# Evaluating the model
predictions <- predict(modelFit, newdata=testing)
predictions

confusionMatrix(predictions, testing$type)


#####
# Data Slicing
#####

library(caret)
library(e1071)
library(kernlab)
data(spam)

inTrain <- createDataPartition(y=spam$ty, p=0.75, list = FALSE)
training <- spam[inTrain, ]
testing <- spam[-inTrain, ]
dim(training)

set.seed(32323)
folds <- createFolds(y=spam$type, k=10, list = TRUE, returnTrain = TRUE)
sapply(folds, length)


# -> With Resampling
set.seed(32323)
folds <- createResample(y=spam$type, times = 10, list = TRUE)
sapply(folds,length)

# -> Time Slices
set.seed(32323)
tme <- 1:1000
folds <- createTimeSlices(y=tme, initialWindow = 20, horizon = 10)
names(folds)
folds$train[[1]]
folds$test[[1]]



