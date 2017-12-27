# Sample Code for Machine Learning Course @ Coursera

setwd("~/git/coursera/MachineLearning")
library(caret)
library(kernlab)
library(e1071)
data("spam")

# Subsetting the data set for Trainig and Testing
inTrain <- createDataPartition(y=spam$type, p=0.75, list = FALSE)
training <- spam[inTrain, ]
testing <- spam[-inTrain, ]

# Fitting a GLM model for the varable TYPE
set.seed(32343)
modelFit <- train(type ~ . , data = training, method="glm" )
modelFit

# Reviewing the output of the fitted model
modelFit$finalModel

# Predicting on the testing data set
predictions <- predict(modelFit, newdata=testing)
predictions

# Assessing the model using the Confusion Matrix
confusionMatrix(predictions, testing$type)

# Doing Cross Validation
set.seed(32323)
folds <- createFolds(y=spam$type, k=10, list = TRUE, returnTrain = TRUE)
sapply(folds, length)

# Creating Time Slices
set.seed(32323)
tme <- 1:1000
folds <- createTimeSlices(y=tme, initialWindow = 20, horizon = 10)
names(folds)
folds$train[[1]]
folds$test[[1]]
