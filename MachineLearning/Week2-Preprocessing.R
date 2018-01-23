# Week2-Preprocessing.R
# 
# Preprocessing

setwd("~/GitHub/coursera/MachineLearning")

library(caret)
library(kernlab)
data(spam)

inTrain <- createDataPartition(y=spam$type, p=0.75, list = FALSE)
training <- spam[inTrain, ]
testing <- spam[-inTrain, ]
dim(testing)

hist(training$capitalAve, main='', xlab='average capital run length')
mean(training$capitalAve)
sd(training$capitalAve)

# Standarizing the variable (mean=0, sd=1)
trainCapAve <- training$capitalAve
trainCapAveS <- (trainCapAve - mean(trainCapAve)) / sd(trainCapAve)
mean(trainCapAveS)
sd(trainCapAveS)

testCapAve <- testing$capitalAve
testCapAveS <- (testCapAve - mean(trainCapAve)) / sd(trainCapAve)
mean(testCapAveS)
sd(testCapAveS)

preObj <- preProcess(training[, -58], method = c('center','scale'))  # Variable 58 = type
trainCapAveS <- predict(preObj, training[, -58])$capitalAve
mean(trainCapAveS)
sd(trainCapAveS)

testCapAveS <- predict(preObj, testing[, -58])$capitalAve
mean(testCapAveS)
sd(testCapAveS)

# Passing the Preprocessing directly into the training model operation
set.seed(32343)
modelFit <- train(type ~ . , data = training, preProcess=c('center', 'scale'), method='glm')
modelFit


# Box-Cox transformation
preObj <- preProcess(training[, -58], method = c('BoxCox'))
trainCapAveS <- predict(preObj, training[, -58])$capitalAve
par(mfrow=c(1,2))
hist(trainCapAveS)
qqnorm(trainCapAveS)


#
# Imputing Data -- Handling Missing Values
#

set.seed(13343)

# Make some NA values
training$capAve <- training$capitalAve
selectNA <- rbinom(dim(training)[1], size = 1, prob = 0.05) == 1
training$capAve[selectNA] <- NA

# Impute and standarize
preObj <- preProcess(training[, -58], method = 'knnImpute')
library(RANN)
capAve <- predict(preObj, training[, -58])$capAve

capAveTruth <- training$capitalAve
capAveTruth <- (capAveTruth - mean(capAveTruth)) / sd(capAveTruth)
