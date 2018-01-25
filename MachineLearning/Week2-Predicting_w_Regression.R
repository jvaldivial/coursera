# Week2-Predicting_w_Regression.R
# 
# Predicting with Regression

setwd("~/GitHub/coursera/MachineLearning")


library(caret)
data("faithful")
set.seed(333)

inTraing <- createDataPartition(faithful$waiting, p=0.5, list = FALSE)
trainFaith <- faithful[inTraing, ]
testFaith <- faithful[-inTraing, ]
dim(trainFaith)
dim(testFaith)
head(trainFaith)

plot(trainFaith$waiting, trainFaith$eruptions, pch=19, col='blue', xlab='Waiting', ylab='Duration')
cor(trainFaith)

# creating the Linear Model
lm1 <- lm(eruptions ~ waiting, data = trainFaith)
summary(lm1)
plot(trainFaith$waiting, trainFaith$eruptions, pch=19, col='blue', xlab='Waiting', ylab='Duration')
lines(trainFaith$waiting, lm1$fitted.values, lwd=3)

# Predicting a new value based on the created model
coef(lm1)[1] + coef(lm1)[2] * 80  # This is an example to evaluate at Waiting = 80
newdata <- data.frame(waiting=80:100)   # Creating fake data to 'predict' based on our model
predict(lm1, newdata)   # Get the prediction for the created vector 'newdata'

# Plot Predictions - training and test
par(mfrow=c(1,2))
plot(trainFaith$waiting, trainFaith$eruptions, pch=19, col='blue', xlab='Waiting', ylab='Duration')
lines(trainFaith$waiting, predict(lm1), lwd=3)
plot(testFaith$waiting, testFaith$eruptions, pch=19, col='blue', xlab='Waiting', ylab='Duration')
lines(testFaith$waiting, predict(lm1, newdata = testFaith), lwd=3)

# Prediction Intervals
par(mfrow=c(1,1))
pred1 <- predict(lm1, newdata = testFaith, interval = 'prediction')
ord <- order(testFaith$waiting)
plot(testFaith$waiting, testFaith$eruptions, pch=19, col='blue', xlab='Waiting', ylab='Duration')
matlines(testFaith$waiting[ord], pred1[ord], type = "l", col = c(1,2,2), lty=c(1,1,1), lwd = 3)

# The same process, but EASIER using caret
modelFit <- train(eruptions ~ waiting, data = trainFaith, method = 'lm')
summary(modelFit$finalModel)
