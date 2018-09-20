# Week 1

library(kernlab)
data(spam)
str(spam[ , 1:5])

# Creating the model
set.seed(3435)
trainIndicator <- rbinom(4601, size = 1, prob = 0.5)
table(trainIndicator)

trainSpam <- spam[trainIndicator == 1 , ]
testSpam <- spam[trainIndicator == 0, ]

names(trainSpam)
head(trainSpam)
table(trainSpam$type)

# Plot of Email Type (Spam or not) vs Number of capital Letters in the email
plot(trainSpam$capitalAve ~ trainSpam$type)  # Not clear as the data is too much skewed
plot( log10(trainSpam$capitalAve + 1) ~ trainSpam$type)

# Relationship between predictors
plot(log10(trainSpam[, 1:4] + 1))

# Clustering Analysis
hClust <- hclust(dist(t(trainSpam[ , 1:57])))
plot(hClust)

hClustUpdated <- hclust(dist(t(log10(trainSpam[ , 1:57] + 1))))
plot(hClustUpdated)


# Statistical Modeling
trainSpam$numType <- as.numeric(trainSpam$type) - 1
costFunction <- function (x,y) sum( x != (y > 0.5) )
cvError <- rep(NA, 55)
library(boot)
for (i in 1:55) {
  lmFormula <- reformulate(names(trainSpam)[i], response = "numType")
  glmFit <- glm(lmFormula, family = "binomial", data = trainSpam)
  cvError[i] <- cv.glm(trainSpam, glmFit, costFunction, 2)$delta[2]
}

# Which predictor has the lower Cross Validation Error?
names(trainSpam)[which.min(cvError)]

# Using the best model from the group ("charDollar" variable)
predictionModel <- glm(numType ~ charDollar, family = "binomial", data = trainSpam)

# Get predictions on the Test Set
predictionTest <- predict(predictionModel, testSpam)
predictedSpam <- rep("nonspam", dim(testSpam)[1])

# Classify as "spam" for those with prob > 0.5
predictedSpam[predictionModel$fitted > 0.5] = "spam"

# Classification Table
table(predictedSpam, testSpam$type)
errorRate <- (61 + 458)/(1346 + 458 + 61 + 449)
errorRate
