# Week2-CovariateCreation.R
# 
# Preprocessing

setwd("~/GitHub/coursera/MachineLearning")

library(ISLR)
library(caret)
data(Wage)
inTrain <- createDataPartition(y=Wage$wage, p=0.7, list = FALSE)
training <- Wage[inTrain, ]
testing <- Wage[-inTrain, ]
dim(training)
dim(testing)

# How to handle Categorial variables -> dummyVars
table(training$jobclass)
dummies <- dummyVars(wage ~ jobclass, data = training)
head(predict(dummies, newdata = training))

# How to handle variables without variability (NZV)
nsv <- nearZeroVar(x = training, saveMetrics = TRUE)
nsv

# Introducing NON-LINEAR models in the set
library(splines)
bsBasis <- bs(training$age, df = 3)  # Calculate the variable, the square and the cube (normalised first)
bsBasis

lm1 <- lm(wage ~ bsBasis, data = training)
plot(training$age, training$wage, pch=19, cex=0.5)
points(training$age, predict(lm1, newdata = training), col='red', pch=19, cex=0.5)

predict(bsBasis, age=testing$age)
