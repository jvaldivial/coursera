# Week2-Predicting_w_Regression_MultipleCovariates.R
# 
# Predicting with Regression Multiple Covariates

setwd("~/GitHub/coursera/MachineLearning")

library(ISLR)
library(ggplot2)
library(caret)
data(Wage)

Wage <- subset(Wage, select = -c(logwage))   # Take out 'logwage' as this is the variable to predict
summary(Wage)

inTrain <- createDataPartition(y=Wage$wage, p=0.7, list = FALSE)
training <- Wage[inTrain,]
testing <- Wage[-inTrain, ]
dim(training)
dim(testing)

featurePlot(x=training[, c('age', 'education', 'jobclass')],  y=training$wage, plot='pairs' )
qplot(age, wage, data = training)
qplot(age, wage, data = training, colour=jobclass)
qplot(age, wage, data = training, colour=education)

modelFit <- train(wage ~ age + education + jobclass, data = training, method = 'lm')
finModel <- modelFit$finalModel
finModel
print(finModel)

# Diagnostics
plot(finModel, 1, pch=19, cex=0.5, col='#00000010')

# Plotting the residuals by variables not used in the model
# -- This is handy to identify which other variables might be included in the model
qplot(finModel$fitted.values, finModel$residuals, colour=race, data = training)

# Plotting the residuals by the Index (this is the row number)
plot(finModel$residuals, pch=19)

# Predicted vs Truth in test set
pred <- predict(modelFit, testing)
qplot(wage, pred, colour=year, data = testing)

# Modeling with ALL covariates
modFitAll <- train(wage ~ . , data = training, method='lm')
pred <- predict(modFitAll, testing)
qplot(wage, pred, data=testing)
