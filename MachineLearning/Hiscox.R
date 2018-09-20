# Predicting with Regression Multiple Covariates

library(ggplot2)
library(caret)
library(data.table)

# The file below is the provided data
# I'm assuming that there is a column called "premium", with the values of the premiums
setwd('/what/ever/directory')
portfolio <- fread('portfolio.csv')   
portfolio <- as.data.frame(portfolio)


# Creating the Training and Testing datasets (70% and 30% respectively)
inTrain <- createDataPartition(y=portfolio$premium, p=0.7, list = FALSE)
training <- portfolio[inTrain,]
testing <- portfolio[-inTrain, ]

# Creating the Linear Model using Caret and considering all available variables as predictors
modelFit <- train(premium ~ . , data = training, method = 'lm')
finModel <- modelFit$finalModel
print(finModel)  # See the model on screen


# Predicting for the Testing dataset
pred <- predict(modelFit, testing)
qplot(premium, pred, data=testing)  # See on screen how the prediction looks like

