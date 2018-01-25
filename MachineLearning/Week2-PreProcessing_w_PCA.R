# Week2-PreProcessing_w_PCA.R
# 
# Preprocessing

setwd("~/GitHub/coursera/MachineLearning")

library(caret)
library(kernlab)
data(spam)

inTrain <- createDataPartition(y=spam$type, p=0.75, list = FALSE)
training <- spam[inTrain, ]
testing <- spam[-inTrain, ]
dim(training)
dim(testing)

# Assessing the correlations between the Covariates (Features)
M <- abs(cor(training[, -58]))
diag(M) <- 0
which(M > 0.85, arr.ind = TRUE)
names(spam)[c(32,34)]
plot(spam[, 32], spam[, 34 ])

# Rotate the axis to get most of the variability in a single covariate
x <- 0.71 * training$num415 + 0.71 * training$num857
y <- 0.71 * training$num415 - 0.71 * training$num857
plot(x, y)

# PCA in R
smallSpam <- spam[, c(34,32)]
prComp <- prcomp(smallSpam)
plot(prComp$x[,1] , prComp$x[,2])
prComp$rotation

# PCA on SPAM data
typeColour <- ((spam$type == 'spam') * 1) + 1 
prComp <- prcomp(log10(spam[, -58] + 1))
plot(prComp$x[, 1], prComp$x[, 2], col=typeColour, xlab='PC1', ylab='PC2')

# PCA with Caret
preProc <- preProcess(log10(spam[, -58] +1), method = 'pca', pcaComp = 2)
dim(preProc$rotation)
spamPC <- predict(preProc, log10(spam[,-58] + 1))
plot(spamPC$PC1, spamPC$PC2, col=typeColour)

# PreProcessing with PCA
preProc <- preProcess(log10(training[, -58] +1), method='pca', pcaComp = 2)
trainingPC <- predict(preProc, log10(training[, -58])+1)
modelFit <- train(training$type ~ ., method = 'glm', data = trainingPC)  # Bug reported in predict

testPC <- predict(preProc, log10(testing[, -58] +1))
confusionMatrix(testing$type, predict(modelFit, testPC))

# PCA as part of the Training process (quicker approach)
modelFit <- train(type ~ . , method='glm', preProcess = 'pca', data = training)
confusionMatrix(testing$type, predict(modelFit, testing))


