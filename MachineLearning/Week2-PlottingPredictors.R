# Week2-ex2.R
# 
# Plotting Predictors

setwd("~/GitHub/coursera/MachineLearning")

library(ISLR)
library(ggplot2)
library(caret)

data(Wage)
summary(Wage)

inTrain <- createDataPartition(Wage$wage, p=0.75, list=FALSE)
training <- Wage[inTrain,]
testing <- Wage[-inTrain, ]
dim(training)
dim(testing)

featurePlot(x=training[ , c('age', 'education', 'jobclass')], y = training$wage, plot = 'pairs')
qplot(age, wage, data = training)
qplot(age, wage, colour=jobclass, data = training)

qq <- qplot(age, wage, colour=education, data=training)
qq + geom_smooth(method = 'lm', formula = y ~ x)

library(Hmisc)
cutWage <- cut2(training$wage, g=3)
table(cutWage)

p1 <- qplot(cutWage, age, data = training, fill=cutWage, geom='boxplot')
p1

p2 <- qplot(cutWage, age, data = training, fill=cutWage, geom=c('boxplot', 'jitter'))
library(grid)
library(gridExtra)
grid.arrange(p1, p2, ncol=2)

# Pivot Tables
t1 <- table(cutWage, training$jobclass)
t1
prop.table(t1, 1)  # Proportion on rows
prop.table(t1, 2)  # Proportion on columns
prop.table(t1)     # Proportion on the whole table


# Smoothing Plots
qplot(wage, colour=education, data = training, geom = 'density')
