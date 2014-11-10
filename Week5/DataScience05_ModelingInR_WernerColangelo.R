# DataSience05_ModelingInR.R
# Complete the code:
# Classify outcomes using Naive Bayes
# Show Confusion Matrix for Naive Bayes

# DATASCI 250: Introduction to Data Science (4690)
# Autumn 14
# Instructor: Ernst Henle
#
# Homework 5
#
# Submitted by:
# Werner Colangelo
# wernercolangelo@gmail.com

# Clear objects from Memory
rm(list=ls())
# Clear Console:
cat("\014")

# Set repeatable random seed
randomNumberSeed <- 4
set.seed(randomNumberSeed)

# Add functions and objects from ModelingHelper.R
source("DataScience05_ModelingHelper.R")

# Install and Load Packages (may already be installed)
installAndLoadModeling()

# Get cleaned data
modelingData <- GetDemoData(1)
dataframe <- modelingData$dataframe
# Data also included a formula and the number of the target column
formula <- modelingData$formula
targetColumnNumber <- modelingData$targetColumnNumber

# Partition data between training and testing sets
# Replace the following line with a function that partitions the data correctly
# print("WrongWayPartition")
# dataframeSplit <- wrongWayPartition(dataframe, fractionOfTest=0.4)
print("SlowAndExact")
dataframeSplit <- SlowAndExact(dataframe, fractionOfTest=0.4)
# print("QuickAndDirty")
# dataframeSplit <- QuickAndDirty(dataframe, fractionOfTest=0.4)

testSet <- dataframeSplit$testSet
trainSet <-dataframeSplit$trainSet

# Actual Test Outcomes
actual <- testSet[,targetColumnNumber]
positiveState <- 1
isPositive <- actual == positiveState

###################################################

# Logistic Regression (glm, binomial)

# http://data.princeton.edu/R/glms.html
# http://www.statmethods.net/advstats/glm.html
# http://stat.ethz.ch/R-manual/R-patched/library/stats/html/glm.html
# http://www.stat.umn.edu/geyer/5931/mle/glm.pdf

# Create logistic regression;  family="binomial" means logistic regression
glmModel <- glm(formula, data=trainSet, family="binomial")
# Predict the outcomes for the test data
predictedProbabilities.GLM <- predict(glmModel, newdata=testSet, type="response")

###################################################

# Naive Bayes

# http://cran.r-project.org/web/packages/e1071/index.html
# http://cran.r-project.org/web/packages/e1071/e1071.pdf

# Create Naive Bayes model
# nBModel <- naiveBayes(formula, data = dataframe, laplace = 0 , trainSet)
nBModel <- naiveBayes(formula, trainSet)

# Predict the outcomes for the test data
#predictedProbabilities.nB <- predict(nBModel, type = "raw", newdata=testSet, threshold = 0.5)
predictedProbabilities.nB <-predict(nBModel, newdata=testSet, type="raw")

###################################################

# Confusion Matrix

threshold = 0.5

#Confusion Matrix for Logistic Regression
predicted.GLM <- as.numeric(predictedProbabilities.GLM > threshold)
print("Confusion Matrix for Logistic Regression")
table(predicted.GLM, actual)

#Confusion Matrix for Naive Bayes
predicted.nB <- as.numeric(predictedProbabilities.nB[,2] > threshold)
print("Confusion Matrix Naive Bayes")
table(predicted.nB, actual)
###################################################

# WrongWayPartition used

# Confusion Matrix for Logistic Regression
# actual
# predicted.GLM  0  1
# 0 41 45
# 1 54 91

# Confusion Matrix Naive Bayes
# actual
# predicted.nB   0   1
# 0  94 122
# 1   1  14


# SlowAndExact used

# Confusion Matrix for Logistic Regression
# actual
# predicted.GLM   0   1
# 0  14  14
# 1  46 157

# Confusion Matrix Naive Bayes
# actual
# predicted.nB   0   1
# 0  58 100
# 1   2  71

# Answers
# 1F - Note the confusion matrix.  Which is better or worse?
# SlowAndExact produces many fewer false negatives, although it still produced false positives. But
# the reduction in false negatives was much larger than the increase in false positives, so it
# does seem to be a better was of partitioning the data.

#2B - How many rows are there in the outcomes? How many columns do you want/need?
# There are 231 rows and 2 columns.
# The first column is the probability that the output is 0 for that input row.
# The second column is the probabilitythat the output is 1 for that input row.
# We just want to take the second column.

