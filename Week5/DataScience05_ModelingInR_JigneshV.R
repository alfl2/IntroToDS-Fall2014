# DataSience05_ModelingInR.R
# Complete the code:
# Classify outcomes using Naive Bayes
# Show Confusion Matrix for Naive Bayes

# Clear objects from Memory
rm(list=ls())
# Clear Console:
cat("\014")

# Set repeatable random seed
randomNumberSeed <- 4
set.seed(randomNumberSeed)

# Add functions and objects from ModelingHelper.R
source("D:/Jignesh/UW-DataScience/Week5/DataScience05_ModelingHelper.R")

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
#dataframeSplit <- wrongWayPartition(dataframe, fractionOfTest=0.4)
dataframeSplit <- SlowandClean(dataframe, fractionOfTest=0.4)
#dataframeSplit <- QuickandDirty(dataframe, fractionOfTest=0.4)

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
# Need some code here!
naivebayesmodel<-naiveBayes(formula,data=trainSet)
# Predict the outcomes for the test data
# Need some code here!
predictbayesmodel<-predict(naivebayesmodel,newdata=testSet, type="raw")
###################################################

# Confusion Matrix

threshold = 0.5

#Confusion Matrix for Logistic Regression
predicted.GLM <- as.numeric(predictedProbabilities.GLM > threshold)
print("Confusion Matrix for Logistic Regression")
table(predicted.GLM, actual)
#wrongway
#actual
#predicted.GLM  0  1
#0 41 45
#1 54 91
#SimpleAndClean
#actual
#predicted.GLM   0   1
#0  14  14
#1  46 157
#QuickAndDirty
#actual
#predicted.GLM   0   1
#0  13  14
#1  52 163

#Confusion Matrix for Naive Bayes
predicted.NBM <- as.numeric(predictbayesmodel[,2] > threshold)
# Need some code here!
print("Confusion Matrix Naive Bayes")
# Need some code here!
table(predicted.NBM, actual)
###################################################
# WrongWay
#actual
#predicted.NBM   0   1
#0  94 122
#1   1  14
#SimpleAndClean
#actual
#predicted.NBM   0   1
#0  58 100
#1   2  71
#QuickAndDirty
#actual
#predicted.NBM   0   1
#0  63 104
#1   2  73

# 1.F  Note the confusion matrix.  Is it better or worse than before?
#SlowandClean has better result.Next best is QuickandDirty.

# 2B 
#How many rows and columns are there in the outcomes? 
# In the Out come of Predictition through Naives Bayes Model we have 1 dimension with 231 members

#How many columns do you want/need?
# In the Out come of Predictition through Naives Bayes Model we have 1 dimension with 231 members, we need
# In the confusion matrix  First column is probability for the outcome is 0, and second column is probability for 
# the outcome to be 1. We intrested in second column. 
# Note : Based on the result of Logistic regression and NaiyeBayes model, i see
# Linear regression giving better result.
