# DataSience05_ModelingInR.R
# 11/7/2014
# Pat Leahy pat@patleahy.com
# Introduction to Data Science - Lecture 5 Assignment
# Classify outcomes using Naive Bayes
# Show Confusion Matrix for Naive Bayes
#
# 1.  Partition 
# a) Add two functions to DataScience05_ModelingHelper.R.  The names of the two 
#    functions are:  QuickAndDirty and SlowAndExact.  The functions take in a 
#    dataframe and a fraction.  The functions return a list of two dataframes.  
#    The names of the two dataframes are trainSet and testSet.  trainSet and 
#    testSet are mutually exclusive cases from the input dataframe.  testSet 
#    contains the fraction of cases as specified by the fraction input.  
#    trainSet contains the rest.  The assignments of cases to testSet or 
#    trainSet are random. 
# b) Run the script:   DataScience05_ModelingInR.R using wrongWayPartition() 
# c) Note  the confusion matrix. 
# d) In DataScience05_ModelingInR.R replace the following line with one of the 
#    functions that partition the data:  
#    dataframeSplit <- wrongWayPartition(dataframe, fractionOfTest=0.4) 
# e) Run the script:   DataScience05_ModelingInR.R  
# f) Note the confusion matrix.   Is it better or worse than before?  
#
# 2. Classification in R 
# a) In DataScience05_ModelingInR.R add code to create a Naive Bayes Model.  
#    You will have to look up the Naive Bayes package "e1071" to determine the 
#    inputs.  Use the formula in DataScience05_ModelingInR.R.  Get help from 
#    the LinkedIn group. 
# b) In DataScience05_ModelingInR.R add code to predict outcomes based on the 
#    Naive Bayes model.  You will have to read the documentation to determine 
#    the "type" parameter.  
#    Answer for yourself:  
#    How many rows are there in the outcomes?  
#    How many columns?  
#    How many columns do you want/need?   
# c) Add code to create a confusion matrix like the one for the logistic 
#    regression.  The two confusion matrices should be similar. 
#

################################################################################
#
# ANSWERS
#
# 1. f) Note the confusion matrix.   Is it better or worse than before?
#
# Very simply, there are more correct predictions when using slowAndExact. 
# Looking a little closer I see that false positives increased when using 
# slowAndExact, however false negatives decreased by a lot more, in both 
# absolute and relative terms, therefore I conclude that using slowAndExact 
# gives us a better model. 
# 
# 2. b) In DataScience05_ModelingInR.R add code to predict outcomes based on the 
#    Naive Bayes model.  You will have to read the documentation to determine 
#    the "type" parameter.  
#    Answer for yourself:  
#    How many rows are there in the outcomes?  
#    How many columns?  
#    How many columns do you want/need?   
# 
# There are 321 rows, one for each input row.
# There are 2 columns. 
# The first is the probability, for each input row, that the output is 0. 
# The second column is the probability, for each input row, that the output is 1. 
# We just want to take the second column.

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
#dataframeSplit <- quickAndDirty(dataframe, fractionOfTest=0.4)
dataframeSplit <- slowAndExact(dataframe, fractionOfTest=0.4)

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
naiveBayesModel <- naiveBayes(formula, data=trainSet)

# Predict the outcomes for the test data
#
# If type = "raw", the conditional a-posterior probabilities for each class are returned, 
# else the class with maximal probability is returned.
#
# predict will return two columns, the first is the probability, for each input row, 
# that the output is 0. The second column is the probability, for each input row, 
# that the output is 1. We just want to take the  second column.
naiveBayesPredictions <- predict(naiveBayesModel, newdata=testSet, type="raw")
predictedProbabilities.NaiveBayes <- naiveBayesPredictions[,2]

###################################################

# Confusion Matrix

threshold = 0.5

#Confusion Matrix for Logistic Regression
predicted.GLM <- as.numeric(predictedProbabilities.GLM > threshold)
print("Confusion Matrix for Logistic Regression")
table(predicted.GLM, actual)

#Confusion Matrix for Naive Bayes
predicted.NaiveBayes <- as.numeric(predictedProbabilities.NaiveBayes > threshold)
"Confusion Matrix Naive Bayes"
table(predicted.NaiveBayes, actual)
###################################################

