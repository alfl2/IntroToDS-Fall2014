#Gabriel Ramirez
#1.f
#The confusion matrix is much better when methods other than
#"wrongWayParition" are used. 
#
#2.d
#How many rows are there in the outcomes? It depends which method is used. 
#The "raw" method has 2 columns, while the "class" method has only 1
#How many columns do you want/need? You only need 1 columns. Using the 
#"raw" type argument the first columns represents the predictions for 0 and
#the second column represent the predictions for 1. The class variable chooses,
#the predictions for 1 based on a threshold (see code below for both types). 
#however, it can be easily inverted using the ifelse function 

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
dataframeSplit <-QuickAndDirty(dataframe, 0.4)

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


#There are two ways to run the prediction function for 
#the naive bayes model: using the raw and class type arguments.
#The main difference between these two is that raw returns the
#raw percentage for each unique selector value. Using class on the
#other hand, returns the predicted class itself using the 
#"threshold" argument. In this example I demonstrate both ways

#raw
# Create Naive Bayes model
naiveBayesModel <- naiveBayes(formula, data=trainSet)
#get the raw predictions
predictedProbabilities.NB.raw <- predict(naiveBayesModel, newdata=testSet, type="raw")

#class
#create a new dataset for this predictor to use, converting the selector to a factor
trainSet.class <- trainSet
testSet.class <- testSet
trainSet.class$selector  <- as.factor(trainSet.class$selector)
testSet.class$selector <- as.factor(testSet.class$selector)

# Create Naive Bayes model
naiveBayesModel <- naiveBayes(formula, data=trainSet.class)
#predict using threashold
predictedProbabilities.NB.class <- predict(naiveBayesModel, newdata=testSet.class,threshold = .5 ,type="class")


###################################################

# Confusion Matrix

threshold = 0.5

#Confusion Matrix for Logistic Regression
predicted.GLM <- as.numeric(predictedProbabilities.GLM > threshold)
print("Confusion Matrix for Logistic Regression")
table(predicted.GLM, actual)

#Confusion Matrix for Naive Bayes

#raw predictions for 0 
predicted.NB.raw <- as.numeric(predictedProbabilities.NB.raw[,1]  > threshold)
print("Confusion Matrix for Naive Bayes using raw")
table(predicted.NB.raw , actual)

#raw predictions for 1 
predicted.NB.raw <- as.numeric(predictedProbabilities.NB.raw[,2]  > threshold)
print("Confusion Matrix for Naive Bayes using raw")
table(predicted.NB.raw , actual)

#class predictions for 0
print("Confusion Matrix for Logistic Regression")
table(ifelse(predictedProbabilities.NB.class == 1,0,1), actual)

#class predictions for 1
print("Confusion Matrix for Logistic Regression")
table(predictedProbabilities.NB.class, actual)

###################################################
