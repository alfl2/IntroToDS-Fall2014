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
dataframeSplit <- SlowAndExact(dataframe, fractionOfTest=0.4)

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

# Make the 
trainSet$selector <- as.factor(trainSet$selector)
testSet$selector <- as.factor(testSet$selector)

# Create Naive Bayes model
model <- naiveBayes(formula, data=trainSet)

# Predict the outcomes for the test data
predicted.Bayes <- predict(model, testSet)

###################################################

# Confusion Matrix

threshold = 0.5

# Confusion Matrix for Logistic Regression
predicted.GLM <- as.numeric(predictedProbabilities.GLM > threshold)
print("Confusion Matrix for Logistic Regression")
table(predicted.GLM, actual)

# Results for WrongWayPartition
# > print("Confusion Matrix for Logistic Regression")
# [1] "Confusion Matrix for Logistic Regression"
# > table(predicted.GLM, actual)
# actual
# predicted.GLM  0  1
#             0 41 45
#             1 54 91

# Results for SlowAndExact
# > print("Confusion Matrix for Logistic Regression")
# [1] "Confusion Matrix for Logistic Regression"
# > table(predicted.GLM, actual)
# actual
# predicted.GLM   0   1
#             0  14  14
#             1  46 157

# Confusion Matrix for Naive Bayes
print("Confusion Matrix for Naive Bayes")
table(predicted.Bayes, actual)

# > print("Confusion Matrix for Naive Bayes")
# [1] "Confusion Matrix for Naive Bayes"
# > table(predictions, actual)
# actual
# predictions   0   1
#           0  58 111
#           1   2  60
###################################################
