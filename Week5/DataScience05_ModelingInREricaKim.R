# DataSience05_ModelingInR.R
#Erica Kim

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
dataframeSplit <- wrongWayPartition(dataframe, fractionOfTest=0.4)

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
PP.GLM <- predict(glmModel, newdata=testSet, type="response")

###################################################

# Naive Bayes

# http://cran.r-project.org/web/packages/e1071/index.html
# http://cran.r-project.org/web/packages/e1071/e1071.pdf

# Create Naive Bayes model
bayesModel <- naiveBayes(formula, trainSet)

# Predict the outcomes for the test data
PP.NBM <-predict(bayesModel,newdata=testSet,type="raw")
head(PP.NBM)
nrow(PP.NBM)
###################################################
# Confusion Matrix with wrongWayPartition

threshold = 0.5

#Confusion Matrix for Logistic Regression
predicted.GLM <- as.numeric(PP.GLM > threshold)
print("Confusion Matrix for Logistic Regression")
table(predicted.GLM, actual)
#results
#   0  1
#0 41 45
#1 54 91

#Confusion Matrix for Naive Bayes
predicted.NBM <- as.numeric(PP.NBM > threshold)
"Confusion Matrix Naive Bayes"
table(predicted.NBM)
#results
#predicted.NBM
#0   1 
#231 231
###################################################
# Confusion Matrix with Slow and Exact
#Confusion Matrix for Logistic Regression
predicted.GLM <- as.numeric(PP.GLM > threshold)
print("Confusion Matrix for Logistic Regression")
table(predicted.GLM, actual)
#results  
#   0   1
#0  17  15
#1  43 156

#Confusion Matrix for Naive Bayes
predicted.NBM <- as.numeric(PP.NBM > threshold)
"Confusion Matrix Naive Bayes"
table(predicted.NBM)
#results
#predicted.NBM
#0   1 
#231 231 
###################################################
#1F compare confusion matrix.  Which is better or worse?
#Slow and Exact has results in more accurate data given that it gives less false positive. 
#So Slow and Exact is a better method for modeling
#2B 
#How many rows are there in the outcomes? 
#231 row
#How many columns? 
# 2 columns
#How many columns do you want/need?
#I want the second column since this column indiciates the probability of correct prediction
