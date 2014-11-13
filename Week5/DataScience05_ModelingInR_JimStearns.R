# DataScience05_ModelingInR.R
# Course: UW PCE Data Science, Introduction, Fall 2014
# Student: Jim Stearns
# Due Date: 14-Nov-2014

# Assignment: Complete the code:
# - Classify outcomes using Naive Bayes
# - Show Confusion Matrix for Naive Bayes
# Also:
# - Apply to both supplied datasets: not only Indian Liver Patient, but Mammographic Masses as well.

# Clear objects from Memory
rm(list=ls())
# Clear Console:
cat("\014")

#setwd("~/GoogleDrive/Learning/Courses/UWPCE-DataScience/Course1_Intro/Repo/IntroToDS-Fall2014/Week5")

# Set repeatable random seed
# Note: the partitioning methods also calls set.seed(), so this may be a nop.
randomNumberSeed <- 1234
set.seed(randomNumberSeed)

# Add functions and objects from ModelingHelper.R
source("DataScience05_ModelingHelper_JimStearns.R")

# Install and Load Packages (may already be installed)
installAndLoadModeling()

doTheAnalysis <- function(modelingData)
{
    # Modeling Data also includes a formula and attributes of the dataset
    formula <- modelingData$formula
    dataframe <- modelingData$dataframe
    targetColumnNumber <- modelingData$targetColumnNumber
    predictorColumnNumbers <- modelingData$predictorColumnNumbers
    datasetDescription <- modelingData$datasetDescription    
    
    print(sprintf("Structure of dataset %s", datasetDescription))
    str(dataframe)
    
    splitIntoTrainAndTest <- function(dataframe, testFraction, FUN=slowAndExact)
    {
        dataframesplit <- FUN(dataframe, testFraction)
        return(dataframesplit)
    }
    
    # Partition data between training and testing sets
    # Replace the following line with a function that partitions the data correctly
    #dataframeSplit <- wrongWayPartition(dataframe, fractionOfTest=0.4)
    #?#dataframeSplit <- splitIntoTrainAndTest(dataframe, 0.4, wrongWayPartition)
    dataframeSplit <- splitIntoTrainAndTest(dataframe, 0.4, slowAndExact)
    
    testSet <- dataframeSplit$testSet
    trainSet <-dataframeSplit$trainSet

    # Outcome needs to be a category, a classification. A factor.
    # Target column (e.g. selector in Indian Liver Patient dataset) may be numeric.
    # Issue and solution pointed out by fellow student Tom Lewin.
    if (class(trainSet[,targetColumnNumber]) != "factor")
    {
        print(sprintf(paste("Converting outcome (col#%d) in both training and ",
                            "test set to a factor rather than a numeric."), 
                      targetColumnNumber))

        trainSet[,targetColumnNumber] <- as.factor(trainSet[,targetColumnNumber])
        testSet[,targetColumnNumber] <- as.factor(testSet[,targetColumnNumber])
        
        print(sprintf("Class of outcome column is now %s", class(trainSet[,targetColumnNumber])))
    }
    else
    {
        print(sprintf("No need to convert col#%d (%s) to factor", 
                      targetColumnNumber, class(trainSet[targetColumnNumber,1])))
    }
    
    ##
    # Exploration of training dataset
    ##
    doPairPlots <- function(dset, predictorColumnIndices, colIdxOutcome, description) 
    {
        # Simplification for now: outcome is binary - two categories
        stopifnot(length(unique(dset[,colIdxOutcome])) == 2)
        
        # Use color to show selector value: Selector==0: red; Selector==1: green
        pairs(dset[predictorColumnIndices], main=description,
              pch=21, bg=c("red", "green3"))
    }
    
    filename <- paste(datasetDescription, "TrainingSetPairs_JimStearns.pdf")
    filename <- gsub(" ", "", filename)
    pdf(filename)
    doPairPlots(trainSet, predictorColumnNumbers, targetColumnNumber, 
                paste(datasetDescription, "Training Set"))
    dev.off()
    doPairPlots(trainSet, predictorColumnNumbers, targetColumnNumber, 
                paste(datasetDescription, "Training Set"))
    
    showPredictorCorrelation <- function(dset, predictorColumnIndices, colIdxOutcome, description) 
    {
        # Parameters assume that predictors are a block, possibly preceded and followed by 
        # outcome, row numbers, etc.
        
        # Correlation among predictors
        # A rough measure of correlation of predictors
        print(sprintf("Predictor Correlation: %s", description))
        print(round(abs(cor(trainSet[,predictorColumnIndices])),2))
    
        # TODO: List all correlations greater than say, +/-0.75. Once
    }
    showPredictorCorrelation(trainSet, predictorColumnNumbers, targetColumnNumber, 
                             paste(datasetDescription, "Training Set"))
    
    # Actual Test Outcomes
    actual <- testSet[,targetColumnNumber]
    positiveState <- 1
    isPositive <- actual == positiveState
    
    ##
    # Modeling - Logistic Regression, Naive Bayes
    ##
    
    print(sprintf("Formula used: %s", as.character(formula)))
    ###################################################
    
    # Logistic Regression (glm, binomial)
    
    # http://data.princeton.edu/R/glms.html
    # http://www.statmethods.net/advstats/glm.html
    # http://stat.ethz.ch/R-manual/R-patched/library/stats/html/glm.html
    # http://www.stat.umn.edu/geyer/5931/mle/glm.pdf
    
    # Create logistic regression;  family="binomial" means logistic regression
    glmModel <- glm(formula, data=trainSet, family="binomial")
    # Predict the outcomes for the test data
    glmPredictedProbabilities <- predict(glmModel, newdata=testSet, type="response")
    
    ###################################################
    
    # Naive Bayes
    
    # http://cran.r-project.org/web/packages/e1071/index.html
    # http://cran.r-project.org/web/packages/e1071/e1071.pdf
    
    #!# Interesting experiment: does removing highly correlated predictors
    #!# improve Naive Bayes performance?
    #!# I.e. Use formula minus high correlation predictors db, sgot, alb
    #!#formula <- selector ~ age + gender + tb + alkphos + sgpt + tp + agratio
    
    # Create Naive Bayes model
    # type=raw returns the conditional a-posterior probabilities for each class (category)
    nbModelRaw <- naiveBayes(formula, data=trainSet, laplace=0, type='raw')
    
    # Predict the outcomes for the test data
    nbPredictedProbabilities <- predict(nbModelRaw, newdata=testSet, type="raw")
    ###################################################
    
    # Confusion Matrices
    
    #Confusion Matrix for Logistic Regression
    threshold = 0.5
    glmPredicted <- as.numeric(glmPredictedProbabilities > threshold)
    print(sprintf("%s: Confusion Matrix for Logistic Regression, Threshold=%1.2f", 
                  datasetDescription, threshold))
    print(table(glmPredicted, actual))
    
    #Confusion Matrix for Naive Bayes
    # nbPredictedProbabilities has two columns: 
    # [0]: probability of being Selector==0
    # [1]: probability of being Selector==1.
    # With two possible selector outcomes, these two probabilities equal 1.
    # Let's check probability of true
    nbPredicted <- as.numeric(nbPredictedProbabilities[,2] > threshold)
    print(sprintf("%s: Confusion Matrix for Naive Bayes, Threshold=%1.2f", 
                  datasetDescription, threshold))
    tableNbRaw <- table(nbPredicted, actual)
    print(tableNbRaw)
    
    # Cross-check: Do the Naive Bayes using type="class" rather than type="raw"
    # The default, if type parameter is omitted, is "class". But specify anyway.
    nbModelClass <- naiveBayes(formula, data=trainSet, laplace=0, type="class")
    nbPredictedClass <- predict(nbModelClass, newdata=testSet, type="class") 
    tableNbClass <- table(nbPredictedClass, actual)
    tableCompareResult <- all(tableNbRaw == tableNbClass)
    print(sprintf("The two Naive Bayes calculations yield the same confusion matrix: %s",
                  tableCompareResult))
    ###################################################
}

# Get cleaned modeling data for Indian Liver Patient dataset, and analyze.
modelingData <- GetDemoData(1)
doTheAnalysis(modelingData)

# Get cleaned modeling data for Mammographic Masses dataset, and analyze.
modelingData <- GetDemoData(2)
doTheAnalysis(modelingData)

