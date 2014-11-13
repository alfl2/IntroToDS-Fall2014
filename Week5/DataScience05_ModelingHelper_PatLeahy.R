# DataSience05_ModelingHelper.R
# 11/7/2014
# Pat Leahy pat@patleahy.com
# Introduction to Data Science - Lecture 5 Assignment
#
# See DataSience05_ModelingInR.R

installAndLoadModeling <- function()
{
  reposURL <- "http://cran.cs.wwu.edu/"
  reposURL <- "http://cran.fhcrc.org/"
  
  # install package with naive bayes if not alreay installed
  if (!require("e1071")) {install.packages("e1071", dep=TRUE, repos=reposURL)} else {" e1071 is already installed "}
  # Now that the package is installed, we want to load the package so that we can use its functions
  library(e1071)
  
  # install package with neuralnet if not alreay installed
  if (!require("neuralnet")) {install.packages("neuralnet", dep = TRUE, repos=reposURL)} else {" neuralnet is already installed "}
  # Now that the package is installed, we want to load the package so that we can use its functions
  library(neuralnet)
  
  # install package with decision trees if not alreay installed
  if (!require("rpart")) {install.packages("rpart", dep = TRUE, repos=reposURL)} else {"rpart is already installed "}
  # Now that the package is installed, we want to load the package so that we can use its functions
  library(rpart)
}

GetDemoData <- function(dataNumber)
{
  if (dataNumber == 1)
  {
    # Get Data
    # http://archive.ics.uci.edu/ml/datasets/ILPD+%28Indian+Liver+Patient+Dataset%29
    # Liver Patients:  selector == 1;  Non-patient:  selector == 2
    dataURL <- "http://archive.ics.uci.edu/ml/machine-learning-databases/00225/Indian%20Liver%20Patient%20Dataset%20(ILPD).csv"
    dataframe <- read.csv(dataURL, header=F)
    headers <- c("age", "gender", "tb", "db", "alkphos", "sgpt", "sgot", "tp", "alb", "agratio", "selector")
    # Assign the headers to the data frame
    names(dataframe) <- headers
    
    # Clean Data:  Change binaries to 1/0 and remove 
    dataframe$gender <- as.numeric(dataframe$gender == 'Female')
    dataframe$selector <- as.numeric(dataframe$selector == 1)
    dataframe <- na.omit(dataframe)
    
    dataframe <- dataframe[order(dataframe$tb), ]
    
    # Shape the data
    formula <- selector ~ age + gender + tb + db + alkphos + sgpt + sgot + tp + alb + agratio
    targetColumnNumber <- 11
    
  } else if(dataNumber == 2)
  {
    # Get Data
    url <- "http://archive.ics.uci.edu/ml/machine-learning-databases/mammographic-masses/mammographic_masses.data"
    # Download a rectangular dataset without a header to create a dataframe
    dataframe <- read.csv(url, header=FALSE, stringsAsFactors=FALSE)
    #dataframe <- read.csv(url, header=FALSE)
    
    # Assign a vector of real names to the data frame;  The following names were from:
    # http://archive.ics.uci.edu/ml/machine-learning-databases/mammographic-masses/mammographic_masses.names
    # See Attribute Information
    headers <- c("BIRADS", "Age", "Shape", "Margin", "Density", "Severity")
    names(dataframe) <- headers
    
    # Clean the data
    
    # Create numeric vectors from factors.  Warning, non-numeric data are converted to NA
    dataframe <- fact2Num(dataframe) # sapply(dataframe, as.numeric)
    # Remove all NA
    dataframe <- na.omit(dataframe)
    # Adjust Outliers
    dataframe$BIRADS[dataframe$BIRADS > 5] <- 5
    dataframe$BIRADS[dataframe$BIRADS < 1] <- 1
    
    dataframe <- dataframe[order(dataframe$Density), ]
    
    # Shape the data
    formula <- Severity ~ BIRADS + Age + Shape + Margin + Density
    targetColumnNumber <- 6
  } else
  {
    dataframe <- data.frame()
    formula <- y ~ x
    targetColumnNumber = 0
  }
  
  # Package data into a list
  modelingData <- list(dataframe=dataframe, formula=formula, targetColumnNumber=targetColumnNumber)
  return(modelingData)
} # GetDemoData

fact2Num <- function(x)
  #############################################################################
#' Converts a factor to a numeric vector
#' 
#' \code {fact2Num} Takes a vector or a data frame and converts each vector
#' into a numeric vector.  NAs will be introduced where there was no number.
#' This means charachters are converted to NAs.
#'
#' Args:
#'   @param x:  A data frame or vector
#'
#' Returns:
#'   @return A dataframe or vector.  All vectors are numeric
#'   
#############################################################################
{
  # If the input x is a dataframe then apply the function to
  # every vector of the data frame
  if (is.data.frame(x))
  {
    x <- as.data.frame(sapply(x, fact2Num))
  }
  # Only apply this function to a factor
  else if (is.factor(x))
  {
    # this line converts levels in x to numbers
    as.numeric(levels(x)[x])
  }
  # this line converts characters in x to numbers 
  else if (is.character(x))
  {
    as.numeric(x)
  }
  else
  {
    x
  }
}

#############################################################################
#' Partition Modeling Data - A Wrong Way
#'
#' This approach doesn't randomly split the data into two sets. Therefore
#' the training and test may contain different correlations.
#'
#' Args:
#'   @param dataSet:  A data frame containing the data to split into test and
#'                    traning data.
#'
#'   @param fractionOfTest: The test fraction.
#'
#' Returns:
#'   @return: A list of two dataframes. The names of the two dataframes are 
#'            trainSet and testSet. trainSet and testSet are mutually 
#'            exclusive cases from the input dataframe. testSet contains the
#'            fraction of cases as specified by the fraction input. trainSet
#'            contains the rest
#'   
#############################################################################
quickAndDirty <- function(dataSet, fractionOfTest = 0.3)
{
  # 1. Take the first fraction of the data as test data
  # 2. Take the rest of the data as training data
  numberOfRows <- nrow(dataSet)
  numberOfTestRows <- fractionOfTest * numberOfRows
  testFlag <- 1:numberOfRows <= numberOfTestRows
  testSet <- dataSet[testFlag, ]
  trainSet <- dataSet[!testFlag, ]
  dataSetSplit <- list(trainSet=trainSet, testSet=testSet)
  return(dataSetSplit)
}


#############################################################################
#' Partition Modeling Data - A Correct Way
#' 
#' This approach randomly split the data into two sets.Therefore
#' the training and test are both good samples of the data set.
#'
#' Args:
#'   @param dataSet:  A data frame containing the data to split into test and
#'                    traning data.
#'
#'   @param fractionOfTest: The test fraction.
#'
#' Returns:
#'   @return: A list of two dataframes. The names of the two dataframes are 
#'            trainSet and testSet. trainSet and testSet are mutually 
#'            exclusive cases from the input dataframe. testSet contains the
#'            fraction of cases as specified by the fraction input. trainSet
#'            contains the rest
#'   
#############################################################################
slowAndExact <- function(dataSet, fractionOfTest = 0.3)
{
  # 1. Generate random number for each case 
  # 2.  Create Flag to partition cases: find quantile 
  #   a)  Sort random numbers 
  #   b)  Determine threshold where quantile is the test fraction 
  #   c)  Compare random numbers to threshold. 
  # 4.  Apply Flag for Test Data selection 
  # 5.  Apply Flag for Train Data selection 

  randoms <- runif(nrow(dataSet)) 
  sortedRandoms <- sort(randoms) 
  testThreshold  <- sortedRandoms[length(sortedRandoms)*fractionOfTest] 
  testSelection <- randoms <= testThreshold 
  testSet  <- dataSet[testSelection, ]  
  trainSet <- dataSet[!testSelection, ]
  dataSetSplit <- list(trainSet=trainSet, testSet=testSet)
  return(dataSetSplit)
}