# DataScience04Homework_incomplete.R
# DataScience 250 Homework04

# Clear Workspace
rm(list=ls())
# Clear Console:
#cat("\014")

# Get the test data from KMeansTest.R
source("D:/Jignesh/UW-DataScience/Week4/KMeansTest.R")

# Test plotClustering()
# Expect: A plot with randomly assigned points and centroids

# Test simpleDetermineCentroids()
simpleDetermineCentroids() # Testing
# Expect: 
#             [,1]        [,2]
# [1,]  0.02392857  0.02464286
# [2,] -0.10321429  0.10071429
# [3,]  0.08370370 -0.13000000

# Test simpleAssignToCentroids
simpleAssignToCentroids() # Testing
# Expect: 
# [1] 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3
# [23] 3 3 3 3 3 3 3 3 3 3 3 3 3 1 3 3 3 1 3 3 3 3
# [45] 3 3 3 3 3 3 3 3 3 3 1 3 3 1 3 3 3 3 2 2 3 3
# [67] 2 2 3 3 3 3 2 3 3 3 3 3 3 3 1 3 3

# Test simpleKMeans()
simpleKMeans() # Real Test
# Expect:  
#            [,1]       [,2]
# [1,] -0.0332000 -0.6508000
# [2,] -1.5163158 -1.0057895
# [3,]  0.7610256  0.9071795

# simpleKMeans is a 2D K-means implementation.  
# The function takes points and initial centroids and 
# returns K-mean centroids.  The function does not
# normalize the inputs. 
simpleKMeans <- function(points = testPoints, centroids = testCentroids)
{ 
  # Get ridiculous values for the initial cluster ids
  clusterIDOld<--1
  # Determine the number of centroids
  noOfCentroid=nrow(centroids)
  # repeat the following processes using a loop.  Use a
  # for loop of size 20 to prevent infinite loops
  for(iter in 1:20)
  {
	  # For each point find its closest cluster centre aka centroid
    clusterID = simpleAssignToCentroids(points, centroids);
	  # Plot the points and centroids
	  plotClustering(points, centroids, clusterID)
	  # If there was no change in cluster assignments, then stop
	  if(sum(clusterID!=clusterIDOld)<1)
	  {
	  # Use "break" to break out of the loop
      break
	  } # ends the if clause
	  # For each cluster of points determine its centroid
    centroids = simpleDetermineCentroids(points, clusterID)
	  # remember clusterID before clusterID is re-assigned in the next iteration
    clusterIDOld = clusterID
  } # ends the for loop
  # Return the centroids
  centroids
}

# For each cluster of points determine its centroid
# The inputs are the points and the cluster ids of the points
# The output is a vector of the new centroids
simpleDetermineCentroids <- function(points=testPoints, clusterID=testClusterID)
{
  # How many centroids will we make?  What is the maximum cluster label? 
  numberOfCentroids<-max(clusterID)
  
  # Create a matrix where each row is a centroid of 2 dimensions
  centroids<-matrix(nrow=numberOfCentroids,ncol=2)
  
  # For loop through each cluster id 
  for(pointNo in 1:numberOfCentroids)
  {
	  # Get only the points from one cluster
    #clusterID==pointNo returns TRUE/FALSE based on the match between clusterid vector and iterator (pointno:1/2/3). Based on the index
    # corresponding to TRUE, subset of points gets assigned to pointsInTheCluster
    pointsInTheCluster<-points[clusterID==pointNo,]
    # Determine the mean of that cluster in the 1st dimension
    dim1mean<-mean(pointsInTheCluster[,1])
	  # Determine the mean of that cluster in the 2nd dimension
    dim2mean<-mean(pointsInTheCluster[,2])
	  # Assign the mean in the 1st dimension to the centroid
    centroids[pointNo,1]<-dim1mean
	  # Assign the mean in the 2nd dimension to the centroid
    centroids[pointNo,2]<-dim2mean
  } # Ends the for loop through each cluster id
  
  # Return the centroids
  centroids
  
} # simpleDetermineCentroids

# A function that returns the cluster IDs for each point
# The function takes the points
# The function takes centroids 
# The cluster that is closest to each point will determine the cluster ID for that point
# A cluster ID indicates the allegiance of a point to a cluster
simpleAssignToCentroids <- function(points = testPoints, centroids=testCentroids)
{
 
  # Get the number of centroids
  numberOfCentroids<-nrow(centroids)
  
  # Get the number of points
  numberOfpoints<-nrow(points)
  # Create a matrix that will contain the squared distances from each point to each centroid
  # The matrix has numberOfPoints rows and numberOfCentroids columns
  distanceSquared<-matrix(nrow=numberOfpoints,ncol=numberOfCentroids)
  # Determine the distance from the centroid to each point
  # For loop for each point number
  for(pointNo in 1:numberOfpoints)
  {
	# For loop for each centroid number
    for(centroidNo in 1:numberOfCentroids)
    {
	  # What is the difference between the current point and the current centroid?
	  # In other words: What is the vector between the point and centroid?
      deltaPoint<-points[pointNo,]-centroids[centroidNo,]
	  # What is the distance squared of this vector?
	  # In other words: what is the sum of the squares of the vector elements?
	    oneDistanceSquared<-sum(deltaPoint^2)
	  # If the distance squared was NA then make it infinite
	  # Assign the distance squared to the proper element in the matrix created above
	  if(is.na(oneDistanceSquared))
	  {
	    distanceSquared[pointNo,centroidNo]<-Inf
	  }
    else
    {
      distanceSquared[pointNo,centroidNo]<-oneDistanceSquared  
    }
    
	  # End the for loop for each centroid number
    } # Ends the for loop for each centroid number
  } # Ends the for loop for each point number
  # Determine the clusterIDs of the closest
  # Additional comment : This line actually returns min distance centroid id for each points
  max.col(-distanceSquared)
  
} # simpleAssignToCentroids

