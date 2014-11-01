# DataScience04Homework_WernerColangelo.R
# DATASCI 250: Introduction to Data Science (4690)
# Autumn 14
# Instructor: Ernst Henle
#
# Homework 4
#
# Submitted by:
# Werner Colangelo
# wernercolangelo@gmail.com

# Clear Workspace
rm(list=ls())
# Clear Console:
cat("\014")

# Get the test data from KMeansTest.R
source("KMeansTest.R")

plotClustering()
# Expect: A plot with randomly assigned points and centroids

# Test simpleDetermineCentroids()
# Expect: 
#             [,1]        [,2]
# [1,]  0.02392857  0.02464286
# [2,] -0.10321429  0.10071429
# [3,]  0.08370370 -0.13000000

# Test simpleAssignToCentroids()
# Expect: 
# [1] 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3
# [23] 3 3 3 3 3 3 3 3 3 3 3 3 3 1 3 3 3 1 3 3 3 3
# [45] 3 3 3 3 3 3 3 3 3 3 1 3 3 1 3 3 3 3 2 2 3 3
# [67] 2 2 3 3 3 3 2 3 3 3 3 3 3 3 1 3 3

# Test simpleKMeans()
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
  clusterIDOld = -1
  # Determine the number of centroids
  numCentroids = nrow(centroids)
  # repeat the following processes using a loop.  Use a
  # for loop of size 20 to prevent infinite loops
  for (idx in 1 : 20)
  {
	  # For each point find its closest cluster centre aka centroid
	  clusterID = simpleAssignToCentroids(points, centroids)
	  # Plot the points and centroids
	  plotClustering(points, centroids, clusterID)
	  # If there was no change in cluster assignments, then stop
	  if(all(clusterIDOld == clusterID))
	  {
	  # Use "break" to break out of the loop
      break
	  } # ends the if clause
	  # For each cluster of points determine its centroid
	  centroids <- simpleDetermineCentroids(points,clusterID)
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
  numCentroids = max(clusterID)
  # Create a matrix where each row is a centroid of 2 dimensions
  centroids <- matrix(nrow = numCentroids, ncol = 2)
  # For loop through each cluster id 
  for (cluster in 1:numCentroids)
  {
	  # Get only the points from one cluster
    clusterPoints = points[clusterID == cluster,]
	  # Determine the mean of that cluster in the 1st dimension
    mean1 = mean(clusterPoints[,1])
	  # Determine the mean of that cluster in the 2nd dimension
    mean2 = mean(clusterPoints[,2])
	  # Assign the mean in the 1st dimension to the centroid
    centroids[cluster, 1] = mean1
	  # Assign the mean in the 2nd dimension to the centroid
    centroids[cluster, 2] = mean2
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
  numCentroids = nrow(centroids)
  # Get the number of points
  numPoints = nrow(points)
  # Create a matrix that will contain the squared distances from each point to each centroid
  # The matrix has numberOfPoints rows and numberOfCentroids columns
  distances = matrix(nrow = numPoints, ncol = numCentroids)
  # Determine the distance from the centroid to each point
  # For loop for each point number
  for (point in 1 : numPoints)
  {
	# For loop for each centroid number
    for (centroid in 1 : numCentroids)
    {
	  # What is the difference between the current point and the current centroid?
	  # In other words: What is the vector between the point and centroid?
      distance = points[point,] - centroids[centroid,]
	  # What is the distance squared of this vector?
	  # In other words: what is the sum of the squares of the vector elements?
      distSquared = sum(distance ^ 2)
	  # If the distance squared was NA then make it infinite
	  # Assign the distance squared to the proper element in the matrix created above
      if(is.na(distSquared))
      {
        distances[point, centroid] = inf
      }
      else
      {
        distances[point, centroid] = distSquared
      }
	  # End the for loop for each centroid number
    } # Ends the for loop for each centroid number
  } # Ends the for loop for each point number
  # Determine the clusterIDs of the closest
  max.col(-distances)
} # simpleAssignToCentroids

