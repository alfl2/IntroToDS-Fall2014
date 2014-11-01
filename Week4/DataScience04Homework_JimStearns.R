# DataScience04Homework_JimStearns.R
# DataScience 250 Homework04
# Due Date: 1 Nov 2014
# Submitted By: Jim Stearns

# Limitations:
# - Only two dimensions supported.
# - Values not normalized before centroids determined.

# Clear Workspace
rm(list=ls())
# Clear Console:
cat("\014")

# Number of dimensions supported by simpleKMeans
nDims <- 2
print(sprintf("Number of dimensions = %d", nDims))

# Present working directory must contain the instructor-provided test R program.
kmeans_test_filename = "KMeansTest.R"
stopifnot(file.exists(kmeans_test_filename))
# Get the test data from KMeansTest.R
source(kmeans_test_filename)

# Test plotClustering()
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
# The function takes points and initial centroids and returns K-mean centroids.
# The function does not normalize the inputs. 
simpleKMeans <- function(points = testPoints, initialCentroids = testCentroids)
{ 
    # Set initial cluster ids to out-of-range values
    clusterIDs_old <- rep(-1, nrow(points))
        
    # repeat the following processes using a loop.  Use a
    # for loop of size 20 to prevent infinite loops
    maxTries <- 20
    centroids <- initialCentroids
    for (try in 1:maxTries)
    {
          # For each point find its closest cluster centre aka centroid
          clusterIDs <- simpleAssignToCentroids(points, centroids)
          
          # Plot the points and centroids
          plotClustering(points, centroids, clusterIDs)
          
          # If there was no change in cluster assignments, then break out of try loop
          if (sum(clusterIDs != clusterIDs_old) < 1)
          {
              break
          }
          
          # For each cluster of points determine its centroid
          centroids <- simpleDetermineCentroids(points, clusterIDs)
          
          # remember clusterID before clusterID is re-assigned in the next iteration
          clusterIDs_old <- clusterIDs
    } 

    return(centroids)
} # simpleKMeans

# For each cluster of points determine its centroid
# The inputs are the points and the cluster ids of the points
# The output is a vector of the new centroids
simpleDetermineCentroids <- function(points=testPoints, clusterIDs=testClusterID)
{
    # Expecting - requiring - that points have nDims columns.
    stopifnot(ncol(points) == nDims)
    
    # How many centroids will we make?  What is the maximum cluster label? 
    # TODO: assess impact of one centroid not being assigned any points - 
    #   never given a second chance?
    uniqueClusterIDs <- unique(clusterIDs)
    nCentroids <- length(uniqueClusterIDs)
    
    # Create a matrix where each row is a centroid of nDims dimensions
    centroids <- matrix(rep(0, nDims * nCentroids), ncol=nDims)
    
    # For loop through each cluster id 
    for (clusterID in uniqueClusterIDs)
    {
        # Get only the points from this one cluster
        pointsInCluster <- points[clusterIDs==clusterID,]

        # Determine the mean of this cluster in each dimension
        # Assign the mean in the that dimension to the same dimension
        # of this cluster's centroid.
        for (dim in 1:nDims)
        {
            centroids[clusterID, dim] <- mean(pointsInCluster[, dim])
            centroids[clusterID, dim] <- mean(pointsInCluster[, dim])
        }
    } 
    
    return(centroids)
} # simpleDetermineCentroids

# A function that returns the cluster IDs for each point
# The function takes the points
# The function takes centroids 
# The cluster that is closest to each point will determine the cluster ID for that point
# A cluster ID indicates the allegiance of a point to a cluster
simpleAssignToCentroids <- function(points = testPoints, centroids=testCentroids)
{
    nCentroids <- nrow(centroids)
    nPoints <- nrow(points)
    
    # Create a matrix that will contain the squared distances from each point to each centroid
    # The matrix has numberOfPoints rows and numberOfCentroids columns
    distanceSquared <- matrix(nrow=nPoints, ncol=nCentroids)
    
    # Determine the distance from the centroid to each point
    # For loop for each point number
    for (pnt in 1:nPoints)
    {
        # For loop for each centroid number
        for (cntrd in 1:nCentroids)
        {
            # What is the difference between the current point and the current centroid?
            # In other words: What is the vector between the point and centroid?
            deltaPoint <- points[pnt,1:2] - centroids[cntrd, 1:2]
            # What is the distance squared of this vector?
            # In other words: what is the sum of the squares of the vector elements?
            oneDistanceSquared <- sum(deltaPoint^2)
            # If the distance squared was NA then make it infinite
            # Assign the distance squared to the proper element in the matrix created above
            # Handle NA (e.g. one centroid may shield another centroid, which gets no points)
            if (!is.na(oneDistanceSquared)) {
                distanceSquared[pnt, cntrd] <- oneDistanceSquared
            } else {
                distanceSquared[pnt, cntrd] <- Inf
            }
        } # Ends the for loop for each centroid number
    } # Ends the for loop for each point number
    
    # Determine the clusterIDs of the closest
    # Use max.col, obtaining the "min.col())" - smallest distance - by negating the values.
    # (Alternative which())
    clusterIDs <- max.col(-distanceSquared)
    
    return(clusterIDs)
} # simpleAssignToCentroids
