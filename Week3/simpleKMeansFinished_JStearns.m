% title: Assignment/Week2/simpleKMeansFinished.m
% author: Jim Stearns, in collaboration with Bethene Britt and Lucas Pedroza.
% due date: 25-Oct-2014
%
%%%% Answers to Assignment Questions 2, 3, and 4
%   2.a. Normalization of dimensions is important in K-means clustering because
%       wide-range dimensions may "drown out" dimensions with smaller ranges of possible values.
%       For example, take a dataset with a human age dimension and an dollar income dimension.
%       Further, reasonably assume that each dimension is equally important.
%       Yet a large value of income, say $100,000, will overwhelm a large value of age, say 100.
%   2.b. Categorical data is encoded in K-means clustering as separate boolean values 
%       for each of the enumerated values. Given a category of fruit with three legal values,
%       apple, orange, and banana, this factor field should be converted into three boolean fields,
%       isApple, isOrange, and isBanana. The alternative, converting the factor variable into an
%       integer field (apple=0, orange=1, banana=2) imposes a numerical priority (bananas are a
%       larger value than an apple), a relative ranking that isn't intended.
%   2.c. Clustering is considered unsupervised learning because it does not rely upon or expect
%       each observation to have an associated response. A response variable is used in supervised 
%       learning to influence our analysis.
%
%   3.  The output of simpleAssignToCentroids is a list of assigned Centroid IDs
%       where the value for 17th row is the Centroid ID to which the 17th point has been assigned.
%
%   4.  The clusters are always in the same order so the centroid for cluster 2
%       is the second element of the list of centroid x-y values returned by
%       simpleDetermineCentroids.
%       
%%%%
% simpleKMeansFinished is a 2D K-means implementation, including normalization.
% The function takes points and initial centroids and returns centroids K-mean centroids
function centroids = simpleKMeans(points, initialCentroids)
    % test:  centroids = simpleKMeans(simplePoints, [0, 0; -1, 0; 0, 1])
    % test:  simpleKMeansTests
    % Get ridiculous values for the initial cluster ids
    clusterIDOld = -1;

    % Maximum number of clustering computation trials to perform
    % End earlier if cluster centroids settle.
    nMaxTrials = 20;

    % Parameters for normalization and de-normalization
    % Determine the minimum and range of the points in both dimensions
    pointMin = min(points);
    pointMax = max(points);
    pointRange = pointMax - pointMin;

    % Normalize points for each dimension for each point:
    % subtract away its minimum and then divide by the range.
    % 
    % Replicating the single row vector to match the number of points isn't strictly required,
    % but it's the clearest presentation of an element-by-element matrix operation.
    normedPoints = points .- repmat(pointMin, size(points, 1), 1); % element-by-element, two N x 2 matrices
    normedPoints = normedPoints ./ repmat(pointRange, size(points, 1), 1);;

    % Normalize Centroids:
    % For each dimension and centroid subtract away the minimum of the dimension that was
    % determined by the points (not the centroids).  Then divide by the range of the dimension
    % determined by the points (not the centroids).
    % 
    % Replicating the 1x2 matrix to match the number of rows of 1st matrix isn't required, as shown here.
    normedCentroids = initialCentroids .- pointMin; % element-by-element, a 3x2 matrix by 1x2 matrix
    normedCentroids = normedCentroids ./ pointRange; % element-by-element, a 3x2 matrix by 1x2 matrix 

    % repeat the following processes using a loop.  Use a for loop to prevent infinite loops
    for (iter1 = 1:nMaxTrials)
        % For each point find its closest cluster centre (centroid)
        clusterID = simpleAssignToCentroids(normedPoints, normedCentroids);
        % If there was no change in cluster assignments, then stop;  Use "break" to break out of the loop
        if (sum(clusterID ~= clusterIDOld) < 1)   
            break;
        end % if
        % For each cluster of points determine its centroid;  The number of clusters is the number of centroids
        normedCentroids = simpleDetermineCentroids(normedPoints, clusterID, size(normedCentroids, 1));
        % remember clusterID before clusterID is re-assigned
        clusterIDOld = clusterID;
        % end the for loop
    end % for

    % Denormalization: for each dimension for each centroid:
    % multiply by range of the dimension and then add the minimum of the dimension
    denormedCentroids = normedCentroids .* pointRange; % 3x2 dot 1x2
    denormedCentroids = denormedCentroids .+ pointMin; % 3x2 dot 1x2

    % End the function
    centroids = denormedCentroids;
    return
