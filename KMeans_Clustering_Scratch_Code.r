# Keiana Dunn
# K-Means Clustering Algorithm from Scratch
# Date: 1/29/19 Update: 2/18/19


# This algorithm will locate the distance between the data points and the center clusters (centroids)

# Overview of R code to K-Means Clustering algorithm for unsupervised learning technique:
# 1. Load provided data
# 2. Create initial random K centroids
# 3. Assign data points to nearest centriod using Euclidean Distance formula 
# 4. Re-calculate data to centroids
# 5. Re-assign centroids to improve on closeness to centroids
# 6. Use K-means function to determine  

# 1. LOAD DATA
# Import package
library(ggplot2)
# Load Data
kdata = read.csv("/home/ktd2001/Downloads/KMeansData_Group3.csv", header=FALSE)
# summary of data and checking for missing values
summary(kdata)
# Dimension of data
dim(kdata)
# Look at the first several rows
head(kdata)
# Column names 
colnames(kdata)

# plot data
ggplot(kdata, aes(V1, V2), colours(TRUE)) + geom_point()

# 2. CREATE INITIAL CENTROIDS

# Use K-means function with for initialization
kcluster <- function(V1, V2, nclus) {
  
  # Create random cluster centers
  xcen <- runif(n = nclus, min = min(V1), max = max(V1))   
  ycen <- runif(n = nclus, min = min(V2), max = max(V2))
  
  # Create data frame where data points and cluster assignment are
  kdata <- data.frame(V1, V2, cluster_coordinates = 1)
  cluster_coordinates <- data.frame(name = 1:nclus, xcen = xcen, ycen = ycen)
 
  finish <- FALSE
  
  while(finish == FALSE) {
    
# 3. ASSIGN DATA POINTS TO NEAREST CENTROID USING EUCLIDEAN DISTANCE FORMULA  
    
   # Assign random clusters with minimum distance to each data point 
    for (i in 1:length(V1)) {
      dist <- sqrt((V1[i]- cluster_coordinates$xcen)^2 + (V2[i]-cluster_coordinates$ycen)^2)
      kdata$cluster_coordinates[i] <- which.min(dist)
    }
    
    xcen_old <- cluster_coordinates$xcen
    ycen_old <- cluster_coordinates$ycen
    
    
# 4. Re-calculate data to centroids
    # Calculate a set of new cluster centers
    for(i in 1:nclus) {
      cluster_coordinates[i,2] <- mean(subset(kdata$V1, kdata$cluster_coordinates == i))
      cluster_coordinates[i,3] <- mean(subset(kdata$V2, kdata$cluster_coordinates == i))
    }
    
    # Interupt the loop if there is no change in cluster coordinates
    if(identical(xcen_old, cluster_coordinates$xcen) & identical(ycen_old, cluster_coordinates$ycen)) finish <- TRUE
  }
  kdata
}

# 5. Re-assign centroids to improve on closeness to centroids
# Compute kmeans function to sample data for a k = 6
cluster <- kcluster(kdata$V1, kdata$V2, nclus=6)
cluster.centers <- aggregate(.~cluster_coordinates, cluster, mean)

# Plot results and have algorithm created clusters   
ggplot(cluster, aes(V1, V2, color= as.factor(cluster_coordinates))) + geom_point(size=1) + 
  geom_point(kdata=cluster.centers, aes(V1, V2, col=as.factor(cluster_coordinates)), pch=9, size=1)
  
### After several runs of the algorithm, the centroids of some of the clusters changed from their intial centers 
### and the cluster numbers also changed from 4, 5 and 6. 6 was the final number of clusters.
  
# 6. Use K-Means function and "Elbow Method" to find optimal number of clusters

# Change dataset name when using K-means function in R 
kdata -> kdata1
head(kdata1)

# Plot kdata1
plot(kdata1)
# We can see 4, 5 or even 6 naturally occuring clusters. I will settle on 6 clusters

# K-means clustering function needs dataset and # of clusters declared. 
kdata1.results <- kmeans( kdata1, centers = 6)

# Show results 
kdata1.results
# Results reveal how many clusters, the size of the clusters, mean of each cluster center, cluster vectors and sum of squares for clusters.

# Isolate to display cluster size information
kdata1.results$size

# Dataset name changed
kdata -> kdata1

# Review data 
head(kdata1) 

# To have kdata return as numeric vector the length of x values
kdata1_scaled <- as.data.frame(lapply(kdata1, scale))

# Seed set for reproducible results
set.seed(100)

# Create loop to plot and determine the optimial number of clusters
mss <- (nrow(kdata1_scaled)-1)*sum(apply(kdata1_scaled,2,var))
  # Vary K parameter from 1-15 
  for (i in 2:15) mss[i] <- sum(kmeans(kdata1_scaled,centers=i)$withinss)
  # Plot graph
plot(1:15, mss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")
## The "Elbow Method" shows K = 6 as the optimial number of clusters

