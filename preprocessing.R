# Load libraries
library(readxl)
library(dplyr)
library(NbClust)
library(factoextra)
library(cluster)

find_optimal_clusters <- function(data, max_clusters = 10) {
  # Calculate and plot elbow method
  elbow_plot <- fviz_nbclust(data, kmeans, method = "wss") +
    ggtitle("Elbow Method")
  print(elbow_plot)
  
  # Calculate and plot silhouette method
  silhouette_plot <- fviz_nbclust(data, kmeans, method = "silhouette") +
    ggtitle("Silhouette Method")
  print(silhouette_plot)
  
  # Calculate gap statistic
  gap_stat <- clusGap(data, FUN = kmeans, K.max = max_clusters, nstart = 25)
  gap_stat_plot <- fviz_gap_stat(gap_stat) +
    ggtitle("Gap Statistic")
  print(gap_stat_plot)
  
  # Find the optimal number of clusters using NbClust
  nb_result <- NbClust(data = data, distance = "euclidean", 
                       min.nc = 2, max.nc = max_clusters, method = "kmeans")
  optimal_k <- nb_result$Best.nc[1]
  
  # Return optimal number of clusters based on these methods
  return(optimal_k)
}

# Function to perform k-means clustering and calculate metrics
perform_kmeans_clustering <- function(data, k) {
  kmeans_result <- kmeans(data, centers = k, nstart = 25)
  BSS <- kmeans_result$betweenss
  TSS <- kmeans_result$totss
  WSS <- kmeans_result$tot.withinss
  BSS_over_TSS <- BSS / TSS
  list(
    kmeans_result = kmeans_result,
    BSS = BSS,
    TSS = TSS,
    WSS = WSS,
    BSS_over_TSS = BSS_over_TSS
  )
}

# Function to calculate silhouette plot
calculate_silhouette <- function(kmeans_result, data) {
  # Calculate the silhouette result
  silhouette_result <- silhouette(kmeans_result$cluster, dist(data))
  
  # Plot silhouette width
  silhouette_plot <- fviz_silhouette(silhouette_result) +
    ggtitle("Silhouette Plot")
  
  # Return the plot and average silhouette width
  list(
    silhouette_plot = silhouette_plot
    )
}

# Opening a new device
dev.new()

# Load the dataset
whiteWine <- read_excel("Whitewine_v6.xlsx")

#Plot the original dataset
boxplot(whiteWine)

# Create a subset of only the first 11 columns
whiteWine_subset <- whiteWine[, 1:11]

# Scale the subset
whiteWine_subset_scaled <- as.data.frame(scale(whiteWine_subset))

#Plot the dataset after first scaling 
boxplot(whiteWine_subset_scaled)

# Define the z-score threshold
z_threshold <- 2

# Clean rows exceeding the threshold
whiteWine_subset_cleaned <- whiteWine_subset_scaled %>%
  filter(rowSums(across(everything(), ~ abs(.) > z_threshold)) == 0)

#Plot the cleaned subset
boxplot(whiteWine_subset_cleaned)

# Rescale the cleaned data
whiteWine_cleaned_scaled <- as.data.frame(scale(whiteWine_subset_cleaned))
whiteWine_cleaned_scaled

#Plot the fully preprocessed dataset
boxplot(whiteWine_cleaned_scaled)

# Find the optimal number of clusters using various methods
optimal_k <- find_optimal_clusters(whiteWine_cleaned_scaled, max_clusters = 10)

#Define the number of k
k=2

# Perform k-means clustering and calculate metrics
kmeans_results <- perform_kmeans_clustering(whiteWine_cleaned_scaled, k)
kmeans_results

#Print the cluster centers
print("Cluster Centers:")
print(kmeans_results$kmeans_result$centers)

# Plot k-means clustering results
fviz_cluster(kmeans_results$kmeans_result, data = whiteWine_cleaned_scaled, ellipse.type = "convex") +
  ggtitle(paste("K-Means Clustering with k =",k))

# Calculate silhouette and plot
silhouette_info <- calculate_silhouette(kmeans_results$kmeans_result,dist(whiteWine_cleaned_scaled))

# Plot silhouette plot
print(silhouette_info$silhouette_plot)
