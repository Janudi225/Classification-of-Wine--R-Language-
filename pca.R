# Load libraries
library(readxl)
library(dplyr)
library(factoextra)
library(cluster)
library(NbClust)
library(fpc)

# Function to perform PCA and select components based on variance threshold
perform_pca <- function(data, variance_threshold = 0.85) {
  pca_result <- prcomp(data, scale. = TRUE)
  
  #Calculate eigen values
  eigenvalues <- pca_result$sdev^2
  print("Eigenvalues:")
  print(eigenvalues)
 
  #Calculate eigen vectors
  eigenvectors<-pca_result$rotation
  #Print eigen vectors
  print(eigenvectors)
  
  #Calculate cumulative score per PC
  cumulative_score <- cumsum(eigenvalues) / sum(eigenvalues)
  #Print cumulative scores
  print(cumulative_score)
  
  #Plot scree plot
  plot((eigenvalues) / sum(eigenvalues),ylab="Cumulative Score",type="b",main="Scree Plot")
  
  #Determining the number of PCs needed for 85% of variance
  num_pcs <- which(cumulative_score >= variance_threshold)[1]
  
  #Transform data with selected PCs
  transformed_data <- pca_result$x[, 1:num_pcs]
  
 head(transformed_data)
  
  list(transformed_data = transformed_data, num_pcs = num_pcs)
}

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

# Function to calculate silhouette width and plot
calculate_silhouette <- function(kmeans_result, data) {
  # Calculate the silhouette result
  silhouette_result <- silhouette(kmeans_result$cluster, dist(data))
  
  # Calculate the average silhouette width
  avg_silhouette_width <- mean(silhouette_result[, 2])
  
  # Print average silhouette width
  print(paste("Average Silhouette Width Score:", avg_silhouette_width))
  
  # Plot silhouette width
  silhouette_plot <- fviz_silhouette(silhouette_result) +
    ggtitle("Silhouette Plot")
  
  # Return the plot and average silhouette width
  list(
    silhouette_plot = silhouette_plot,
    avg_silhouette_width = avg_silhouette_width
  )
}

# Function to calculate the Calinski-Harabasz index for k-means clustering results
calculate_calinski_harabasz <- function(kmeans_result, data) {
  calinski_harabasz_index <- cluster.stats(dist(data), kmeans_result$cluster)$ch
  return(calinski_harabasz_index)
}

# Load the dataset
whiteWine <- read_excel("Whitewine_v6.xlsx")

# Create a subset of only the first 11 columns
whiteWine_subset <- whiteWine[, 1:11]

# Scale the subset
whiteWine_subset_scaled <- as.data.frame(scale(whiteWine_subset))

# Define the z-score threshold
z_threshold <- 2

# Clean rows exceeding the threshold
whiteWine_subset_cleaned <- whiteWine_subset_scaled %>%
  filter(rowSums(across(everything(), ~ abs(.) > z_threshold)) == 0)

boxplot(whiteWine_subset_cleaned)

# Perform PCA and get transformed data
pca_result <- perform_pca(whiteWine_subset_cleaned)
head(pca_result)

# Find the optimal number of clusters
optimal_k <- find_optimal_clusters(pca_result$transformed_data, max_clusters = 10)

#Define the value of k
k=2

# Perform k-means clustering and calculate metrics
kmeans_results <- perform_kmeans_clustering(pca_result$transformed_data, k)
kmeans_results

#Print the cluster centers
print("Cluster Centers:")
print(kmeans_results$kmeans_result$centers)

# Calculate silhouette and plot
silhouette_info <- calculate_silhouette(kmeans_results$kmeans_result, pca_result$transformed_data)

# Plot silhouette plot
print(silhouette_info$silhouette_plot)

# Plot k-means clustering results
fviz_cluster(kmeans_results$kmeans_result, data = pca_result$transformed_data, ellipse.type = "convex") +
  ggtitle(paste("K-Means Clustering with k =", optimal_k))

# Calculate the Calinski-Harabasz index
calinski_harabasz_index <- calculate_calinski_harabasz(kmeans_results$kmeans_result, pca_result$transformed_data)
print(paste("Calinski-Harabasz Index:", calinski_harabasz_index))

# Plotting Calinski-Harabasz index for different k values
k_values <- 2:10  # Define a range of k values 
calinski_harabasz_values <- sapply(k_values, function(k) {
  
  # Perform k-means clustering with k clusters
  kmeans_temp <- kmeans(pca_result$transformed_data, centers = k, nstart = 25)
  
  # Calculate the Calinski-Harabasz index
  cluster.stats(dist(pca_result$transformed_data), kmeans_temp$cluster)$ch
})

# Plot Calinski-Harabasz index
plot(k_values, calinski_harabasz_values, type = "b", xlab = "Number of Clusters (k)",
     ylab = "Calinski-Harabasz Index", col = "blue", pch = 19, cex = 1.2)
title("Calinski-Harabasz Index for Different Numbers of Clusters")

