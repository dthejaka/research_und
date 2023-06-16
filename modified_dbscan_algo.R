library(distances)
dbscan_optimized <- function(big_df, epsilon, minPts) {
  
  #rownames(big_df) = NULL
  data = big_df[c("longitude", "latitude")]
  n = nrow(data)
  #### Testing
  
  
  #minPts = 3
  
  
  # Compute the distance matrix
  dist_matrix <- as.matrix(distances(data))
  
  # Find neighbors of each point
  neighbors <- lapply(1:nrow(dist_matrix), function(i) {
    which(dist_matrix[i,] <= epsilon)
  })
  
  #big_df[unlist(unique(neighbors[[1000]])),]$owner
  
  
  # Initialize labels
  labels <- rep(0, nrow(data))
  cluster_id <- 0
  
  # Initialize core points
  
  core_points <- c()
  for (i in 1:n) {
    usernames1 = big_df[neighbors[[i]],]$owner ## extracting the owner list in the neighbors
    if (length(unique(usernames1)) >= minPts) {
      core_points <- c(core_points, i)
    }
  }
  
  
  
  #core_points <- which(lengths(unique(big_df[unlist(unique(neighbors[[10]])),]$owner)) >= minPts)
  
  # Expand the clusters
  for (i in core_points) {
    if (labels[i] == 0) {
      cluster_id <- cluster_id + 1
      labels[i] <- cluster_id
      
      # Depth-first search to find connected points
      connected_points <- neighbors[[i]]
      while (length(connected_points) > 0) {
        j <- connected_points[1] ## extracting the first connected point
        connected_points <- connected_points[-1] ## removing that connected point from the connected_point vec
        
        ## checking whether a label has assigned or not if not assigning current cluster_id
        if (labels[j] == 0) {
          labels[j] <- cluster_id
          
          usernames2 = big_df[neighbors[[j]],]$owner
          if (length(unique(usernames2)) >= minPts) { ## expanding the connected_point vec only if j is a core point
            connected_points <- c(connected_points, neighbors[[j]])
          }
        }
      }
    }
  }
  
  # Identify noise points
  noise_points <- which(labels == 0)
  
  # Return results
  results <- list(cluster = labels, noise = noise_points)
  return(results)
}
