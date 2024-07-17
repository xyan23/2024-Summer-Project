max_heapify <- function(A, i, comp_fun) {
  heap_size <- nrow(A)
  # Left child index
  l <- 2 * i  
  # Right child index
  r <- 2 * i + 1         
  # Make sure this is a complete binary tree
  # Compare between root and left child
  if (l <= heap_size && comp_fun(A[l, ], A[i, ])) {
    largest <- l
  } else {
    largest <- i
  }
  # Compare between largest and left child
  if (r <= heap_size && comp_fun(A[r, ], A[largest, ])) {
    largest <- r
  }
  # If largest is not root
  if (largest != i) {
    # Swap the larger child with parent
    key <- A[i, ]
    A[i, ] <- A[largest, ]
    A[largest, ] <- key
    # Recursively heapify for sub-tree
    A <- max_heapify(A, largest, comp_fun)
  }
  
  return(A)
}

build_max_heap <- function(A, comp_fun) {
  n <- nrow(A)
  # Check all the parental nodes
  for (i in floor(n / 2):1) {
    A <- max_heapify(A, i, comp_fun)
  }
  return(A)
}

#Calculate the distance 
distance <- function(p1, p2) {
  return(sqrt(sum((p1 - p2)^2)))
}

#Calculate the minimum distance
min_distance <- function(p, ordered_points){
  min(sapply(ordered_points, function(point) {
    distance(p, point)
  }))
}

heap_mmd_ver1 <- function(A, comp_fun) {
  # Start with a random point
  start_index <- sample(1:nrow(A), 1)
  # Store ordered points
  ordered_points <- A[start_index, , drop = FALSE]
  # Store remaining points
  remaining_points <- A[-start_index, , drop = FALSE]
  
  while (nrow(remaining_points) > 1){
   # Find minimum distance
   min_dist_vec <- c()
   for (i in 1:nrow(remaining_points)) {
     dist_vec <- c()
     for (j in 1:nrow(ordered_points)){
       dist_vec[j] <- distance(remaining_points[i, ], ordered_points[j, ])
     }
     min_dist_vec[i] <- min(dist_vec)
   }
  
   remaining_points <- cbind(remaining_points, min_dist_vec)
   
   remaining_points <- build_max_heap(remaining_points, comp_fun)
   
   ordered_points <- rbind(ordered_points, remaining_points[1, -3])
   
   remaining_points <- remaining_points[-1, -3, drop = FALSE]
  }
  ordered_points <- rbind(ordered_points, remaining_points[1, -3])
  return(ordered_points)
}

heap_mmd_ver2 <- function(A, comp_fun) {
  # Start with a random point
  start_index <- sample(1:nrow(A), 1)
  # Store ordered points
  ordered_points <- A[start_index, , drop = FALSE]
  # Store remaining points
  remaining_points <- A[-start_index, , drop = FALSE]

  count <- 0
  while (nrow(remaining_points) > 1){
    count <- count + 1
    
    if (count == 1) {
      # Find the  distance from the first ordered point
      dist_vec <- c()
      for (i in 1:nrow(remaining_points)) {
        dist_vec[i] <- distance(remaining_points[i, ], ordered_points[count, ])
      }
      
      remaining_points <- cbind(remaining_points, dist_vec)
      remaining_points <- build_max_heap(remaining_points, comp_fun)
      ordered_points <- rbind(ordered_points, remaining_points[1, -3])
      remaining_points <- remaining_points[-1, , drop = FALSE]
      
    } else {
      # Find the maximum minimum distance for the rest of the points
      for (i in 1:nrow(remaining_points)) {
        dist <- distance(remaining_points[i,-3], ordered_points[count, ])
        if (dist < remaining_points[i,3]) {
          remaining_points[i,3] <- dist
        } 
      }  
      remaining_points <- build_max_heap(remaining_points, comp_fun)
      ordered_points <- rbind(ordered_points, remaining_points[1, -3])
      remaining_points <- remaining_points[-1, , drop = FALSE]
      
    }
  }
  ordered_points <- rbind(ordered_points, remaining_points[1, -3])
  return(ordered_points)
}

compare_mmd <- function(p1, p2) {
  comparison_criterion <- p1[3] > p2[3]
  if(comparison_criterion) {
    TRUE
  } else {
    FALSE
  }
}

#MMD Example
x <- c(1, 3, 0, 2, 2)
y <- c(5, 3, 1, 2, 0)
m <- matrix(c(x, y), nrow = length(x), ncol = 2)

heap_mmd_ver1(m, compare_mmd)
heap_mmd_ver2(m, compare_mmd)

library(microbenchmark)
microbenchmark(m1 = heap_mmd_ver1(m, compare_mmd), m2 = heap_mmd_ver2(m, compare_mmd))
