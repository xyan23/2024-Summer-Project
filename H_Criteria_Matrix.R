max_heapify <- function(A, i, comp_fun) {
  heap_size <- nrow(A)
  # Left child index
  l <- 2 * i  
  # Right child index
  r <- 2 * i + 1         
  # Make sure this is a complete binary tree
  # Compare between root and left child
  if (l <= heap_size && comp_fun(A[l,],A[i,])) {
    largest <- l
  } else {
    largest <- i
  }
  # Compare between largest and left child
  if (r <= heap_size && comp_fun(A[r,],A[largest,])) {
    largest <- r
  }
  # If largest is not root
  if (largest != i) {
    # Swap the larger child with parent
    key <- A[i,]
    A[i,] <- A[largest,]
    A[largest,] <- key
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


heapsort <- function(A, comp_fun) {
  n <- nrow(A)
  A <- build_max_heap(A, comp_fun)
  for (i in n:2) {
    # Exchange the first element with the ith element
    key <- A[1,]
    A[1,] <- A[i,]
    A[i,] <- key
    # Decrease the heap size by 1
    A[1:(i-1), ] <- max_heapify(A[1:(i-1), , drop = FALSE], 1, comp_fun)
  }
  return(A)
}

####Example
x<-c(2, 9, 7, 6, 5, 8)
y<-c(1, 2, 3, 4, 5, 6)
m<-matrix(c(x,y), nrow = length(x), ncol = 2)

# Order by x-coordinate
compare_function1 <- function(p1, p2) {
  comparison_criterion <- p1[1] > p2[1]
  if(comparison_criterion) {
    TRUE
  } else {
    FALSE
  }
}

build_max_heap(m, compare_function1)
heapsort(m, compare_function1)

#Order by y-coordinate
compare_function2 <- function(p1, p2) {
  comparison_criterion <- p1[2] > p2[2]
  if(comparison_criterion) {
    TRUE
  } else {
    FALSE
  }
}

build_max_heap(m, compare_function2)
heapsort(m, compare_function2)

compare_function3 <- function(p1, p2, ordered_points) {
  # Calculate the distance between two points
  distance <- function(x1, x2) {
    return(sqrt(sum((x1 - x1)^2)))
  }
  # Calculate the minimum distance
  min_distance <- function(p, ordered_points){
    min(sapply(ordered_points, function(point) {
      distance(p, point)
    }))
  }
  
  comparison_criterion <- min_distance(p1, ordered_points) < min_distance(p2, ordered_points)
  
  if(comparison_criterion) {
    TRUE
  } else {
    FALSE
  }
}

#MMD Example
x1 <- c(1, 3, 0, 2, 2)
y1 <- c(5, 3, 1, 2, 0)
m1 <- matrix(c(x1, y1), nrow = length(x), ncol = 2)

build_max_heap(m1, compare_function3)
heapsort(m1, compare_function3)

library("Rcpp")
sourceCpp("H_Criteria_Matrix.cpp")
