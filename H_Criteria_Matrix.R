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
    A[1:(i-1),] <- max_heapify(A[1:(i-1),], 1, comp_fun)
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

library("Rcpp")
sourceCpp("H_Criteria_Matrix.cpp")
