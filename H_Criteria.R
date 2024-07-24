#' Two algorithm conducting heapsort
#'
#' @param vec A vector.
#' @param comp_fun A compare function which provides the compare criteria for heap.
#'
#' @return An ordered vector.
#' @export
#'
#' @examples
#### Algorithm 1
heap_bottom_up <- function(vec,comp_fun) {
  n <- length(vec)
  # Locate the parental nodes
  f <- floor(n / 2)
  for (i in f:1) {
    # Index
    k <- i
    v <- vec[i]
    heap <- FALSE
    # Make sure this is a complete binary tree
    while (!heap && 2 * k <= n) {
      j <- 2 * k
      # Left child index
      if (j < n) {
        # Compare between left and right child
        if (comp_fun(vec[j], vec[j + 1])) {
          j <- j + 1
        }
      }
      # Compare parent and child nodes
      if (comp_fun(vec[j], v)) {
        heap <- TRUE
      } else {
        # Swap the child with parent
        vec[k] <- vec[j]
        k <- j
      }
    }
    vec[k] <- v
  }  
  return(vec)
} 

heapsort <- function(vec, comp_fun) {
  l <- length(vec)
  # Create empty vector to restore ordered element 
  sortvec <- numeric(length(vec))
  i = 1
  # Repeat for n-1 times
  while (length(vec) > 1) {
    # Heapify the vector
    vec <- heap_bottom_up(vec,comp_fun)
    n <- length(vec)
    # Swap the root with the last element
    v_n <- vec[n]
    # Restore the root of each heap
    sortvec[i] <- vec[1]
    vec[1] <- v_n
    # Delete the last element
    vec <- vec[1:n-1]
    i <- i+1
  }
  sortvec[l] <- vec[1]
  return(sortvec)
}

####Example
x<-c(2,9,7,6,5,8)

compare_function <- function(p1, p2) {
  comparison_criterion <- p1 < p2
  if(comparison_criterion) {
    TRUE
  } else {
    FALSE
  }
}

heap_bottom_up(x,compare_function)
heapsort(x,compare_function)

#### Algorithm 2
max_heapify <- function(A, i, comp_fun) {
  heap_size <- length(A)
  # Left child index
  l <- 2 * i  
  # Right child index
  r <- 2 * i + 1         
  # Make sure this is a complete binary tree
  # Compare between root and left child
  if (l <= heap_size && comp_fun(A[l],A[i])) {
    largest <- l
  } else {
    largest <- i
  }
  # Compare between largest and left child
  if (r <= heap_size && comp_fun(A[r],A[largest])) {
    largest <- r
  }
  # If largest is not root
  if (largest != i) {
    # Swap the larger child with parent
    key <- A[i]
    A[i] <- A[largest]
    A[largest] <- key
    # Recursively heapify for sub-tree
    A <- max_heapify(A, largest, comp_fun)
  }
  
  return(A)
}

build_max_heap <- function(A, comp_fun) {
  n <- length(A)
  # Check all the parental nodes
  for (i in floor(n / 2):1) {
    A <- max_heapify(A, i, comp_fun)
  }
  return(A)
}


heapsort <- function(A, comp_fun) {
  n <- length(A)
  A <- build_max_heap(A, comp_fun)
  for (i in n:2) {
    # Exchange the first element with the ith element
    key <- A[1]
    A[1] <- A[i]
    A[i] <- key
    # Decrease the heap size by 1
    A[1:(i-1)] <- max_heapify(A[1:(i-1)], 1, comp_fun)
  }
  return(A)
}

####Example
x<-c(2,9,7,6,5,8)

compare_function <- function(p1, p2) {
  comparison_criterion <- p1 > p2
  if(comparison_criterion) {
    TRUE
  } else {
    FALSE
  }
}

build_max_heap(x, compare_function)
heapsort(x, compare_function)


