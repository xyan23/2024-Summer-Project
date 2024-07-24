#Remove top node, then heapify (Another algorithm to conduct heap)
#Input: Heap(vector), compare function
#Output: Heap(vector)
RemoveMin <- function(heap, comp_fun) {
  size <- length(heap)
  if (size == 1) {
    # Remove the element
    heap <- heap[-1]
  } else {
    # Swap the root with the last element
    key <- heap[1]
    heap[1] <- heap[size]
    heap[size] <- key
    # Remove the last element
    heap <- heap[-length(heap)]
    # Down-heap bubbling
    u <- 1
    # Make sure this is a complete binary tree
    while ((2 * u) <= length(heap)) { 
      # Left child index
      v <- 2 * u
      # Compare left and right child
      if ((v + 1) <= length(heap) && comp_fun(heap[v + 1], heap[v])) {
        v <- v + 1
      }
      # If the child is smaller than the current node
      if (comp_fun(heap[v], heap[u])) {
        # Swap the child with parent
        key <- heap[u]
        heap[u] <- heap[v]
        heap[v] <- key
        u <- v
      } else {
        break
      }
    }
  }
  return(heap)
}

#Example
Heap<-c(9,6,8,2,5,7)

compare_function <- function(p1, p2) {
  comparison_criterion <- p1[1] < p2[1]
  if(comparison_criterion) {
    return(p1)
  } else {
    return(p2)
  }
}

RemoveMin(Heap,compare_function)

