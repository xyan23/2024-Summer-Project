#include <Rcpp.h>
using namespace Rcpp;
using namespace std;

void max_heapify_c(NumericMatrix A, int i, Function comp_fun) {
  int heap_size = A.nrow();
  // Left child index
  int l = 2 * i + 1; 
  // Right child index
  int r = 2 * i + 2; 
  int largest; 
  NumericVector key;       
  // Make sure this is a complete binary tree
  // Compare between root and left child
  if (l <= heap_size && comp_fun(A[l],A[i])) {
    largest = l;
  } else {
    largest = i;
  }
  // Compare between largest and left child
  if (r <= heap_size && comp_fun(A[r],A[largest])) {
    largest = r;
  }
  // If largest is not root
  if (largest != i) {
    // Swap the larger child with parent
    key = A[i];
    A[i] = A[largest];
    A[largest] = key;
    // Recursively heapify for sub-tree
    A = max_heapify_c(A, largest, comp_fun);
  }
  
  return(A);
}

void build_max_heap_c(NumericMatrix A, Function comp_fun) {
  int n = A.nrow();
  // Check all the parental nodes
  for (int i = n / 2 - 1; i >= 0; i--) {
    A = max_heapify_c(A, i, comp_fun);
  }
  return(A);
}


void heapsort_c(NumericMatrix A, Function comp_fun) {
  int n = A.nrow();
  A = build_max_heap_c(A, comp_fun);
  NumericVector key;
  for (int i = n - 1; i >= 1; i--) {
    // Exchange the first element with the ith element
    key = A[0];
    A[0] = A[i];
    A[i] = key;
    // Decrease the heap size by 1
    A[1:(i-1)] <- max_heapify_c(A[1:(i-1)], 1, comp_fun);
  }
  return(A);
}