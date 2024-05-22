#include <Rcpp.h>
using namespace Rcpp;
using namespace std;
// Constructs a heap from the elements of a given vector
// by the bottom-up algorithm
// Input: A vector H
// Output: A heap H

// [[Rcpp::export]]
NumericVector HeapBottomUp(NumericVector H) {
  NumericVector C = Rcpp::clone(H);
  int n = C.size();  // Number of elements in the heap
  for (int i = floor(n / 2) - 1; i >= 0; i--) {
    int k = i;
    double v = C[k];
    bool heap = false;
    while (!heap && 2 * k + 1 < n) {
      int j = 2 * k + 1;  // Left child
      if (j + 1 < n && C[j] < C[j + 1]) {
        j=j+1;
      }
      if (v >= C[j]) {
        heap = true;
      } else {
        C[k] = C[j];
        k = j;
      }
    }
    C[k] = v;
  }
  return C;
}

// [[Rcpp::export]]
NumericVector Heapsort(NumericVector H){
  NumericVector C = Rcpp::clone(H);
  int l = C.size();
  NumericVector sortvec(C.size(),0);
  int m = 0;
  while (int i = C.size() > 1){
    C = HeapBottomUp(C);
    int n = C.size();
    double v_n= C[n-1];
    sortvec[m] = C[0];
    C[0] = v_n;
    C.erase(n-1);
    m=m+1;
  }
  sortvec[l-1]=C[0];
  return sortvec;
}

