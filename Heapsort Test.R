library("Rcpp")
source("H_Criteria_Matrix.R")
sourceCpp("H_Criteria_Matrix.cpp")

#' Test the R and c++ function provide the same result
#'
#' @param f1 R function.
#' @param f2 c++ function.
#' @param matrix A n by 2 matrix where each row represents a point.
#' @param comp_fun A compare function which states compare criterion in heap.
#'
#' @return True or False
#' @export
#'
#' @examples
#' x1 <- c(2, 9, 7, 6, 5, 8)
#' y1 <- c(1, 2, 3, 4, 5, 6)
#' m1 <- matrix(c(x1,y1), nrow = length(x1), ncol = 2)
#' test_result(heapsort, heapsort_c, m1, compare_function1)
#' 
#' set.seed(1)
#' x2 <- runif(5)
#' y2 <- runif(5)
#' m2 <- matrix(c(x2,y2), nrow = length(x2), ncol = 2)
#' test_result(heapsort, heapsort_c, m2, compare_function1)
#' 

test_result <- function(f1, f2, matrix, comp_fun) {
  result1 <- f1(matrix, comp_fun)
  result2 <- f2(matrix)
  no_row = nrow(result1)
  no_col = ncol(result1)
  zero_matrix = matrix(0, nrow = no_row, ncol = no_col)
  if (all((result1 - result2) == zero_matrix)) {
    print("The results are equivalent")
  }
}

