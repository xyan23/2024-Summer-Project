# Example of using MaxMincpp function
library(Rcpp)

# Source the C++ file
sourceCpp("MaxMin.cpp")

MaxMincpp(matrix(c(1,2,3,11,12,13), nrow = 3, ncol = 2))

#Create random points in unit square
library(spatstat.geom)
  w <- unit.square()
  xy <- stratrand(w, 2, 3)
  plot(w)
  points(xy)
  x<-xy$x
  y<-xy$y
  
  m<-matrix(c(x,y), nrow = length(x), ncol = 2)
  m
  
  #Vertical order
  tao_vert<-order(x)
  #Arrange the matrix in vertical order
  m_tao_vert<-m[tao_vert,]
  browser()
  plot(m_tao_vert[, 1], m_tao_vert[, 2], pch = as.character(1:nrow(m_tao_vert)))
  
  #MMD 
  mmd<-MaxMincpp(m)
  m_mmd<-m[mmd,]
  plot(m_mmd[, 1], m_mmd[, 2], pch = as.character(1:nrow(m_mmd)))
  

