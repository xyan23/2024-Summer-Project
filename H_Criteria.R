x<-c(1,8,5,7,4,2)
y<-c(2,4,10,3,5,6)
m<-matrix(c(x,y), nrow = length(x), ncol = 2)

#HeapBottomUp with matrix
HeapBottomUp_m<-function(M){
  n<-nrow(M)
  f<-floor(n/2)
  M_c<-M
  for (i in f:1){
    k<-i
    v<-M_c[i,]
    heap<-FALSE
    while (!heap && 2*k<=n){
      j<-2*k
      if (j < n){
        if (M_c[j,][1]< M_c[(j+1),][1]){
          j<-j+1
        }
      }
      if (v[1]>=M_c[j,][1]){
        heap<-TRUE
      }
      else {
        M_c[k,][1]<-M_c[j,][1]
        k<-j
      }
    }
    M_c[k,]<-v
  }
  order_h<-match(M_c[,1],M[,1])
  H<-M[order_h,]
  return(H)
}    

HeapBottomUp(m)

compare_function_1 <- function(p1, p2) {
  comparison_criterion <- p1[1] > p2[1]
  if(comparison_criterion) {
    return(p1)
  } else {
    retrun(p2)
  }
  
  
  v<-M_c[i,]
  j<-2*i
  if (j < n){
    if (M_c[j,][1]< M_c[(j+1),][1]){
      j<-j+1
    }
  }
  if (v[1]>=M_c[j,][1]){
    heap<-TRUE
  }
  else {
    M_c[i,][1]<-M_c[j,][1]
    i<-j
  }
}

compare_function_2 <- function(p1, p2) {
  comparison_criterion <- p1[2] > p2[2]
  if(comparison_criterion) {
    return(p1)
  } else {
    retrun(p2)
  }
  
  v<-M_c[i,]
  j<-2*i
  if (j < n){
    if (M_c[j,][2]< M_c[(j+1),][2]){
      j<-j+1
    }
  }
  if (v[2]>=M_c[j,][2]){
    heap<-TRUE
  }
  else {
    M_c[i,][2]<-M_c[j,][2]
    i<-j
  }
}


HeapBottomUp_m<-function(M,comp_func){
  n<-nrow(M)
  f<-floor(n/2)
  M_c<-M
  for (i in f:1){
    k<-i
    v<-M_c[i,]
    heap<-FALSE
    while (!heap && 2*k<=n){
      j<-2*k
      if (j < n){
        if (M_c[j,][1]< M_c[(j+1),][1]){
          j<-j+1
        }
      }
      if (v[1]>=M_c[j,][1]){
        heap<-TRUE
      }
      else {
        M_c[k,][1]<-M_c[j,][1]
        k<-j
      }
    }
    M_c[k,]<-v
  }
  order_h<-match(M_c[,1],M[,1])
  H<-M[order_h,]
  return(H)
}    

HeapBottomUp_c(m)

Heapsort<-function(M,comp_func){
  l1<-nrow(M)
  l2<-ncol(M)
  M_c = M
  sortmatrix<-matrix(,nrow=l1,ncol=l2)
  i=1
  while (nrow(M_c) >1){
    M_c<-HeapBottomUp(M_c)
    n<-nrow(M_c)
    #Last row
    p_n<-M_c[n,]
    sortmatrix[i,]<-M_c[1,]
    #Exchange the first row and the last row
    M_c[1,]<-p_n
    M_c<-M_c[1:(n-1),] #argument is of length zero????
    i<-i+1
  }
  sortmatrix[l1,]<-M_c[1,]
  return(sortmatrix)
}
Heapsort(m)


