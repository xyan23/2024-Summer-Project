library(spatstat.geom)
w <- unit.square()
xy <- stratrand(w, 1, 7)
plot(w)
points(xy)
x<-xy$x

#' Conducting heapsort
#'
#' @param vec A vector.
#'
#' @return An ordered vector.
#' @export
#'
#' @examples
#' h<-c(2,9,7,6,5,8)
#' HeapBottomUp(h)
#' Heapsort(h)


HeapBottomUp<-function(vec){
  n<-length(vec)
  f<-floor(n/2)
  for (i in f:1){
    k<-i
    v<-vec[i]
    heap<-FALSE
    while (!heap && 2*k<=n){
      j<-2*k
      if (j < n){
        if (vec[j]<vec[j+1]){
          j<-j+1
        }
      }
      if (v>=vec[j]){
        heap<-TRUE
      }
      else {
        vec[k]<-vec[j]
        k<-j
      }
    }
    vec[k]<-v
  }  
    return(vec)
}    
    
Heapsort<-function(vec){
  l<-length(vec)
  sortvec<-numeric(length(vec))
  i=1
  while (length(vec)>1){
    vec<-HeapBottomUp(vec)
    n<-length(vec)
    v_n<-vec[n]
    sortvec[i]<-vec[1]
    vec[1]<-v_n
    vec<-vec[1:n-1]
    i<-i+1
  }
  sortvec[l]<-vec[1]
  return(sortvec)
}

