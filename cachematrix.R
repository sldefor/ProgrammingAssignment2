## These 2 functions paired together make it possible to establish a matrix
## and compute its inverse. The inverse will be cached so that the same 
## computation does not have to be done twice. 

## returns a list of functions that can be called on a matrix you enter (x)

makeCacheMatrix <- function(x = matrix()) {
 i<-NULL
  set<- function(y){
    x<<-y
    i<<-NULL
  }
  get<- function() x
  setinverse<- function(inv) i<<-inv
  getinverse<-function() i
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## takes in the result of makeCacheMatrix, and either returns a previously 
## cached matrix inverse or computes a new inverse if there is nothing in 
## the cache

cacheSolve <- function(x, ...) {
  i<-x$getinverse()
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  data<-x$get()
  i<-solve(data, ...)
  x$setinverse(i)
  i
}
